#' Process Swedish migration registry data for cohort studies
#'
#' Handles exclusions and censoring based on Swedish migration registry data.
#' Removes individuals who were not in Sweden at study baseline and computes
#' emigration censoring dates for those who leave during follow-up.
#'
#' @details
#' The function applies three exclusion types (matching the Stata `migrations`
#' command) and then computes censoring dates:
#'
#' \describe{
#'   \item{**Type 1 - Emigrated before start**}{Person's last emigration is
#'     before `study_start` and they never returned (or returned before
#'     emigrating again). They were not in Sweden at baseline.}
#'   \item{**Type 2 - Immigration only**}{Person's only migration record is an
#'     immigration after `study_start` with no prior emigration. They were
#'     not in the country at baseline.}
#'   \item{**Type 3 - Abroad at baseline**}{Person emigrated before
#'     `study_start` and returned after `study_start`. They were abroad at
#'     baseline even though they eventually returned.}
#' }
#'
#' After exclusions, any remaining person who emigrated during follow-up
#' (after `study_start`) and did not return gets a `migration_out_dt`
#' censoring date set to their emigration date.
#'
#' The migration file must be in wide format with paired columns: `out_1`,
#' `in_1`, `out_2`, `in_2`, etc. Each pair represents one migration episode.
#' `out_*` = emigration date, `in_*` = immigration date. Use `NA` for
#' missing dates.
#'
#' @param master A data.frame or data.table of the study cohort. Must contain
#'   at least the ID column and study start date column. Additional columns
#'   (e.g., age, sex) are preserved in the output.
#' @param migfile A data.frame or data.table with migration data in wide format.
#'   Must contain: the ID column, and paired columns `out_1`, `in_1`,
#'   `out_2`, `in_2`, etc. (Date class). Persons not in `migfile` are assumed
#'   to have no migration events (they remain in the cohort).
#' @param idvar Name of the ID variable (default `"id"`).
#' @param startvar Name of the study start date variable in `master`
#'   (default `"study_start"`).
#' @return A list with two elements:
#'   \describe{
#'     \item{`$data`}{A data.table of the remaining cohort after exclusions,
#'       with all original `master` columns preserved plus `migration_out_dt`
#'       (Date or NA). Non-NA values indicate the emigration censoring date.}
#'     \item{`$info`}{A list with:
#'       \itemize{
#'         \item `N_excluded_emigrated` - Type 1 exclusions
#'         \item `N_excluded_inmigration` - Type 2 exclusions
#'         \item `N_excluded_abroad` - Type 3 exclusions
#'         \item `N_excluded_total` - Total excluded
#'         \item `N_censored` - Persons with emigration censoring dates
#'         \item `N_final` - Remaining cohort size
#'       }}
#'   }
#'
#' @seealso [covarclose()] for extracting covariates after cohort filtering,
#'   [cci_se()] for comorbidity scoring
#'
#' @examples
#' library(data.table)
#'
#' # --- Basic censoring: person A emigrates during follow-up ---
#' master <- data.table(
#'   id = c("A", "B"),
#'   study_start = as.Date(c("2010-01-01", "2010-01-01")),
#'   age = c(45L, 62L)
#' )
#' migfile <- data.table(
#'   id = c("A"),
#'   out_1 = as.Date("2015-06-15"),
#'   in_1 = as.Date(NA),
#'   out_2 = as.Date(NA),
#'   in_2 = as.Date(NA)
#' )
#' res <- migrations(master, migfile)
#' res$data
#' #>    id study_start age migration_out_dt
#' #> 1:  A  2010-01-01  45       2015-06-15
#' #> 2:  B  2010-01-01  62             <NA>
#' res$info$N_censored  # 1
#'
#' # --- Exclusion Type 1: emigrated before start, never returned ---
#' migfile2 <- data.table(
#'   id = c("A", "B"),
#'   out_1 = as.Date(c("2005-01-01", "2008-01-01")),
#'   in_1 = as.Date(c(NA, NA)),
#'   out_2 = as.Date(c(NA, NA)),
#'   in_2 = as.Date(c(NA, NA))
#' )
#' res2 <- migrations(master, migfile2)
#' res2$info$N_excluded_emigrated  # 2
#' nrow(res2$data)                 # 0
#'
#' # --- Exclusion Type 3: abroad at baseline ---
#' migfile3 <- data.table(
#'   id = "A",
#'   out_1 = as.Date("2008-01-01"),   # emigrated before study start
#'   in_1 = as.Date("2012-06-01"),    # returned after study start
#'   out_2 = as.Date(NA),
#'   in_2 = as.Date(NA)
#' )
#' res3 <- migrations(master, migfile3)
#' res3$info$N_excluded_abroad  # 1 (person A was abroad at baseline)
#' res3$info$N_final            # 1 (only person B remains)
#'
#' # --- No migration records: everyone stays ---
#' migfile_empty <- data.table(
#'   id = character(0), out_1 = as.Date(character(0)),
#'   in_1 = as.Date(character(0)),
#'   out_2 = as.Date(character(0)), in_2 = as.Date(character(0))
#' )
#' res4 <- migrations(master, migfile_empty)
#' nrow(res4$data)  # 2 (both remain, no exclusions or censoring)
#' @export
migrations <- function(master, migfile, idvar = "id", startvar = "study_start") {
  master <- data.table::as.data.table(data.table::copy(master))
  migfile <- data.table::as.data.table(data.table::copy(migfile))

  # Validate required columns
  if (!idvar %in% names(master)) stop(sprintf("ID variable '%s' not found in master data", idvar), call. = FALSE)
  if (!startvar %in% names(master)) stop(sprintf("Study start variable '%s' not found in master data", startvar), call. = FALSE)
  if (!idvar %in% names(migfile)) stop(sprintf("ID variable '%s' not found in migration file", idvar), call. = FALSE)
  if (!"in_1" %in% names(migfile)) stop("Variable 'in_1' not found in migration file", call. = FALSE)
  if (!"out_1" %in% names(migfile)) stop("Variable 'out_1' not found in migration file", call. = FALSE)

  # Identify in_/out_ columns
  in_cols <- grep("^in_[0-9]+$", names(migfile), value = TRUE)
  out_cols <- grep("^out_[0-9]+$", names(migfile), value = TRUE)

  # Merge: keep only cohort members present in migration file
  mig <- merge(migfile[, c(idvar, in_cols, out_cols), with = FALSE],
               master[, c(idvar, startvar), with = FALSE],
               by = idvar)

  if (nrow(mig) == 0) {
    master[, migration_out_dt := as.Date(NA)]
    return(list(
      data = master,
      info = list(N_excluded_emigrated = 0L, N_excluded_inmigration = 0L,
                  N_excluded_abroad = 0L, N_excluded_total = 0L,
                  N_censored = 0L, N_final = nrow(master))
    ))
  }

  # Reshape wide -> long
  mig_long <- data.table::melt(mig, id.vars = c(idvar, startvar),
                                measure.vars = list(out_cols, in_cols),
                                variable.name = "num",
                                value.name = c("out_", "in_"))
  mig_long <- mig_long[!is.na(mig_long$out_) | !is.na(mig_long$in_)]

  # Calculate last emigration and immigration per person
  sv <- startvar  # local copy for get() scoping
  iv <- idvar
  mig_long[, last_out := max(out_, na.rm = TRUE), by = iv]
  mig_long[, last_in  := max(in_, na.rm = TRUE), by = iv]
  .inf_to_na(mig_long, "last_out")
  .inf_to_na(mig_long, "last_in")

  # EXCLUSION 1: Emigrated before study_start and never returned
  study_starts <- mig_long[[sv]]
  excl1_mask <- mig_long$last_out < study_starts & (is.na(mig_long$last_in) | mig_long$last_in < mig_long$last_out)
  excl1_ids <- unique(mig_long[[iv]][excl1_mask])

  # Remove exclusion 1 from working set
  mig_work <- mig_long[!mig_long[[iv]] %in% excl1_ids]

  if (nrow(mig_work) == 0) {
    master_out <- master[!master[[iv]] %in% excl1_ids]
    master_out[, migration_out_dt := as.Date(NA)]
    return(list(
      data = master_out,
      info = list(N_excluded_emigrated = length(excl1_ids),
                  N_excluded_inmigration = 0L,
                  N_excluded_abroad = 0L,
                  N_excluded_total = length(excl1_ids),
                  N_censored = 0L, N_final = nrow(master_out))
    ))
  }

  # EXCLUSION 3: Abroad at baseline (emigrated before study_start, returned after)
  # Row-level check: out_ < study_start AND in_ > study_start (matches Stata)
  excl3_mask <- !is.na(mig_work$out_) & mig_work$out_ < mig_work[[sv]] &
    !is.na(mig_work$in_) & mig_work$in_ > mig_work[[sv]]
  excl3_ids <- unique(mig_work[[iv]][excl3_mask])

  # Remove exclusion 3 from working set
  mig_work <- mig_work[!mig_work[[iv]] %in% excl3_ids]

  # Drop emigration records before study_start
  mig_work <- mig_work[is.na(mig_work$out_) | mig_work$out_ >= mig_work[[sv]]]

  if (nrow(mig_work) == 0) {
    all_excluded <- unique(c(excl1_ids, excl3_ids))
    master_out <- master[!master[[iv]] %in% all_excluded]
    master_out[, migration_out_dt := as.Date(NA)]
    return(list(
      data = master_out,
      info = list(N_excluded_emigrated = length(excl1_ids),
                  N_excluded_inmigration = 0L,
                  N_excluded_abroad = length(excl3_ids),
                  N_excluded_total = length(all_excluded),
                  N_censored = 0L, N_final = nrow(master_out))
    ))
  }

  # Recalculate per-person summaries
  mig_work[, last_out := suppressWarnings(max(out_, na.rm = TRUE)), by = iv]
  mig_work[, last_in  := suppressWarnings(max(in_, na.rm = TRUE)), by = iv]
  .inf_to_na(mig_work, "last_out")
  .inf_to_na(mig_work, "last_in")

  # Count migrations per person
  data.table::setorderv(mig_work, idvar)
  mig_work[, total_mig := .N, by = iv]

  # EXCLUSION 2: Only migration is immigration after study_start (no emigration)
  mig_work[, excl_inmig := FALSE]
  excl2_mask <- !is.na(mig_work$in_) & is.na(mig_work$out_) &
    mig_work$in_ > mig_work[[sv]] & mig_work$total_mig == 1L
  mig_work[excl2_mask, excl_inmig := TRUE]

  excl2_ids <- unique(mig_work[[iv]][mig_work$excl_inmig == TRUE])

  # Compute censoring: first emigration after study_start
  # Exclude people who returned after emigrating (in_ > out_)
  censor_mask <- mig_work$excl_inmig == FALSE & !is.na(mig_work$out_) & mig_work$out_ > mig_work[[sv]]
  censor <- mig_work[censor_mask]
  # Drop rows where return immigration occurred after the emigration (person came back)
  censor <- censor[is.na(censor$in_) | censor$in_ <= censor$out_]
  if (nrow(censor) > 0) {
    censor_dt <- censor[, .(migration_out_dt = min(out_)), by = iv]
  } else {
    censor_dt <- data.table::data.table(x = character(0), migration_out_dt = as.Date(character(0)))
    data.table::setnames(censor_dt, "x", idvar)
  }

  n_censored <- sum(!is.na(censor_dt$migration_out_dt))

  # Build final dataset
  all_excluded <- unique(c(excl1_ids, excl2_ids, excl3_ids))
  master_out <- master[!master[[iv]] %in% all_excluded]

  # Merge censoring dates
  master_out <- merge(master_out, censor_dt, by = idvar, all.x = TRUE)

  list(
    data = master_out,
    info = list(
      N_excluded_emigrated = length(excl1_ids),
      N_excluded_inmigration = length(excl2_ids),
      N_excluded_abroad = length(excl3_ids),
      N_excluded_total = length(all_excluded),
      N_censored = n_censored,
      N_final = nrow(master_out)
    )
  )
}
