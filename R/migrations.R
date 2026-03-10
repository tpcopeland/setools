#' Process Swedish migration registry data for cohort studies
#'
#' Handles exclusions and censoring based on Swedish migration registry data.
#' Removes individuals who were not in Sweden at study baseline and computes
#' emigration censoring dates.
#'
#' @param master A data.frame or data.table of the study cohort.
#' @param migfile A data.frame or data.table with migration data in wide format
#'   (columns: id, in_1, in_2, ..., out_1, out_2, ...).
#' @param idvar Name of the ID variable (default `"id"`).
#' @param startvar Name of the study start date variable (default `"study_start"`).
#' @return A list with `$data` (data.table of remaining cohort with
#'   `migration_out_dt` added for censoring) and `$info` containing
#'   N_excluded_emigrated, N_excluded_inmigration, N_excluded_abroad,
#'   N_excluded_total, N_censored, N_final.
#' @examples
#' master <- data.table::data.table(
#'   id = c("A", "B"),
#'   study_start = as.Date(c("2010-01-01", "2010-01-01"))
#' )
#' migfile <- data.table::data.table(
#'   id = c("A", "B"),
#'   out_1 = as.Date(c("2012-06-01", NA)),
#'   in_1 = as.Date(c(NA, NA))
#' )
#' migrations(master, migfile)
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
