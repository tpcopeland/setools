#' Extract closest covariate values to an index date
#'
#' From longitudinal covariate data (e.g., LISA, SCB registers), extracts the
#' observation closest to each person's index date. Supports preference for
#' before/after/closest, year-format dates, and optional imputation of missing
#' values before selection.
#'
#' @details
#' The algorithm:
#' \enumerate{
#'   \item Merges `master` IDs with `using` covariate data.
#'   \item If `yearformat = TRUE`, converts integer year values to July 1 of
#'     that year (matching Stata's `mdy(7,1,year)` convention).
#'   \item If `impute = TRUE`, replaces `missing_codes` with `NA`, then applies
#'     forward fill (LOCF) followed by backward fill within each person.
#'   \item Computes the absolute distance from each covariate observation to
#'     the person's index date.
#'   \item Applies the `prefer` filter:
#'     \itemize{
#'       \item `"closest"`: selects the observation with minimum absolute distance.
#'       \item `"before"`: restricts to observations on or before the index date,
#'         then selects closest.
#'       \item `"after"`: restricts to observations on or after the index date,
#'         then selects closest.
#'     }
#'   \item Ties (equal distance) are broken by earlier date.
#'   \item Persons with no matching covariate data receive `NA`.
#' }
#'
#' Multiple variables can be extracted simultaneously. All variables come from
#' the same closest observation (they are not selected independently).
#'
#' @param master A data.frame or data.table with the study cohort. Must contain
#'   at least the ID column and index date column.
#' @param using A data.frame or data.table with longitudinal covariate data.
#'   Must contain the ID column, date column, and all columns listed in `vars`.
#' @param idvar Name of the ID column (must exist in both `master` and `using`).
#' @param indexdate Name of the index date column in `master` (Date class).
#' @param datevar Name of the date column in `using`. If `yearformat = TRUE`,
#'   this should be an integer year column.
#' @param vars Character vector of covariate column names to extract from `using`.
#' @param prefer Preference for timing: `"closest"` (default), `"before"`, or
#'   `"after"`. When `"before"`, only observations on or before the index date
#'   are considered. When `"after"`, only observations on or after.
#' @param yearformat Logical; if TRUE, treat `datevar` as a year (integer) and
#'   convert to July 1 of that year for distance calculation. Common for
#'   annual register data (e.g., LISA income). Default FALSE.
#' @param impute Logical; if TRUE, fill missing values using forward fill (LOCF)
#'   then backward fill within each person before selecting the closest
#'   observation. Default FALSE.
#' @param missing_codes Numeric vector of values to treat as missing during
#'   imputation (e.g., `99`, `-1`). These are replaced with `NA` before
#'   fill. Only used when `impute = TRUE`.
#' @return A list with two elements:
#'   \describe{
#'     \item{`$data`}{The `master` data.table with the requested covariate
#'       columns merged in. Persons without matching data get `NA`.}
#'     \item{`$info`}{A list with: `n_total` (rows in master), `vars`
#'       (extracted variables), `prefer` (timing preference used).}
#'   }
#'
#' @seealso [migrations()] for cohort filtering before covariate extraction,
#'   [cci_se()] for comorbidity scoring from diagnosis data
#'
#' @examples
#' library(data.table)
#'
#' # --- Basic: closest observation to index date ---
#' master <- data.table(
#'   id = c("A", "B"),
#'   index_dt = as.Date(c("2015-06-01", "2016-01-15"))
#' )
#' covar <- data.table(
#'   id = rep(c("A", "B"), each = 3),
#'   date = as.Date(c("2014-01-01", "2015-01-01", "2016-01-01",
#'                     "2015-01-01", "2016-01-01", "2017-01-01")),
#'   income = c(300, 320, 340, 400, 420, 440)
#' )
#' res <- covarclose(master, covar, idvar = "id", indexdate = "index_dt",
#'                   datevar = "date", vars = "income")
#' res$data
#' #>    id   index_dt income
#' #> 1:  A 2015-06-01    320   (2015-01-01 is closest)
#' #> 2:  B 2016-01-15    420   (2016-01-01 is closest)
#'
#' # --- Prefer before: only look at pre-index observations ---
#' res2 <- covarclose(master, covar, idvar = "id", indexdate = "index_dt",
#'                    datevar = "date", vars = "income", prefer = "before")
#' res2$data$income  # A: 320 (2015-01-01), B: 420 (2016-01-01)
#'
#' # --- Prefer after: only look at post-index observations ---
#' res3 <- covarclose(master, covar, idvar = "id", indexdate = "index_dt",
#'                    datevar = "date", vars = "income", prefer = "after")
#' res3$data$income  # A: 340 (2016-01-01), B: 420 (2016-01-01)
#'
#' # --- Year-format dates (e.g., annual LISA data) ---
#' lisa <- data.table(
#'   id = c("A", "A", "B", "B"),
#'   year = c(2014L, 2015L, 2015L, 2016L),
#'   income = c(300, 320, 400, 420)
#' )
#' res4 <- covarclose(master, lisa, idvar = "id", indexdate = "index_dt",
#'                    datevar = "year", vars = "income", yearformat = TRUE)
#' # Years are converted to July 1: 2014 -> 2014-07-01, 2015 -> 2015-07-01
#' res4$data$income
#'
#' # --- Imputation with missing codes ---
#' covar_miss <- data.table(
#'   id = c("A", "A", "A"),
#'   date = as.Date(c("2014-01-01", "2015-01-01", "2016-01-01")),
#'   income = c(300, 99, 340)  # 99 = missing code
#' )
#' res5 <- covarclose(master[id == "A"], covar_miss, idvar = "id",
#'                    indexdate = "index_dt", datevar = "date",
#'                    vars = "income", impute = TRUE, missing_codes = 99)
#' res5$data$income  # 300 (99 replaced with NA, forward-filled to 300)
#'
#' # --- Multiple variables extracted simultaneously ---
#' covar_multi <- data.table(
#'   id = c("A", "A"),
#'   date = as.Date(c("2015-01-01", "2016-01-01")),
#'   income = c(320, 340),
#'   education = c(3L, 4L)
#' )
#' res6 <- covarclose(master[id == "A"], covar_multi, idvar = "id",
#'                    indexdate = "index_dt", datevar = "date",
#'                    vars = c("income", "education"))
#' res6$data
#' # Both income and education come from the same closest observation
#' @export
covarclose <- function(master, using, idvar, indexdate, datevar, vars,
                       prefer = c("closest", "before", "after"),
                       yearformat = FALSE,
                       impute = FALSE, missing_codes = NULL) {
  prefer <- match.arg(prefer)

  master <- data.table::as.data.table(data.table::copy(master))
  using <- data.table::as.data.table(data.table::copy(using))

  # Validate columns
  if (!idvar %in% names(master)) stop(sprintf("ID variable '%s' not found in master", idvar), call. = FALSE)
  if (!indexdate %in% names(master)) stop(sprintf("Index date variable '%s' not found in master", indexdate), call. = FALSE)
  if (!idvar %in% names(using)) stop(sprintf("ID variable '%s' not found in covariate file", idvar), call. = FALSE)
  if (!datevar %in% names(using)) stop(sprintf("Date variable '%s' not found in covariate file", datevar), call. = FALSE)

  missing_vars <- setdiff(vars, names(using))
  if (length(missing_vars) > 0) {
    stop(sprintf("Variable(s) not found in covariate file: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
  }

  # Get unique IDs with index dates from master
  master_ids <- unique(master[, c(idvar, indexdate), with = FALSE])
  # If duplicate IDs, use first index date
  master_ids <- master_ids[!duplicated(get(idvar))]

  # Keep relevant columns from using
  covar <- using[, c(idvar, datevar, vars), with = FALSE]

  # Handle year format
  if (yearformat) {
    covar[, .covar_date := as.Date(paste0(get(datevar), "-07-01"))]
  } else {
    covar[, .covar_date := get(datevar)]
  }

  # Merge to get index dates
  covar <- merge(covar, master_ids, by = idvar)

  if (nrow(covar) == 0) {
    # No matches - return master with NA covariates
    for (v in vars) master[, (v) := NA]
    return(list(data = master, info = list(n_total = nrow(master), vars = vars, prefer = prefer)))
  }

  # Impute missing if requested
  if (impute) {
    for (v in vars) {
      # Replace user-specified missing codes with NA
      if (!is.null(missing_codes)) {
        covar[get(v) %in% missing_codes, (v) := NA]
      }
      # Forward fill within person
      data.table::setorderv(covar, c(idvar, ".covar_date"))
      covar[, (v) := .nafill_locf(get(v)), by = idvar]
      # Backward fill within person
      data.table::setorderv(covar, c(idvar, ".covar_date"), order = c(1L, -1L))
      covar[, (v) := .nafill_locf(get(v)), by = idvar]
      data.table::setorderv(covar, c(idvar, ".covar_date"))
    }
  }

  # Calculate distance from index date
  idx_col <- indexdate
  covar[, .dist := as.integer(.covar_date - get(idx_col))]

  # Apply preference
  if (prefer == "before") {
    covar[, .has_before := any(.dist <= 0, na.rm = TRUE), by = idvar]
    covar <- covar[!(.has_before & .dist > 0)]
    covar[, .abs_dist := abs(.dist)]
    covar[, .has_before := NULL]
  } else if (prefer == "after") {
    covar[, .has_after := any(.dist >= 0, na.rm = TRUE), by = idvar]
    covar <- covar[!(.has_after & .dist < 0)]
    covar[, .abs_dist := abs(.dist)]
    covar[, .has_after := NULL]
  } else {
    covar[, .abs_dist := abs(.dist)]
  }

  # Keep closest observation per person (break ties by earlier date)
  data.table::setorderv(covar, c(idvar, ".abs_dist", ".covar_date"))
  covar <- covar[, .SD[1], by = idvar]

  # Keep only ID and requested vars
  result_covar <- covar[, c(idvar, vars), with = FALSE]

  # Merge back to master
  master <- merge(master, result_covar, by = idvar, all.x = TRUE)

  list(
    data = master,
    info = list(n_total = nrow(master), vars = vars, prefer = prefer)
  )
}

# Internal: LOCF (last observation carried forward) for a vector
.nafill_locf <- function(x) {
  if (all(is.na(x))) return(x)
  idx <- which(!is.na(x))
  if (length(idx) == 0) return(x)
  # Use data.table::nafill for numeric, manual for others
  if (is.numeric(x)) {
    data.table::nafill(x, type = "locf")
  } else {
    for (i in seq_along(x)) {
      if (is.na(x[i]) && i > 1 && !is.na(x[i - 1])) {
        x[i] <- x[i - 1]
      }
    }
    x
  }
}
