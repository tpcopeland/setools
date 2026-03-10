#' Extract closest covariate values to an index date
#'
#' From longitudinal covariate data (e.g., LISA), extracts the observation
#' closest to each person's index date. Supports preference for before/after/closest
#' and optional forward/backward fill imputation.
#'
#' @param master A data.frame or data.table with ID and index date columns.
#' @param using A data.frame or data.table with longitudinal covariate data.
#' @param idvar Name of the ID column (must exist in both datasets).
#' @param indexdate Name of the index date column in master (Date class).
#' @param datevar Name of the date column in the covariate file.
#' @param vars Character vector of covariate column names to extract.
#' @param prefer Preference for timing: `"closest"` (default), `"before"`, or `"after"`.
#' @param yearformat Logical; if TRUE, treat datevar as a year and convert to
#'   July 1 of that year.
#' @param impute Logical; if TRUE, fill missing values using forward then backward
#'   fill within person before selecting closest.
#' @param missing_codes Numeric vector of values to treat as missing during imputation.
#' @return A list with `$data` (master data.table with extracted covariate columns
#'   merged) and `$info` containing n_total, vars, prefer.
#' @examples
#' master <- data.table::data.table(
#'   id = c("A", "B"),
#'   index_dt = as.Date(c("2015-06-01", "2016-01-15"))
#' )
#' covar_data <- data.table::data.table(
#'   id = rep(c("A", "B"), each = 3),
#'   date = as.Date(c("2014-01-01", "2015-01-01", "2016-01-01",
#'                     "2015-01-01", "2016-01-01", "2017-01-01")),
#'   income = c(300, 320, 340, 400, 420, 440)
#' )
#' covarclose(master, covar_data, idvar = "id", indexdate = "index_dt",
#'            datevar = "date", vars = "income")
#' @export
covarclose <- function(master, using, idvar, indexdate, datevar, vars,
                       prefer = "closest", yearformat = FALSE,
                       impute = FALSE, missing_codes = NULL) {
  if (!prefer %in% c("before", "after", "closest")) {
    stop("prefer must be: before, after, or closest", call. = FALSE)
  }

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
      covar[, (v) := nafill_locf(get(v)), by = idvar]
      # Backward fill within person
      data.table::setorderv(covar, c(idvar, ".covar_date"), order = c(1L, -1L))
      covar[, (v) := nafill_locf(get(v)), by = idvar]
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
nafill_locf <- function(x) {
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
