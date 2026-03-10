#' Match procedure codes across procedure variables
#'
#' Generates a binary indicator for rows where any procedure variable matches
#' the specified KVA codes. Supports exact and prefix matching.
#'
#' @param dt A data.frame or data.table with procedure code columns.
#' @param codes Character vector of procedure codes (or space/comma-separated string).
#' @param procvars Character vector of column names containing procedure codes.
#' @param prefix Logical; if TRUE, use prefix matching (startsWith) instead of exact match.
#' @return A list with `$data` (data.table with `proc_match` column) and
#'   `$info` (n_codes, n_matches, codes).
#' @examples
#' dt <- data.table::data.table(
#'   id = 1:3,
#'   op1 = c("AB123", "CD456", "AB123"),
#'   op2 = c(NA, "EF789", NA)
#' )
#' procmatch_match(dt, codes = "AB123", procvars = c("op1", "op2"))
#' @export
procmatch_match <- function(dt, codes, procvars, prefix = FALSE) {
  dt <- data.table::as.data.table(data.table::copy(dt))

  # Normalize codes
  codes <- .procmatch_normalize_codes(codes)
  n_codes <- length(codes)
  if (n_codes == 0) stop("No valid codes specified", call. = FALSE)

  # Validate procvars exist
  missing_vars <- setdiff(procvars, names(dt))
  if (length(missing_vars) > 0) {
    stop(sprintf("Procedure variable(s) not found: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
  }

  # Initialize match indicator
  matched <- rep(FALSE, nrow(dt))

  for (pv in procvars) {
    vals <- toupper(as.character(dt[[pv]]))
    if (prefix) {
      for (code in codes) {
        matched <- matched | (!is.na(vals) & startsWith(vals, code))
      }
    } else {
      matched <- matched | (!is.na(vals) & vals %in% codes)
    }
  }

  data.table::set(dt, j = "proc_match", value = as.integer(matched))

  list(
    data = dt,
    info = list(
      varname = "proc_match",
      codes = codes,
      n_codes = n_codes,
      n_matches = sum(matched)
    )
  )
}

#' Find first occurrence of procedure codes per person
#'
#' Identifies whether each person ever had a matching procedure and extracts
#' the earliest date of that match.
#'
#' @param dt A data.frame or data.table with procedure code and date columns.
#' @param codes Character vector of procedure codes.
#' @param procvars Character vector of column names containing procedure codes.
#' @param datevar Name of the date column.
#' @param idvar Name of the person ID column.
#' @param prefix Logical; if TRUE, use prefix matching.
#' @return A list with `$data` (data.table with `proc_ever` and `proc_first_dt`
#'   columns added) and `$info`.
#' @examples
#' dt <- data.table::data.table(
#'   pid = c("A", "A", "B"),
#'   op1 = c("AB123", "CD456", "AB123"),
#'   date = as.Date(c("2015-01-01", "2016-01-01", "2017-01-01"))
#' )
#' procmatch_first(dt, codes = "AB123", procvars = "op1",
#'                 datevar = "date", idvar = "pid")
#' @export
procmatch_first <- function(dt, codes, procvars, datevar, idvar, prefix = FALSE) {
  dt <- data.table::as.data.table(data.table::copy(dt))

  # Normalize codes
  codes <- .procmatch_normalize_codes(codes)
  n_codes <- length(codes)
  if (n_codes == 0) stop("No valid codes specified", call. = FALSE)

  # Validate columns exist
  for (v in c(procvars, datevar, idvar)) {
    if (!v %in% names(dt)) stop(sprintf("Variable '%s' not found", v), call. = FALSE)
  }

  # Match rows
  matched <- rep(FALSE, nrow(dt))
  for (pv in procvars) {
    vals <- toupper(as.character(dt[[pv]]))
    if (prefix) {
      for (code in codes) {
        matched <- matched | (!is.na(vals) & startsWith(vals, code))
      }
    } else {
      matched <- matched | (!is.na(vals) & vals %in% codes)
    }
  }

  # Find first date per person where matched
  dt[, .row_match := matched]
  dv <- datevar
  first_dates <- dt[.row_match == TRUE,
                    .(proc_first_dt = suppressWarnings(min(get(dv), na.rm = TRUE))),
                    by = idvar]
  first_dates <- first_dates[is.finite(as.numeric(proc_first_dt))]

  # Merge back
  dt[, .row_match := NULL]
  dt <- merge(dt, first_dates, by = idvar, all.x = TRUE)
  dt[, proc_ever := as.integer(!is.na(proc_first_dt))]

  # Count unique persons with match
  n_persons <- first_dates[, .N]
  n_matches <- sum(matched)

  list(
    data = dt,
    info = list(
      varname = "proc_ever",
      datevarname = "proc_first_dt",
      codes = codes,
      n_codes = n_codes,
      n_persons = n_persons,
      n_matches = n_matches
    )
  )
}

# Internal: normalize codes from string or vector
.procmatch_normalize_codes <- function(codes) {
  if (length(codes) == 1 && is.character(codes)) {
    codes <- unlist(strsplit(gsub(",", " ", codes), "\\s+"))
  }
  codes <- toupper(trimws(codes))
  codes <- codes[codes != ""]
  codes
}
