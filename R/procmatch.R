#' Match procedure codes across procedure variables
#'
#' Generates a binary indicator for rows where any of the procedure code
#' columns matches the specified codes. Supports exact matching and prefix
#' (starts-with) matching. Codes are compared case-insensitively.
#'
#' @details
#' Procedure code data in Swedish registries typically has multiple procedure
#' columns per row (e.g., `op1` through `op20`). This function checks all
#' specified columns and sets `proc_match = 1` if any column matches any of
#' the target codes.
#'
#' Codes can be provided as a character vector, a single space-separated
#' string, or a comma-separated string. All codes are normalized to uppercase
#' before matching.
#'
#' With `prefix = TRUE`, matching uses `startsWith()` so code `"ABC"` matches
#' `"ABC10"`, `"ABC20"`, etc. This is useful for matching code groups.
#'
#' @param dt A data.frame or data.table with procedure code columns.
#' @param codes Character vector of procedure codes to match. Also accepts a
#'   single space-separated or comma-separated string (e.g., `"ABC10 DEF20"`
#'   or `"ABC10, DEF20"`).
#' @param procvars Character vector of column names containing procedure codes
#'   to search across.
#' @param prefix Logical; if TRUE, use prefix matching (`startsWith`) instead
#'   of exact match. Default FALSE.
#' @return A list with two elements:
#'   \describe{
#'     \item{`$data`}{The input data.table with a `proc_match` column added
#'       (1 = matched, 0 = not matched).}
#'     \item{`$info`}{A list with: `varname` ("proc_match"), `codes` (normalized
#'       code vector), `n_codes`, `n_matches` (number of rows matching).}
#'   }
#'
#' @seealso [procmatch_first()] for finding the earliest match per person
#'
#' @examples
#' library(data.table)
#'
#' # --- Exact matching across two procedure columns ---
#' dt <- data.table(
#'   id = 1:4,
#'   op1 = c("ABC10", "DEF20", "ABC15", "GHI30"),
#'   op2 = c("XYZ99", "ABC10", NA, "DEF20")
#' )
#' res <- procmatch_match(dt, codes = "ABC10", procvars = c("op1", "op2"))
#' res$data$proc_match
#' #> [1] 1 1 0 0
#' # Row 1: op1 matches. Row 2: op2 matches. Rows 3-4: no match.
#'
#' # --- Prefix matching: all codes starting with "ABC" ---
#' res2 <- procmatch_match(dt, codes = "ABC", procvars = c("op1", "op2"),
#'                         prefix = TRUE)
#' res2$data$proc_match
#' #> [1] 1 1 1 0
#' # Row 3 now matches (ABC15 starts with ABC)
#'
#' # --- Multiple codes as a single string ---
#' res3 <- procmatch_match(dt, codes = "ABC10 DEF20",
#'                         procvars = c("op1", "op2"))
#' res3$data$proc_match
#' #> [1] 1 1 0 1
#' res3$info$n_matches  # 3
#'
#' # --- Comma-separated codes ---
#' res4 <- procmatch_match(dt, codes = "ABC10, GHI30",
#'                         procvars = c("op1", "op2"))
#' res4$data$proc_match
#' #> [1] 1 1 0 1
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
#' Identifies whether each person ever had a matching procedure code and
#' extracts the earliest date of that match. Useful for determining time to
#' first procedure or creating binary exposure indicators.
#'
#' @details
#' The function first matches rows using the same logic as [procmatch_match()],
#' then collapses by person to find the earliest matching date. Two columns
#' are added to the original data:
#' \itemize{
#'   \item `proc_ever` - Binary indicator (1 = person has at least one match,
#'     0 = never matched)
#'   \item `proc_first_dt` - Date of the earliest match for that person
#'     (NA if `proc_ever = 0`)
#' }
#'
#' These values are repeated across all rows for each person (merged back to
#' the original data).
#'
#' @param dt A data.frame or data.table with procedure code columns and a
#'   date column (one row per visit/episode).
#' @param codes Character vector of procedure codes to match. Also accepts
#'   space-separated or comma-separated strings.
#' @param procvars Character vector of column names containing procedure codes.
#' @param datevar Name of the date column (Date class).
#' @param idvar Name of the person ID column.
#' @param prefix Logical; if TRUE, use prefix matching. Default FALSE.
#' @return A list with two elements:
#'   \describe{
#'     \item{`$data`}{The input data.table with `proc_ever` (integer 0/1)
#'       and `proc_first_dt` (Date or NA) columns added.}
#'     \item{`$info`}{A list with: `varname` ("proc_ever"),
#'       `datevarname` ("proc_first_dt"), `codes`, `n_codes`,
#'       `n_persons` (unique persons with a match),
#'       `n_matches` (total matching rows).}
#'   }
#'
#' @seealso [procmatch_match()] for row-level matching without collapsing
#'
#' @examples
#' library(data.table)
#'
#' # --- Find earliest procedure date per person ---
#' dt <- data.table(
#'   pid = c("A", "A", "A", "B", "B"),
#'   op1 = c("ABC10", "ABC10", "DEF20", "GHI30", "ABC10"),
#'   date = as.Date(c("2015-01-01", "2016-01-01", "2017-01-01",
#'                     "2015-06-01", "2018-01-01"))
#' )
#' res <- procmatch_first(dt, codes = "ABC10", procvars = "op1",
#'                        datevar = "date", idvar = "pid")
#' res$data
#' # A: proc_ever=1, proc_first_dt=2015-01-01 (earliest of two matches)
#' # B: proc_ever=1, proc_first_dt=2018-01-01
#'
#' res$info$n_persons  # 2 (both A and B matched)
#'
#' # --- Person with no matching codes ---
#' dt2 <- data.table(
#'   pid = c("A", "B"),
#'   op1 = c("ABC10", "XYZ99"),
#'   date = as.Date(c("2015-01-01", "2016-01-01"))
#' )
#' res2 <- procmatch_first(dt2, codes = "ABC10", procvars = "op1",
#'                         datevar = "date", idvar = "pid")
#' res2$data[pid == "B", proc_ever]     # 0
#' res2$data[pid == "B", proc_first_dt] # NA
#'
#' # --- Prefix matching with multiple procedure columns ---
#' dt3 <- data.table(
#'   pid = c("A", "A"),
#'   op1 = c("AB001", "CD999"),
#'   op2 = c(NA, "AB050"),
#'   date = as.Date(c("2015-01-01", "2016-01-01"))
#' )
#' res3 <- procmatch_first(dt3, codes = "AB", procvars = c("op1", "op2"),
#'                         datevar = "date", idvar = "pid", prefix = TRUE)
#' res3$data[pid == "A", proc_first_dt]  # 2015-01-01 (AB001 in op1)
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
