#' Parse a date string to R Date object
#'
#' Auto-detects format from common Swedish registry patterns (ISO YYYY-MM-DD,
#' compact YYYYMMDD, European DD/MM/YYYY, Stata text DDmonYYYY). Defaults to
#' YMD for Swedish data.
#'
#' @details
#' Auto-detection rules:
#' \itemize{
#'   \item `YYYY-MM-DD` (e.g., "2020-01-15") -> YMD
#'   \item `YYYYMMDD` (e.g., "20200115") -> YMD
#'   \item `DD/MM/YYYY` (e.g., "15/01/2020") -> DMY
#'   \item `DDmonYYYY` (e.g., "15jan2020") -> DMY (Stata text format)
#'   \item Everything else -> YMD (Swedish default)
#' }
#'
#' If auto-detection fails, all format families are tried as fallback.
#'
#' @param datestring Character string representing a date.
#' @param format Optional format hint: `"YMD"`, `"DMY"`, or `"MDY"`. If NULL
#'   (default), auto-detected from the string pattern.
#' @return A list with `$date` (R Date object) and `$datestr` (original string).
#'
#' @seealso [dateparse_validate()] for checking date ranges,
#'   [dateparse_window()] for computing time windows
#'
#' @examples
#' # ISO format (most common in Swedish registers)
#' dateparse_parse("2020-01-15")$date
#' #> [1] "2020-01-15"
#'
#' # Compact YYYYMMDD (common in integer date columns)
#' dateparse_parse("20200115")$date
#'
#' # European DD/MM/YYYY
#' dateparse_parse("15/01/2020")$date
#'
#' # Stata text format
#' dateparse_parse("15jan2020")$date
#'
#' # Explicit format hint
#' dateparse_parse("01/15/2020", format = "MDY")$date
#' @export
dateparse_parse <- function(datestring, format = NULL) {
  datestring <- trimws(datestring)
  if (is.na(datestring) || datestring == "") {
    stop("Empty date string provided", call. = FALSE)
  }

  if (is.null(format)) {
    # Auto-detect format based on pattern
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", datestring)) {
      format <- "YMD"
    } else if (grepl("^[0-9]{8}$", datestring)) {
      format <- "YMD"
    } else if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", datestring)) {
      format <- "DMY"
    } else if (grepl("^[0-9]{1,2}[a-zA-Z]{3}[0-9]{4}$", datestring)) {
      format <- "DMY"
    } else {
      format <- "YMD"
    }
  }

  format <- toupper(format)

  # Map to R strptime format strings
  fmt_map <- list(
    YMD = c("%Y-%m-%d", "%Y%m%d"),
    DMY = c("%d/%m/%Y", "%d%b%Y", "%d-%m-%Y"),
    MDY = c("%m/%d/%Y", "%m-%d-%Y")
  )

  # Try the requested format first
  result <- NA
  if (format %in% names(fmt_map)) {
    for (f in fmt_map[[format]]) {
      result <- as.Date(datestring, format = f)
      if (!is.na(result)) break
    }
  }

  # Fallback: try all formats
  if (is.na(result)) {
    for (fmt_group in fmt_map) {
      for (f in fmt_group) {
        result <- as.Date(datestring, format = f)
        if (!is.na(result)) break
      }
      if (!is.na(result)) break
    }
  }

  if (is.na(result)) {
    stop(sprintf("Could not parse date: %s\nTried formats: YMD (YYYY-MM-DD), DMY, MDY", datestring), call. = FALSE)
  }

  list(date = result, datestr = datestring)
}

#' Calculate lookback/followup window dates around an index date
#'
#' Creates start and end dates for a time window around each observation's
#' index date. Adds `window_start` and `window_end` columns.
#'
#' @details
#' Window behavior depends on which periods are specified:
#' \itemize{
#'   \item Both lookback and followup: `window_start = index - lookback`,
#'     `window_end = index + followup`
#'   \item Lookback only: `window_start = index - lookback`,
#'     `window_end = index - 1` (day before index)
#'   \item Followup only: `window_start = index + 1` (day after index),
#'     `window_end = index + followup`
#' }
#'
#' @param dt A data.frame or data.table with an index date column.
#' @param indexvar Name of the index date column (Date class).
#' @param lookback Number of days to look back (non-negative integer).
#' @param followup Number of days to follow up (non-negative integer).
#' @return A list with two elements:
#'   \describe{
#'     \item{`$data`}{The input data.table with `window_start` and
#'       `window_end` columns added (Date class).}
#'     \item{`$info`}{A list with `lookback` and `followup` values.}
#'   }
#'
#' @seealso [dateparse_inwindow()] for checking if dates fall within a window,
#'   [dateparse_filerange()] for determining which year files to load
#'
#' @examples
#' library(data.table)
#'
#' # --- Both lookback and followup ---
#' dt <- data.table(
#'   pid = c("A", "B"),
#'   index = as.Date(c("2020-01-01", "2020-06-15"))
#' )
#' res <- dateparse_window(dt, "index", lookback = 365, followup = 180)
#' res$data
#' #>    pid      index window_start window_end
#' #> 1:   A 2020-01-01   2019-01-01 2020-06-29
#' #> 2:   B 2020-06-15   2019-06-16 2020-12-12
#'
#' # --- Lookback only (pre-index period) ---
#' res2 <- dateparse_window(dt, "index", lookback = 365)
#' res2$data$window_end  # index - 1 day
#'
#' # --- Followup only (post-index period) ---
#' res3 <- dateparse_window(dt, "index", followup = 180)
#' res3$data$window_start  # index + 1 day
#' @export
dateparse_window <- function(dt, indexvar, lookback = 0L, followup = 0L) {
  if (lookback < 0) stop("lookback must be non-negative", call. = FALSE)
  if (followup < 0) stop("followup must be non-negative", call. = FALSE)
  if (lookback == 0 && followup == 0) {
    stop("Must specify either lookback or followup or both", call. = FALSE)
  }

  dt <- data.table::as.data.table(data.table::copy(dt))

  if (!indexvar %in% names(dt)) {
    stop(sprintf("Index date variable '%s' not found", indexvar), call. = FALSE)
  }

  idx <- dt[[indexvar]]

  if (lookback > 0 && followup > 0) {
    data.table::set(dt, j = "window_start", value = idx - lookback)
    data.table::set(dt, j = "window_end", value = idx + followup)
  } else if (lookback > 0) {
    data.table::set(dt, j = "window_start", value = idx - lookback)
    data.table::set(dt, j = "window_end", value = idx - 1L)
  } else {
    data.table::set(dt, j = "window_start", value = idx + 1L)
    data.table::set(dt, j = "window_end", value = idx + followup)
  }

  list(
    data = dt,
    info = list(lookback = lookback, followup = followup)
  )
}

#' Validate a date range (start before end)
#'
#' Parses start and end date strings (or Date objects), validates that start
#' is not after end, and computes the span in days and years.
#'
#' @param start Start date as a character string or Date object.
#' @param end End date as a character string or Date object.
#' @param format Optional format hint passed to [dateparse_parse()]. Ignored
#'   if Date objects are provided.
#' @return A list with:
#'   \describe{
#'     \item{`$start_date`}{Parsed start Date}
#'     \item{`$end_date`}{Parsed end Date}
#'     \item{`$span_days`}{Integer span in days (inclusive)}
#'     \item{`$span_years`}{Approximate span in years (rounded to 1 decimal)}
#'   }
#'
#' @examples
#' # String input
#' dateparse_validate("2010-01-01", "2020-12-31")
#' #> $span_days = 4018, $span_years = 11.0
#'
#' # Date object input
#' dateparse_validate(as.Date("2010-01-01"), as.Date("2020-12-31"))
#'
#' # Throws error if start > end
#' \dontrun{
#' dateparse_validate("2020-01-01", "2010-01-01")
#' }
#' @export
dateparse_validate <- function(start, end, format = NULL) {
  # Handle Date objects directly without re-parsing
  if (inherits(start, "Date")) {
    s <- start
  } else {
    s <- dateparse_parse(start, format)$date
  }
  if (inherits(end, "Date")) {
    e <- end
  } else {
    e <- dateparse_parse(end, format)$date
  }

  if (s > e) {
    stop(sprintf("Start date (%s) is after end date (%s)", start, end), call. = FALSE)
  }

  span_days <- as.integer(e - s) + 1L
  span_years <- round(span_days / 365.25, 1)

  list(
    start_date = s,
    end_date = e,
    span_days = span_days,
    span_years = span_years
  )
}

#' Check if dates fall within a window
#'
#' Adds a logical indicator column (`in_window`) for each row where the date
#' variable falls within the range \code{[start, end]} (inclusive on both ends).
#'
#' @param dt A data.frame or data.table.
#' @param datevar Name of the date column to check (Date class).
#' @param start Either a column name in `dt` containing per-row start dates, or
#'   a date string for a fixed window start (parsed via [dateparse_parse()]).
#' @param end Either a column name in `dt` containing per-row end dates, or a
#'   date string for a fixed window end.
#' @return A list with two elements:
#'   \describe{
#'     \item{`$data`}{The input data.table with `in_window` column added
#'       (logical: TRUE/FALSE).}
#'     \item{`$info`}{A list with `n_inwindow` (count of TRUE rows).}
#'   }
#'
#' @seealso [dateparse_window()] for creating window columns
#'
#' @examples
#' library(data.table)
#'
#' # --- Fixed window (date strings) ---
#' dt <- data.table(
#'   id = 1:4,
#'   visit = as.Date(c("2019-12-01", "2020-03-01", "2020-07-15", "2021-01-10"))
#' )
#' res <- dateparse_inwindow(dt, "visit", "2020-01-01", "2020-12-31")
#' res$data$in_window
#' #> [1] FALSE  TRUE  TRUE FALSE
#' res$info$n_inwindow  # 2
#'
#' # --- Per-row window (column names) ---
#' dt2 <- data.table(
#'   id = 1:3,
#'   visit = as.Date(c("2020-03-01", "2020-07-15", "2021-01-10")),
#'   win_start = as.Date(c("2020-01-01", "2020-06-01", "2020-01-01")),
#'   win_end = as.Date(c("2020-06-30", "2020-12-31", "2020-12-31"))
#' )
#' res2 <- dateparse_inwindow(dt2, "visit", "win_start", "win_end")
#' res2$data$in_window
#' #> [1]  TRUE  TRUE FALSE
#' @export
dateparse_inwindow <- function(dt, datevar, start, end) {
  dt <- data.table::as.data.table(data.table::copy(dt))

  if (!datevar %in% names(dt)) {
    stop(sprintf("Date variable '%s' not found", datevar), call. = FALSE)
  }

  dates <- dt[[datevar]]

  # Resolve start: column name or date string

  if (is.character(start) && length(start) == 1 && start %in% names(dt)) {
    s <- dt[[start]]
  } else {
    s <- dateparse_parse(as.character(start))$date
  }

  # Resolve end: column name or date string
  if (is.character(end) && length(end) == 1 && end %in% names(dt)) {
    e <- dt[[end]]
  } else {
    e <- dateparse_parse(as.character(end))$date
  }

  in_win <- !is.na(dates) & dates >= s & dates <= e
  in_win[is.na(in_win)] <- FALSE
  data.table::set(dt, j = "in_window", value = in_win)

  list(
    data = dt,
    info = list(n_inwindow = sum(in_win))
  )
}

#' Determine which year files are needed for a date range
#'
#' Calculates the range of year-based registry files needed given index dates
#' and lookback/followup periods. Useful when Swedish registers are delivered
#' as one file per year (e.g., NPR, LISA).
#'
#' @param index_start Start of index date range (date string or YYYY-MM-DD).
#' @param index_end End of index date range (date string or YYYY-MM-DD).
#' @param lookback Lookback period in days (default 0).
#' @param followup Followup period in days (default 0).
#' @return A list with:
#'   \describe{
#'     \item{`$index_start_year`}{Year of the earliest index date}
#'     \item{`$index_end_year`}{Year of the latest index date}
#'     \item{`$file_start_year`}{Earliest year file needed (accounting for
#'       lookback)}
#'     \item{`$file_end_year`}{Latest year file needed (accounting for
#'       followup)}
#'     \item{`$lookback_years`}{Lookback period in years (ceiling)}
#'     \item{`$followup_years`}{Followup period in years (ceiling)}
#'   }
#'
#' @examples
#' # Index dates 2010-2015, 10-year lookback, 1-year followup
#' res <- dateparse_filerange("2010-01-01", "2015-12-31",
#'                            lookback = 3650, followup = 365)
#' res$file_start_year  # 2000 (2010 - 10 years)
#' res$file_end_year    # 2016 (2015 + 1 year)
#'
#' # No lookback or followup: just the index date range
#' dateparse_filerange("2010-01-01", "2015-12-31")
#' @export
dateparse_filerange <- function(index_start, index_end, lookback = 0L, followup = 0L) {
  idx_s <- dateparse_parse(index_start)$date
  idx_e <- dateparse_parse(index_end)$date

  earliest <- idx_s - lookback
  latest <- idx_e + followup

  list(
    index_start_year = as.integer(format(idx_s, "%Y")),
    index_end_year = as.integer(format(idx_e, "%Y")),
    file_start_year = as.integer(format(earliest, "%Y")),
    file_end_year = as.integer(format(latest, "%Y")),
    lookback_years = ceiling(lookback / 365.25),
    followup_years = ceiling(followup / 365.25)
  )
}
