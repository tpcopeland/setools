#' Parse a date string to R Date object
#'
#' Auto-detects format from common Swedish registry patterns (ISO YYYY-MM-DD,
#' compact YYYYMMDD, European DD/MM/YYYY). Defaults to YMD for Swedish data.
#'
#' @param datestring Character string representing a date.
#' @param format Optional format hint: "YMD", "DMY", or "MDY". If NULL,
#'   auto-detected from pattern.
#' @return A list with `$date` (Date object) and `$datestr` (original string).
#' @examples
#' dateparse_parse("2020-01-15")
#' dateparse_parse("20200115")
#' dateparse_parse("15/01/2020", format = "DMY")
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
#' index date. Returns a modified data.table with window variables added.
#'
#' @param dt A data.frame or data.table with an index date column.
#' @param indexvar Name of the index date column (Date class).
#' @param lookback Number of days to look back (non-negative).
#' @param followup Number of days to follow up (non-negative).
#' @return A list with `$data` (data.table with added window_start and/or
#'   window_end columns) and `$info` containing lookback and followup values.
#' @examples
#' dt <- data.table::data.table(id = 1:3, index = as.Date("2020-01-01"))
#' dateparse_window(dt, "index", lookback = 365, followup = 180)
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
#' Parses start and end date strings, validates ordering, and computes span.
#'
#' @param start Start date string.
#' @param end End date string.
#' @param format Optional format hint passed to `dateparse_parse()`.
#' @return A list with `$start_date`, `$end_date` (Date objects),
#'   `$span_days`, `$span_years`.
#' @examples
#' dateparse_validate("2010-01-01", "2020-12-31")
#' dateparse_validate(as.Date("2010-01-01"), as.Date("2020-12-31"))
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
#' Generates a logical indicator for each row where the date variable falls
#' within the range from start to end (inclusive).
#'
#' @param dt A data.frame or data.table.
#' @param datevar Name of the date column to check.
#' @param start Either a column name in dt or a date string for the window start.
#' @param end Either a column name in dt or a date string for the window end.
#' @return A list with `$data` (data.table with `in_window` column added) and
#'   `$info` containing `n_inwindow`.
#' @examples
#' dt <- data.table::data.table(
#'   id = 1:3,
#'   visit = as.Date(c("2020-03-01", "2020-07-15", "2021-01-10"))
#' )
#' dateparse_inwindow(dt, "visit", "2020-01-01", "2020-12-31")
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
#' and lookback/followup periods.
#'
#' @param index_start Start of index date range (date string).
#' @param index_end End of index date range (date string).
#' @param lookback Lookback period in days.
#' @param followup Followup period in days.
#' @return A list with index_start_year, index_end_year, file_start_year,
#'   file_end_year, lookback_years, followup_years.
#' @examples
#' dateparse_filerange("2010-01-01", "2015-12-31", lookback = 3650, followup = 365)
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
