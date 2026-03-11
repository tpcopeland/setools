#' Compute sustained EDSS progression date
#'
#' Iterative algorithm that identifies the first date when EDSS reaches a
#' threshold and is sustained throughout a confirmation window. Events that
#' are not confirmed as sustained are rejected and the search repeats.
#'
#' @details
#' The algorithm works as follows:
#' \enumerate{
#'   \item Find the first date where EDSS >= `threshold` for each person.
#'   \item Check the confirmation window: within `confirmwindow` days after
#'     that date, the minimum EDSS and the last EDSS value are evaluated.
#'     The event is rejected (not sustained) only when **both** the minimum
#'     EDSS is below `baselinethreshold` **and** the last value is below
#'     `threshold`. This ensures the replacement value (last) is below
#'     threshold, preventing infinite iteration.
#'   \item If not sustained, the EDSS at the event date is replaced with the
#'     last observed value in the window (using `min()` for same-date
#'     duplicates, matching Stata behavior), and the search restarts.
#'   \item This repeats until no more rejections occur or 1000 iterations
#'     are reached.
#' }
#'
#' If no measurements exist in the confirmation window after the candidate
#' event, the event is considered sustained (absence of disconfirmation).
#'
#' @param dt A data.frame or data.table with EDSS measurements (long format,
#'   one row per measurement).
#' @param idvar Name of the person ID column.
#' @param edssvar Name of the EDSS score column (numeric, 0-10 scale).
#' @param datevar Name of the date column (Date class).
#' @param threshold EDSS threshold value (must be positive). The first date
#'   where EDSS >= this value is the candidate sustained event.
#' @param confirmwindow Days for confirmation window (default 182, approx.
#'   6 months). Measurements within `(event_date, event_date + confirmwindow]`
#'   are checked for sustaining.
#' @param baselinethreshold Minimum EDSS to require in confirmation window.
#'   Defaults to `threshold` if not specified.
#' @return A list with two elements:
#'   \describe{
#'     \item{`$data`}{A data.table with one row per person who has a sustained
#'       event, containing the person ID and `sustained_dt` (Date). Persons
#'       who never reach or sustain the threshold are excluded (zero rows).}
#'     \item{`$info`}{A list with: `N_events` (number of persons with sustained
#'       events), `iterations` (number of algorithm iterations), `threshold`,
#'       `confirmwindow`.}
#'   }
#'
#' @seealso [cdp()] for confirmed disability progression with baseline and
#'   diagnosis date, [pira()] for relapse-adjusted classification
#'
#' @examples
#' library(data.table)
#'
#' # --- Basic sustained threshold crossing ---
#' edss <- data.table(
#'   pid = rep("A", 4),
#'   edss = c(2.0, 4.5, 4.0, 4.5),
#'   date = as.Date(c("2010-01-01", "2010-06-01", "2010-09-01", "2010-12-01"))
#' )
#' res <- sustainedss(edss, idvar = "pid", edssvar = "edss",
#'                    datevar = "date", threshold = 4.0)
#' res$data
#' #>    pid sustained_dt
#' #> 1:   A   2010-06-01
#'
#' # --- Rejected event (drops below threshold, then recovers) ---
#' edss2 <- data.table(
#'   pid = rep("A", 4),
#'   edss = c(2.0, 4.5, 2.0, 4.5),
#'   date = as.Date(c("2010-01-01", "2010-06-01", "2010-09-01", "2011-06-01"))
#' )
#' res2 <- sustainedss(edss2, idvar = "pid", edssvar = "edss",
#'                     datevar = "date", threshold = 4.0)
#' res2$data$sustained_dt  # 2011-06-01 (first event rejected, second sustained)
#' res2$info$iterations    # >= 2 (at least one rejection cycle)
#'
#' # --- No measurements in confirmation window = sustained ---
#' edss3 <- data.table(
#'   pid = rep("A", 2),
#'   edss = c(2.0, 4.5),
#'   date = as.Date(c("2010-01-01", "2010-06-01"))
#' )
#' res3 <- sustainedss(edss3, idvar = "pid", edssvar = "edss",
#'                     datevar = "date", threshold = 4.0)
#' nrow(res3$data)  # 1 (absence of disconfirmation = sustained)
#'
#' # --- Multiple patients: some sustained, some not ---
#' edss4 <- data.table(
#'   pid = c(rep("A", 3), rep("B", 3), rep("C", 3)),
#'   edss = c(2.0, 4.5, 4.0,      # A: sustained at 4.0 threshold
#'            1.0, 3.0, 3.5,       # B: never reaches 4.0
#'            3.0, 4.0, 2.0),      # C: reaches 4.0 but drops
#'   date = as.Date(c("2010-01-01", "2010-06-01", "2010-12-01",
#'                     "2010-01-01", "2010-06-01", "2010-12-01",
#'                     "2010-01-01", "2010-06-01", "2010-12-01"))
#' )
#' res4 <- sustainedss(edss4, idvar = "pid", edssvar = "edss",
#'                     datevar = "date", threshold = 4.0)
#' res4$data
#' # Only patient A has a sustained event
#' res4$info$N_events  # 1
#' @export
sustainedss <- function(dt, idvar, edssvar, datevar, threshold,
                        confirmwindow = 182L, baselinethreshold = NULL) {
  if (threshold <= 0) stop("threshold must be positive", call. = FALSE)
  if (confirmwindow <= 0) stop("confirmwindow must be positive", call. = FALSE)
  if (is.null(baselinethreshold)) baselinethreshold <- threshold

  dt <- data.table::as.data.table(data.table::copy(dt))

  for (v in c(idvar, edssvar, datevar)) {
    if (!v %in% names(dt)) stop(sprintf("Variable '%s' not found", v), call. = FALSE)
  }

  # Standardize column names
  work <- dt[, c(idvar, edssvar, datevar), with = FALSE]
  data.table::setnames(work, c(idvar, edssvar, datevar), c("ss_id_", "ss_edss_", "ss_date_"))

  work <- work[!is.na(work$ss_edss_) & !is.na(work$ss_date_)]
  if (nrow(work) == 0) stop("No valid observations after dropping missing values", call. = FALSE)

  date_is_Date <- inherits(work$ss_date_, "Date")
  if (date_is_Date) {
    work[, ss_date_ := as.numeric(ss_date_)]
  }

  work[, ss_edss_work_ := ss_edss_]
  data.table::setorder(work, ss_id_, ss_date_, ss_edss_)
  work[, ss_obs_id_ := seq_len(.N)]

  keep_going <- TRUE
  iteration <- 1L
  max_iterations <- 1000L

  while (keep_going) {
    if (iteration > max_iterations) {
      warning(sprintf("sustainedss reached %d iterations without converging", max_iterations))
      break
    }

    work[, ss_first_dt_ := suppressWarnings(
      min(ifelse(ss_edss_work_ >= threshold, ss_date_, NA), na.rm = TRUE)
    ), by = ss_id_]
    .inf_to_na(work, "ss_first_dt_")

    work[, ss_in_confirm_ := !is.na(ss_first_dt_) & ss_date_ > ss_first_dt_ & ss_date_ <= ss_first_dt_ + confirmwindow]

    work[, ss_lowest_ := suppressWarnings(
      min(ifelse(ss_in_confirm_, ss_edss_work_, NA), na.rm = TRUE)
    ), by = ss_id_]
    .inf_to_na(work, "ss_lowest_")

    work[, ss_lastdt_ := suppressWarnings(
      max(ifelse(ss_in_confirm_, ss_date_, NA), na.rm = TRUE)
    ), by = ss_id_]
    .inf_to_na(work, "ss_lastdt_")

    # Use min() for conservative handling of same-date duplicates (matches Stata)
    work[, ss_last_val_ := suppressWarnings(
      min(ifelse(ss_date_ == ss_lastdt_ & !is.na(ss_lastdt_), ss_edss_work_, NA), na.rm = TRUE)
    ), by = ss_id_]
    .inf_to_na(work, "ss_last_val_")

    work[, ss_not_sust_ := !is.na(ss_lowest_) &
           ss_lowest_ < baselinethreshold &
           !is.na(ss_last_val_) &
           ss_last_val_ < threshold]

    rejected <- work[work$ss_date_ == work$ss_first_dt_ & work$ss_not_sust_ == TRUE]
    n_rejected <- nrow(rejected)

    if (n_rejected == 0) {
      keep_going <- FALSE
    } else {
      update_ids <- rejected$ss_obs_id_
      update_vals <- rejected$ss_last_val_
      work[work$ss_obs_id_ %in% update_ids,
           ss_edss_work_ := update_vals[match(ss_obs_id_, update_ids)]]
      iteration <- iteration + 1L
    }

    work[, c("ss_first_dt_", "ss_in_confirm_", "ss_lowest_", "ss_lastdt_",
             "ss_last_val_", "ss_not_sust_") := NULL]
  }

  work[, ss_sustained_dt_ := suppressWarnings(
    min(ifelse(ss_edss_work_ >= threshold, ss_date_, NA), na.rm = TRUE)
  ), by = ss_id_]
  .inf_to_na(work, "ss_sustained_dt_")

  result <- unique(work[!is.na(work$ss_sustained_dt_), .(ss_id_, ss_sustained_dt_)])
  data.table::setnames(result, c("ss_id_", "ss_sustained_dt_"), c(idvar, "sustained_dt"))

  if (date_is_Date) {
    result[, sustained_dt := as.Date(sustained_dt, origin = "1970-01-01")]
  }

  n_events <- nrow(result)

  list(
    data = result,
    info = list(
      N_events = n_events,
      iterations = iteration,
      threshold = threshold,
      confirmwindow = confirmwindow
    )
  )
}
