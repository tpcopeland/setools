#' Progression Independent of Relapse Activity (PIRA)
#'
#' Identifies confirmed disability progression (CDP) events and classifies each
#' as either PIRA (progression independent of relapse activity, occurring outside
#' relapse windows) or RAW (relapse-associated worsening, occurring within a
#' relapse window).
#'
#' @details
#' The algorithm proceeds in three steps:
#' \enumerate{
#'   \item **CDP detection**: Runs the standard CDP algorithm (see [cdp()]) to
#'     identify confirmed progression events from EDSS measurements.
#'   \item **Relapse window check**: For each CDP event, checks whether the
#'     event falls within any relapse window
#'     `[relapse_date - windowbefore, relapse_date + windowafter]`. Uses
#'     inclusive boundaries (matching Stata `inrange()`).
#'   \item **Classification**: Events outside all relapse windows are classified
#'     as PIRA. Events inside at least one relapse window are classified as RAW.
#' }
#'
#' If `rebaselinerelapse = TRUE`, the EDSS baseline is reset to the first
#' measurement >= 30 days after the last relapse (instead of using the
#' diagnosis-window baseline). This reflects the idea that post-relapse EDSS
#' may settle at a new level.
#'
#' Each person gets at most one row in the output, with `pira_date` set for
#' PIRA events and `raw_date` set for RAW events (the other is `NA`).
#'
#' @param dt A data.frame or data.table with EDSS measurements (long format,
#'   one row per measurement).
#' @param idvar Name of the person ID column.
#' @param edssvar Name of the EDSS score column (numeric, 0-10 scale).
#' @param datevar Name of the measurement date column (Date class).
#' @param dxdate Name of the diagnosis date column (Date class).
#' @param relapses A data.frame or data.table with relapse data. Must contain
#'   at least an ID column and a relapse date column. Multiple relapses per
#'   person are supported (one row per relapse).
#' @param relapseidvar Name of the ID column in `relapses`. Defaults to the
#'   same as `idvar`.
#' @param relapsedatevar Name of the relapse date column in `relapses`
#'   (default `"relapse_date"`).
#' @param windowbefore Days before a relapse that define the relapse-associated
#'   window (default 90). A CDP event occurring within this many days before a
#'   relapse is classified as RAW.
#' @param windowafter Days after a relapse that define the relapse-associated
#'   window (default 30). A CDP event occurring within this many days after a
#'   relapse is classified as RAW.
#' @param confirmdays Days for CDP confirmation (default 180).
#' @param baselinewindow Days from diagnosis for baseline EDSS (default 730).
#' @param rebaselinerelapse Logical; if TRUE, reset baseline EDSS to the first
#'   measurement >= 30 days after the last relapse. Default FALSE.
#' @return A list with two elements:
#'   \describe{
#'     \item{`$data`}{A data.table with one row per person who has a CDP event,
#'       containing the person ID, `pira_date` (Date or NA), and `raw_date`
#'       (Date or NA). Exactly one of these is non-NA per row.}
#'     \item{`$info`}{A list with: `N_cdp` (total CDP events), `N_pira`
#'       (PIRA events), `N_raw` (RAW events), `windowbefore`, `windowafter`,
#'       `confirmdays`, `baselinewindow`.}
#'   }
#'
#' @seealso [cdp()] for the underlying CDP algorithm,
#'   [sustainedss()] for sustained threshold crossing
#'
#' @examples
#' library(data.table)
#'
#' # --- PIRA: CDP far from any relapse ---
#' edss <- data.table(
#'   pid = rep("A", 3),
#'   edss = c(2.0, 4.0, 4.0),
#'   date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01")),
#'   dx = as.Date("2009-06-01")
#' )
#' rel <- data.table(
#'   pid = "A",
#'   relapse_date = as.Date("2015-01-01")  # far from CDP
#' )
#' res <- pira(edss, idvar = "pid", edssvar = "edss", datevar = "date",
#'             dxdate = "dx", relapses = rel, relapseidvar = "pid")
#' res$data
#' #>    pid  pira_date   raw_date
#' #> 1:   A 2010-07-01       <NA>
#' res$info$N_pira  # 1
#' res$info$N_raw   # 0
#'
#' # --- RAW: CDP within relapse window ---
#' rel2 <- data.table(
#'   pid = "A",
#'   relapse_date = as.Date("2010-06-15")  # 16 days before CDP
#' )
#' res2 <- pira(edss, idvar = "pid", edssvar = "edss", datevar = "date",
#'              dxdate = "dx", relapses = rel2, relapseidvar = "pid")
#' res2$info$N_raw   # 1 (within 90-day windowbefore)
#' res2$info$N_pira  # 0
#'
#' # --- No relapses at all: all CDP classified as PIRA ---
#' rel_empty <- data.table(pid = character(0),
#'                         relapse_date = as.Date(character(0)))
#' res3 <- pira(edss, idvar = "pid", edssvar = "edss", datevar = "date",
#'              dxdate = "dx", relapses = rel_empty, relapseidvar = "pid")
#' res3$info$N_pira  # 1
#'
#' # --- Mixed cohort: some PIRA, some RAW ---
#' edss4 <- data.table(
#'   pid = c(rep("A", 3), rep("B", 3)),
#'   edss = c(2.0, 4.0, 4.0, 2.0, 4.0, 4.0),
#'   date = as.Date(c("2010-01-01", "2010-07-01", "2011-07-01",
#'                     "2010-01-01", "2010-07-01", "2011-07-01")),
#'   dx = as.Date("2009-06-01")
#' )
#' rel4 <- data.table(
#'   pid = "A",
#'   relapse_date = as.Date("2010-06-01")  # only A has a relapse
#' )
#' res4 <- pira(edss4, idvar = "pid", edssvar = "edss", datevar = "date",
#'              dxdate = "dx", relapses = rel4, relapseidvar = "pid")
#' res4$info
#' # N_cdp = 2, N_raw = 1 (A), N_pira = 1 (B)
#'
#' # --- Custom relapse window (30 days before, 60 days after) ---
#' res5 <- pira(edss, idvar = "pid", edssvar = "edss", datevar = "date",
#'              dxdate = "dx", relapses = rel2, relapseidvar = "pid",
#'              windowbefore = 30L, windowafter = 60L)
#' # With narrower window, the same relapse may no longer overlap the CDP
#' @export
pira <- function(dt, idvar, edssvar, datevar, dxdate, relapses,
                 relapseidvar = NULL, relapsedatevar = "relapse_date",
                 windowbefore = 90L, windowafter = 30L,
                 confirmdays = 180L, baselinewindow = 730L,
                 rebaselinerelapse = FALSE) {
  if (windowbefore < 0) stop("windowbefore must be non-negative", call. = FALSE)
  if (windowafter < 0) stop("windowafter must be non-negative", call. = FALSE)
  if (confirmdays <= 0) stop("confirmdays must be positive", call. = FALSE)
  if (baselinewindow <= 0) stop("baselinewindow must be positive", call. = FALSE)

  if (is.null(relapseidvar)) relapseidvar <- idvar

  dt <- data.table::as.data.table(data.table::copy(dt))
  relapses <- data.table::as.data.table(data.table::copy(relapses))

  for (v in c(idvar, edssvar, datevar, dxdate)) {
    if (!v %in% names(dt)) stop(sprintf("Variable '%s' not found in EDSS data", v), call. = FALSE)
  }
  if (!relapseidvar %in% names(relapses)) {
    stop(sprintf("Variable '%s' not found in relapse data", relapseidvar), call. = FALSE)
  }
  if (!relapsedatevar %in% names(relapses)) {
    stop(sprintf("Variable '%s' not found in relapse data", relapsedatevar), call. = FALSE)
  }

  # Prepare relapse data with standardized names
  rel <- relapses[, c(relapseidvar, relapsedatevar), with = FALSE]
  data.table::setnames(rel, c(relapseidvar, relapsedatevar), c("cd_id_", "pi_reldt_"))
  rel <- rel[!is.na(rel$pi_reldt_)]

  # Prepare EDSS data with standardized names (matching .cdp_bl expectations)
  work <- dt[, c(idvar, edssvar, datevar, dxdate), with = FALSE]
  data.table::setnames(work, c(idvar, edssvar, datevar, dxdate),
                        c("cd_id_", "cd_edss_", "cd_date_", "cd_dxdate_"))
  work <- work[!is.na(work$cd_edss_) & !is.na(work$cd_date_)]
  if (nrow(work) == 0) stop("No valid observations after dropping missing values", call. = FALSE)

  date_is_Date <- inherits(work$cd_date_, "Date")
  if (date_is_Date) {
    work[, cd_date_ := as.numeric(cd_date_)]
    work[, cd_dxdate_ := as.numeric(cd_dxdate_)]
    rel[, pi_reldt_ := as.numeric(pi_reldt_)]
  }

  data.table::setorder(work, cd_id_, cd_date_, cd_edss_)

  # Step 1: Determine baseline
  work <- .cdp_bl(work, baselinewindow)

  # Rebaseline after relapse if requested
  if (rebaselinerelapse && nrow(rel) > 0) {
    rel_max <- rel[, .(pi_lastrel_ = max(pi_reldt_)), by = cd_id_]
    work <- merge(work, rel_max, by = "cd_id_", all.x = TRUE)

    work[, pi_hasrel_ := !is.na(pi_lastrel_) & pi_lastrel_ > cd_bl_date_]
    work[pi_hasrel_ == FALSE, pi_lastrel_ := NA]

    work[, pi_postrel_ := !is.na(pi_lastrel_) & cd_date_ >= pi_lastrel_ + 30]

    work[, pi_newbldt_ := suppressWarnings(
      min(ifelse(pi_postrel_, cd_date_, NA), na.rm = TRUE)
    ), by = cd_id_]
    .inf_to_na(work, "pi_newbldt_")

    work[, pi_newbledss_ := ifelse(pi_postrel_ & cd_date_ == pi_newbldt_, cd_edss_, NA_real_)]
    work[, pi_newbledss_ := suppressWarnings(min(pi_newbledss_, na.rm = TRUE)), by = cd_id_]
    .inf_to_na(work, "pi_newbledss_")

    work[!is.na(work$pi_newbledss_), cd_bl_edss_ := pi_newbledss_]
    work[!is.na(work$pi_newbldt_), cd_bl_date_ := pi_newbldt_]

    work[, c("pi_hasrel_", "pi_lastrel_", "pi_postrel_", "pi_newbldt_", "pi_newbledss_") := NULL]
  }

  # Step 2: CDP algorithm
  work[, cd_pthresh_ := ifelse(cd_bl_edss_ <= 5.5, 1.0, 0.5)]
  work[, cd_echg_ := cd_edss_ - cd_bl_edss_]
  work[, cd_isprog_ := cd_echg_ >= cd_pthresh_ & cd_date_ > cd_bl_date_]

  work[, cd_fpdt_ := suppressWarnings(
    min(ifelse(cd_isprog_, cd_date_, NA), na.rm = TRUE)
  ), by = cd_id_]
  .inf_to_na(work, "cd_fpdt_")

  work[, cd_confedss_ := ifelse(
    !is.na(cd_fpdt_) & cd_date_ >= cd_fpdt_ + confirmdays,
    cd_edss_, NA_real_
  )]
  work[, cd_minconf_ := suppressWarnings(min(cd_confedss_, na.rm = TRUE)), by = cd_id_]
  .inf_to_na(work, "cd_minconf_")
  work[, cd_confirmed_ := !is.na(cd_minconf_) & cd_minconf_ >= cd_bl_edss_ + cd_pthresh_]

  # Extract CDP events
  cdp_events <- unique(work[work$cd_confirmed_ == TRUE, .(cd_id_, cd_fpdt_)])
  cdp_events <- cdp_events[!duplicated(cd_id_)]

  n_cdp <- nrow(cdp_events)

  if (n_cdp == 0) {
    r <- data.table::data.table(x_ = character(0),
                                pira_date = as.Date(character(0)),
                                raw_date = as.Date(character(0)))
    data.table::setnames(r, "x_", idvar)
    return(list(
      data = r,
      info = list(N_cdp = 0L, N_pira = 0L, N_raw = 0L,
                  windowbefore = windowbefore, windowafter = windowafter,
                  confirmdays = confirmdays, baselinewindow = baselinewindow)
    ))
  }

  data.table::setnames(cdp_events, "cd_fpdt_", "cdp_date")

  # Step 3: Classify as PIRA or RAW
  if (nrow(rel) > 0) {
    classified <- merge(cdp_events, rel, by = "cd_id_", all.x = TRUE, allow.cartesian = TRUE)
    classified[, pi_inrelwin_ := !is.na(pi_reldt_) &
                 cdp_date >= pi_reldt_ - windowbefore &
                 cdp_date <= pi_reldt_ + windowafter]
    classified[, pi_anyrel_ := max(pi_inrelwin_, na.rm = TRUE), by = cd_id_]
    classified[is.na(pi_anyrel_) | pi_anyrel_ < 0, pi_anyrel_ := 0]
    result <- unique(classified[, .(cd_id_, cdp_date, pi_anyrel_)])
    result <- result[!duplicated(cd_id_)]
  } else {
    result <- cdp_events
    result[, pi_anyrel_ := 0L]
  }

  result[, pira_date := NA_real_]
  result[, raw_date := NA_real_]
  result[pi_anyrel_ == 0, pira_date := cdp_date]
  result[pi_anyrel_ == 1, raw_date := cdp_date]

  if (date_is_Date) {
    result[, pira_date := as.Date(pira_date, origin = "1970-01-01")]
    result[, raw_date := as.Date(raw_date, origin = "1970-01-01")]
  }

  result[, c("cdp_date", "pi_anyrel_") := NULL]
  data.table::setnames(result, "cd_id_", idvar)

  n_pira <- sum(!is.na(result$pira_date))
  n_raw <- sum(!is.na(result$raw_date))

  list(
    data = result,
    info = list(
      N_cdp = n_cdp,
      N_pira = n_pira,
      N_raw = n_raw,
      windowbefore = windowbefore,
      windowafter = windowafter,
      confirmdays = confirmdays,
      baselinewindow = baselinewindow
    )
  )
}
