#' Progression Independent of Relapse Activity (PIRA)
#'
#' Identifies confirmed disability progression events and classifies them as
#' PIRA (outside relapse windows) or RAW (relapse-associated worsening).
#'
#' @param dt A data.frame or data.table with EDSS measurements (long format).
#' @param idvar Name of the person ID column.
#' @param edssvar Name of the EDSS score column (numeric).
#' @param datevar Name of the measurement date column (Date class).
#' @param dxdate Name of the diagnosis date column (Date class).
#' @param relapses A data.frame or data.table with relapse data containing
#'   id and relapse date columns.
#' @param relapseidvar Name of the ID column in relapses (default: same as idvar).
#' @param relapsedatevar Name of the relapse date column (default: "relapse_date").
#' @param windowbefore Days before relapse to exclude (default 90).
#' @param windowafter Days after relapse to exclude (default 30).
#' @param confirmdays Days for CDP confirmation (default 180).
#' @param baselinewindow Days from diagnosis for baseline EDSS (default 730).
#' @param rebaselinerelapse Logical; if TRUE, reset baseline after last relapse
#'   (default FALSE).
#' @return A list with `$data` (data.table with one row per person containing
#'   `pira_date` and `raw_date`) and `$info` containing N_cdp, N_pira, N_raw,
#'   windowbefore, windowafter, confirmdays, baselinewindow.
#' @examples
#' edss <- data.table::data.table(
#'   pid = rep("A", 4),
#'   edss = c(2.0, 3.5, 3.5, 3.5),
#'   date = as.Date(c("2010-01-01", "2011-06-01", "2011-09-01", "2012-01-01")),
#'   dx = as.Date("2009-06-01")
#' )
#' rel <- data.table::data.table(
#'   pid = "A",
#'   relapse_date = as.Date("2010-06-01")
#' )
#' pira(edss, idvar = "pid", edssvar = "edss", datevar = "date",
#'      dxdate = "dx", relapses = rel, relapseidvar = "pid")
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
    r <- data.table::data.table(x_ = character(0), pira_date = numeric(0), raw_date = numeric(0))
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
