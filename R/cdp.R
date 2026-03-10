#' Confirmed Disability Progression (CDP)
#'
#' Identifies confirmed disability progression events from EDSS measurements.
#' Baseline EDSS is determined from the first measurement within a window of
#' diagnosis. Progression requires a sustained increase above threshold.
#'
#' @param dt A data.frame or data.table with EDSS measurements (long format).
#' @param idvar Name of the person ID column.
#' @param edssvar Name of the EDSS score column (numeric).
#' @param datevar Name of the measurement date column (Date class).
#' @param dxdate Name of the diagnosis date column (Date class).
#' @param confirmdays Days required for confirmation (default 180).
#' @param baselinewindow Days from diagnosis to search for baseline EDSS (default 730).
#' @param roving Logical; if TRUE, use roving baseline (reset after each confirmed
#'   progression). Default FALSE.
#' @param allevents Logical; if TRUE and roving=TRUE, track all CDP events not
#'   just first. Default FALSE.
#' @return A list with `$data` (data.table with one row per person/event with
#'   `cdp_date`) and `$info` containing N_persons, N_events, confirmdays,
#'   baselinewindow.
#' @examples
#' edss <- data.table::data.table(
#'   pid = rep("A", 4),
#'   edss = c(2.0, 3.5, 3.5, 3.5),
#'   date = as.Date(c("2010-01-01", "2011-06-01", "2011-09-01", "2012-01-01")),
#'   dx = as.Date("2009-06-01")
#' )
#' cdp(edss, idvar = "pid", edssvar = "edss", datevar = "date", dxdate = "dx")
#' @export
cdp <- function(dt, idvar, edssvar, datevar, dxdate,
                confirmdays = 180L, baselinewindow = 730L,
                roving = FALSE, allevents = FALSE) {
  if (confirmdays <= 0) stop("confirmdays must be positive", call. = FALSE)
  if (baselinewindow <= 0) stop("baselinewindow must be positive", call. = FALSE)

  dt <- data.table::as.data.table(data.table::copy(dt))

  for (v in c(idvar, edssvar, datevar, dxdate)) {
    if (!v %in% names(dt)) stop(sprintf("Variable '%s' not found", v), call. = FALSE)
  }

  # Standardize column names to avoid get() scoping issues
  work <- dt[, c(idvar, edssvar, datevar, dxdate), with = FALSE]
  data.table::setnames(work, c(idvar, edssvar, datevar, dxdate),
                        c("cd_id_", "cd_edss_", "cd_date_", "cd_dxdate_"))

  work <- work[!is.na(work$cd_edss_) & !is.na(work$cd_date_)]
  if (nrow(work) == 0) stop("No valid observations after dropping missing values", call. = FALSE)

  date_is_Date <- inherits(work$cd_date_, "Date")
  if (date_is_Date) {
    work[, cd_date_ := as.numeric(cd_date_)]
    work[, cd_dxdate_ := as.numeric(cd_dxdate_)]
  }

  data.table::setorder(work, cd_id_, cd_date_, cd_edss_)

  if (!roving) {
    result <- .cdp_run(work, confirmdays, baselinewindow)
  } else {
    result <- .cdp_roving_run(work, confirmdays, baselinewindow, allevents)
  }

  if (date_is_Date && nrow(result) > 0) {
    result[, cdp_date := as.Date(cdp_date, origin = "1970-01-01")]
  }

  data.table::setnames(result, "cd_id_", idvar, skip_absent = TRUE)

  n_events <- nrow(result)
  if (allevents && roving && n_events > 0) {
    n_persons <- length(unique(result[[idvar]]))
  } else {
    n_persons <- n_events
  }

  list(
    data = result,
    info = list(
      N_persons = n_persons,
      N_events = n_events,
      confirmdays = confirmdays,
      baselinewindow = baselinewindow
    )
  )
}

# Internal: standard CDP on standardized columns
.cdp_run <- function(work, confirmdays, baselinewindow) {
  work <- .cdp_bl(work, baselinewindow)

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

  result <- unique(work[work$cd_confirmed_ == TRUE, .(cd_id_, cd_fpdt_)])
  result <- result[!duplicated(cd_id_)]

  if (nrow(result) > 0) {
    data.table::setnames(result, "cd_fpdt_", "cdp_date")
  } else {
    result <- data.table::data.table(cd_id_ = character(0), cdp_date = numeric(0))
  }

  result
}

# Internal: roving baseline CDP
.cdp_roving_run <- function(work, confirmdays, baselinewindow, allevents) {
  all_results <- list()
  event_counter <- 1L

  work <- .cdp_bl(work, baselinewindow)

  keep_going <- TRUE
  while (keep_going) {
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

    events <- unique(work[work$cd_confirmed_ == TRUE & work$cd_date_ == work$cd_fpdt_, .(cd_id_, cd_fpdt_)])
    events <- events[!duplicated(cd_id_)]

    if (nrow(events) == 0) {
      keep_going <- FALSE
    } else {
      events[, event_num := event_counter]
      data.table::setnames(events, "cd_fpdt_", "cdp_date")
      all_results[[event_counter]] <- data.table::copy(events)

      if (!allevents) {
        keep_going <- FALSE
      } else {
        event_dt <- data.table::data.table(cd_id_ = events$cd_id_, .evt_dt_ = events$cdp_date)
        work <- merge(work, event_dt, by = "cd_id_", all.x = TRUE)
        work <- work[is.na(.evt_dt_) | cd_date_ > .evt_dt_]
        work[, .evt_dt_ := NULL]

        if (nrow(work) == 0) {
          keep_going <- FALSE
        } else {
          data.table::setorder(work, cd_id_, cd_date_)
          work[work$cd_id_ %in% event_dt$cd_id_, cd_bl_edss_ := cd_edss_[1], by = cd_id_]
          work[work$cd_id_ %in% event_dt$cd_id_, cd_bl_date_ := cd_date_[1], by = cd_id_]
          event_counter <- event_counter + 1L
        }
      }
    }

    for (col in c("cd_pthresh_", "cd_echg_", "cd_isprog_", "cd_fpdt_",
                  "cd_confedss_", "cd_minconf_", "cd_confirmed_")) {
      if (col %in% names(work)) work[, (col) := NULL]
    }
  }

  if (length(all_results) == 0) {
    result <- data.table::data.table(cd_id_ = character(0), cdp_date = numeric(0),
                                     event_num = integer(0))
  } else {
    result <- data.table::rbindlist(all_results)
    if (!allevents) {
      data.table::setorder(result, cd_id_, event_num)
      result <- result[!duplicated(cd_id_)]
      result[, event_num := NULL]
    }
  }

  result
}

# Internal: determine baseline EDSS (standardized column names)
.cdp_bl <- function(work, baselinewindow) {
  work[, cd_inwin_ := cd_date_ >= cd_dxdate_ & cd_date_ <= cd_dxdate_ + baselinewindow]

  work[, cd_fwdt_ := suppressWarnings(
    min(ifelse(cd_inwin_, cd_date_, NA), na.rm = TRUE)
  ), by = cd_id_]
  .inf_to_na(work, "cd_fwdt_")

  work[, cd_bl_edss_ := ifelse(cd_date_ == cd_fwdt_ & !is.na(cd_fwdt_), cd_edss_, NA_real_)]
  work[, cd_bl_edss_ := cd_bl_edss_[which(!is.na(cd_bl_edss_))[1]], by = cd_id_]

  work[, cd_bl_date_ := ifelse(!is.na(cd_fwdt_), cd_fwdt_, NA_real_)]
  work[, cd_bl_date_ := cd_bl_date_[which(!is.na(cd_bl_date_))[1]], by = cd_id_]

  # Fallback to earliest
  data.table::setorder(work, cd_id_, cd_date_)
  work[is.na(cd_bl_edss_), cd_bl_edss_ := cd_edss_[1], by = cd_id_]
  work[is.na(cd_bl_date_), cd_bl_date_ := cd_date_[1], by = cd_id_]

  work[, cd_bl_edss_ := cd_bl_edss_[1], by = cd_id_]
  work[, cd_bl_date_ := cd_bl_date_[1], by = cd_id_]

  work[, c("cd_inwin_", "cd_fwdt_") := NULL]
  work
}
