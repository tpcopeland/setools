#' Swedish Charlson Comorbidity Index
#'
#' Computes the Swedish adaptation of the Charlson Comorbidity Index from
#' diagnosis-level (long format) registry data. Supports ICD-7 through ICD-10
#' codes as used in Swedish national health registries.
#'
#' Based on Ludvigsson et al. Clinical Epidemiology 2021;13:21-41.
#'
#' @param dt A data.frame or data.table in long format (one row per diagnosis).
#' @param id Name of the patient ID column.
#' @param icd Name of the ICD code column (character).
#' @param date Name of the date column.
#' @param dateformat How to interpret the date column:
#'   - `"stata"` (default): R Date class
#'   - `"yyyymmdd"`: integer YYYYMMDD
#'   - `"ymd"`: character YYYY-MM-DD string
#' @param components Logical; if TRUE, include individual comorbidity indicators
#'   in output.
#' @param prefix Prefix for component variable names (default `"cci_"`).
#' @return A list with `$data` (data.table collapsed to one row per patient
#'   with `charlson` score and optionally component indicators) and `$info`
#'   containing N_input, N_patients, N_any, mean_cci, max_cci.
#' @examples
#' dx <- data.table::data.table(
#'   pid = c("A", "A", "B"),
#'   code = c("I21", "E119", "J44"),
#'   date = as.Date(c("2005-03-01", "2010-06-15", "2015-09-20"))
#' )
#' cci_se(dx, id = "pid", icd = "code", date = "date")
#' cci_se(dx, id = "pid", icd = "code", date = "date", components = TRUE)
#' @export
cci_se <- function(dt, id, icd, date, dateformat = "stata",
                   components = FALSE, prefix = "cci_") {
  dateformat <- tolower(dateformat)
  if (!dateformat %in% c("stata", "yyyymmdd", "ymd")) {
    stop("dateformat must be: stata, yyyymmdd, or ymd", call. = FALSE)
  }

  dt <- data.table::as.data.table(data.table::copy(dt))

  # Capture column names to avoid data.table scoping clashes
  id_col <- id
  icd_col <- icd
  date_col <- date

  # Validate columns exist
  for (v in c(id_col, icd_col, date_col)) {
    if (!v %in% names(dt)) stop(sprintf("Variable '%s' not found", v), call. = FALSE)
  }

  # Drop rows with missing date
  if (is.character(dt[[date_col]])) {
    dt <- dt[!is.na(dt[[date_col]]) & trimws(dt[[date_col]]) != ""]
  } else {
    dt <- dt[!is.na(dt[[date_col]])]
  }

  n_input <- nrow(dt)
  if (n_input == 0) stop("No valid observations", call. = FALSE)

  # Normalize ICD codes: uppercase, strip dots, prepend space
  dt[, .code := paste0(" ", toupper(gsub("\\.", "", trimws(get(icd_col)))))]

  # Extract year from date
  if (dateformat == "ymd" || is.character(dt[[date_col]])) {
    dt[, .yr := as.integer(substr(trimws(get(date_col)), 1, 4))]
  } else if (dateformat == "yyyymmdd") {
    dt[, .yr := as.integer(floor(get(date_col) / 10000))]
  } else {
    # R Date class
    dt[, .yr := as.integer(format(get(date_col), "%Y"))]
  }

  # Drop rows with unparseable years
  n_bad <- sum(is.na(dt$.yr))
  if (n_bad > 0) {
    message(sprintf("Warning: %d observations with unparseable dates (dropped)", n_bad))
    dt <- dt[!is.na(.yr)]
  }
  if (nrow(dt) == 0) stop("No valid observations after date parsing", call. = FALSE)

  # ICD version flags (Swedish transition dates)
  dt[, .v7  := (.yr <= 1968)]
  dt[, .v8  := (.yr >= 1969 & .yr <= 1986)]
  dt[, .v9  := (.yr >= 1987 & .yr <= 1997)]
  dt[, .v10 := (.yr >= 1997)]

  # Score all 19 comorbidity components
  comp_names <- names(.cci_patterns)

  for (comp in comp_names) {
    pats <- .cci_patterns[[comp]]
    dt[, (comp) := 0L]

    for (ver in c("icd7", "icd8", "icd9", "icd10")) {
      if (length(pats[[ver]]) == 0) next
      ver_flag <- switch(ver, icd7 = ".v7", icd8 = ".v8", icd9 = ".v9", icd10 = ".v10")
      # Build regex: each pattern is a prefix match
      # The Stata code uses regexm(code, " X| Y| Z") which matches any of these
      regex <- paste0("(", paste(gsub("([.|()\\^${}+*?\\[\\]])", "\\\\\\1", pats[[ver]]), collapse = "|"), ")")
      dt[get(ver_flag) == TRUE, (comp) := pmax(get(comp), as.integer(grepl(regex, .code)))]
    }
  }

  # Collapse to patient level: max of each indicator
  result <- dt[, lapply(.SD, max, na.rm = TRUE), by = id_col, .SDcols = comp_names]

  # Replace -Inf (from max of empty) with 0

  for (comp in comp_names) {
    .inf_to_na(result, comp)
    vals <- result[[comp]]
    set(result, which(is.na(vals)), comp, 0L)
  }

  # Apply hierarchy rules
  # Liver: mild + ascites -> severe; clear mild if severe
  result[livmild > 0 & ascites > 0, livsev := 1L]
  result[livsev > 0, livmild := 0L]

  # Diabetes: clear uncomplicated if complicated present
  result[diabcomp > 0, diab := 0L]

  # Cancer: clear non-metastatic if metastatic present
  result[mets > 0, cancer := 0L]

  # Compute weighted Charlson score
  # Weights: most=1, plegia/diabcomp/renal/cancer=2, livsev=3, mets/aids=6
  scoring_comps <- names(.cci_weights)  # excludes ascites
  result[, charlson := 0L]
  for (comp in scoring_comps) {
    result[, charlson := charlson + .cci_weights[comp] * get(comp)]
  }

  # Clean up: drop ascites (internal only), drop or rename components
  if (components) {
    result[, ascites := NULL]
    # Rename with prefix
    for (comp in scoring_comps) {
      data.table::setnames(result, comp, paste0(prefix, comp))
    }
  } else {
    for (comp in comp_names) {
      result[, (comp) := NULL]
    }
  }

  # Summary statistics
  n_patients <- nrow(result)
  n_any <- sum(result$charlson > 0)
  mean_cci <- mean(result$charlson)
  max_cci <- max(result$charlson)

  list(
    data = result,
    info = list(
      N_input = n_input,
      N_patients = n_patients,
      N_any = n_any,
      mean_cci = mean_cci,
      max_cci = max_cci
    )
  )
}
