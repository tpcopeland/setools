#' @keywords internal
#' @import data.table
#' @importFrom utils packageVersion
"_PACKAGE"

#' Display setools package information
#'
#' Lists available commands grouped by category, similar to the Stata
#' `setools` command.
#'
#' @param category Filter by category: `"all"` (default), `"dates"`, `"codes"`,
#'   `"migration"`, or `"ms"`. Partial matching is supported.
#' @return Invisibly returns a data.frame of commands with name, category,
#'   and description.
#' @examples
#' \dontrun{
#' setools()
#' setools("ms")
#' }
#' @export
setools <- function(category = c("all", "dates", "codes", "migration", "ms")) {
  category <- match.arg(category)

  cmds <- data.frame(
    name = c("dateparse_parse", "dateparse_window", "dateparse_validate",
             "dateparse_inwindow", "dateparse_filerange",
             "procmatch_match", "procmatch_first",
             "cci_se",
             "migrations",
             "covarclose",
             "sustainedss", "cdp", "pira"),
    category = c(rep("dates", 5), rep("codes", 2), "codes",
                 "migration", "codes", rep("ms", 3)),
    description = c(
      "Parse date strings to R Date objects",
      "Calculate lookback/followup window dates",
      "Validate date range (start <= end)",
      "Check if dates fall within a window",
      "Determine year files needed for date range",
      "Match procedure codes across variables",
      "Find first occurrence of procedure codes",
      "Swedish Charlson Comorbidity Index (ICD-7 to ICD-10)",
      "Process Swedish migration registry data",
      "Extract closest covariate values to index date",
      "Sustained EDSS progression date",
      "Confirmed Disability Progression",
      "Progression Independent of Relapse Activity"
    ),
    stringsAsFactors = FALSE
  )

  if (category != "all") {
    cmds <- cmds[cmds$category == category, ]
  }

  cat(sprintf("setools v%s - Swedish Registry Epidemiological Research Tools\n",
              utils::packageVersion("setools")))
  cat(sprintf("%d commands available\n\n", nrow(cmds)))

  for (cat_name in unique(cmds$category)) {
    cat(sprintf("[%s]\n", cat_name))
    sub <- cmds[cmds$category == cat_name, ]
    for (i in seq_len(nrow(sub))) {
      cat(sprintf("  %-25s %s\n", sub$name[i], sub$description[i]))
    }
    cat("\n")
  }

  invisible(cmds)
}
