# Internal utility: replace Inf/-Inf with NA in a data.table column
# Uses $ access to avoid NSE scoping issues in R CMD check
.inf_to_na <- function(dt, col) {
  vals <- dt[[col]]
  bad <- is.infinite(vals)
  if (any(bad)) data.table::set(dt, which(bad), col, NA)
  invisible(dt)
}
