#' x_ci
#' @description Combine estimate with confidence interval into one string.
#'
#' @param x Central estimate.
#' @param l Lower estimate.
#' @param u Upper estimate.
#' @param digits Number of digits to use after decimal point.
#' @param brackets Option to enclose confidence interval in brackets.
#' @param ci_sep Separator for confidence interval.
#' @param ci_prefix Optional prefix to add text before confidence interval.
#' @return Estimate combined with confidence interval as a string.
#' @export
#' @examples
#' \dontrun{
#' x_ci(1.2, 0.7, 1.5, ci_sep = "--", ci_prefix = "95% CI ")
#' }
x_ci <- function(x, l, u, digits = 2, brackets = TRUE, ci_sep = "-", ci_prefix = NULL) {
  stopifnot(is.numeric(x) && is.numeric(l) && is.numeric(u))
  
  x <- formatC(x, digits = 2, format = "f")
  l <- formatC(l, digits = 2, format = "f")
  u <- formatC(u, digits = 2, format = "f")
  
  if (!is.null(ci_prefix)) {
    l <- paste0(ci_prefix, l)
  }
  
  if (brackets) {
    out <- glue::glue("{x} ({l}{ci_sep}{u})", .na = NULL)
  } else {
    out <- glue::glue("{x}, {l}{ci_sep}{u}", .na = NULL)
  }
  
  return(out)
}
