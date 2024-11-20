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
#' @param na_vals Value to use to replace missing values; if NULL, uses x.
#' @return Estimate combined with confidence interval as a string.
#' @export
#' @examples
#' \dontrun{
#' x_ci(1.2, 0.7, 1.5, ci_sep = "--", ci_prefix = "95% CI ")
#' }
x_ci <- function(x, l, u, digits = 2, brackets = TRUE, ci_sep = "-", ci_prefix = NULL, na_vals = NULL) {
  stopifnot(is.numeric(x) && is.numeric(l) && is.numeric(u))
  
  x <- ifelse(!is.na(x), formatC(x, digits = 2, format = "f"), NA_character_)
  l <- ifelse(!is.na(l), formatC(l, digits = 2, format = "f"), NA_character_)
  u <- ifelse(!is.na(u), formatC(u, digits = 2, format = "f"), NA_character_)
  
  if (!is.null(ci_prefix)) {
    l <- paste0(ci_prefix, l)
  }
  
  if (brackets) {
    out <- glue::glue("{x} ({l}{ci_sep}{u})", .na = NULL)
  } else {
    out <- glue::glue("{x}, {l}{ci_sep}{u}", .na = NULL)
  }
  
  if (is.null(na_vals)) {
    out[is.na(out)] <- x[is.na(out)]
  } else {
    out[is.na(out)] <- na_vals
  }
  
  return(out)
}
