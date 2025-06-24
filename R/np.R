#' np
#' @description Paste n and percentage values together.
#'
#' @param n Counts.
#' @param p Proportions.
#' @param digits Number of digits to use after the decimal point in percentages.
#' @param comma Option to use comma separator on thousands.
#' @return Counts with proportions in brackets.
#' @export
#' @examples
#' \dontrun{
#' np(n, p, digits = 1)
#' }
np <- function(n, p, digits = 0, comma = FALSE) {
  stopifnot(is.numeric(n) && is.numeric(p))

  perc <- janitor::round_half_up(p * 100, digits = digits)
  perc <- formatC(perc, digits = digits, format = "f")
  perc <- paste0(perc, "%")

  if (comma) {
    n <- prettyNum(n, big.mark = ",")
  }

  glue::glue("{n} ({perc})", .na = NULL)
}
