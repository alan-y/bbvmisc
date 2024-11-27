#' np
#' @description Paste n and percentage values together.
#'
#' @param n Counts.
#' @param p Proportions.
#' @param acc Number to round to for percentages in the combined count and percentage variable (e.g. 0.1 shows one decimal place for the percentage).
#' @return Counts with proportions in brackets.
#' @export
#' @examples
#' \dontrun{
#' np(n, p, acc = 0.1)
#' }
np <- function(n, p, acc = 1) {
  stopifnot(is.numeric(n) && is.numeric(p))
  
  perc <- scales::percent(p, accuracy = acc)
  glue::glue("{n} ({perc})", .na = NULL)
}
