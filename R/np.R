#' np
#' @description Paste n and percentage values together.
#'
#' @param n Counts.
#' @param p Proportions.
#' @param acc Number of decimals to use on percentages.
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
