#' format_p
#' @description Format p-values.
#'
#' @param x p-values.
#' @param cutoff Cut-off for p-values.
#' @return Formatted p-values.
#' @export
#' @examples
#' \dontrun{
#' format_p(x)
#' }
format_p <- function(x, cutoff = 0.001) {
  stopifnot(cutoff > 0 && cutoff < 1)

  decimals <- nchar(cutoff) - 2

  ifelse(
    x < cutoff,
    paste0("<", cutoff),
    formatC(x, digits = decimals, format = "f")
  )
}
