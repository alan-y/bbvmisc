#' Formats a Frequency Table with Combined Ns and Percentages
#'
#' Takes a data frame or \code{tabyl} and formats it to display counts (N)
#' and percentages combined into a single string cell (e.g., \code{"12 (34.3\%)"}).
#'
#' @param x A data frame or \code{\link[janitor]{tabyl}} containing frequency data.
#' @param denom A character string specifying the direction for percentage
#'   calculation. Must be one of \code{"row"} (default), \code{"col"}, or \code{"all"}.
#' @param digits An integer specifying the number of decimal places to display
#'   for percentages. Default is \code{1}.
#' @param trim A logical scalar. If \code{TRUE} (default), trims and collapses
#'   whitespace in the output strings using \code{\link[stringr]{str_squish}}.
#'   If \code{FALSE}, preserves padding created by formatting.
#'
#' @details
#' This function chains together several \code{janitor} package helpers:
#' \enumerate{
#'   \item \code{\link[janitor]{adorn_percentages}} to calculate relative proportions.
#'   \item \code{\link[janitor]{adorn_pct_formatting}} to convert proportions to
#'         formatted percentage strings rounded "half up".
#'   \item \code{\link[janitor]{adorn_ns}} to attach the raw counts (\code{N}) to
#'         the front of each formatted percentage string.
#' }
#'
#' @return A data frame (or \code{tabyl}) of character columns where each entry
#'   combines the raw count and percentage in the format \code{"N (X.X\%)"}.
#'
#' @seealso
#' \code{\link[janitor]{tabyl}},
#' \code{\link[janitor]{adorn_percentages}},
#' \code{\link[janitor]{adorn_ns}}
#'
#' @examples
#' \dontrun{
#' library(janitor)
#'
#' # Create a basic tabyl
#' tab <- tabyl(mtcars, cyl, gear)
#'
#' # Format with row percentages (default)
#' adorn_np(tab, denom = "row")
#'
#' # Format with column percentages and 2 decimal places
#' adorn_np(tab, denom = "col", digits = 2)
#'}
#' @export
adorn_np <- function(
  x,
  denom = c("row", "col", "all"),
  digits = 1,
  trim = TRUE
) {
  denom <- match.arg(denom, choices = c("row", "col", "all"))

  stopifnot(
    is.numeric(digits),
    length(digits) == 1,
    digits >= 0,
    is.logical(trim)
  )

  out <- janitor::adorn_percentages(x, denominator = denom) |>
    janitor::adorn_pct_formatting(digits = digits, rounding = "half up") |>
    janitor::adorn_ns("front")

  if (trim) {
    out[] <- lapply(out, function(col) {
      if (is.character(col)) stringr::str_squish(col) else col
    })
  }

  out
}
