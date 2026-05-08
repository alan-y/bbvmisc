
#' Count complete calendar units between two date-time values
#'
#' Counts the number of complete calendar units (years, months, or days)
#' between two date-time values, rounding down to the nearest whole unit.
#'
#' @param x A date-time representing the start of the interval.
#' @param y A date-time representing the end of the interval.
#' @param unit A character string specifying the calendar unit to count.
#'   One of \code{"years"}, \code{"months"}, or \code{"days"}.
#'
#' @return An integer giving the number of complete calendar units between
#'   \code{x} and \code{y}.
#'
#' @examples
#' \dontrun{
#' x <- lubridate::ymd("2020-01-01")
#' y <- lubridate::ymd("2023-06-01")
#'
#' count_calendar_units(x, y, "years")
#' count_calendar_units(x, y, "months")
#' count_calendar_units(x, y, "days")
#'}
#' @export
count_calendar_units <- function(x, y, unit = c("years", "months", "days")) {
  unit <- match.arg(unit)
  
  interval <- lubridate::interval(x, y)
  
  lubridate::time_length(interval, unit = unit) |>
    floor() |>
    as.integer()
}
