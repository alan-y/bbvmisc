#' pois_byar_ci
#' @description
#' Calculate confidence intervals for Poisson counts or rates using Byar's method, and return tidy output
#'
#' @param data Input dataset containing counts and person time
#' @param x count or vector of counts
#' @param pt person-time at risk or vector of person-times
#' @param factor per how many person-time
#' @param conf.level confidence level (default = 0.95)
#' @param digits number of digits to round to
#' @param rate_ci_only only print rate and CIs (default = TRUE)
#'
#' @return original dataframe with rate, lower, upper variables added
#' @export
#'
#' @examples
#' \dontrun{
#' pois_byar_ci(data=counts, x="deaths", pt="person_years")
#'  }

pois_byar_ci <- function(
  data,
  x,
  pt,
  factor = 1000,
  conf.level = 0.95,
  digits = 2,
  rate_ci_only = TRUE
) {
  x <- dplyr::enquo(x)
  pt <- dplyr::enquo(pt)
  x <- rlang::as_name(x)
  pt <- rlang::as_name(pt)

  Z <- stats::qnorm(0.5 * (1 + conf.level))
  aprime <- data[[x]] + 0.5
  Zinsert <- (Z / 3) * sqrt(1 / aprime)
  conf.low <- (aprime * (1 - 1 / (9 * aprime) - Zinsert)^3) / data[[pt]]
  conf.high <- (aprime * (1 - 1 / (9 * aprime) + Zinsert)^3) / data[[pt]]
  rate = (data[[x]] / data[[pt]])

  out <- cbind(
    data,
    data.frame(
      rate = round(rate * factor, digits),
      conf.low = round(conf.low * factor, digits),
      conf.high = round(conf.high * factor, digits)
    )
  )

  out <- out %>%
    dplyr::mutate(rate_ci = x_ci(rate, conf.low, conf.high, ci_sep = ", "))

  out_vars <- names(out)

  if (rate_ci_only) {
    out_vars <- setdiff(out_vars, c("rate", "conf.low", "conf.high"))
  }

  out[out_vars]
}
