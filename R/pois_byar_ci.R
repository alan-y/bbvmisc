#' Title pois_byar_ci
#' @description
#' Calculate confidence intervals for Poisson counts or rates using Byar's method, and return tidy output
#'
#' @param data Input dataset containing counts and person time
#' @param x count or vector of counts
#' @param pt person-time at risk or vector of person-times
#' @param factor per how many person-time
#' @param conf.level confidence level (default = 0.95)
#'
#' @return original dataframe with rate, lower, upper, and rate_ci variables added
#' @export
#'
#' @examples
#' \dontrun{
#' pois_byar_ci(data=counts, x="deaths", pt="person_years")
#'  }

pois_byar_ci <- function(data,
                         x,
                         pt,
                         factor = 1000,
                         conf.level = 0.95) {
  
  x <- dplyr::enquo(x)
  pt <- dplyr::enquo(pt)
  x <- rlang::as_name(x)
  pt <- rlang::as_name(pt)
  
  Z <- stats::qnorm(0.5 * (1 + conf.level))
  aprime <- data[, x] + 0.5
  Zinsert <- (Z / 3) * sqrt(1 / aprime)
  conf.low <- (aprime * (1 - 1 / (9 * aprime) - Zinsert) ^ 3) / data[, pt]
  conf.high <- (aprime * (1 - 1 / (9 * aprime) + Zinsert) ^ 3) / data[, pt]
  
  out <- cbind(
    data,
    data.frame(
      rate = data[, x] / data[, pt] * factor,
      conf.low =  conf.low * factor,
      conf.high = conf.high * factor
    )
  )
  
  out$rate_ci <- paste0(round(out$rate, 2),
                        " (",
                        round(out$conf.low, 2),
                        ", ",
                        round(out$conf.high, 2),
                        ")")
  
  return(out)
  
}