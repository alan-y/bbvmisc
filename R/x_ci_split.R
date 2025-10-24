#' x_ci_split
#' @description Split estimate and confidence intervals into columns
#'
#' This function takes a column of text-formatted estimates and confidence intervals
#' (e.g. `"2.1 (1.1, 3.2)"`) and splits it into numeric columns: `est`, `lower`, and `upper`.
#'
#' @param dat A data frame containing the variable to be split.
#' @param var The unquoted name of the variable containing estimate and CI strings.
#'
#' @return A data frame with three new numeric columns: `est`, `lower`, and `upper`.
#' @export
#'
#' @examples
#' df <- data.frame(ci_text = c("2.1 (1.1, 3.2)", "0.5 (-0.1, 1.2)", "1.0 (0.8, 1.3)"))
#' x_ci_split(df, ci_text)
x_ci_split <- function(dat, var) {
  var <- rlang::ensym(var)
  
  dat %>%
    tidyr::separate_wider_regex(
      !!var,
      patterns = c(
        est   = "[0-9.+-eE]+",   # estimate
        "\\s*\\(\\s*",           # opening parenthesis
        lower = "[-0-9.+eE]+",   # lower CI
        "\\s*,\\s*",             # comma
        upper = "[-0-9.+eE]+",   # upper CI
        "\\s*\\)\\s*"            # closing parenthesis
      )
    ) %>%
    dplyr::mutate(dplyr::across(c(est, lower, upper), as.numeric))
}
