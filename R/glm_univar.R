#' glm_univar
#' @description Run a univariable `glm()` for a vector of predictor variables with tidy df output
#'
#' @param data Input dataset
#' @param respvar Single character string of response variable
#' @param predvars Vector of character strings of predictor variables
#' @param family Single character string of family function
#' @param exp Option to exponentiate model estimates (default is TRUE)
#' @param additional_term Option to add additional terms to model (default is blank)
#' @param fast Option to use normal approximation for confidence intervals (default is TRUE)
#' @return Regression model table with confidence intervals
#' @export
#'
#' @examples
#' \dontrun{
#' glm_univar(dat, "var", c("var1", "var2"), additional_term="+ offset(log(py))")
#' }

glm_univar <- function(
  data,
  respvar,
  predvars,
  family = "poisson",
  exp = TRUE,
  additional_term = "",
  fast = TRUE
) {
  formulas <- purrr::map(
    .x = predvars,
    .f = ~ reformulate(paste(.x, additional_term), response = respvar)
  )

  names(formulas) <- predvars
  mod_list <- purrr::map(formulas, ~ glm(.x, family = family, data = data))

  if (!fast) {
    out <- purrr::map(
      .x = mod_list,
      .f = ~ {
        broom::tidy(.x, exponentiate = exp, conf.int = TRUE)
      }
    )
  } else {
    out <- purrr::map(
      .x = mod_list,
      .f = ~ {
        ftidy(.x, exp = exp)
      }
    )
  }

  out <- dplyr::bind_rows(out)
  out <- out[out$term != "(Intercept)", ]
  out$predvars <- stringr::str_match(out$term, paste0(predvars, collapse = "|"))
  out$term <- stringr::str_remove(out$term, paste0(predvars, collapse = "|"))

  out[, c(ncol(out), seq(ncol(out) - 1))]
}
