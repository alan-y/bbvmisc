#' glm_multivar
#' @description Run a multivariable `glm()` for a vector of predictor variables with tidy df output
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
#' glm_multivar(dat, "var", c("var1", "var2"), additional_term="+ offset(log(py))")
#' }

glm_multivar <- function(data, respvar, predvars, family = "poisson", exp = TRUE, 
                         additional_term = "", fast = TRUE) {
  
  formula <- paste(respvar, "~", paste(predvars, collapse =" + "), additional_term)
  model <- glm(formula = formula, data = data, family = family)
  
  if (!fast) {
    out <- broom::tidy(model, exponentiate = exp, conf.int = TRUE)  
  } else {
    out <- ftidy(model, exp = exp)
  }
  
  out <- add_refcats(out, model)
  
  out[out$term != "(Intercept)", ]
  out$predvars <- stringr::str_match(out$term, paste0(predvars, collapse = "|"))
  out$term <- stringr::str_remove(out$term, paste0(predvars, collapse = "|"))
  out[, c(ncol(out), seq(ncol(out) - 1))]
  
  return(out)
  
}