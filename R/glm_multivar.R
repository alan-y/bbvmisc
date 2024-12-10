#' glm_multivar
#' @description Run a multivariable `glm()` for a vector of predictor variables with tidy df output
#'
#' @param data 
#' @param respvar 
#' @param predvars 
#' @param family 
#' @param exp 
#' @param additional_term 
#' @param fast
#' @return
#' @export
#'
#' @examples

glm_multivar <- function(data, respvar, predvars, family = "poisson", exp = TRUE, 
                         additional_term = "", fast = TRUE) {
  
  formula <- paste(respvar, "~", paste(predvars, collapse =" + "), additional_term)
  model <- glm(formula = formula, data = data, family = family)
  
  if (!fast) {
    out <- broom::tidy(model, exponentiate = exp, conf.int = TRUE)  
  } else {
    out <- ftidy(model, exp = exp)
  }
  
  out <- add_refcats(out, model) %>% 
    dplyr::filter(term != "(Intercept)") %>% 
    dplyr::mutate(
      predvars = stringr::str_match(term, paste0(predvars, collapse="|")),
      term = stringr::str_remove(term, paste0(predvars, collapse="|")),
      .before = 1
    )
  
  return(out)
  
}