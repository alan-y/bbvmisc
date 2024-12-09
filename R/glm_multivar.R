#' glm_multivar
#' @description Run a multivariable glm() for a vector of predictor variables with tidy df output
#'
#' @param data 
#' @param respvar 
#' @param predvars 
#' @param fast 
#' @param family 
#' @param additional_term 
#'
#' @return
#' @export
#'
#' @examples

glm_multivar <- function(data, respvar, predvars, fast=TRUE, family="poisson", additional_term="") {
  
  formula <- paste(respvar, "~", paste(predvars, collapse =" + "), additional_term)
  model <- glm(formula=formula, data=data, family=family)
  
  if (!fast) {
    out <- tidy(model, exponentiate=TRUE, conf.int=TRUE)  
  }
  
  else
    out <- ftidy(model)
  
  out <- out %>% 
    add_refcats(., model) %>% 
    dplyr::filter(term != "(Intercept)") %>% 
    dplyr::mutate(predvars = stringr::str_match(term, paste0(predvars, collapse="|"))) %>% 
    dplyr::mutate(term = stringr::str_remove(term, paste0(predvars, collapse="|")))
  
  return(out)
  
}