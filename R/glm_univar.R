#' glm_univar
#' @description Run a univariable glm() for a vector of predictor variables with tidy df output
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

glm_univar <- function(data, respvar, predvars, fast=TRUE, family="poisson", additional_term="") {
  
  formulas <- purrr::map(.x=predvars, 
                         .f=~{reformulate(paste(.x, additional_term), 
                                          response = respvar)})  
  
  names(formulas) <- predvars          
  
  mod_list <- purrr::map(.x=formulas, 
                         .f=~{glm(.x, family=family, data=data)})
  
  if (!fast) {
    out <- purrr::map(.x=mod_list, 
                      .f=~{broom::tidy(.x, exponentiate=TRUE, conf.int=TRUE)}) 
  }
  
  else {
    out <- purrr::map(.x=mod_list, 
                      .f=ftidy) 
  }
  
  out <- out %>% 
    dplyr::bind_rows(.) %>% 
    add_refcats(., model) %>% 
    dplyr::filter(term != "(Intercept)") %>% 
    dplyr::mutate(predvars = stringr::str_match(term, paste0(predvars, collapse="|"))) %>% 
    dplyr::mutate(term = stringr::str_remove(term, paste0(predvars, collapse="|")))
  
  return(out)
  
}