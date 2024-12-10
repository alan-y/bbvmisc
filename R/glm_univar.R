#' glm_univar
#' @description Run a univariable `glm()` for a vector of predictor variables with tidy df output
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

glm_univar <- function(data, respvar, predvars, family = "poisson", exp = TRUE, 
                       additional_term = "", fast = TRUE) {
  
  formulas <- purrr::map(
    .x = predvars, 
    .f = ~reformulate(paste(.x, additional_term), response = respvar)
  )  
  
  names(formulas) <- predvars          
  mod_list <- purrr::map(formulas, ~glm(.x, family = family, data = data))
  
  if (!fast) {
    out <- purrr::map(
      .x = mod_list, 
      .f = ~{broom::tidy(.x, exponentiate = exp, conf.int = TRUE) %>% 
          add_refcats(.x)}
    ) 
  } else {
    out <- purrr::map(
      .x = mod_list, 
      .f = ~{ftidy(.x, exp = exp) %>% 
          add_refcats(.x)}
    ) 
  }
  
  out <- dplyr::bind_rows(out) %>% 
    dplyr::filter(term != "(Intercept)") %>% 
    dplyr::mutate(
      predvars = stringr::str_match(term, paste0(predvars, collapse = "|")),
      term = stringr::str_remove(term, paste0(predvars, collapse = "|")),
      .before = 1
    )
  
  return(out)
}