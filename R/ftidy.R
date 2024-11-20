#' ftidy
#' @description Fast version of the tidy function from the broom package that uses the normal approximation for confidence intervals.
#'
#' @param model Regression model.
#' @param exp Option to exponentiate coefficients.
#' @param drop_intercept Option to drop intercept term from the output table.
#' @return Regression model table with confidence intervals.
#' @export
#' @examples
#' \dontrun{
#' ftidy(mod)
#' }
ftidy <- function(model, exp = TRUE, drop_intercept = FALSE) {
  model_tab <- broom::tidy(model, exponentiate = exp)
  mod_ci <- tibble::as_tibble(confint.default(model))
  names(mod_ci) <- c("conf_low", "conf_high")
  
  if (exp) {
    mod_ci <- exp(mod_ci)
  }
  
  out <- cbind(model_tab, mod_ci)
  names(out)[names(out) == "p.value"] <- "p"
  
  out <- out[c("term", "estimate", "conf_low", "conf_high", "p")]
  
  if (drop_intercept) {
    out[!grepl("Intercept", out$term), ]
  } else {
    out
  }
}
