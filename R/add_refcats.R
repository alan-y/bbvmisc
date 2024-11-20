#' add_refcats
#' @description Add reference categories for factor variables to a regression coefficients dataframe.
#'
#' @param df Regression coefficients dataframe.
#' @param model Regression model.
#' @param ref_val Coefficient value for reference category.
#' @param sort_factors Option to sort factor variables in the table alphabetically.
#' @return Regression model table with factor reference categories added.
#' @export
#' @examples
#' \dontrun{
#' add_refcats(df, mod)
#' }
add_refcats <- function(df, model, ref_val = 1, sort_factors = FALSE) {
  # Number of categories in each variable
  num_cats <- purrr::map_int(model$xlevels, length)
  
  if (any(num_cats) > 0) {
    # Find factor variables
    factor_vars <- names(num_cats)[num_cats > 0]
    # Drop any that are not in the regression table
    in_df_terms <- purrr::map_lgl(
      factor_vars, 
      ~any(stringr::str_detect(df$term, .x))
    )
    factor_vars <- factor_vars[in_df_terms]
    mod_xlevels <- model$xlevels[names(model$xlevels) %in% factor_vars]
    
    # Find reference categories of factor variables
    refcats <- purrr::map_chr(mod_xlevels, ~.x[1])
    refcats_term <- paste0(factor_vars, refcats)
    
    out <- df
    
    for (i in seq_along(factor_vars)) {
      rows <- stringr::str_which(out$term, factor_vars[i])
      row_start <- min(rows)
      row_end <- max(rows)
      out <- dplyr::add_row(out, term = refcats_term[i], estimate = ref_val, 
                            .before = row_start)
      if (sort_factors) {
        out_sub <- out[c(rows, row_end + 1), ]
        out_sub <- out_sub[order(out_sub$term), ]
        out[c(rows, row_end + 1), ] <- out_sub
      }
    }
  }
  
  return(out)
}
