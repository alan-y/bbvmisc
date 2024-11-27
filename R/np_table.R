#' np_table
#' @description Produce table of counts and percentages for a list of variables in a dataset.
#'
#' @param data Input dataset.
#' @param vars Character vector of variables to get counts and percentages for.
#' @param np_only Option to return only the combined count and percentage variable.
#' @return Data frame with counts and percentages for a list of variables.
#' @export
#' @examples
#' \dontrun{
#' np_table(dat, c("var1", "var2"))
#' }
np_table <- function(data, vars, np_only = TRUE) {
  out <- purrr::map2_df(
    .x = dplyr::syms(vars),
    .y = vars,
    .f = ~ {data %>% 
        janitor::tabyl(!!{{.x}}) %>% 
        dplyr::rename(category = 1) %>% 
        dplyr::mutate(category = as.character(category)) %>% 
        dplyr::add_row(category = .y, .before = 1)
    }) %>%
    dplyr::add_row(category = "Total", n = nrow(data), percent = 1, 
                   .before = 1) %>% 
    dplyr::mutate(n_perc = np(.data$n, .data$percent))
  
  if (np_only) {
    tibble::as_tibble(dplyr::select(out, "category", "n_perc"))
  } else {
    tibble::as_tibble(dplyr::select(out, "category", "n", "percent", "n_perc"))
  }
}
