#' np_table
#' @description Produce table of counts and percentages for a list of variables in a dataset.
#'
#' @param data Input dataset.
#' @param vars Character vector of variables to get counts and percentages for.
#' @param digits Number of digits to use after the decimal point in percentages.
#' @param np_only Option to return only the combined count and percentage variable.
#' @param header Option to use variable names as headers.
#' @param var_col Option to include variable name as a separate column.
#' @return Data frame with counts and percentages for a list of variables.
#' @export
#' @examples
#' \dontrun{
#' np_table(dat, c("var1", "var2"), digits = 1)
#' }
np_table <- function(
  data,
  vars,
  digits = 0,
  np_only = TRUE,
  header = TRUE,
  var_col = FALSE
) {
  out <- purrr::map2_df(
    .x = dplyr::syms(vars),
    .y = vars,
    .f = ~ {
      data %>%
        janitor::tabyl(!!{{ .x }}) %>%
        dplyr::rename(category = 1) %>%
        dplyr::mutate(category = as.character(category)) %>%
        dplyr::add_row(category = .y, .before = 1) %>%
        dplyr::mutate(var = .y, .before = 1)
    }
  ) %>%
    dplyr::add_row(
      var = "Total",
      category = "Total",
      n = nrow(data),
      percent = 1,
      .before = 1
    ) %>%
    dplyr::mutate(n_perc = np(.data$n, .data$percent, digits = digits)) %>%
    tibble::as_tibble()

  if (!header) {
    out <- dplyr::filter(out, !is.na(.data$n))
  }

  out_vars <- names(out)

  if (np_only) {
    out_vars <- setdiff(out_vars, c("n", "percent", "valid_percent"))
  }

  if (!var_col) {
    out_vars <- setdiff(out_vars, "var")
  }

  out[out_vars]
}
