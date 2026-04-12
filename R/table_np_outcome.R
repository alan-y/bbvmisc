#' table_np_outcome
#' @description
#' Produce table of counts of outcome by variable, and percentage of full data within group.
#'
#' @param data Input dataset.
#' @param outcome Binary variable with outcome as "1".
#' @param vars Character vector of variables to get counts and percentages for.
#' @param digits Number of digits to use after the decimal point in percentages.
#' @param np_only Option to return only the combined count and percentage variable.
#' @param var_col Option to include variable name as a separate column.
#' @return Data frame with counts and percentages of outcome for a list of variables.
#' @export
#'
#' @examples
#' \dontrun{
#' table_np_outcome(data, outcome = "death", c("var1", "var2"))
#' }
table_np_outcome <- function(
  data,
  outcome,
  vars,
  digits = 0,
  np_only = TRUE,
  var_col = FALSE
) {
  out <- purrr::map2_df(
    .x = rlang::syms(vars),
    .y = vars,
    .f = ~ {
      data %>%
        filter(!is.na(!!{{ .x }})) %>%
        janitor::tabyl(!!{{ .x }}, !!rlang::ensym(outcome)) %>%
        dplyr::rename(category = 1, outcome0 = 2, outcome1 = 3) %>%
        dplyr::mutate(category = as.character(category)) %>%
        dplyr::add_row(category = .y, .before = 1) %>%
        dplyr::mutate(var = .y, .before = 1)
    }
  )

  total <- data %>%
    dplyr::filter(!is.na(vars[1])) %>%
    janitor::tabyl(vars[1], !!rlang::ensym(outcome)) %>%
    dplyr::rename(category = 1, outcome0 = 2, outcome1 = 3) %>%
    dplyr::mutate(var = "Total", category = "Total")

  out <- total %>%
    dplyr::bind_rows(out) %>%
    dplyr::mutate(
      n_perc_outcome = bbvmisc::np(
        .data$outcome1,
        (.data$outcome1 / (.data$outcome1 + .data$outcome0)),
        digits = digits
      )
    ) %>%
    tibble::as_tibble()

  out_vars <- names(out)

  if (np_only) {
    out_vars <- setdiff(out_vars, c("outcome0", "outcome1"))
  }

  if (!var_col) {
    out_vars <- setdiff(out_vars, "var")
  }

  out[out_vars]
}
