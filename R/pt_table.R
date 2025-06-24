#' Title pt_table
#' @description
#' Summarise number of people, number of events, and sum of person time by group.
#'
#' @param data Input dataset.
#' @param id_var unique identifier for individuals in dataset.
#' @param outcome binary outcome (outcome = 1).
#' @param vars Variables by which to group.
#' @param pt Variable containing person time.
#' @param var_col Option to include variable name as a separate column.
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' pt_table(data, id_var="bbv_id", outcome="drd", vars=c("sex", "hb"), pt="person-years")
#' }
#'
pt_table <- function(
  data,
  id_var = "bbv_id",
  outcome,
  vars,
  pt,
  var_col = TRUE
) {
  out <- purrr::map2_df(
    .x = rlang::syms(vars),
    .y = vars,
    .f = ~ {
      data %>%
        filter(!is.na(!!{{ .x }})) %>%
        dplyr::group_by(!!{{ .x }}) %>%
        dplyr::summarise(
          person = dplyr::n_distinct(!!rlang::ensym(id_var), na.rm = TRUE),
          outcome = sum(!!rlang::ensym(outcome), na.rm = TRUE),
          pt = sum(!!rlang::ensym(pt), na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename(category = 1) %>%
        dplyr::mutate(category = as.character(category)) %>%
        dplyr::add_row(category = .y, .before = 1) %>%
        dplyr::mutate(var = .y, .before = 1)
    }
  )

  out <- out %>%
    dplyr::add_row(
      var = "Total",
      category = "Total",
      person = nrow(dplyr::distinct(data, !!rlang::ensym(id_var))),
      outcome = dplyr::pull(dplyr::summarise(
        data,
        sum(
          !!rlang::ensym(outcome)
        )
      )),
      pt = dplyr::pull(dplyr::summarise(
        data,
        sum(
          !!rlang::ensym(pt)
        )
      )),
      .before = 1
    ) %>%
    tibble::as_tibble()

  out_vars <- names(out)

  if (!var_col) {
    out_vars <- setdiff(out_vars, "var")
  }

  out[out_vars]
}
