utils::globalVariables(c("gap", "new_episode", "episode_id"))
# A gap-based episode constructor for longitudinal/event data
#' episodes_by_gap
#'
#' Constructs episodes for each individual where consecutive records are
#' separated by no more than a specified gap. A new episode is started when
#' the gap between dates exceeds the threshold.
#'
#' @param data A data frame.
#' @param id_col Column identifying individuals (unquoted).
#' @param date_col Column containing dates (unquoted).
#' @param gap_threshold Numeric. Maximum allowed gap between consecutive
#'   dates before a new episode is started.
#' @param time_unit Character. Unit for gap calculation. One of
#'   `"days"` or `"months"`.
#' @param collapse_episodes Logical. If `TRUE`, returns one row per episode;
#'   otherwise returns the original data with episode identifiers.
#'
#' @return
#' If `collapse_episodes = FALSE`, returns the input data with additional columns:
#' \describe{
#'   \item{gap}{Gap between consecutive dates (in selected units)}
#'   \item{new_episode}{Logical indicator for the start of a new episode}
#'   \item{episode_id}{Episode identifier (unique within `id_col` when combined)}
#' }
#'
#' If `collapse_episodes = TRUE`, returns one row per episode with:
#' \describe{
#'   \item{episode_start}{First date in the episode}
#'   \item{episode_end}{Last date in the episode}
#'   \item{n_records}{Number of records in the episode}
#' }
#'
#' @details
#' Episodes are defined as contiguous sequences of records where the gap
#' between consecutive dates does not exceed `gap_threshold`.
#'
#' Gaps are calculated using `lubridate::time_length()` and represent the
#' number of complete units (e.g. full days or months) between dates.
#'
#' Data must be sortable by `id_col` and `date_col`, and records for each
#' individual must be contiguous after sorting.
#'
#' @examples
#' \dontrun{
#' df %>%
#'   episodes_by_gap(iain, paid_date, gap_threshold = 2L, time_unit = "months")
#' }
#' \dontrun{
#' df %>%
#'   episodes_by_gap(iain, paid_date, gap_threshold = 2L, collapse_episodes = TRUE)
#'   }
#'
#' @export
episodes_by_gap <- function(
  data,
  id_col,
  date_col,
  gap_threshold = 2,
  time_unit = c("months", "days"),
  collapse_episodes = FALSE
) {
  time_unit <- match.arg(time_unit)
  id_col <- rlang::ensym(id_col)
  date_col <- rlang::ensym(date_col)

  # time difference function
  time_diff <- function(x, y) {
    interval <- lubridate::interval(x, y)
    unit <- switch(time_unit, "months" = "months", "days" = "days")
    as.integer(floor(lubridate::time_length(interval, unit)))
  }

  data <- data %>%
    dplyr::arrange(!!id_col, !!date_col) %>%
    dplyr::mutate(
      gap = time_diff(dplyr::lag(!!date_col), !!date_col),
      gap = dplyr::if_else(dplyr::lag(!!id_col) != !!id_col, NA_integer_, gap),
      new_episode = is.na(gap) | gap > gap_threshold,
      episode_id = cumsum(new_episode)
    )

  if (collapse_episodes) {
    data <- data %>%
      dplyr::group_by(!!id_col, episode_id) %>%
      dplyr::summarise(
        episode_start = min(!!date_col),
        episode_end = max(!!date_col),
        n_records = dplyr::n(),
        .groups = "drop"
      )
  }

  return(data)
}
