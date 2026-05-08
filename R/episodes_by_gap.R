utils::globalVariables(c("gap", "new_episode", "episode_id"))
# A gap-based episode constructor for longitudinal/event data
#'
#' Constructs episodes for each individual based on gaps between consecutive
#' date records. A new episode is started when the gap between dates exceeds
#' a specified threshold, measured in complete calendar units.
#'
#' @param data A data frame containing longitudinal or event-level records.
#' @param id_col Column identifying individuals (unquoted).
#' @param date_col Column containing dates or date-time values (unquoted).
#' @param calendar_unit Character. Calendar unit used to compute gaps between
#'   consecutive dates. One of \code{"years"}, \code{"months"}, or \code{"days"}.
#' @param gap_threshold Numeric. Maximum allowed gap (in complete calendar units)
#'   between consecutive dates before a new episode is started.
#' @param run_arrange Logical. If \code{TRUE}, the data are sorted by
#'   \code{id_col} and \code{date_col} before episode construction.
#' @param collapse_episodes Logical. If \code{TRUE}, returns one row per episode;
#'   otherwise returns the original data with episode identifiers.
#'
#' @return
#' If \code{collapse_episodes = FALSE}, returns the input data with additional
#' columns:
#' \describe{
#'   \item{gap}{Gap between consecutive dates (in complete calendar units)}
#'   \item{new_episode}{Logical indicator for the start of a new episode}
#'   \item{episode_id}{Episode identifier (unique within each individual)}
#' }
#'
#' If \code{collapse_episodes = TRUE}, returns one row per episode with:
#' \describe{
#'   \item{episode_start}{First date in the episode}
#'   \item{episode_end}{Last date in the episode}
#'   \item{n_records}{Number of records in the episode}
#' }
#'
#' @details
#' Episodes are defined as contiguous sequences of records where the gap between
#' consecutive dates does not exceed \code{gap_threshold}.
#'
#' Gaps are calculated as the number of complete calendar units between dates
#' using \code{count_calendar_units()}. The first record for each individual
#' always starts a new episode.
#'
#' Records must be sortable by \code{id_col} and \code{date_col}, and all records
#' for each individual must be contiguous after sorting.
#'
#' @examples
#' \dontrun{
#' df %>%
#'   episodes_by_gap(iain, paid_date, gap_threshold = 2,
#'                   calendar_unit = "months")
#' }
#'
#' \dontrun{
#' df %>%
#'   episodes_by_gap(iain, paid_date,
#'                   gap_threshold = 2,
#'                   calendar_unit = "months",
#'                   collapse_episodes = TRUE)
#' }
#'
#' @export
episodes_by_gap <- function(
  data,
  id_col,
  date_col,
  calendar_unit = c("years", "months", "days"),
  gap_threshold = 2,
  run_arrange = TRUE,
  collapse_episodes = FALSE
) {
  calendar_unit <- match.arg(calendar_unit)
  id_col <- rlang::ensym(id_col)
  date_col <- rlang::ensym(date_col)

  if (run_arrange) {
    data <- data %>%
      dplyr::arrange(!!id_col, !!date_col)
  }

  data <- data %>%
    dplyr::mutate(
      gap = count_calendar_units(
        dplyr::lag(!!date_col),
        !!date_col,
        unit = calendar_unit
      ),
      gap = dplyr::if_else(dplyr::lag(!!id_col) != !!id_col, NA_integer_, gap),
      new_episode = is.na(gap) | gap > gap_threshold,
      episode_id = cumsum(new_episode)
    )

  if (collapse_episodes) {
    data <- data %>%
      dplyr::summarise(
        .by = c(!!id_col, episode_id),
        episode_start = min(!!date_col),
        episode_end = max(!!date_col),
        n_records = dplyr::n()
      )
  }

  return(data)
}
