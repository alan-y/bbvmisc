#' hb_list
#'
#' @description List of NHS board names in Scotland. Helps to turn a healthboard
#' variable into a factor to ensure all boards are listed in output tables,
#' including those with zero counts.
#'
#' @param scot Boolean. Include Scotland in the list.
#' @return Character vector of NHS boards in Scotland.
#' @export
#' @examples
#' \dontrun{
#' hb_list()
#' }
hb_list <- function(scot = FALSE) {
  hboards <- c(
    "Ayrshire and Arran",
    "Borders",
    "Dumfries and Galloway",
    "Fife",
    "Forth Valley",
    "Grampian",
    "Greater Glasgow and Clyde",
    "Highland",
    "Lanarkshire",
    "Lothian",
    "Orkney",
    "Shetland",
    "Tayside",
    "Western Isles"
  )

  if (scot) {
    c(hboards, "Scotland")
  } else {
    hboards
  }
}
