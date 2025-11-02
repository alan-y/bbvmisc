#' format_p
#' @description Sets the file path for whether or not the user is using R on the desktop or on the server.
#'
#' @param x Input file path.
#' @param server_path Start of the server path.
#' @return File path after desktop or server detection.
#' @export
#' @examples
#' \dontrun{
#' get_path("//stats/my_folder")
#' }
get_path <- function(x, server_path = "conf") {
  if (on_server() && stringr::str_detect(x, "stats")) {
    stringr::str_replace(x, "//stats", paste0("//", server_path))
  } else if (!on_server() && stringr::str_detect(x, server_path)) {
    stringr::str_replace(x, paste0("//", server_path), "//stats")
  } else {
    x
  }
}
