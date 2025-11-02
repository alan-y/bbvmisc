#' on_server
#' @description Detect if user is using the R on a server.
#'
#' @return Boolean.
#' @export
#' @examples
#' \dontrun{
#' on_server()
#' }
on_server <- function() {
  if (interactive()) {
    rs_mode <- rstudioapi::versionInfo()[["mode"]]
    out <- rs_mode == "server"
  } else {
    system_name <- tolower(Sys.info()[["sysname"]])
    out <- system_name == "linux"
  }
  out
}
