#' soundex
#' @description Soundex code of name.
#'
#' @param name Name to get soundex code for.
#' @return Soundex code of name.
#' @export
#' @examples
#' \dontrun{
#' soundex("mysurname")
#' }
soundex <- function(name) {
  # Uses phonetic soundex algorithm used by the National Archives
  # May differ from the one used by CDC -- not sure
  stopifnot(is.character(name))

  # Convert 'St ' or 'St.' to 'Saint' first
  name <- gsub("^St[ \\.]", "Saint ", name, ignore.case = TRUE)
  stringdist::phonetic(name)
}
