#' hb_recode
#' @description Recode Scottish NHS boards.
#'
#' @param x NHS board vector.
#' @param recode_to Output format.
#' @param and_choice Choice for displaying and.
#' @return NHS boards in desired form.
#' @export
#' @examples
#' \dontrun{
#' hb_recode(x)
#' }
hb_recode <- function(x, recode_to = "long", and_choice = "and") {
  x <- stringr::str_trim(toupper(x), "both")

  if (recode_to == "short") {
    hb_out <- dplyr::case_when(
      x %in%
        c(
          "S08000001",
          "S08000015",
          "AA",
          "AYRSHIRE AND ARRAN",
          "AYRSHIRE & ARRAN"
        ) ~
        "AA",
      x %in% c("S08000002", "S08000016", "BR", "BOR", "BORDERS") ~ "BR",
      x %in%
        c(
          "S08000003",
          "S08000017",
          "DG",
          "DUMFRIES AND GALLOWAY",
          "DUMFRIES & GALLOWAY"
        ) ~
        "DG",
      x %in% c("S08000004", "S08000018", "S08000029", "FF", "FIFE") ~ "FF",
      x %in% c("S08000005", "S08000019", "FV", "FORTH VALLEY") ~ "FV",
      x %in%
        c(
          "S08000007",
          "S08000021",
          "S08000031",
          "GGC",
          "GC",
          "GG",
          "GGHB",
          "GREATER GLASGOW AND CLYDE",
          "GREATER GLASGOW & CLYDE"
        ) ~
        "GC",
      x %in% c("S08000006", "S08000020", "GR", "GRA", "GRAMPIAN") ~ "GR",
      x %in% c("S08000008", "S08000022", "HG", "HIG", "HIGHLAND", "HIGHLANDS") ~
        "HG",
      x %in%
        c("S08000009", "S08000023", "S08000032", "LN", "LAN", "LANARKSHIRE") ~
        "LN",
      x %in% c("S08000010", "S08000024", "LO", "LOT", "LOTHIAN") ~ "LO",
      x %in% c("S08000011", "S08000025", "OR", "ORK", "ORKNEY") ~ "OR",
      x %in% c("S08000012", "S08000026", "SH", "SHT", "SHETLAND") ~ "SH",
      x %in% c("S08000013", "S08000027", "S08000030", "TY", "TAY", "TAYSIDE") ~
        "TY",
      x %in% c("S08000014", "S08000028", "WI", "WESTERN ISLES") ~ "WI",
      x %in% c("S08200001") ~ "England/Wales/Northern Ireland",
      x %in% c("S08200002") ~ "No Fixed Abode",
      x %in% c("S08200003") ~ "Not Known",
      x %in% c("S08200004") ~ "Outside U.K.",
      x %in% c("S08100001") ~ "National Facility",
      x %in% c("S08100008") ~ "The State Hospital",
      x %in% c("S27000001") ~ "Non-NHS Provider/Location",
      x %in% c("SCOTLAND") ~ "Scotland",
      .default = x
    )
  } else {
    hb_out <- dplyr::case_when(
      x %in%
        c(
          "S08000001",
          "S08000015",
          "AA",
          "AYRSHIRE AND ARRAN",
          "AYRSHIRE & ARRAN"
        ) ~
        "Ayrshire and Arran",
      x %in% c("S08000002", "S08000016", "BR", "BOR", "BORDERS") ~ "Borders",
      x %in%
        c(
          "S08000003",
          "S08000017",
          "DG",
          "DUMFRIES AND GALLOWAY",
          "DUMFRIES & GALLOWAY"
        ) ~
        "Dumfries and Galloway",
      x %in% c("S08000004", "S08000018", "S08000029", "FF", "FIFE") ~ "Fife",
      x %in% c("S08000005", "S08000019", "FV", "FORTH VALLEY") ~ "Forth Valley",
      x %in%
        c(
          "S08000007",
          "S08000021",
          "S08000031",
          "GGC",
          "GC",
          "GG",
          "GGHB",
          "GREATER GLASGOW AND CLYDE",
          "GREATER GLASGOW & CLYDE"
        ) ~
        "Greater Glasgow and Clyde",
      x %in% c("S08000006", "S08000020", "GR", "GRA", "GRAMPIAN") ~ "Grampian",
      x %in% c("S08000008", "S08000022", "HG", "HIG", "HIGHLAND", "HIGHLANDS") ~
        "Highland",
      x %in%
        c("S08000009", "S08000023", "S08000032", "LN", "LAN", "LANARKSHIRE") ~
        "Lanarkshire",
      x %in% c("S08000010", "S08000024", "LO", "LOT", "LOTHIAN") ~ "Lothian",
      x %in% c("S08000011", "S08000025", "OR", "ORK", "ORKNEY") ~ "Orkney",
      x %in% c("S08000012", "S08000026", "SH", "SHT", "SHETLAND") ~ "Shetland",
      x %in% c("S08000013", "S08000027", "S08000030", "TY", "TAY", "TAYSIDE") ~
        "Tayside",
      x %in% c("S08000014", "S08000028", "WI", "WESTERN ISLES") ~
        "Western Isles",
      x %in% c("S08200001") ~ "England/Wales/Northern Ireland",
      x %in% c("S08200002") ~ "No Fixed Abode",
      x %in% c("S08200003") ~ "Not Known",
      x %in% c("S08200004") ~ "Outside U.K.",
      x %in% c("S08100001") ~ "National Facility",
      x %in% c("S08100008") ~ "The State Hospital",
      x %in% c("S27000001") ~ "Non-NHS Provider/Location",
      x %in% c("SCOTLAND") ~ "Scotland",
      .default = x
    ) %>%
      stringr::str_replace_all("(\\&|\\band\\b)", and_choice) %>%
      stringr::str_replace_all("UNKNOWN", "Unknown")
  }

  return(hb_out)
}
