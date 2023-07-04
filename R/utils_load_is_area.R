#' Load and clean IS area data
#'
#' @param is_area_path Character string specifying the path to the IS area data file.
#' @param delim Character string indicating the delimiter used in the file. Defaults to "\t".
#' @param na Character vector of strings to be treated as NA. Defaults to c("N/A", "Unknown").
#' @return A tibble containing the cleaned IS area data.
#' 
#' @export
#' @author Yaoxiang Li \email{yl814@georgetown.edu}
#'
#' Georgetown University, USA
#'
#' License: GPL (>= 3)
load_is_area <- function(is_area_path, delim = "\t", na = c("N/A", "Unknown")) {
  is_tibble <- readr::read_delim(file = is_area_path, delim = delim, na = na)
  is_tibble <- is_tibble %>%
    dplyr::filter(!grepl('Blank', `Sample ID`)) %>%
    dplyr::filter(!grepl('BLANK', `Sample ID`))
  return(is_tibble)
}