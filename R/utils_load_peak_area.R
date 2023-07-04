#' Load and clean peak area data
#'
#' @param peak_area_path Character string specifying the path to the peak area data file.
#' @param delim Character string indicating the delimiter used in the file. Defaults to "\t".
#' @param na Character vector of strings to be treated as NA. Defaults to c("N/A", "Unknown").
#' @return A tibble containing the cleaned peak area data.
#' 
#' @export
#' @author Yaoxiang Li \email{yl814@georgetown.edu}
#'
#' Georgetown University, USA
#'
#' License: GPL (>= 3)
load_peak_area <- function(peak_area_path, delim = "\t", na = c("N/A", "Unknown")) {
  loaded_tibble <- readr::read_delim(file = peak_area_path, delim = delim, na = na)
  loaded_tibble <- loaded_tibble %>%
    dplyr::filter(!grepl('Blank', `Sample ID`)) %>%
    dplyr::filter(!grepl('BLANK', `Sample ID`))
  return(loaded_tibble)
}