#' Normalize loaded tibble with IS tibble
#'
#' @param loaded_tibble A tibble loaded by 'load_peak_area' or similar function.
#' @param is_tibble A tibble loaded by 'load_is_area' or similar function.
#' @return A tibble that is normalized by the IS tibble.
#'
#' @export
#' @author Yaoxiang Li \email{yl814@georgetown.edu}
#'
#' Georgetown University, USA
#'
#' License: GPL (>= 3)
is_normalize <- function(loaded_tibble, is_tibble) {
  normalized_tibble <- loaded_tibble[, 4:ncol(loaded_tibble)] /  is_tibble[, 4:ncol(is_tibble)]
  normalized_tibble <- dplyr::bind_cols(loaded_tibble[, 1:3], normalized_tibble) %>%
    dplyr::mutate(Sample = paste0(`Sample Name`, "_", `Sample ID`), .before = "Sample Name") %>%
    dplyr::select(-c("Sample Name", "Sample ID", "Sample Type"))
  return(normalized_tibble)
}

utils::globalVariables(c("Sample", "Sample ID", "Sample Name"))
