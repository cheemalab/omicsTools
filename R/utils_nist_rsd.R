#' Calculate the relative standard deviation for NIST samples
#'
#' @param normalized_tibble A tibble normalized by 'is_normalize' or similar function.
#' @param nist_name Character string specifying the name of the NIST sample. Defaults to "NIST".
#' @return A tibble containing the mean, standard deviation, and coefficient of variation for NIST samples.
#'
#' @export
#' @author Yaoxiang Li \email{yl814@georgetown.edu}
#'
#' Georgetown University, USA
#'
#' License: GPL (>= 3)
nist_rsd <- function(normalized_tibble, nist_name = "NIST") {
  nist_tibble <- normalized_tibble %>% dplyr::filter(grepl(nist_name, Sample))
  df.transposed <- nist_tibble %>%
    tidyr::pivot_longer(cols= -1) %>%
    tidyr::pivot_wider(names_from = "Sample",values_from = "value")
  nist_nm <- colnames(df.transposed)[-1]
  df.transposed <- df.transposed %>%
    mutate(Mean = rowMeans(.[nist_nm]),
           `Standard Deviation` = matrixStats::rowSds(as.matrix(.[nist_nm])),
           `Coefficient of variation` = `Standard Deviation` / Mean)
  return(df.transposed)
}


utils::globalVariables(c(".", "Standard Deviation", "Mean"))
