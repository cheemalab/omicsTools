#' Calculate the relative standard deviation for QC samples
#'
#' @param normalized_tibble A tibble normalized by 'is_normalize' or similar function.
#' @param qc_name Character string specifying the name of the QC sample. Defaults to "Pooled QC".
#' @return A tibble containing the mean, standard deviation, and coefficient of variation for QC samples.
#'
#' @export
#' @author Yaoxiang Li \email{yl814@georgetown.edu}
#'
#' Georgetown University, USA
#'
#' License: GPL (>= 3)
qc_rsd <- function(normalized_tibble, qc_name = "Pooled QC") {
  qc_tibble <- normalized_tibble %>% dplyr::filter(grepl(qc_name, Sample))
  df.transposed <- qc_tibble %>%
    tidyr::pivot_longer(cols= -1) %>%
    tidyr::pivot_wider(names_from = "Sample",values_from = "value")
  qc_nm <- colnames(df.transposed)[-1]
  df.transposed <- df.transposed %>%
    mutate(Mean = rowMeans(.[qc_nm]),
           `Standard Deviation` = matrixStats::rowSds(as.matrix(.[qc_nm])),
           `Coefficient of variation` = `Standard Deviation` / Mean)
  return(df.transposed)
}

utils::globalVariables(c(".", "Standard Deviation", "Mean"))
