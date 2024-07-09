#' Calculate QC Statistics and RSD
#'
#' This function calculates the mean, standard deviation, and coefficient of variation
#' for QC data, optionally ignoring NA values. It also calculates the relative standard deviation (RSD)
#' for both Pooled QC and NIST samples. The input data should be a tibble where
#' the first column is the sample ID and all other columns are features.
#'
#' @param data A tibble with the first column as sample IDs and other columns as features.
#' @param qc_string A string pattern to filter Pooled QC samples. Default is "Pooled QC|Pooled_QC".
#' @param nist_string A string pattern to filter NIST samples. Default is "NIST".
#' @param id_col The name of the sample ID column. Default is "sample_id".
#' @param ignore_na Logical, whether to ignore NA values in calculations. Default is TRUE.
#' @return A list containing tibbles with calculated statistics for Pooled QC and NIST samples. If no matching samples are found, returns NULL for that category.
#' @importFrom dplyr filter mutate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang sym
#' @importFrom matrixStats rowSds
#' @author Yaoxiang Li
#' @export
#' @examples
#' # Generate example data
#' example_data <- tibble::tibble(
#'   sample_id = c("Pooled QC 1", "Pooled QC 2", "NIST 1", "NIST 2", "Sample 1"),
#'   feature1 = c(10, 15, 20, NA, 5),
#'   feature2 = c(20, NA, 25, 30, 10),
#'   feature3 = c(NA, 25, 30, 35, 15)
#' )
#'
#' # Calculate QC statistics and RSD
#' rsd_stats <- calculate_qc_rsd(example_data)
#'
#' # Access results
#' pooled_qc_rsd <- rsd_stats$Pooled_QC_RSD
#' nist_rsd <- rsd_stats$NIST_RSD
calculate_qc_rsd <- function(data, qc_string = "Pooled QC|Pooled_QC", nist_string = "NIST", id_col = "sample_id", ignore_na = TRUE) {
  calculate_qc_statistics <- function(data, filter_strings, id_col, ignore_na) {
    qc_tibble <- data |> dplyr::filter(grepl(filter_strings, !!rlang::sym(id_col)))

    if (nrow(qc_tibble) == 0) {
      return(NULL)
    }

    df.transposed <- qc_tibble |>
      tidyr::pivot_longer(cols = -all_of(id_col)) |>
      tidyr::pivot_wider(names_from = all_of(id_col), values_from = "value")

    qc_nm <- colnames(df.transposed)[-1]

    df.transposed <- df.transposed %>%
      dplyr::mutate(
        Mean = rowMeans(.[qc_nm], na.rm = ignore_na),
        `Standard Deviation` = matrixStats::rowSds(as.matrix(.[qc_nm]), na.rm = ignore_na),
        `Coefficient of variation` = `Standard Deviation` / Mean
      )

    return(df.transposed)
  }

  pooled_qc_stats <- calculate_qc_statistics(data, qc_strings, id_col, ignore_na)
  nist_stats <- calculate_qc_statistics(data, nist_strings, id_col, ignore_na)

  return(list(Pooled_QC_RSD = pooled_qc_stats, NIST_RSD = nist_stats))
}


