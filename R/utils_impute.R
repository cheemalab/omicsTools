#' Handle Missing Values in a Tibble
#'
#' This function filters features based on a missing value threshold and imputes missing values using various methods.
#' Metadata columns are specified by the user and are exempt from filtering and imputation.
#'
#' @param data A tibble containing the data with potential missing values.
#' @param threshold A numeric value between 0 and 1 representing the maximum allowable proportion of missing values in a feature. Default is 0.20.
#' @param imputation_method A character string indicating the method to use for imputation. Valid methods are "mean", "median", "mode", and "half_min". Default is "mean".
#' @param metadata_cols A vector of column names or indices to be treated as metadata, exempt from filtering and imputation. Default is NULL.
#' @return A tibble with filtered features and imputed missing values.
#' @examples
#' data <- tibble::tibble(
#'   Feature1 = c(1, 2, NA, 4, 5),
#'   Feature2 = c(NA, 2, 3, 4, NA),
#'   Feature3 = c(1, NA, 3, NA, 5),
#'   Metadata = c("A", "B", "C", "D", "E")
#' )
#' imputed_data <- handle_missing_values(
#'   data,
#'   threshold = 0.20,
#'   imputation_method = "half_min",
#'   metadata_cols = "Metadata"
#' )
#' print(imputed_data)
#' @export
#' @author Yaoxiang Li
handle_missing_values <- function(data, threshold = 0.20, imputation_method = "half_min", metadata_cols = NULL) {

  # Ensure the data is a tibble
  data <- tibble::as_tibble(data)

  # Convert metadata_cols to names if indices are provided
  if (!is.null(metadata_cols) && is.numeric(metadata_cols)) {
    metadata_cols <- names(data)[metadata_cols]
  }

  # Identify non-metadata columns
  non_metadata_cols <- setdiff(names(data), metadata_cols)

  # Display starting message
  cli::cli_alert_info("Starting missing value handling... \ud83d\ude80")

  # 1. Feature-wise Missing Value Filtering
  cli::cli_alert_info("Calculating missing value percentages... \u23f3")
  # Author: Yaoxiang Li
  missing_percentage <- vapply(data[non_metadata_cols], function(x) mean(is.na(x)), numeric(1))

  cli::cli_progress_bar("Filtering features based on missing value threshold", total = length(non_metadata_cols))
  filtered_cols <- names(missing_percentage)[missing_percentage <= threshold]
  data_filtered <- data |> dplyr::select(all_of(c(filtered_cols, metadata_cols)))
  cli::cli_progress_done()

  if (length(filtered_cols) < length(non_metadata_cols)) {
    cli::cli_alert_success("{length(non_metadata_cols) - length(filtered_cols)} features removed due to missing values exceeding the threshold. \u2705")
  } else {
    cli::cli_alert_success("No features removed based on the missing value threshold. \u2705")
  }

  # 2. Missing Value Imputation
  cli::cli_alert_info("Imputing missing values using method: {imputation_method} \ud83d\udd27")

  cli::cli_progress_bar("Imputing missing values", total = length(filtered_cols))
  if (imputation_method == "mean") {
    data_imputed <- data_filtered |> dplyr::mutate(across(all_of(filtered_cols), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  } else if (imputation_method == "median") {
    data_imputed <- data_filtered |> dplyr::mutate(across(all_of(filtered_cols), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  } else if (imputation_method == "mode") {
    # Author: Yaoxiang Li
    get_mode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    data_imputed <- data_filtered |> dplyr::mutate(across(all_of(filtered_cols), ~ifelse(is.na(.), get_mode(.), .)))
  } else if (imputation_method == "half_min") {
    data_imputed <- data_filtered |> dplyr::mutate(across(all_of(filtered_cols), ~ifelse(is.na(.), min(., na.rm = TRUE) * 0.5, .)))
  } else {
    stop("Invalid imputation method provided! \u274c")
  }
  cli::cli_progress_done()

  cli::cli_alert_success("Missing value handling completed! \ud83c\udf89")

  return(data_imputed)
}
