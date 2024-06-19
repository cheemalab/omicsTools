#' Handle Missing Values in a Tibble
#'
#' This function filters features based on a missing value threshold and imputes missing values using various methods.
#' It provides an interactive experience with CLI emojis and progress bars.
#'
#' @param data A tibble containing the data with potential missing values.
#' @param threshold A numeric value between 0 and 1 representing the maximum allowable proportion of missing values in a feature. Default is 0.20.
#' @param imputation_method A character string indicating the method to use for imputation. Valid methods are "mean", "median", "mode", and "half_min". Default is "mean".
#' @return A tibble with filtered features and imputed missing values.
#' @examples
#' data <- tibble::tibble(
#'   Feature1 = c(1, 2, NA, 4, 5),
#'   Feature2 = c(NA, 2, 3, 4, NA),
#'   Feature3 = c(1, NA, 3, NA, 5)
#' )
#' imputed_data <- data |> handle_missing_values(threshold = 0.20, imputation_method = "median")
#' print(imputed_data)
#' @export
#' @author Yaoxiang Li
handle_missing_values <- function(data, threshold = 0.20, imputation_method = "mean") {

  # Ensure the data is a tibble
  data <- tibble::as_tibble(data)

  # Display starting message
  cli::cli_alert_info("Starting missing value handling... 🚀")

  # 1. Feature-wise Missing Value Filtering
  cli::cli_alert_info("Calculating missing value percentages... ⏳")
  missing_percentage <- data |> purrr::map_dbl(~ mean(is.na(.)))

  cli::cli_progress_bar("Filtering features based on missing value threshold", total = ncol(data))
  data_filtered <- data |>
    dplyr::select(which(missing_percentage <= threshold))
  cli::cli_progress_done()

  if (ncol(data_filtered) < ncol(data)) {
    cli::cli_alert_success("{ncol(data) - ncol(data_filtered)} features removed due to missing values exceeding the threshold. ✅")
  } else {
    cli::cli_alert_success("No features removed based on the missing value threshold. ✅")
  }

  # 2. Missing Value Imputation
  cli::cli_alert_info("Imputing missing values using method: {imputation_method} 🔧")

  cli::cli_progress_bar("Imputing missing values", total = ncol(data_filtered))
  if (imputation_method == "mean") {
    data_imputed <- data_filtered |>
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  } else if (imputation_method == "median") {
    data_imputed <- data_filtered |>
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  } else if (imputation_method == "mode") {
    get_mode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    data_imputed <- data_filtered |>
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), get_mode(., na.rm = TRUE), .)))
  } else if (imputation_method == "half_min") {
    data_imputed <- data_filtered |>
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), min(., na.rm = TRUE) * 0.5, .)))
  } else {
    stop("Invalid imputation method provided! ❌")
  }
  cli::cli_progress_done()

  cli::cli_alert_success("Missing value handling completed! 🎉")

  return(data_imputed)
}
