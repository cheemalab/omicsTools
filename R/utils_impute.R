#' Impute function
#'
#' This function performs data cleaning and imputation on a given data matrix.
#' It removes blank and NIST samples, features with NA values more than the specified threshold,
#' and imputes remaining NA values with half of the smallest non-NA value.
#'
#' @param data A data frame containing the sample data. The first column should contain
#' the sample identifiers, and the rest of the columns contain the peaks.
#'
#' @param percent A numeric value between 0 and 1 representing the threshold of the
#' percentage of NA values a feature should have for it to be removed from the dataset.
#' Default value is 0.2.
#'
#' @return A data frame with the first column as the sample identifiers and
#' the rest of the columns containing the cleaned and imputed peak intensities.
#' @examples
#'
#' # Load the CSV data
#' data_file <- system.file("extdata", "example1.csv", package = "omicsTools")
#' data <- readr::read_csv(data_file)
#' # Apply the impute function
#' imputed_data <- omicsTools::impute(data, percent = 0.2)
#'
#' \donttest{
#' # Write the imputed data to a new CSV file
#' readr::write_csv(imputed_data, paste0(tempdir(), "/imputed_data.csv"))
#' }
#' @importFrom dplyr filter bind_cols
#' @export
#' @author Yaoxiang Li \email{yl814@georgetown.edu}
#'
#' Georgetown University, USA
#'
#' License: GPL (>= 3)
impute <- function(data, percent = 0.2, .remove_nist = TRUE) {

  # Filter out Blank and NIST samples
  data <- data %>% dplyr::filter(!grepl('Blank', Sample))
  data <- data %>% dplyr::filter(!grepl('BLANK', Sample))


  if (.remove_nist) {
      data <- data %>% dplyr::filter(!grepl('NIST', Sample))
  }

  # Convert the data except the sample identifiers to a matrix
  peaks <- as.matrix(data[, -1])

  # Identify features with NA values more than the specified threshold
  na_features <- which(colMeans(is.na(peaks)) > percent)

  if (length(na_features)) {
    # Remove features if NA > threshold rate
    peaks <- peaks[ ,-na_features]
    message(paste0(
      length(na_features),
      " features removed by percent of missing values > ",
      as.character(percent)
    ))
  } else {
    message('No features removed by missing values')
  }

  for (i in 1:ncol(peaks)) {
    v <- peaks[, i]

    # If there are any NA values, replace them with half the smallest non-NA value
    if (any(is.na(v))) {
      v[which(is.na(v))] <- min(v[which(!is.na(v))][-1]) / 2
      peaks[, i] <- v
    }
  }
  # Combine the sample identifiers with the cleaned and imputed peaks
  return(dplyr::bind_cols(data[, 1], peaks))
}


#' Handle Missing Values in a Tibble
#'
#' This function filters features based on a missing value threshold and imputes missing values using various methods.
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

  # 1. Feature-wise Missing Value Filtering
  missing_percentage <- purrr::map_dbl(data, ~ mean(is.na(.)))
  data_filtered <- data %>%
    dplyr::select(which(missing_percentage <= threshold))

  # 2. Missing Value Imputation
  if (imputation_method == "mean") {
    data_imputed <- data_filtered %>%
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  } else if (imputation_method == "median") {
    data_imputed <- data_filtered %>%
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  } else if (imputation_method == "mode") {
    get_mode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    data_imputed <- data_filtered %>%
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), get_mode(., na.rm = TRUE), .)))
  } else if (imputation_method == "half_min") {
    data_imputed <- data_filtered %>%
      dplyr::mutate(across(everything(), ~ifelse(is.na(.), min(., na.rm = TRUE) * 0.5, .)))
  } else {
    stop("Invalid imputation method provided!")
  }

  return(data_imputed)
}
