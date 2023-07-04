#' Load and clean peak area data
#'
#' @param peak_area_path Character string specifying the path to the peak area data file.
#' @param delim Character string indicating the delimiter used in the file. Defaults to "\t".
#' @param na Character vector of strings to be treated as NA. Defaults to c("N/A", "Unknown").
#' @return A tibble containing the cleaned peak area data.
load_peak_area <- function(peak_area_path, delim = "\t", na = c("N/A", "Unknown")) {
  loaded_tibble <- readr::read_delim(file = peak_area_path, delim = delim, na = na)
  loaded_tibble <- loaded_tibble %>%
    dplyr::filter(!grepl('Blank', `Sample ID`)) %>%
    dplyr::filter(!grepl('BLANK', `Sample ID`))
  return(loaded_tibble)
}

#' Load and clean IS area data
#'
#' @param is_area_path Character string specifying the path to the IS area data file.
#' @param delim Character string indicating the delimiter used in the file. Defaults to "\t".
#' @param na Character vector of strings to be treated as NA. Defaults to c("N/A", "Unknown").
#' @return A tibble containing the cleaned IS area data.
load_is_area <- function(is_area_path, delim = "\t", na = c("N/A", "Unknown")) {
  is_tibble <- readr::read_delim(file = is_area_path, delim = delim, na = na)
  is_tibble <- is_tibble %>%
    dplyr::filter(!grepl('Blank', `Sample ID`)) %>%
    dplyr::filter(!grepl('BLANK', `Sample ID`))
  return(is_tibble)
}

#' Normalize loaded tibble with IS tibble
#'
#' @param loaded_tibble A tibble loaded by 'load_peak_area' or similar function.
#' @param is_tibble A tibble loaded by 'load_is_area' or similar function.
#' @return A tibble that is normalized by the IS tibble.
is_normalize <- function(loaded_tibble, is_tibble) {
  normalized_tibble <- loaded_tibble[, 4:ncol(loaded_tibble)] /  is_tibble[, 4:ncol(is_tibble)]
  normalized_tibble <- dplyr::bind_cols(loaded_tibble[, 1:3], normalized_tibble) %>%
    dplyr::mutate(Sample = paste0(`Sample Name`, "_", `Sample ID`), .before = "Sample Name") %>%
    dplyr::select(-c("Sample Name", "Sample ID", "Sample Type"))
  return(normalized_tibble)
}

#' Calculate the relative standard deviation for QC samples
#'
#' @param normalized_tibble A tibble normalized by 'is_normalize' or similar function.
#' @param qc_name Character string specifying the name of the QC sample. Defaults to "Pooled QC".
#' @return A tibble containing the mean, standard deviation, and coefficient of variation for QC samples.
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

#' Calculate the relative standard deviation for NIST samples
#'
#' @param normalized_tibble A tibble normalized by 'is_normalize' or similar function.
#' @param nist_name Character string specifying the name of the NIST sample. Defaults to "NIST".
#' @return A tibble containing the mean, standard deviation, and coefficient of variation for NIST samples.
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
impute <- function(data, percent = 0.2) {

  # Filter out Blank and NIST samples
  data <- data %>% dplyr::filter(!grepl('Blank', Sample))
  data <- data %>% dplyr::filter(!grepl('BLANK', Sample))
  data <- data %>% dplyr::filter(!grepl('NIST', Sample))

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



#' QC-Normalize function
#'
#' This function performs normalization on the input data matrix using
#' the loess regression method. Normalization is done based on Quality Control
#' (QC) samples in the data.
#'
#' @param data A data frame containing the sample data. The first column
#' should contain the sample identifiers, and the rest of the columns
#' contain the peaks to be normalized. QC samples should be indicated
#' in the sample identifiers with 'QC'.
#'
#' @return A data frame with the first column as the sample identifiers and
#' the rest of the columns containing the normalized peak intensities.
#' @examples
#'
#' # Load the CSV data
#' data_file <- system.file("extdata", "example2.csv", package = "omicsTools")
#' data <- readr::read_csv(data_file)
#' # Apply the qc_normalize function
#' normalized_data <- omicsTools::qc_normalize(data)
#'
#' \donttest{
#' # Write the normalized data to a new CSV file
#' readr::write_csv(normalized_data, paste0(tempdir(), "/normalized_data.csv"))
#' }
#' @importFrom stats loess approx
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
#' @author Yaoxiang Li \email{yl814@georgetown.edu}
#'
#' Georgetown University, USA
#'
#' License: GPL (>= 3)
qc_normalize <- function(data) {
  # Convert the data except the sample identifiers to a matrix
  peaks <- as.matrix(data[, -1])

  # Create a vector to index QC samples
  qc_idx <- rep(0, nrow(peaks))
  qc_idx[grep("QC", data$Sample)] <- 1

  # Create a sequence for the acquisition order
  acq_seq <- 1:nrow(peaks)

  # Initialize the matrix for the normalized peaks
  peaks_normalized <- matrix(ncol = ncol(peaks), nrow = nrow(peaks))
  colnames(peaks_normalized) <- colnames(peaks)

  for (i in 1:ncol(peaks)) {
    # Perform a loess fit on the QC samples for each peak
    loess_fit <- stats::loess(peaks[which(qc_idx == 1), i] ~ acq_seq[which(qc_idx == 1)])

    # Interpolate the loess fit to the full acquisition order
    interpolation <- stats::approx(x    = acq_seq[which(qc_idx == 1)],
                                   y    = loess_fit$fitted,
                                   xout = acq_seq)

    # Normalize the peak by the interpolated loess fit
    peaks_normalized[, i] <- peaks[, i] / interpolation$y
  }

  # Convert the normalized peaks matrix to a tibble
  peaks_normalized <- tibble::as_tibble(peaks_normalized)

  # Combine the sample identifiers with the normalized peaks
  return(dplyr::bind_cols(data[, 1], peaks_normalized))
}

utils::globalVariables(c("Sample"))


# DEMO part ---------------------------------------------------------------



# Load necessary libraries
library(magrittr) # Provides pipe operators (%>% and others)

# Define paths to data files
peak_area_path <- "example/area.txt" # Path to peak area data
is_area_path <- "example/IS_area.txt" # Path to IS area data

# Load and clean peak area data
loaded_tibble <- load_peak_area(peak_area_path) # The function load_peak_area reads and cleans the data

# Load and clean IS area data
is_tibble <- load_is_area(is_area_path) # The function load_is_area reads and cleans the data

# Normalize loaded data with IS data
normalized_tibble <- is_normalize(loaded_tibble, is_tibble) # The function is_normalize performs normalization

# Calculate the relative standard deviation for QC samples
qc_rsd_tibble <- qc_rsd(normalized_tibble) # The function qc_rsd calculates relative standard deviation for QC samples

# Calculate the relative standard deviation for NIST samples
nist_rsd_tibble <- nist_rsd(normalized_tibble) # The function nist_rsd calculates relative standard deviation for NIST samples

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
#'
#' #' @examples
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
