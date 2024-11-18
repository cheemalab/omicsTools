#' Perform Probabilistic Quotient Normalization for intensities
#'
#' Perform Probabilistic Quotient Normalization (PQN) for sample intensities.
#' The PQN method determines a dilution factor for each sample by comparing
#' the distribution of quotients between samples and a reference spectrum,
#' followed by sample normalization using this dilution factor.
#' The reference spectrum in this method is the median spectrum of all samples.
#'
#' @param data A data frame containing the sample data. The first column
#' should contain the sample identifiers, and the rest of the columns
#' contain the peaks to be normalized.
#'
#' @return A data frame with the first column as the sample identifiers and
#' the rest of the columns containing the normalized peak intensities.
#' @examples
#' # Load the CSV data
#' data_file <- system.file("extdata", "example2.csv", package = "omicsTools")
#' data <- readr::read_csv(data_file)
#'
#' # Display the first few rows of the original data
#' print(head(data))
#'
#' # Apply the pqn_normalize function
#' normalized_data <- pqn_normalize(data)
#'
#' # Display the first few rows of the normalized data
#' print(head(normalized_data))
#'
#' \donttest{
#' # Write the normalized data to a new CSV file
#' readr::write_csv(normalized_data, paste0(tempdir(), "/normalized_data.csv"))
#' }
#' @importFrom stats median
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
#' @author Yaoxiang Li
#' @references Dieterle, F., Ross, A., Schlotterbeck, G., & Senn, H. (2006).
#' Probabilistic quotient normalization as robust method to account for dilution
#' of complex biological mixtures. Application in 1H NMR metabonomics.
#' Analytical chemistry, 78(13), 4281-4290.
pqn_normalize <- function(data) {
  # Convert the data except the sample identifiers to a matrix
  peaks <- as.matrix(data[, -1])
  
  # Calculate the median spectrum of the samples
  median_spectrum <- apply(peaks, 2, stats::median)
  
  # Initialize the matrix for the normalized peaks
  peaks_normalized <- matrix(ncol = ncol(peaks), nrow = nrow(peaks))
  colnames(peaks_normalized) <- colnames(peaks)
  
  for (i in 1:nrow(peaks)) {
    # Calculate the quotient of each sample spectrum by the median spectrum
    quotient <- peaks[i, ] / median_spectrum
    
    # Calculate the median of the quotients for each sample
    median_quotient <- stats::median(quotient)
    
    # Normalize the peaks by dividing by the median quotient
    peaks_normalized[i, ] <- peaks[i, ] / median_quotient
  }
  
  # Convert the normalized peaks matrix to a tibble
  peaks_normalized <- tibble::as_tibble(peaks_normalized)
  
  # Combine the sample identifiers with the normalized peaks
  normalized_data <- dplyr::bind_cols(data[, 1], peaks_normalized)
  
  return(normalized_data)
}


#' Internal Standard Normalize
#'
#' This function performs internal standard normalization.
#'
#' @param area_data A data frame containing the area data.
#' @param istd_area_data A data frame containing the internal standard area data.
#'
#' @return A data frame with normalized data.
#' @importFrom dplyr bind_cols
#' @export
#' @examples
#' area_data <- data.frame(sample_id = c("S1", "S2", "S3"), A = 1:3, B = 4:6)
#' istd_area_data <- data.frame(sample_id = c("S1", "S2", "S3"), A = 1:3, B = 4:6)
#' normalized_data <- internal_standard_normalize(area_data, istd_area_data)
internal_standard_normalize <- function(area_data, istd_area_data) {
  if (check_match(area_data, istd_area_data)) {
    normalized_data <- area_data[, 2:ncol(area_data)] / istd_area_data[, 2:ncol(istd_area_data)]
    normalized_data <- dplyr::bind_cols(area_data[, 1, drop = FALSE], normalized_data)
    cli::cli_alert_success("Internal standard normalization complete.")
    return(normalized_data)
  } else {
    cli::cli_alert_warning("Mismatch in column names or sample IDs between area data and internal standard area data.")
    return(NULL)
  }
}


#' Check Match
#'
#' This function checks if sample IDs and column names match between area data and internal standard area data.
#'
#' @param area_data A data frame containing the area data.
#' @param istd_area_data A data frame containing the internal standard area data.
#'
#' @return A logical value indicating if the sample IDs and column names match.
#' @export
#' @examples
#' area_data <- data.frame(sample_id = c("S1", "S2", "S3"), A = 1:3, B = 4:6)
#' istd_area_data <- data.frame(sample_id = c("S1", "S2", "S3"), A = 1:3, B = 4:6)
#' check_match(area_data, istd_area_data)
check_match <- function(area_data, istd_area_data) {
  area_sample_ids <- unique(area_data$sample_id)
  istd_sample_ids <- unique(istd_area_data$sample_id)
  colnames_match <- all(colnames(area_data) == colnames(istd_area_data))
  sample_ids_match <- all(area_sample_ids %in% istd_sample_ids)
  return(colnames_match && sample_ids_match)
}

