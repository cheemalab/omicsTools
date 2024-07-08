#' QC-RLSC Normalize function
#'
#' This function performs normalization on the input data matrix using
#' the robust loess signal correction (RLSC) method. Normalization is based on
#' Quality Control (QC) samples in the data.
#'
#' @param data A data frame containing the sample data. The first column
#' should contain the sample identifiers by default named 'sample_id', and the rest of the columns
#' contain the peaks to be normalized. QC samples should be indicated
#' in the sample identifiers with 'QC'.
#' @param qc_label A string indicating the label used for QC samples. Default is 'QC'.
#' @param sample_id_col A string indicating the column name used for sample identifiers. Default is 'sample_id'.
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
#' # Apply the qc_rlsc_normalize function
#' normalized_data <- qc_normalize(data, qc_label = "QC", sample_id_col = "Sample")
#'
#' # Display the first few rows of the normalized data
#' print(head(normalized_data))
#'
#' \donttest{
#' # Write the normalized data to a new CSV file
#' readr::write_csv(normalized_data, paste0(tempdir(), "/normalized_data.csv"))
#' }
#' @importFrom stats loess approx
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
#' @author Yaoxiang Li
qc_normalize <- function(data, qc_label = "QC", sample_id_col = "sample_id") {
  # Ensure the sample_id_col exists in the data
  if (!(sample_id_col %in% colnames(data))) {
    cli::cli_alert_warning(paste("Column", sample_id_col, "not found in the data."))
    stop(paste("Column", sample_id_col, "not found in the data."))
  }

  cli::cli_alert_info("Starting QC-RLSC normalization...")

  # Convert the data except the sample identifiers to a matrix
  peaks <- as.matrix(data[, !(colnames(data) %in% sample_id_col)])

  # Create a vector to index QC samples
  qc_idx <- rep(0, nrow(peaks))
  qc_idx[grep(qc_label, data[[sample_id_col]])] <- 1

  # Create a sequence for the acquisition order
  acq_seq <- 1:nrow(peaks)

  # Initialize the matrix for the normalized peaks
  peaks_normalized <- matrix(ncol = ncol(peaks), nrow = nrow(peaks))
  colnames(peaks_normalized) <- colnames(peaks)

  # Initialize the progress bar
  cli_progress_bar("Normalizing features", total = ncol(peaks))

  for (i in 1:ncol(peaks)) {
    cli_progress_update()

    # Perform a loess fit on the QC samples for each peak
    loess_fit <- stats::loess(peaks[which(qc_idx == 1), i] ~ acq_seq[which(qc_idx == 1)])

    # Interpolate the loess fit to the full acquisition order
    interpolation <- stats::approx(
      x = acq_seq[which(qc_idx == 1)],
      y = loess_fit$fitted,
      xout = acq_seq
    )

    # Normalize the peak by the interpolated loess fit
    peaks_normalized[, i] <- peaks[, i] / interpolation$y
  }

  cli::cli_alert_success("Normalization complete.")

  # Convert the normalized peaks matrix to a tibble
  peaks_normalized <- tibble::as_tibble(peaks_normalized)

  # Combine the sample identifiers with the normalized peaks
  return(dplyr::bind_cols(data[, sample_id_col, drop = FALSE], peaks_normalized))
}

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

