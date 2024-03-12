#' QC-Normalize function
#'
#' This function performs normalization on the input data matrix using
#' the loess regression method. Normalization is done based on Quality Control
#' (QC) samples in the data.
#'
#' @param data A data frame containing the sample data. The first column
#' @param qc_label A string indicating the string
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
qc_normalize <- function(data, qc_label = "QC") {
  # Convert the data except the sample identifiers to a matrix
  peaks <- as.matrix(data[, -1])

  # Create a vector to index QC samples
  qc_idx <- rep(0, nrow(peaks))
  qc_idx[grep(qc_label, data$Sample)] <- 1

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
