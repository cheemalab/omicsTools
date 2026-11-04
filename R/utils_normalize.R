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
#' data <- data.frame(
#'   Sample = c("QC_1", "S1", "QC_2", "S2", "QC_3", "QC_4"),
#'   feature_a = c(10, 11, 12, 13, 14, 15),
#'   feature_b = c(20, 19, 18, 17, 16, 15)
#' )
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

  # Convert the data except the sample identifiers to a numeric matrix.
  peaks <- data[, !(colnames(data) %in% sample_id_col), drop = FALSE]
  non_numeric <- names(peaks)[!vapply(peaks, is.numeric, logical(1))]
  if (length(non_numeric)) {
    stop("All feature columns must be numeric: ", paste(non_numeric, collapse = ", "), call. = FALSE)
  }
  peaks <- as.matrix(peaks)

  # Create a logical index for QC samples.
  qc_idx <- grepl(qc_label, as.character(data[[sample_id_col]]))
  if (sum(qc_idx) < 2L) {
    stop("At least two QC samples are required for QC-RLSC normalization.", call. = FALSE)
  }

  # Create a sequence for the acquisition order
  acq_seq <- 1:nrow(peaks)

  # Initialize the matrix for the normalized peaks
  peaks_normalized <- matrix(ncol = ncol(peaks), nrow = nrow(peaks))
  colnames(peaks_normalized) <- colnames(peaks)

  # Initialize the progress bar
  cli_progress_bar("Normalizing features", total = ncol(peaks))

  for (i in 1:ncol(peaks)) {
    cli_progress_update()

    complete_qc <- qc_idx & is.finite(peaks[, i])
    if (sum(complete_qc) < 2L) {
      peaks_normalized[, i] <- NA_real_
      next
    }
    qc_order <- acq_seq[complete_qc]
    qc_values <- peaks[complete_qc, i]
    fitted_values <- if (length(qc_values) >= 5L) {
      suppressWarnings(stats::loess(
        qc_values ~ qc_order,
        span = 0.75,
        degree = 2,
        control = stats::loess.control(surface = "direct")
      )$fitted)
    } else {
      qc_values
    }
    correction <- stats::approx(
      x = qc_order,
      y = fitted_values,
      xout = acq_seq,
      rule = 2,
      ties = "ordered"
    )$y
    correction[!is.finite(correction) | correction == 0] <- NA_real_

    # Normalize the peak by the interpolated QC response.
    peaks_normalized[, i] <- peaks[, i] / correction
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
#' @param reference_rows Rows used to calculate the reference spectrum.
#' @return A data frame with the first column as the sample identifiers and
#' the rest of the columns containing the normalized peak intensities.
#' @examples
#' data <- data.frame(
#'   Sample = paste0("S", 1:4),
#'   feature_a = c(10, 11, 12, 13),
#'   feature_b = c(20, 18, 16, 14)
#' )
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
# Author: Yaoxiang Li
pqn_normalize <- function(data, reference_rows = seq_len(nrow(data))) {
  if (!is.data.frame(data) || ncol(data) < 2L ||
      !all(vapply(data[-1], is.numeric, logical(1)))) {
    stop("Supply sample IDs followed by numeric intensity columns.", call. = FALSE)
  }
  if (!is.numeric(reference_rows) || !length(reference_rows) || anyNA(reference_rows) ||
      any(reference_rows != as.integer(reference_rows)) ||
      any(reference_rows < 1 | reference_rows > nrow(data))) {
    stop("Reference rows must identify samples in the table.", call. = FALSE)
  }
  peaks <- as.matrix(data[, -1, drop = FALSE])
  if (anyNA(peaks) || any(!is.finite(peaks)) || any(peaks < 0)) {
    stop("PQN requires finite, non-negative intensities without missing values.", call. = FALSE)
  }
  reference <- apply(peaks[reference_rows, , drop = FALSE], 2, stats::median)
  usable <- reference > 0
  if (!any(usable)) stop("PQN requires a positive reference spectrum.", call. = FALSE)
  for (i in seq_len(nrow(peaks))) {
    quotients <- peaks[i, usable] / reference[usable]
    quotients <- quotients[quotients > 0]
    if (length(quotients)) peaks[i, ] <- peaks[i, ] / stats::median(quotients)
  }
  dplyr::bind_cols(data[, 1, drop = FALSE], tibble::as_tibble(peaks))
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
#' @author Yaoxiang Li
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
#' @author Yaoxiang Li
check_match <- function(area_data, istd_area_data) {
  area_sample_ids <- unique(area_data$sample_id)
  istd_sample_ids <- unique(istd_area_data$sample_id)
  colnames_match <- all(colnames(area_data) == colnames(istd_area_data))
  sample_ids_match <- all(area_sample_ids %in% istd_sample_ids)
  return(colnames_match && sample_ids_match)
}
