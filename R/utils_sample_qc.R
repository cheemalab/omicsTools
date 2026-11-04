#' Detect multivariate sample outliers
#'
#' @param data Numeric samples-by-features matrix or data frame.
#' @param threshold Threshold for robust multivariate distance.
#'
#' @return A tibble with sample identifiers, scores, and outlier flags.
#' @export
#' @author Yaoxiang Li
detect_sample_outliers <- function(data,
                                   threshold = NULL) {
  values <- as.matrix(data)
  storage.mode(values) <- "double"
  if (any(!is.finite(values))) stop("`data` must contain only finite numeric values.", call. = FALSE)
  if (nrow(values) < 3L || ncol(values) < 1L) {
    stop("At least three samples and one feature are required.", call. = FALSE)
  }
  sample_id <- rownames(values) %||% as.character(seq_len(nrow(values)))

  center <- apply(values, 2L, stats::median)
  spread <- apply(values, 2L, stats::mad)
  spread[!is.finite(spread) | spread == 0] <- 1
  standardized <- sweep(sweep(values, 2L, center, "-"), 2L, spread, "/")
  score <- sqrt(rowSums(standardized^2))
  if (is.null(threshold)) threshold <- stats::median(score) + 3 * stats::mad(score)

  tibble::tibble(
    sample = sample_id,
    score = as.numeric(score),
    threshold = as.numeric(threshold),
    is_outlier = score > threshold
  )
}
