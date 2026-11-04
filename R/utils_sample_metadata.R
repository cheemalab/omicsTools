#' Generate sample metadata from file or sample names
#'
#' Splits sample identifiers into structured fields and preserves acquisition
#' order.
#'
#' @param samples Character vector of sample or file names.
#' @param separator Regular expression used to split identifiers.
#' @param fields Names assigned to the split components.
#' @param strip_extension Whether to remove a final file extension.
#'
#' @return A tibble containing `sample_id`, `acquisition_order`, and requested
#'   metadata fields.
#' @export
#' @author Yaoxiang Li
generate_sample_metadata <- function(samples,
                                     separator = "[_-]",
                                     fields = c("group", "subject", "replicate"),
                                     strip_extension = TRUE) {
  samples <- as.character(samples)
  if (!length(samples) || anyNA(samples) || any(!nzchar(samples))) {
    stop("`samples` must contain non-empty identifiers.", call. = FALSE)
  }
  sample_id <- if (isTRUE(strip_extension)) tools::file_path_sans_ext(basename(samples)) else basename(samples)
  pieces <- strsplit(sample_id, separator, perl = TRUE)
  width <- max(lengths(pieces), length(fields))
  # Author: Yaoxiang Li
  padded <- lapply(pieces, function(value) c(value, rep(NA_character_, width - length(value))))
  metadata <- as.data.frame(do.call(rbind, padded), stringsAsFactors = FALSE)
  if (width > length(fields)) fields <- c(fields, paste0("field", seq_len(width - length(fields)) + length(fields)))
  names(metadata) <- fields[seq_len(width)]
  tibble::as_tibble(cbind(
    data.frame(
      sample_id = sample_id,
      acquisition_order = seq_along(sample_id),
      stringsAsFactors = FALSE
    ),
    metadata
  ))
}
