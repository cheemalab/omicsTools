#' Transpose a feature table using its first column as identifiers
#'
#' @param df Data frame whose first column contains identifiers.
#'
#' @return A tibble with original measurement columns as rows and original
#'   identifiers as columns.
#' @export
#' @author Yaoxiang Li
transpose_df <- function(df) {
  df <- tibble::as_tibble(df)
  if (ncol(df) < 2L) stop("`df` must contain an identifier and at least one value column.", call. = FALSE)
  id <- names(df)[[1L]]
  long <- tidyr::pivot_longer(df, cols = -1L, names_to = "name", values_to = "value")
  tidyr::pivot_wider(long, names_from = dplyr::all_of(id), values_from = "value")
}

#' Filter features by missing-value rate
#'
#' Supports data frames and matrices.
#'
#' @param data A data frame or matrix.
#' @param percent Maximum allowed missing-value proportion per feature.
#' @param metadata_cols Columns in a tabular input that should not be filtered.
#'
#' @return The filtered object, preserving the input representation.
#' @export
#' @author Yaoxiang Li
filter_missing_value <- function(data, percent = 0.2, metadata_cols = 1L) {
  .validate_fraction(percent, "percent")

  input_is_matrix <- is.matrix(data)
  if (!input_is_matrix && !is.data.frame(data)) {
    stop("`data` must be a data frame or matrix.", call. = FALSE)
  }
  data_frame <- as.data.frame(data, check.names = FALSE)
  metadata_cols <- .resolve_columns(data_frame, metadata_cols)
  feature_cols <- setdiff(seq_along(data_frame), metadata_cols)
  if (!length(feature_cols)) {
    return(data)
  }

  keep_features <- colMeans(is.na(data_frame[feature_cols])) <= percent
  result <- data_frame[c(metadata_cols, feature_cols[keep_features])]
  if (input_is_matrix) {
    return(as.matrix(result))
  }
  tibble::as_tibble(result)
}

#' Impute missing values with half of a feature minimum
#'
#' @param data A data frame or matrix.
#' @param metadata_cols Columns in a tabular input that should not be imputed.
#'
#' @return The imputed object, preserving the input representation.
#' @export
#' @author Yaoxiang Li
impute_half_min <- function(data, metadata_cols = 1L) {
  input_is_matrix <- is.matrix(data)
  if (!input_is_matrix && !is.data.frame(data)) {
    stop("`data` must be a data frame or matrix.", call. = FALSE)
  }
  result <- as.data.frame(data, check.names = FALSE)
  metadata_cols <- .resolve_columns(result, metadata_cols)
  feature_cols <- setdiff(seq_along(result), metadata_cols)
  result[feature_cols] <- lapply(result[feature_cols], .impute_half_min_vector)
  if (input_is_matrix) {
    return(as.matrix(result))
  }
  tibble::as_tibble(result)
}

#' Filter features by quality-control relative standard deviation
#'
#' @param data A data frame or matrix.
#' @param percent Maximum allowed QC relative standard deviation.
#' @param qc_index Logical or numeric indicator identifying QC rows. When
#'   omitted for tabular inputs, `qc_string` is matched in `id_col`.
#' @param qc_string Pattern used to identify QC rows.
#' @param id_col Sample identifier column for tabular data.
#' @param metadata_cols Columns that should not be filtered.
#'
#' @return The filtered object, preserving the input representation.
#' @export
#' @author Yaoxiang Li
filter_qc_rsd <- function(data,
                          percent = 0.2,
                          qc_index = NULL,
                          qc_string = "QC|Pooled QC|Pooled_QC",
                          id_col = 1L,
                          metadata_cols = 1L) {
  .validate_fraction(percent, "percent")

  input_is_matrix <- is.matrix(data)
  data_frame <- as.data.frame(data, check.names = FALSE)
  metadata_cols <- .resolve_columns(data_frame, metadata_cols)
  feature_cols <- setdiff(seq_along(data_frame), metadata_cols)
  if (is.null(qc_index)) {
    id_position <- .resolve_columns(data_frame, id_col)
    if (length(id_position) != 1L) {
      stop("`id_col` must identify exactly one column.", call. = FALSE)
    }
    qc_index <- grepl(qc_string, as.character(data_frame[[id_position]]))
  }
  keep <- .qc_rsd_keep(data_frame[feature_cols], qc_index, percent)
  result <- data_frame[c(metadata_cols, feature_cols[keep])]
  if (input_is_matrix) {
    return(as.matrix(result))
  }
  tibble::as_tibble(result)
}

# Author: Yaoxiang Li
.qc_rsd_keep <- function(peaks, qc_index, percent) {
  qc_index <- as.logical(qc_index)
  if (length(qc_index) != nrow(peaks) || !any(qc_index, na.rm = TRUE)) {
    stop("`qc_index` must identify at least one row in `data`.", call. = FALSE)
  }
  peaks <- as.data.frame(peaks, check.names = FALSE)
  numeric_columns <- vapply(peaks, is.numeric, logical(1))
  rsd <- rep(Inf, ncol(peaks))
  rsd[numeric_columns] <- vapply(
    peaks[numeric_columns],
    # Author: Yaoxiang Li
    function(x) {
      values <- x[qc_index & is.finite(x)]
      if (length(values) < 2L || mean(values) == 0) {
        return(Inf)
      }
      stats::sd(values) / abs(mean(values))
    },
    numeric(1)
  )
  numeric_columns & is.finite(rsd) & rsd <= percent
}

# Author: Yaoxiang Li
.impute_half_min_vector <- function(x) {
  if (!is.numeric(x) || !anyNA(x)) {
    return(x)
  }
  observed <- x[is.finite(x)]
  positive <- observed[observed > 0]
  replacement <- if (length(positive)) min(positive) / 2 else 0
  x[is.na(x)] <- replacement
  x
}

# Author: Yaoxiang Li
.resolve_columns <- function(data, columns) {
  if (is.null(columns) || !length(columns)) {
    return(integer())
  }
  if (is.character(columns)) {
    missing <- setdiff(columns, names(data))
    if (length(missing)) {
      stop("Unknown columns: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    return(match(columns, names(data)))
  }
  columns <- as.integer(columns)
  if (anyNA(columns) || any(columns < 1L | columns > ncol(data))) {
    stop("Column positions are outside the data range.", call. = FALSE)
  }
  unique(columns)
}

# Author: Yaoxiang Li
.validate_fraction <- function(value, name) {
  if (length(value) != 1L || !is.finite(value) || value < 0 || value > 1) {
    stop("`", name, "` must be a number between 0 and 1.", call. = FALSE)
  }
}
