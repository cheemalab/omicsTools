#' Validate and align omics tables
#'
#' Intensity tables have one row per sample and one column per feature.
#' Sample metadata are aligned by ID. Feature metadata use a feature_id column.
#' Missing intensities are allowed; negative or infinite intensities are rejected.
#' @param data A samples-by-features data frame including sample IDs.
#' @param id_col Sample ID column.
#' @param metadata_cols Columns excluded from intensity processing. NULL also
#'   identifies nonnumeric columns as metadata.
#' @param sample_info Optional sample metadata with the same sample IDs.
#' @param feature_info Optional feature metadata with feature_id and optional mz
#'   and rt columns. Retention times are in seconds.
#' @return A list containing aligned data, feature and metadata column names,
#'   and aligned feature metadata.
#' @author Yaoxiang Li
#' @export
# Author: Yaoxiang Li
validate_omics_data <- function(data, id_col = "sample_id", metadata_cols = NULL,
                                sample_info = NULL, feature_info = NULL) {
  data <- .check_omics_table(data, id_col)
  if (!is.null(sample_info)) {
    sample_info <- .check_omics_table(sample_info, id_col)
    if (!setequal(data[[id_col]], sample_info[[id_col]])) {
      stop("Sample metadata IDs must exactly match the intensity table.", call. = FALSE)
    }
    sample_info <- sample_info[match(data[[id_col]], sample_info[[id_col]]), , drop = FALSE]
    shared <- intersect(setdiff(names(sample_info), id_col), names(data))
    for (column in shared) {
      if (!identical(as.character(data[[column]]), as.character(sample_info[[column]]))) {
        stop("Conflicting sample metadata column: ", column, call. = FALSE)
      }
    }
    data[setdiff(names(sample_info), names(data))] <- sample_info[setdiff(names(sample_info), names(data))]
    metadata_cols <- unique(c(metadata_cols, names(sample_info)))
  }
  if (is.null(metadata_cols)) {
    metadata_cols <- names(data)[!vapply(data, is.numeric, logical(1))]
  }
  metadata_cols <- unique(c(id_col, metadata_cols))
  .require_columns(data, metadata_cols)
  features <- setdiff(names(data), metadata_cols)
  if (!length(features) || !all(vapply(data[features], is.numeric, logical(1)))) {
    stop("Select numeric feature columns and identify all metadata columns.", call. = FALSE)
  }
  values <- as.matrix(data[features])
  if (any(is.infinite(values)) || any(is.nan(values)) || any(values < 0, na.rm = TRUE)) {
    stop("Intensities must be finite, non-negative numbers or NA.", call. = FALSE)
  }
  if (is.null(feature_info)) {
    feature_info <- data.frame(feature_id = features)
  } else {
    feature_info <- .check_omics_table(feature_info, "feature_id")
    if (!setequal(features, feature_info$feature_id)) {
      stop("Feature metadata IDs must match all intensity feature columns.", call. = FALSE)
    }
    feature_info <- feature_info[match(features, feature_info$feature_id), , drop = FALSE]
    for (column in intersect(c("mz", "rt"), names(feature_info))) {
      value <- feature_info[[column]]
      if (!is.numeric(value) || anyNA(value) || any(!is.finite(value)) ||
          any(value < 0) || (column == "mz" && any(value == 0))) {
        stop("Feature metadata ", column, " must contain valid numeric values.", call. = FALSE)
      }
    }
  }
  list(data = tibble::as_tibble(data), feature_cols = features,
       metadata_cols = metadata_cols, feature_info = tibble::as_tibble(feature_info))
}

# Author: Yaoxiang Li
.check_omics_table <- function(data, id_col) {
  if (!is.data.frame(data) || !nrow(data) || anyDuplicated(names(data)) ||
      anyNA(names(data)) || any(!nzchar(names(data)))) {
    stop("Supply a nonempty table with unique column names.", call. = FALSE)
  }
  if (!is.character(id_col) || length(id_col) != 1L || is.na(id_col) || !nzchar(id_col)) {
    stop("Select one sample ID column.", call. = FALSE)
  }
  .require_columns(data, id_col)
  ids <- as.character(data[[id_col]])
  if (anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids)) {
    stop("IDs must be nonempty and unique.", call. = FALSE)
  }
  data <- as.data.frame(data, check.names = FALSE)
  data[[id_col]] <- ids
  data
}

#' Filter features against blank samples
#'
#' Positive intensities are averaged separately in blanks and study samples.
#' A feature is retained when its study mean is positive and at least ratio times
#' its blank mean in every batch. The audit table records each decision.
#' @inheritParams validate_omics_data
#' @param sample_type_col Column identifying blanks and study samples.
#' @param blank_value,study_value Labels for blank and study samples.
#' @param ratio Minimum study-to-blank intensity ratio, at least one.
#' @param batch_col Optional batch column. Every batch needs blanks and study samples.
#' @return A list containing the filtered data and a per-feature, per-batch summary.
#' @author Yaoxiang Li
#' @export
# Author: Yaoxiang Li
filter_blank_features <- function(data, sample_type_col, blank_value = "Blank",
                                   study_value = "Sample", ratio = 3,
                                   batch_col = NULL, id_col = "sample_id",
                                   metadata_cols = NULL) {
  roles <- unique(c(id_col, metadata_cols, sample_type_col, batch_col))
  checked <- validate_omics_data(data, id_col, roles)
  data <- checked$data
  if (length(ratio) != 1L || !is.finite(ratio) || ratio < 1) {
    stop("The blank ratio must be at least one.", call. = FALSE)
  }
  if (identical(tolower(blank_value), tolower(study_value))) {
    stop("Blank and study sample labels must differ.", call. = FALSE)
  }
  types <- tolower(as.character(data[[sample_type_col]]))
  batches <- if (is.null(batch_col)) rep("All", nrow(data)) else as.character(data[[batch_col]])
  if (anyNA(batches) || any(!nzchar(batches))) stop("Every sample needs a batch.", call. = FALSE)
  summaries <- list()
  for (batch in unique(batches)) {
    blanks <- which(batches == batch & types == tolower(blank_value))
    studies <- which(batches == batch & types == tolower(study_value))
    if (!length(blanks) || !length(studies)) {
      stop("Every batch needs blank and study samples for blank filtering.", call. = FALSE)
    }
    # Author: Yaoxiang Li
    positive_mean <- function(x) {
      x <- x[is.finite(x) & x > 0]
      if (length(x)) mean(x) else 0
    }
    blank_mean <- vapply(data[blanks, checked$feature_cols, drop = FALSE], positive_mean, numeric(1))
    study_mean <- vapply(data[studies, checked$feature_cols, drop = FALSE], positive_mean, numeric(1))
    summaries[[batch]] <- tibble::tibble(
      feature_id = checked$feature_cols, batch = batch,
      blank_mean = unname(blank_mean), study_mean = unname(study_mean),
      keep = study_mean > 0 & study_mean >= ratio * blank_mean
    )
  }
  summary <- dplyr::bind_rows(summaries)
  retained <- checked$feature_cols[!checked$feature_cols %in% summary$feature_id[!summary$keep]]
  if (!length(retained)) stop("Blank filtering removed every feature.", call. = FALSE)
  list(data = data[c(checked$metadata_cols, retained)], summary = summary)
}

#' Compare two groups of samples
#'
#' Calculates Welch or Wilcoxon tests on log2 intensities and adjusts p-values
#' using Benjamini-Hochberg. When zeros occur, half the smallest positive value
#' in the two groups is added to that feature before transformation.
#' Log2 fold change is the log2 ratio of group means with the same offset.
#' Untestable features retain an NA p-value and an explanatory status.
#' @inheritParams validate_omics_data
#' @param group_col Group column.
#' @param reference,comparison Reference and comparison group labels.
#' @param method Welch t-test or Wilcoxon rank-sum test.
#' @return A tibble with feature IDs, group means, log2FC, p_value, fdr, and status.
#' @author Yaoxiang Li
#' @export
# Author: Yaoxiang Li
compare_omics_groups <- function(data, group_col, reference, comparison,
                                  method = c("welch", "wilcoxon"),
                                  id_col = "sample_id", metadata_cols = NULL) {
  method <- match.arg(method)
  checked <- validate_omics_data(data, id_col, unique(c(metadata_cols, group_col)))
  groups <- as.character(checked$data[[group_col]])
  if (length(reference) != 1L || length(comparison) != 1L ||
      is.na(reference) || is.na(comparison) || reference == comparison ||
      !all(c(reference, comparison) %in% groups)) {
    stop("Choose two different groups present in the table.", call. = FALSE)
  }
  rows <- vector("list", length(checked$feature_cols))
  for (i in seq_along(checked$feature_cols)) {
    feature <- checked$feature_cols[[i]]
    value <- checked$data[[feature]]
    a <- value[which(groups == reference & !is.na(value))]
    b <- value[which(groups == comparison & !is.na(value))]
    positive <- c(a, b)[c(a, b) > 0]
    offset <- if (any(c(a, b) == 0) && length(positive)) min(positive) / 2 else 0
    mean_a <- if (length(a)) mean(a) else NA_real_
    mean_b <- if (length(b)) mean(b) else NA_real_
    effect <- if (is.finite(mean_a) && is.finite(mean_b) &&
                  mean_a + offset > 0 && mean_b + offset > 0) {
      log2(mean_b + offset) - log2(mean_a + offset)
    } else NA_real_
    p <- NA_real_
    status <- "insufficient_observations"
    if (length(a) >= 2L && length(b) >= 2L) {
      if (length(unique(c(a, b))) == 1L) {
        p <- 1
        status <- "constant"
      } else if (method == "welch" && stats::var(a) == 0 && stats::var(b) == 0) {
        status <- "insufficient_variation"
      } else {
        a <- log2(a + offset)
        b <- log2(b + offset)
        # Author: Yaoxiang Li
        p <- tryCatch({
          if (method == "welch") stats::t.test(b, a)$p.value else {
            stats::wilcox.test(b, a, exact = FALSE)$p.value
          }
        # Author: Yaoxiang Li
        }, error = function(e) NA_real_)
        status <- if (is.finite(p)) "ok" else "insufficient_variation"
      }
    }
    rows[[i]] <- tibble::tibble(
      feature_id = feature, reference = reference, comparison = comparison,
      n_reference = length(a), n_comparison = length(b),
      mean_reference = mean_a, mean_comparison = mean_b, pseudocount = offset,
      log2FC = effect, p_value = p, status = status
    )
  }
  result <- dplyr::bind_rows(rows)
  result$fdr <- stats::p.adjust(result$p_value, method = "BH")
  result
}

#' Process an omics feature table
#'
#' Runs validated preprocessing, optional blank filtering and group comparison,
#' and records the settings and sample/feature counts at each step.
#' QC-RLSC uses injection order within each batch and requires three QC samples
#' per batch. Table order is used when no injection-order column is supplied.
#' @inheritParams validate_omics_data
#' @inheritParams filter_blank_features
#' @inheritParams compare_omics_groups
#' @param blank_filter Whether to filter against blank samples.
#' @param missing Maximum missing fraction per feature.
#' @param imputation One of none, half_min, mean, median, or mode.
#' @param normalization One of none, qc_rlsc, or pqn.
#' @param qc_value Label identifying QC samples.
#' @param qc_rsd Optional maximum QC relative standard deviation.
#' @param order_col Optional numeric injection-order column, unique within batch.
#' @return A list with original/processed tables, metadata, QC summaries,
#'   comparison results, blank audit, processing steps, and settings.
#' @author Yaoxiang Li
#' @export
# Author: Yaoxiang Li
process_omics_data <- function(data, id_col = "sample_id", metadata_cols = NULL,
                                sample_info = NULL, feature_info = NULL,
                                sample_type_col = NULL, batch_col = NULL, order_col = NULL,
                                blank_filter = FALSE, blank_value = "Blank", study_value = "Sample",
                                ratio = 3, missing = 0.2, imputation = "half_min",
                                normalization = "none", qc_value = "QC", qc_rsd = NULL,
                                group_col = NULL, reference = NULL, comparison = NULL,
                                method = "welch") {
  if (!is.logical(blank_filter) || length(blank_filter) != 1L || is.na(blank_filter)) {
    stop("blank_filter must be TRUE or FALSE.", call. = FALSE)
  }
  labels <- list(blank_value, study_value, qc_value)
  if (any(lengths(labels) != 1L) || anyNA(unlist(labels)) ||
      any(!nzchar(trimws(as.character(unlist(labels)))))) {
    stop("Sample type labels must be single, nonempty strings.", call. = FALSE)
  }
  method <- match.arg(method, c("welch", "wilcoxon"))
  imputation <- match.arg(imputation, c("none", "half_min", "mean", "median", "mode"))
  normalization <- match.arg(normalization, c("none", "qc_rlsc", "pqn"))
  if (length(missing) != 1L || !is.finite(missing) || missing < 0 || missing > 1) {
    stop("Maximum missingness must be between zero and one.", call. = FALSE)
  }
  if (!is.null(qc_rsd) && (length(qc_rsd) != 1L || !is.finite(qc_rsd) || qc_rsd < 0)) {
    stop("QC RSD must be a non-negative fraction.", call. = FALSE)
  }
  roles <- unique(c(metadata_cols, sample_type_col, batch_col, order_col, group_col))
  checked <- validate_omics_data(data, id_col, if (length(roles)) roles else NULL,
                                 sample_info, feature_info)
  original <- checked$data
  value <- original
  metadata <- checked$metadata_cols
  batches <- if (is.null(batch_col)) rep("All", nrow(value)) else as.character(value[[batch_col]])
  if (anyNA(batches) || any(!nzchar(batches))) stop("Every sample needs a batch.", call. = FALSE)
  if (!is.null(order_col)) {
    order <- value[[order_col]]
    if (!is.numeric(order) || anyNA(order) || any(!is.finite(order)) || any(order <= 0) ||
        anyDuplicated(data.frame(batch = batches, order = order))) {
      stop("Injection order must be positive, numeric, and unique within batch.", call. = FALSE)
    }
  }
  if (!is.null(sample_type_col) &&
      (anyNA(value[[sample_type_col]]) || any(!nzchar(trimws(value[[sample_type_col]]))))) {
    stop("Every sample needs a sample type.", call. = FALSE)
  }
  steps <- tibble::tibble(step = "Input", samples = nrow(value), features = length(checked$feature_cols))
  # Author: Yaoxiang Li
  record <- function(label) {
    tibble::tibble(step = label, samples = nrow(value), features = ncol(value) - length(metadata))
  }
  blank_audit <- tibble::tibble()
  if (isTRUE(blank_filter)) {
    if (is.null(sample_type_col)) stop("Select a sample-type column for blank filtering.", call. = FALSE)
    blank_result <- filter_blank_features(value, sample_type_col, blank_value, study_value,
                                          ratio, batch_col, id_col, metadata)
    value <- blank_result$data
    blank_audit <- blank_result$summary
    steps <- dplyr::bind_rows(steps, record("Blank filtering"))
  }
  # Measure detection in study samples when sample roles are provided.
  studies <- if (is.null(sample_type_col)) seq_len(nrow(value)) else {
    which(tolower(as.character(value[[sample_type_col]])) == tolower(study_value))
  }
  if (!length(studies)) stop("No study samples match the study label.", call. = FALSE)
  features <- setdiff(names(value), metadata)
  missing_fraction <- colMeans(is.na(value[studies, features, drop = FALSE]))
  retained <- features[missing_fraction <= missing & missing_fraction < 1]
  if (!length(retained)) stop("Missing-value filtering removed every feature.", call. = FALSE)
  value <- value[c(metadata, retained)]
  steps <- dplyr::bind_rows(steps, record("Missing-value filtering"))
  qc <- if (is.null(sample_type_col)) grepl(qc_value, value[[id_col]], fixed = TRUE) else {
    tolower(as.character(value[[sample_type_col]])) == tolower(qc_value)
  }
  if (imputation != "none") {
    # Author: Yaoxiang Li
    for (feature in retained) {
      x <- value[[feature]]
      observed <- x[studies]
      observed <- observed[!is.na(observed)]
      fill <- switch(imputation,
        half_min = if (any(observed > 0)) min(observed[observed > 0]) / 2 else 0,
        mean = mean(observed), median = stats::median(observed),
        mode = as.numeric(names(sort(table(observed), decreasing = TRUE))[[1L]])
      )
      x[is.na(x)] <- fill
      value[[feature]] <- x
    }
    steps <- dplyr::bind_rows(steps, record("Imputation"))
  }
  if (normalization != "none") {
    for (batch in unique(batches)) {
      rows <- which(batches == batch)
      if (!is.null(order_col)) rows <- rows[order(value[[order_col]][rows])]
      model_data <- as.data.frame(value[rows, c(id_col, retained), drop = FALSE])
      # QC-RLSC uses explicit role labels while preserving the original sample IDs.
      model_data[[id_col]] <- ifelse(qc[rows], paste0("QC_", seq_along(rows)), paste0("S_", seq_along(rows)))
      if (anyNA(model_data[retained])) stop("Impute missing intensities before normalization.", call. = FALSE)
      normalized <- if (normalization == "qc_rlsc") {
        if (sum(qc[rows]) < 3L) stop("QC-RLSC requires at least three QCs in every batch.", call. = FALSE)
        suppressMessages(qc_normalize(model_data, qc_label = "QC_", sample_id_col = id_col))
      } else pqn_normalize(model_data, reference_rows = which(rows %in% studies))
      value[rows, retained] <- normalized[retained]
    }
    if (any(!is.finite(as.matrix(value[retained])))) stop("Normalization produced non-finite intensities.", call. = FALSE)
    steps <- dplyr::bind_rows(steps, record("Normalization"))
  }
  if (!is.null(qc_rsd)) {
    if (sum(qc) < 2L) stop("QC filtering requires at least two QC samples.", call. = FALSE)
    rsd <- .workflow_qc(value, retained, qc)$qc_rsd
    retained <- retained[!is.na(rsd) & rsd <= qc_rsd]
    if (!length(retained)) stop("QC filtering removed every feature.", call. = FALSE)
    value <- value[c(metadata, retained)]
    steps <- dplyr::bind_rows(steps, record("QC filtering"))
  }
  comparison_result <- tibble::tibble()
  if (!is.null(group_col)) {
    comparison_result <- compare_omics_groups(value[studies, , drop = FALSE], group_col,
                                               reference, comparison, method, id_col, metadata)
    steps <- dplyr::bind_rows(steps, record("Group comparison"))
  }
  sample_qc <- tibble::tibble(
    sample_id = original[[id_col]],
    missing_before = rowMeans(is.na(original[checked$feature_cols])),
    missing_after = rowMeans(is.na(value[retained])),
    detected_before = rowSums(original[checked$feature_cols] > 0, na.rm = TRUE),
    detected_after = rowSums(value[retained] > 0, na.rm = TRUE),
    total_intensity_before = rowSums(original[checked$feature_cols], na.rm = TRUE),
    total_intensity_after = rowSums(value[retained], na.rm = TRUE)
  )
  sample_qc$status <- ifelse(sample_qc$detected_after == 0, "no_signal", "ok")
  list(
    original = original, data = tibble::as_tibble(value), id_col = id_col,
    sample_qc = sample_qc, original_features = checked$feature_info,
    feature_cols = retained, metadata_cols = metadata,
    feature_info = checked$feature_info[match(retained, checked$feature_info$feature_id), , drop = FALSE],
    blank_filter = blank_audit, comparison = comparison_result, steps = steps,
    qc_before = .workflow_qc(original, checked$feature_cols, qc),
    qc_after = .workflow_qc(value, retained, qc),
    settings = list(author = "Yaoxiang Li", package_version = as.character(utils::packageVersion("omicsTools")),
      created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      id_col = id_col, metadata_cols = metadata, sample_type_col = sample_type_col,
      batch_col = batch_col, order_col = order_col, blank_filter = blank_filter,
      blank_value = blank_value, study_value = study_value, ratio = ratio,
      missing = missing, imputation = imputation, normalization = normalization,
      qc_value = qc_value, qc_rsd = qc_rsd, group_col = group_col,
      reference = reference, comparison = comparison, method = method)
  )
}

# Author: Yaoxiang Li
.workflow_qc <- function(data, features, qc) {
  # Author: Yaoxiang Li
  rsd <- vapply(data[features], function(x) {
    x <- x[qc & !is.na(x)]
    if (length(x) < 2L || mean(x) <= 0) return(NA_real_)
    stats::sd(x) / mean(x)
  }, numeric(1))
  tibble::tibble(feature_id = features,
                 missing_fraction = unname(colMeans(is.na(data[features]))),
                 qc_rsd = unname(rsd))
}
