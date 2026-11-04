#' Compute and plot principal components for omics data
#'
#' @param data Numeric matrix/data frame with samples in rows.
#' @param group Optional grouping vector.
#' @param labels Optional sample labels.
#' @param center,scale. Passed to [stats::prcomp()].
#'
#' @return A `ggplot` object with the fitted `prcomp` object attached as the
#'   `pca` attribute.
#' @export
#' @author Yaoxiang Li
plot_omics_pca <- function(data,
                           group = NULL,
                           labels = NULL,
                           center = TRUE,
                           scale. = TRUE) {
  matrix_data <- as.matrix(data)
  storage.mode(matrix_data) <- "double"
  complete <- stats::complete.cases(matrix_data)
  matrix_data <- matrix_data[complete, , drop = FALSE]
  if (nrow(matrix_data) < 2L || ncol(matrix_data) < 2L) {
    stop("PCA requires at least two complete samples and two features.", call. = FALSE)
  }
  pca <- stats::prcomp(matrix_data, center = center, scale. = scale.)
  scores <- data.frame(PC1 = pca$x[, 1L], PC2 = pca$x[, 2L])
  if (!is.null(group)) scores$Group <- as.factor(group[complete])
  if (!is.null(labels)) scores$Label <- as.character(labels[complete])

  mapping <- if (is.null(group)) {
    ggplot2::aes(x = .data$PC1, y = .data$PC2)
  } else {
    ggplot2::aes(x = .data$PC1, y = .data$PC2, colour = .data$Group)
  }
  plot <- ggplot2::ggplot(scores, mapping) +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = sprintf("PC1 (%.1f%%)", 100 * summary(pca)$importance[2L, 1L]),
      y = sprintf("PC2 (%.1f%%)", 100 * summary(pca)$importance[2L, 2L])
    )
  if (!is.null(labels)) {
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(label = .data$Label),
      vjust = -0.6,
      check_overlap = TRUE,
      show.legend = FALSE
    )
  }
  plot <- plot + ggplot2::labs(caption = "Created by Yaoxiang Li") +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 7, hjust = 1))
  attr(plot, "pca") <- pca
  plot
}

#' Plot detected LC-MS features by retention time and m/z
#'
#' @param data Feature table.
#' @param rt_col,mz_col Column names for retention time and mass-to-charge.
#' @param mode_col Optional polarity/mode column.
#' @param title Plot title.
#'
#' @return A `ggplot` object.
#' @export
#' @author Yaoxiang Li
plot_lcms_features <- function(data,
                               rt_col = "rt",
                               mz_col = "mz",
                               mode_col = "mode",
                               title = "Detected LC-MS features") {
  data <- as.data.frame(data, check.names = FALSE)
  .require_columns(data, c(rt_col, mz_col))
  plot_data <- data.frame(
    rt = as.numeric(data[[rt_col]]),
    mz = as.numeric(data[[mz_col]])
  )
  has_mode <- mode_col %in% names(data)
  if (has_mode) plot_data$mode <- as.factor(data[[mode_col]])
  mapping <- if (has_mode) {
    ggplot2::aes(x = .data$rt, y = .data$mz, colour = .data$mode)
  } else {
    ggplot2::aes(x = .data$rt, y = .data$mz)
  }
  plot <- ggplot2::ggplot(plot_data, mapping) +
    ggplot2::geom_point(alpha = 0.55) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = paste("Number of features detected:", nrow(plot_data)),
      x = "Retention time (RT)",
      y = "Mass-to-charge ratio"
    )
  plot <- plot + ggplot2::labs(caption = "Created by Yaoxiang Li") +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 7, hjust = 1))
  if (has_mode) plot + ggplot2::facet_grid(. ~ mode) else plot
}


#' Plot a feature intensity heatmap
#' @param data A samples-by-features data frame.
#' @param id_col Sample ID column.
#' @param feature_cols Numeric feature columns; NULL selects all numeric columns.
#' @param scale_features Whether to center and standardize each feature.
#' @param max_features Maximum number of features, chosen by decreasing variance.
#' @return A ggplot heatmap with creator credit.
#' @author Yaoxiang Li
#' @export
# Author: Yaoxiang Li
plot_omics_heatmap <- function(data, id_col = "sample_id", feature_cols = NULL,
                                scale_features = TRUE, max_features = 50L) {
  data <- .check_omics_table(data, id_col)
  if (is.null(feature_cols)) {
    feature_cols <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], id_col)
  }
  .require_columns(data, feature_cols)
  if (!length(feature_cols) || !all(vapply(data[feature_cols], is.numeric, logical(1)))) {
    stop("Select at least one numeric feature.", call. = FALSE)
  }
  if (length(max_features) != 1L || !is.finite(max_features) || max_features < 1) {
    stop("max_features must be a positive number.", call. = FALSE)
  }
  values <- as.matrix(data[feature_cols])
  if (any(is.infinite(values))) stop("Heatmap values must be finite or NA.", call. = FALSE)
  spread <- apply(values, 2, stats::sd, na.rm = TRUE)
  spread[!is.finite(spread)] <- 0
  selected <- utils::head(order(spread, decreasing = TRUE), max_features)
  values <- values[, selected, drop = FALSE]
  if (isTRUE(scale_features)) {
    center <- colMeans(values, na.rm = TRUE)
    center[!is.finite(center)] <- 0
    scale <- spread[selected]
    scale[scale == 0] <- 1
    values <- sweep(sweep(values, 2, center, "-"), 2, scale, "/")
  }
  plot_data <- expand.grid(sample = data[[id_col]], feature = colnames(values), stringsAsFactors = FALSE)
  plot_data$value <- as.vector(values)
  plot_data$sample <- factor(plot_data$sample, levels = data[[id_col]])
  plot_data$feature <- factor(plot_data$feature, levels = rev(colnames(values)))
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$sample, y = .data$feature, fill = .data$value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#16425b", mid = "#ffffff", high = "#138f87", na.value = "#e5e7eb") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   plot.caption = ggplot2::element_text(size = 7, hjust = 1)) +
    ggplot2::labs(x = "Sample", y = "Feature", fill = if (scale_features) "Z-score" else "Intensity",
                  caption = "Created by Yaoxiang Li")
}
