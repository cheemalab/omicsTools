#' Write a self-contained workflow QC report
#'
#' Includes processing settings, counts, missingness, QC RSD, PCA before and
#' after processing, blank-filter decisions, and group comparisons.
#' @param result A result from process_omics_data().
#' @param file Output HTML file.
#' @return Invisibly, the output file path.
#' @author Yaoxiang Li
#' @export
# Author: Yaoxiang Li
write_omics_report <- function(result, file) {
  if (!is.list(result) || !all(c("data", "original", "steps", "settings") %in% names(result))) {
    stop("Supply a result from process_omics_data().", call. = FALSE)
  }
  # Author: Yaoxiang Li
  table <- function(data) {
    if (!nrow(data)) return(htmltools::tags$p("Not requested or no results."))
    # Author: Yaoxiang Li
    rows <- lapply(seq_len(min(nrow(data), 100L)), function(i) {
      # Author: Yaoxiang Li
      cells <- lapply(data[i, , drop = FALSE], function(x) {
        htmltools::tags$td(if (is.na(x)) "" else format(x, digits = 5, trim = TRUE))
      })
      do.call(htmltools::tags$tr, unname(cells))
    })
    htmltools::tagList(
      if (nrow(data) > 100L) htmltools::tags$p("Showing the first 100 rows. The results bundle contains the full tables."),
      htmltools::tags$div(class = "table",
        htmltools::tags$table(
          htmltools::tags$thead(do.call(htmltools::tags$tr, lapply(names(data), htmltools::tags$th))),
          do.call(htmltools::tags$tbody, rows)
        ))
    )
  }
  pca <- dplyr::bind_rows(
    .workflow_pca(result$original, setdiff(names(result$original), result$metadata_cols), result$id_col, "Before"),
    .workflow_pca(result$data, result$feature_cols, result$id_col, "After")
  )
  if (nrow(pca)) pca$stage <- factor(pca$stage, levels = c("Before", "After"))
  figure <- htmltools::tags$p("PCA requires at least three samples and two variable features.")
  if (nrow(pca)) {
    group_col <- result$settings$sample_type_col %||% result$settings$group_col
    pca$group <- if (is.null(group_col)) "Sample" else {
      as.character(result$original[[group_col]][match(pca$sample_id, result$original[[result$id_col]])])
    }
    plot <- ggplot2::ggplot(pca, ggplot2::aes(x = .data$PC1, y = .data$PC2, colour = .data$group)) +
      ggplot2::geom_point(size = 2) + ggplot2::facet_wrap(~stage, scales = "free") +
      ggplot2::theme_bw() + ggplot2::labs(colour = "Sample group")
    png <- tempfile(fileext = ".png")
    on.exit(unlink(png), add = TRUE)
    ggplot2::ggsave(png, plot, width = 10, height = 4, dpi = 120)
    encoded <- jsonlite::base64_enc(readBin(png, "raw", n = file.info(png)$size))
    figure <- htmltools::tags$img(src = paste0("data:image/png;base64,", encoded),
                                  alt = "Before and after processing PCA", style = "max-width:100%;")
  }
  report_settings <- result$settings
  report_settings$author <- NULL
  document <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title("omicsTools QC report"),
      htmltools::tags$style(htmltools::HTML(
        "body{font-family:Arial,sans-serif;max-width:1100px;margin:32px auto;padding:0 20px;color:#173348}
         h1,h2{font-weight:600}.table{overflow-x:auto}table{border-collapse:collapse;font-size:13px}
         th,td{padding:7px 10px;border-bottom:1px solid #ddd;text-align:left}
         pre{white-space:pre-wrap;background:#f4f7fa;padding:16px}"
      ))
    ),
    htmltools::tags$body(
      htmltools::tags$h1("omicsTools QC report"),
      htmltools::tags$p(result$settings$created_utc),
      htmltools::tags$h2("Processing steps"), table(result$steps),
      htmltools::tags$h2("Before and after"), figure,
      htmltools::tags$p("PCA uses log2(intensity + 1) and unit-variance scaling. Missing values are median-filled for the PCA display only."),
      htmltools::tags$h2("Sample QC"), table(result$sample_qc),
      htmltools::tags$h2("QC before processing"), table(result$qc_before),
      htmltools::tags$h2("QC after processing"), table(result$qc_after),
      htmltools::tags$h2("Blank filtering"), table(result$blank_filter),
      htmltools::tags$h2("Group comparison"), table(result$comparison),
      htmltools::tags$h2("Feature metadata"), table(result$feature_info),
      htmltools::tags$h2("Processing settings"),
      htmltools::tags$pre(jsonlite::toJSON(report_settings, auto_unbox = TRUE, pretty = TRUE, null = "null"))
    )
  )
  temporary <- tempfile("omicsTools-report-", tmpdir = dirname(file), fileext = ".html")
  on.exit(unlink(temporary), add = TRUE)
  rendered <- htmltools::renderTags(document)
  html <- sub("<html>", paste0("<html><head>", rendered$head, "</head>"), rendered$html, fixed = TRUE)
  writeLines(c("<!DOCTYPE html>", html), temporary, useBytes = TRUE)
  if (!file.copy(temporary, file, overwrite = TRUE)) stop("Could not write the report.", call. = FALSE)
  invisible(file)
}

# Author: Yaoxiang Li
.workflow_pca <- function(data, features, id_col, stage) {
  if (nrow(data) < 3L || length(features) < 2L) return(NULL)
  values <- log2(as.matrix(data[features]) + 1)
  usable <- colSums(!is.na(values)) >= 2L
  values <- values[, usable, drop = FALSE]
  for (j in seq_len(ncol(values))) {
    values[is.na(values[, j]), j] <- stats::median(values[, j], na.rm = TRUE)
  }
  variable <- apply(values, 2, stats::sd) > 0
  values <- values[, variable, drop = FALSE]
  if (ncol(values) < 2L) return(NULL)
  pca <- stats::prcomp(values, center = TRUE, scale. = TRUE, rank. = 2)
  tibble::tibble(sample_id = data[[id_col]], PC1 = pca$x[, 1], PC2 = pca$x[, 2], stage = stage)
}

#' Export a complete workflow result
#'
#' Writes separate intensity, sample, feature, QC, comparison, blank audit,
#' processing-step, and settings files together with a self-contained HTML report.
#' A temporary directory is published only after every output succeeds.
#' @param result A result from process_omics_data().
#' @param directory A new output directory. Existing directories are not overwritten.
#' @return Invisibly, the output directory path.
#' @author Yaoxiang Li
#' @export
# Author: Yaoxiang Li
export_omics_results <- function(result, directory) {
  if (file.exists(directory)) stop("Choose a new output directory.", call. = FALSE)
  parent <- normalizePath(dirname(directory), winslash = "/", mustWork = TRUE)
  staging <- tempfile("omicsTools-export-", tmpdir = parent)
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  tables <- list(
    intensities = result$data[c(result$id_col, result$feature_cols)],
    input_intensities = result$original[c(result$id_col, result$original_features$feature_id)],
    input_features = result$original_features,
    samples = result$data[result$metadata_cols],
    features = result$feature_info, sample_qc = result$sample_qc, qc_before = result$qc_before, qc_after = result$qc_after,
    comparison = result$comparison, blank_filter = result$blank_filter, processing_steps = result$steps
  )
  for (name in names(tables)) {
    utils::write.csv(tables[[name]], file.path(staging, paste0(name, ".csv")), row.names = FALSE, na = "")
  }
  jsonlite::write_json(result$settings, file.path(staging, "settings.json"),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")
  write_omics_report(result, file.path(staging, "report.html"))
  if (!file.rename(staging, directory)) stop("Could not publish the results directory.", call. = FALSE)
  invisible(normalizePath(directory, winslash = "/", mustWork = TRUE))
}
