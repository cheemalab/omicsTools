test_that("validation aligns metadata and rejects ambiguous inputs", {
  x <- data.frame(sample_id = c("s2", "s1"), a = c(4, 2))
  info <- data.frame(sample_id = c("s1", "s2"), group = c("control", "case"))
  result <- validate_omics_data(x, sample_info = info)
  expect_equal(result$data$group, c("case", "control"))
  expect_equal(result$feature_cols, "a")
  expect_error(validate_omics_data(transform(x, sample_id = "same")), "unique")
  expect_error(validate_omics_data(transform(x, a = c(Inf, 2))), "finite")
  expect_error(validate_omics_data(x, sample_info = info[1, ]), "match")
  expect_error(validate_omics_data(x, feature_info = data.frame(feature_id = "other", mz = 100)), "feature")
})

test_that("blank filtering uses real samples and keeps metadata", {
  x <- data.frame(sample_id = c("b", "s1", "s2"), type = c("Blank", "Sample", "Sample"),
                  signal = c(1, 10, 12), background = c(10, 11, 12), absent = 0)
  result <- filter_blank_features(x, "type")
  expect_named(result$data, c("sample_id", "type", "signal"))
  expect_equal(result$summary$keep, c(TRUE, FALSE, FALSE))
  expect_error(filter_blank_features(x[-1, ], "type"), "blank")
})

test_that("group comparisons return reproducible effects and adjusted p-values", {
  x <- data.frame(sample_id = paste0("s", 1:8), group = rep(c("A", "B"), each = 4),
                  signal = c(2, 3, 4, 5, 4, 6, 8, 10), constant = 1,
                  sparse = c(1, NA, NA, NA, 2, NA, NA, NA))
  result <- compare_omics_groups(x, "group", "A", "B")
  expect_equal(result$log2FC[result$feature_id == "signal"], 1)
  expect_equal(result$p_value[result$feature_id == "constant"], 1)
  expect_true(is.na(result$p_value[result$feature_id == "sparse"]))
  expect_equal(result$fdr, p.adjust(result$p_value, "BH"))
  expect_warning(compare_omics_groups(x, "group", "A", "B", method = "wilcoxon"), NA)
})

test_that("the workflow runs through report export without network access", {
  x <- data.frame(sample_id = c("b", "q1", "q2", paste0("s", 1:6)),
                  type = c("Blank", "QC", "QC", rep("Sample", 6)),
                  group = c(NA, NA, NA, rep(c("A", "B"), each = 3)),
                  signal = c(1, 8, 8, 4, 5, 6, 8, 10, 12),
                  background = c(10, 10, 10, 11, 10, 12, 11, 12, 10))
  result <- process_omics_data(x, metadata_cols = c("type", "group"),
                              sample_type_col = "type", blank_filter = TRUE,
                              group_col = "group", reference = "A", comparison = "B")
  expect_equal(result$feature_cols, "signal")
  expect_equal(result$comparison$log2FC, 1)
  expect_equal(result$steps$step[[1]], "Input")
  report <- tempfile(fileext = ".html")
  expect_warning(write_omics_report(result, report), NA)
  html <- paste(readLines(report), collapse = "")
  expect_false(grepl("Yaoxiang Li", html, fixed = TRUE))
  expect_false(grepl("<footer", html, fixed = TRUE))
  expect_false(grepl('"author"', html, fixed = TRUE))
  expect_equal(result$settings$author, "Yaoxiang Li")
  expect_match(html, "Processing steps", fixed = TRUE)
  expect_match(html, "Before and after", fixed = TRUE)
})
test_that("PQN preserves single-feature names and handles zero-signal blanks", {
  x <- data.frame(sample_id = c("blank", "s1", "s2"), a = c(0, 2, 4))
  result <- pqn_normalize(x, reference_rows = 2:3)
  expect_named(result, names(x))
  expect_equal(result$a, c(0, 3, 3))
  expect_equal(result$sample_id, x$sample_id)
  expect_error(pqn_normalize(transform(x, a = 0)), "positive reference")
})

test_that("batch-aware processing preserves injection and sample identities", {
  x <- data.frame(sample_id = c("b2", "s2", "b1", "s1"),
                  type = c("Blank", "Sample", "Blank", "Sample"),
                  batch = c("B", "B", "A", "A"), injection = c(1, 2, 1, 2),
                  a = c(1, 10, 2, 12), b = c(5, 20, 5, 10))
  result <- process_omics_data(x, sample_type_col = "type", batch_col = "batch",
                               order_col = "injection", blank_filter = TRUE,
                               normalization = "pqn")
  expect_equal(result$data$sample_id, x$sample_id)
  expect_equal(result$feature_cols, "a")
  expect_equal(nrow(result$blank_filter), 4)
  expect_error(process_omics_data(transform(x, injection = 1), batch_col = "batch",
                                 order_col = "injection", metadata_cols = "type"), "Injection order")
})

test_that("the report embeds PCA and exports complete separate tables", {
  x <- data.frame(sample_id = paste0("s", 1:6), group = rep(c("A", "B"), each = 3),
                  a = c(1, 2, 4, 5, 6, 7), b = c(7, 5, 3, 2, 3, 5))
  result <- process_omics_data(x, group_col = "group", reference = "A", comparison = "B")
  directory <- tempfile()
  expect_warning(export_omics_results(result, directory), NA)
  files <- list.files(directory)
  expect_true(all(c("report.html", "settings.json", "samples.csv", "features.csv",
                    "intensities.csv", "comparison.csv") %in% files))
  html <- paste(readLines(file.path(directory, "report.html")), collapse = "")
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_false(grepl("data:image/svg+xml;base64,", html, fixed = TRUE))
  expect_false(grepl("Yaoxiang Li", html, fixed = TRUE))
  settings <- jsonlite::read_json(file.path(directory, "settings.json"))
  expect_equal(settings$author, result$settings$author)
  expect_error(export_omics_results(result, directory), "new output")
})
test_that("the app connects uploaded metadata to blank filtering and comparisons", {
  values <- data.frame(sample_id = c("blank", paste0("s", 1:6)),
                       signal = c(1, 2, 3, 4, 4, 6, 8),
                       background = c(10, rep(11, 6)))
  metadata <- data.frame(sample_id = rev(values$sample_id),
                         type = c(rep("Sample", 6), "Blank"),
                         group = c(rep("B", 3), rep("A", 3), NA))
  value_file <- tempfile(fileext = ".csv")
  metadata_file <- tempfile(fileext = ".csv")
  readr::write_csv(values, value_file)
  readr::write_csv(metadata, metadata_file)
  shiny::testServer(omicsTools:::mod_preprocess_server, args = list(id = "workflow"), {
    session$setInputs(file = list(name = "values.csv", datapath = value_file),
                      sample_info = list(name = "samples.csv", datapath = metadata_file))
    session$flushReact()
    session$setInputs(id_col = "sample_id", metadata = c("sample_id", "type", "group"),
                      sample_type = "type", study_label = "Sample", blank_label = "Blank",
                      blank_filter = TRUE, blank_ratio = 2, missing = 0.2,
                      imputation = "half_min", normalization = "none",
                      group = "group", reference = "A", comparison = "B", test = "welch",
                      process = 1)
    expect_equal(analysis()$data$sample_id, values$sample_id)
    expect_equal(analysis()$feature_cols, "signal")
    expect_equal(analysis()$comparison$log2FC, 1)
    expect_match(output$summary, "7 samples and 1 features", fixed = TRUE)
  })
})

test_that("reporting remains valid for sparse and constant feature matrices", {
  x <- data.frame(sample_id = paste0("s", 1:4),
                   a = c(1, 2, 3, 4), b = c(NA, NA, NA, 1), constant = 1)
  result <- process_omics_data(x, missing = 1, imputation = "none")
  report <- tempfile(fileext = ".html")
  expect_warning(write_omics_report(result, report), NA)
  expect_true(file.exists(report))
})
test_that("report HTML contains visible table cells and its document styles", {
  x <- data.frame(sample_id = c("s1", "s2", "s3"), a = 1:3, b = c(3, 1, 2))
  result <- process_omics_data(x)
  file <- tempfile(fileext = ".html")
  write_omics_report(result, file)
  html <- paste(readLines(file), collapse = "")
  expect_true(grepl("<head>", html, fixed = TRUE))
  expect_true(grepl("<style>", html, fixed = TRUE))
  expect_true(grepl("<td>Input</td>", html, fixed = TRUE))
  expect_false(grepl('step="&lt;td&gt;', html, fixed = TRUE))
})
