test_that("missing-value filtering and half-minimum imputation work", {
  data <- data.frame(
    sample = paste0("S", 1:4),
    keep = c(2, NA, 6, 8),
    remove = c(NA, NA, NA, 1)
  )
  filtered <- filter_missing_value(data, percent = 0.25, metadata_cols = "sample")
  expect_named(filtered, c("sample", "keep"))
  imputed <- impute_half_min(filtered, metadata_cols = "sample")
  expect_equal(imputed$keep, c(2, 1, 6, 8))
})

test_that("QC RSD filtering keeps stable features", {
  data <- data.frame(
    sample = c("QC_1", "QC_2", "sample_1"),
    stable = c(100, 102, 90),
    unstable = c(1, 100, 50)
  )
  result <- filter_qc_rsd(data, percent = 0.1, metadata_cols = "sample")
  expect_named(result, c("sample", "stable"))
})
