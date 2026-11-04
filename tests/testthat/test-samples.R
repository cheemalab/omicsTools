test_that("sample metadata and robust outlier detection are deterministic", {
  metadata <- generate_sample_metadata(c("case-P01-1.raw", "control-P02-2.raw"), separator = "-")
  expect_named(metadata, c("sample_id", "acquisition_order", "group", "subject", "replicate"))
  expect_equal(metadata$group, c("case", "control"))

  values <- rbind(matrix(0, nrow = 5, ncol = 2), c(20, 20))
  rownames(values) <- paste0("S", 1:6)
  outliers <- detect_sample_outliers(values)
  expect_true(outliers$is_outlier[[6]])
})
