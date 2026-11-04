test_that("Qtrap long data converts to the package wide format", {
  input <- tibble::tibble(
    data_filename = rep(c("run1", "run2"), each = 2),
    sample_id = rep(c("S1", "S2"), each = 2),
    component_name = rep(c("A", "B"), 2),
    Area = c(10, 20, 30, 40)
  )
  result <- convert_mrm_data(input, "Area")
  expect_named(result, c("sample_id", "A", "B"))
  expect_equal(result$sample_id, c("run1_S1", "run2_S2"))
  expect_equal(result$A, c(10, 30))
})

test_that("normalization and QC RSD workflows retain sample identity", {
  data <- data.frame(
    sample_id = c("QC_1", "S1", "QC_2", "S2", "QC_3", "S3", "QC_4", "S4"),
    feature_a = c(10, 11, 12, 13, 14, 15, 16, 17),
    feature_b = c(20, 19, 18, 17, 16, 15, 14, 13)
  )
  normalized <- suppressMessages(qc_normalize(data, sample_id_col = "sample_id"))
  expect_equal(normalized$sample_id, data$sample_id)
  expect_true(all(vapply(normalized[-1], is.numeric, logical(1))))

  pqn <- pqn_normalize(data)
  expect_equal(pqn[[1L]], data[[1L]])
  expect_equal(dim(pqn), dim(data))

  rsd <- calculate_qc_rsd(data, qc_string = "QC", id_col = "sample_id")
  expect_named(rsd, c("Pooled_QC_RSD", "NIST_RSD"))
  expect_true(all(c("name", "Coefficient of variation") %in% names(rsd$Pooled_QC_RSD)))
})

test_that("internal-standard normalization checks and divides matching tables", {
  area <- data.frame(sample_id = c("S1", "S2"), A = c(10, 20), B = c(30, 40))
  internal <- data.frame(sample_id = c("S1", "S2"), A = c(2, 4), B = c(5, 8))
  expect_true(check_match(area, internal))
  normalized <- internal_standard_normalize(area, internal)
  expect_equal(normalized$A, c(5, 5))
  expect_equal(normalized$B, c(6, 5))
})
