test_that("sample-outlier module scores uploaded samples", {
  input_file <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(id = paste0("S", 1:5), x = c(1, 1, 2, 2, 30), y = c(2, 2, 3, 3, 50)), input_file)
  shiny::testServer(omicsTools:::mod_sample_outliers_server, args = list(id = "outlier"), {
    session$setInputs(file = list(name = "samples.csv", datapath = input_file))
    session$flushReact()
    session$setInputs(sample_id = "id", features = c("x", "y"), threshold = 3, detect = 1)
    expect_equal(nrow(result()), 5)
    expect_true(result()$is_outlier[[5]])
  })
})

test_that("SCIEX pipeline exposes parsing, duplicate review, and wide conversion", {
  input_file <- tempfile(fileext = ".txt")
  writeLines("placeholder", input_file)
  testthat::local_mocked_bindings(
    load_parse_sciex_txt = function(...) tibble::tibble(
      data_filename = c("file1", "file1"), sample_id = c("S1", "S1"),
      component_name = c("A", "B"), precursor_mz = c(100, 200), product_mz = c(50, 75),
      polarity = c("Positive", "Positive"), retention_time = c(1, 2), area = c(10, 20)
    ),
    process_mrm_duplicates = function(value, ...) dplyr::mutate(value, MRM_Duplicate_Flag = ""),
    convert_mrm_data = function(value, response_col, ...) tibble::tibble(sample_id = "file1_S1", A = value[[response_col]][[1]], B = value[[response_col]][[2]]),
    .package = "omicsTools"
  )
  shiny::testServer(omicsTools:::mod_qtrap_pipeline_server, args = list(id = "sciex"), {
    session$setInputs(file = list(name = "sciex.txt", datapath = input_file))
    session$setInputs(response = "area", duplicates = TRUE, process = 1)
    expect_equal(nrow(parsed()), 2)
    expect_equal(wide()$sample_id, "file1_S1")
  })
})

test_that("Qtrap QC module normalizes representative uploaded tables", {
  area_file <- testthat::test_path("fixtures", "gui_qtrap_area.tsv")
  istd_file <- testthat::test_path("fixtures", "gui_qtrap_istd.tsv")

  shiny::testServer(omicsTools:::mod_qtrap_qc_server, args = list(id = "qtrap"), {
    session$setInputs(file_path = list(name = "area.tsv", datapath = area_file))
    session$setInputs(is_area_path = list(name = "istd.tsv", datapath = istd_file))
    session$setInputs(qc_name = "Pooled QC", nist_name = "NIST")
    session$flushReact()

    expect_equal(nrow(dataset_all()), 6)
    expect_equal(names(dataset_all()), c("Sample", "feature_a", "feature_b"))
    expect_equal(dataset_all()$feature_a, c(10, 11, 10.5, 10.2, 11.5, 10.8))
    expect_equal(nrow(dataset_qc()), 2)
    expect_equal(nrow(dataset_nist()), 2)
  })
})
