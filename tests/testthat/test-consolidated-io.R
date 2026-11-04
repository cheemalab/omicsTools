test_that("common data formats load without installed example files", {
  csv <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(id = c("a", "b"), value = 1:2), csv)
  loaded <- load_omics_data(csv)
  expect_s3_class(loaded, "tbl_df")
  expect_equal(loaded$value, 1:2)

  rds <- tempfile(fileext = ".rds")
  saveRDS(list(value = 42), rds)
  expect_equal(load_data(rds)$value, 42)

  rdata <- tempfile(fileext = ".RData")
  first <- 1
  second <- "two"
  save(first, second, file = rdata)
  expect_equal(sort(names(load_omics_data(rdata))), c("first", "second"))
})

test_that("plot examples are bundled without the MS1 database", {
  path <- omics_example_file("examples/heatmap.csv")
  expect_true(file.exists(path))
  expect_error(omics_example_file("../outside.csv"), "inside")
  expect_error(omics_example_file("examples/ms1_data.RData"), "No bundled")
})
