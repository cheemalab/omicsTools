test_that("MSP output is standards-compatible and preserves ion mode", {
  destination <- tempfile(fileext = ".MSP")
  save_msp(
    destination,
    netcdf_file = "sample_neg.mzML",
    precursor_ion = 123.45,
    peak_list = data.frame(mz = c(50, 75), intensity = c(100, 25))
  )
  lines <- readLines(destination)
  expect_true(any(lines == "Ion_mode: N"))
  expect_true(any(lines == "Num Peaks: 2"))
  expect_true(any(grepl("PrecursorMZ: 123.45", lines, fixed = TRUE)))
  expect_s3_class(plot_peak_list(data.frame(mz = 1:2, intensity = c(5, 9))), "ggplot")
})
