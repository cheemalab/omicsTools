test_that("MS1 annotation passes m/z settings to CEU Mass Mediator", {
  expected <- data.frame(name = "Example", error_ppm = 0)
  testthat::local_mocked_bindings(.search_ms1 = function(...) {
    args <- list(...)
    expect_equal(args$unique_mz, c(101.007276, 120))
    expect_equal(args$ion_mode, "negative")
    expect_equal(args$tolerance, 5)
    expect_equal(args$tolerance_mode, "ppm")
    expected
  }, .package = "omicsTools")
  expect_equal(
    ms1_annotation(c(101.007276, 120), 5, "ppm", ionMode = "neg"),
    tibble::as_tibble(expected)
  )
})

test_that("Da tolerances and neutral masses map to the API settings", {
  testthat::local_mocked_bindings(.search_ms1 = function(...) {
    args <- list(...)
    expect_equal(args$tolerance, 50)
    expect_equal(args$tolerance_mode, "mDa")
    expect_equal(args$masses_mode, "neutral")
    expect_equal(jsonlite::fromJSON(args$databases), "hmdb")
    NULL
  }, .package = "omicsTools")
  expect_equal(nrow(ms1_annotation(100, annotaDB = "hmdb", ionMode = "neu")), 0)
})
