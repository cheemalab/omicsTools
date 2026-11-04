test_that("the application launches and exposes the core workflow", {
  expect_s3_class(run_app(), "shiny.appobj")
  html <- htmltools::renderTags(omicsTools:::app_ui(NULL))$html
  for (label in c("Import & Preprocess", "MS1 Annotation", "Run workflow", "Created by Yaoxiang Li")) {
    expect_true(grepl(htmltools::htmlEscape(label), html, fixed = TRUE))
  }
})
