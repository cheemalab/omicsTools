test_that("linear targeted calibration fits and predicts", {
  standards <- data.frame(std_conc = c(1, 2, 4, 8), response = c(2, 4, 8, 16))
  fit <- fit_targeted_calibration(standards)
  expect_s3_class(fit, "omics_calibration")
  prediction <- predict_targeted_calibration(fit, data.frame(response = 10))
  expect_equal(prediction$predicted_concentration, 5, tolerance = 1e-8)
  expect_s3_class(plot_targeted_calibration(fit), "ggplot")
})

test_that("exponential targeted calibration supports the former UI choice", {
  standards <- data.frame(std_conc = exp(1:5), response = 1:5)
  fit <- fit_targeted_calibration(standards, model = "exponential")
  prediction <- predict_targeted_calibration(fit, data.frame(response = 2.5))
  expect_equal(prediction$predicted_concentration, exp(2.5), tolerance = 1e-7)
  expect_error(
    fit_targeted_calibration(data.frame(std_conc = c(0, 1), response = 1:2), model = "exponential"),
    "positive"
  )
})
