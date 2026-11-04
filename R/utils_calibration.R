#' Fit a targeted-assay calibration model
#'
#' @param standards Data frame containing known concentrations and responses.
#' @param concentration_col Name of the concentration column.
#' @param response_col Name of the instrument-response column.
#' @param model Calibration model: ordinary linear regression or an exponential
#'   curve fitted by regressing log concentration on response.
#' @param response_range Optional two-element response range used to subset
#'   standards before fitting.
#' @param weights Optional non-negative observation weights.
#'
#' @return An object of class `omics_calibration`.
#' @export
#' @author Yaoxiang Li
fit_targeted_calibration <- function(standards,
                                     concentration_col = "std_conc",
                                     response_col = "response",
                                     model = c("linear", "exponential"),
                                     response_range = NULL,
                                     weights = NULL) {
  model <- match.arg(model)
  standards <- as.data.frame(standards, check.names = FALSE)
  .require_columns(standards, c(concentration_col, response_col))

  frame <- data.frame(
    concentration = as.numeric(standards[[concentration_col]]),
    response = as.numeric(standards[[response_col]])
  )
  complete <- stats::complete.cases(frame)
  frame <- frame[complete, , drop = FALSE]
  if (!is.null(weights)) {
    weights <- as.numeric(weights)[complete]
  }
  if (!is.null(response_range)) {
    if (length(response_range) != 2L || any(!is.finite(response_range))) {
      stop("`response_range` must contain two finite values.", call. = FALSE)
    }
    keep <- frame$response >= min(response_range) &
      frame$response <= max(response_range)
    frame <- frame[keep, , drop = FALSE]
    if (!is.null(weights)) {
      weights <- weights[keep]
    }
  }
  if (nrow(frame) < 2L || length(unique(frame$response)) < 2L) {
    stop("At least two distinct complete standard responses are required.", call. = FALSE)
  }
  if (model == "exponential" && any(frame$concentration <= 0)) {
    stop("Exponential calibration requires positive concentrations.", call. = FALSE)
  }
  if (!is.null(weights) &&
      (length(weights) != nrow(frame) || any(!is.finite(weights) | weights < 0))) {
    stop("`weights` must be finite, non-negative, and match the standards.", call. = FALSE)
  }

  fit_frame <- frame
  if (model == "exponential") {
    fit_frame$target <- log(fit_frame$concentration)
  } else {
    fit_frame$target <- fit_frame$concentration
  }
  fit <- if (is.null(weights)) {
    stats::lm(target ~ response, data = fit_frame)
  } else {
    stats::lm(target ~ response, data = fit_frame, weights = weights)
  }

  structure(
    list(
      fit = fit,
      model = model,
      standards = frame,
      concentration_col = concentration_col,
      response_col = response_col
    ),
    class = "omics_calibration"
  )
}

#' Predict concentrations with a targeted calibration model
#'
#' @param calibration An object returned by [fit_targeted_calibration()].
#' @param new_data Data frame containing an instrument-response column.
#' @param response_col Optional response-column override.
#' @param output_col Name of the appended concentration column.
#'
#' @return `new_data` with predicted concentrations appended.
#' @export
#' @author Yaoxiang Li
predict_targeted_calibration <- function(calibration,
                                         new_data,
                                         response_col = calibration$response_col,
                                         output_col = "predicted_concentration") {
  if (!inherits(calibration, "omics_calibration")) {
    stop("`calibration` must be created by `fit_targeted_calibration()`.", call. = FALSE)
  }
  new_data <- as.data.frame(new_data, check.names = FALSE)
  .require_columns(new_data, response_col)
  response <- as.numeric(new_data[[response_col]])
  prediction <- stats::predict(calibration$fit, newdata = data.frame(response = response))
  if (calibration$model == "exponential") {
    prediction <- exp(prediction)
  }
  new_data[[output_col]] <- as.numeric(prediction)
  tibble::as_tibble(new_data)
}

#' Plot a targeted calibration curve
#'
#' @param calibration An object returned by [fit_targeted_calibration()].
#'
#' @return A `ggplot` object.
#' @export
#' @author Yaoxiang Li
plot_targeted_calibration <- function(calibration) {
  if (!inherits(calibration, "omics_calibration")) {
    stop("`calibration` must be created by `fit_targeted_calibration()`.", call. = FALSE)
  }
  response_grid <- seq(
    min(calibration$standards$response),
    max(calibration$standards$response),
    length.out = 200L
  )
  curve <- predict_targeted_calibration(
    calibration,
    data.frame(response = response_grid),
    response_col = "response"
  )
  ggplot2::ggplot(
    calibration$standards,
    ggplot2::aes(x = .data$response, y = .data$concentration)
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(
      data = curve,
      ggplot2::aes(x = .data$response, y = .data$predicted_concentration),
      colour = "#2878B5",
      linewidth = 0.8
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Targeted calibration",
      x = calibration$response_col,
      y = calibration$concentration_col, caption = "Created by Yaoxiang Li"
    )
}

# Author: Yaoxiang Li
.require_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}
