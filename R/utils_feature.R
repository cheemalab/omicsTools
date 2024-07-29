#' Calculate Cook's Distance
#'
#' @param values A numeric vector of values for which to calculate Cook's Distance.
#' @return A numeric vector of Cook's Distance values.
#' @export
#' @examples
#' values <- rnorm(100)
#' calculate_cooks_distance(values)
calculate_cooks_distance <- function(values) {
  group <- 1:length(values) # Dummy grouping
  model <- lm(values ~ group)
  cooks_distance <- cooks.distance(model)
  return(cooks_distance)
}

#' Calculate Local Outlier Factor (LOF)
#'
#' @param values A numeric vector of values for which to calculate LOF.
#' @param k The number of neighbors to use for calculating LOF. Defaults to 5.
#' @return A numeric vector of LOF values.
#' @export
#' @examples
#' values <- rnorm(100)
#' calculate_lof(values)
calculate_lof <- function(values, k = 5) {
  lof <- dbscan::lof(matrix(values, ncol = 1), k)
  return(lof)
}

#' Generate High-Dimensional Data with Anomalies
#'
#' @param n_samples The number of samples to generate. Defaults to 100.
#' @param n_features The number of features to generate. Defaults to 2000.
#' @return A data frame containing the generated data with anomalies.
#' @export
#' @examples
#' data <- generate_data_with_anomalies()
generate_data_with_anomalies <- function(n_samples = 100, n_features = 2000) {
  cli::cli_h1("Generating High-Dimensional Data with Anomalies")

  set.seed(123)

  # Generate normal data for most features
  data <- as.data.frame(matrix(rnorm(n_samples * (n_features - 20)), nrow = n_samples, ncol = (n_features - 20)))
  colnames(data) <- paste0("F", 1:(n_features - 20))

  # Introduce anomalies in specific features
  cli::cli_alert_info("Introducing anomalies in specific features")
  anomalous_features <- data.frame(
    F1 = rnorm(n_samples, mean = 0, sd = 10), # High variance
    F2 = rnorm(n_samples, mean = 50, sd = 1), # Outliers with high mean
    F3 = c(rnorm(n_samples - 5), rep(100, 5)), # Few extreme outliers
    F4 = c(rnorm(n_samples / 2, mean = 0), rnorm(n_samples / 2, mean = 100)), # Bimodal distribution
    F5 = rexp(n_samples, rate = 1 / 0.1), # Exponential distribution
    F6 = rnorm(n_samples, mean = 0, sd = 15), # Another high variance
    F7 = rnorm(n_samples, mean = 30, sd = 2), # Another outlier with high mean
    F8 = c(rnorm(n_samples - 3), rep(80, 3)), # Few extreme outliers
    F9 = c(rnorm(n_samples / 2, mean = -50), rnorm(n_samples / 2, mean = 50)), # Another bimodal
    F10 = rexp(n_samples, rate = 1 / 0.2), # Another exponential distribution
    F11 = rnorm(n_samples, mean = 0, sd = 20), # Another high variance
    F12 = rnorm(n_samples, mean = 25, sd = 2.5), # Another outlier with high mean
    F13 = c(rnorm(n_samples - 2), rep(90, 2)), # Few extreme outliers
    F14 = c(rnorm(n_samples / 2, mean = -30), rnorm(n_samples / 2, mean = 60)), # Another bimodal
    F15 = rexp(n_samples, rate = 1 / 0.3), # Another exponential distribution
    F16 = rnorm(n_samples, mean = 0, sd = 5), # Another high variance
    F17 = rnorm(n_samples, mean = 60, sd = 1.5), # Another outlier with high mean
    F18 = c(rnorm(n_samples - 4), rep(70, 4)), # Few extreme outliers
    F19 = c(rnorm(n_samples / 2, mean = -60), rnorm(n_samples / 2, mean = 40)), # Another bimodal
    F20 = rexp(n_samples, rate = 1 / 0.4) # Another exponential distribution
  )

  # Combine normal and anomalous data
  data <- cbind(data, anomalous_features)

  # Add a group column for demonstration
  data$group <- sample(1:4, n_samples, replace = TRUE)

  cli::cli_alert_success("Data generation complete")
  return(data)
}

#' OmicsData Class
#'
#' A class to represent and analyze high-dimensional omics data.
#'
#' @slot data The data frame containing the generated data with anomalies.
#' @slot anomaly_measures The data frame containing the calculated anomaly measures.
#' @slot thresholds The list of thresholds for anomaly detection.
#' @export
setClass("OmicsData",
  slots = c(
    data = "data.frame",
    anomaly_measures = "data.frame",
    thresholds = "list"
  )
)

#' Constructor for OmicsData
#'
#' @param data The data frame containing the data. If not provided, synthetic data will be generated.
#' @param n_samples The number of samples to generate if data is not provided. Defaults to 100.
#' @param n_features The number of features to generate if data is not provided. Defaults to 2000.
#' @return An OmicsData object.
#' @export
#' @examples
#' omics_data <- createOmicsData()
#' user_data <- generate_data_with_anomalies()
#' omics_data <- createOmicsData(data = user_data)
createOmicsData <- function(data = NULL, n_samples = 100, n_features = 2000) {
  if (is.null(data)) {
    data <- generate_data_with_anomalies(n_samples, n_features)
  }
  anomaly_measures <- initialize_results_df(data)
  thresholds <- define_thresholds()
  new("OmicsData", data = data, anomaly_measures = anomaly_measures, thresholds = thresholds)
}

#' Initialize Results Data Frame
#'
#' @param data The data frame containing the generated data.
#' @return A data frame initialized with columns for anomaly measures.
#' @export
#' @examples
#' data <- generate_data_with_anomalies()
#' anomaly_measures <- initialize_results_df(data)
initialize_results_df <- function(data) {
  anomaly_measures <- data.frame(
    Feature = colnames(data)[-ncol(data)],
    Skewness = numeric(ncol(data) - 1),
    Kurtosis = numeric(ncol(data) - 1),
    Shapiro_p = numeric(ncol(data) - 1),
    Cooks_Distance = numeric(ncol(data) - 1),
    LOF = numeric(ncol(data) - 1),
    Skewness_Flag = logical(ncol(data) - 1),
    Kurtosis_Flag = logical(ncol(data) - 1),
    Shapiro_p_Flag = logical(ncol(data) - 1),
    Cooks_Distance_Flag = logical(ncol(data) - 1),
    LOF_Flag = logical(ncol(data) - 1)
  )
  return(anomaly_measures)
}

#' Define Anomaly Thresholds
#'
#' @param skewness The threshold for skewness. Defaults to 2.5.
#' @param kurtosis The threshold for kurtosis. Defaults to 10.
#' @param shapiro_p The threshold for Shapiro-Wilk test p-value. Defaults to 1e-10.
#' @param cooks_distance The threshold for Cook's Distance. Defaults to 1.
#' @param lof The threshold for Local Outlier Factor. Defaults to 15.
#' @return A list of thresholds for anomaly detection.
#' @export
#' @examples
#' thresholds <- define_thresholds()
#' thresholds <- define_thresholds(skewness = 3, kurtosis = 8)
define_thresholds <- function(skewness = 2.5, kurtosis = 10, shapiro_p = 1e-10, cooks_distance = 1, lof = 15) {
  thresholds <- list(
    Skewness = skewness,
    Kurtosis = kurtosis,
    Shapiro_p = shapiro_p,
    Cooks_Distance = cooks_distance,
    LOF = lof
  )
  return(thresholds)
}

#' Calculate Measures for Each Feature
#'
#' @param object An OmicsData object.
#' @return The OmicsData object with calculated measures for each feature.
#' @export
#' @examples
#' omics_data <- createOmicsData()
#' omics_data <- calculate_measures(omics_data)
setGeneric("calculate_measures", function(object) standardGeneric("calculate_measures"))

#' @import moments
#' @import outliers
#' @import dplyr
setMethod("calculate_measures", "OmicsData", function(object) {
  cli::cli_h1("Calculating Measures for Each Feature")
  data <- object@data
  anomaly_measures <- object@anomaly_measures
  for (i in 1:(ncol(data) - 1)) {
    feature_values <- data[[i]]
    anomaly_measures$Skewness[i] <- moments::skewness(feature_values)
    anomaly_measures$Kurtosis[i] <- moments::kurtosis(feature_values)
    anomaly_measures$Shapiro_p[i] <- shapiro.test(feature_values)$p.value
    anomaly_measures$Cooks_Distance[i] <- max(calculate_cooks_distance(feature_values))
    anomaly_measures$LOF[i] <- max(calculate_lof(feature_values))
  }
  object@anomaly_measures <- anomaly_measures
  cli::cli_alert_success("Measures calculation complete")
  return(object)
})

#' Flag Anomalies
#'
#' @param object An OmicsData object.
#' @return The OmicsData object with flagged anomalies.
#' @export
#' @examples
#' omics_data <- createOmicsData()
#' omics_data <- calculate_measures(omics_data)
#' omics_data <- flag_anomalies(omics_data)
setGeneric("flag_anomalies", function(object) standardGeneric("flag_anomalies"))

#' @import dplyr
setMethod("flag_anomalies", "OmicsData", function(object) {
  cli::cli_h1("Flagging Anomalies")
  anomaly_measures <- object@anomaly_measures
  thresholds <- object@thresholds
  anomaly_measures <- anomaly_measures |>
    dplyr::mutate(
      Skewness_Flag = Skewness > thresholds$Skewness,
      Kurtosis_Flag = Kurtosis > thresholds$Kurtosis,
      Shapiro_p_Flag = Shapiro_p < thresholds$Shapiro_p,
      Cooks_Distance_Flag = Cooks_Distance > thresholds$Cooks_Distance,
      LOF_Flag = LOF > thresholds$LOF
    )
  object@anomaly_measures <- anomaly_measures
  cli::cli_alert_success("Anomaly flagging complete")
  return(object)
})

#' Plot Distribution Measures
#'
#' @param object An OmicsData object.
#' @export
#' @examples
#' omics_data <- createOmicsData()
#' omics_data <- calculate_measures(omics_data)
#' omics_data <- flag_anomalies(omics_data)
#' plot_distribution_measures(omics_data)
setGeneric("plot_distribution_measures", function(object) standardGeneric("plot_distribution_measures"))

#' @import ggplot2
setMethod("plot_distribution_measures", "OmicsData", function(object) {
  cli::cli_h1("Plotting Distribution Measures")
  anomaly_measures <- object@anomaly_measures
  dist_plot <- ggplot(anomaly_measures, aes(x = Feature)) +
    geom_bar(aes(y = Skewness, fill = "Skewness"), stat = "identity", position = "dodge") +
    geom_bar(aes(y = Kurtosis, fill = "Kurtosis"), stat = "identity", position = "dodge") +
    geom_point(aes(y = -log10(Shapiro_p), color = "Shapiro-Wilk p-value"), size = 3) +
    labs(y = "Measure", x = "Feature", fill = "Measure", color = "Measure") +
    theme_minimal() +
    theme(
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold")
    ) +
    scale_color_manual(values = c("blue")) +
    scale_fill_manual(values = c("Skewness" = "red", "Kurtosis" = "green"))

  print(dist_plot)
  cli::cli_alert_success("Distribution measures plot complete")
})

#' Plot Sample Measures
#'
#' @param object An OmicsData object.
#' @export
#' @examples
#' omics_data <- createOmicsData()
#' omics_data <- calculate_measures(omics_data)
#' omics_data <- flag_anomalies(omics_data)
#' plot_sample_measures(omics_data)
setGeneric("plot_sample_measures", function(object) standardGeneric("plot_sample_measures"))

#' @import ggplot2
setMethod("plot_sample_measures", "OmicsData", function(object) {
  cli::cli_h1("Plotting Sample Measures")
  anomaly_measures <- object@anomaly_measures
  sample_plot <- ggplot(anomaly_measures, aes(x = Feature)) +
    geom_bar(aes(y = Cooks_Distance, fill = "Cook's Distance"), stat = "identity", position = "dodge") +
    geom_bar(aes(y = LOF, fill = "LOF"), stat = "identity", position = "dodge") +
    labs(y = "Measure", x = "Feature", fill = "Measure") +
    theme_minimal() +
    theme(
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold")
    ) +
    scale_fill_manual(values = c("Cook's Distance" = "purple", "LOF" = "orange"))

  print(sample_plot)
  cli::cli_alert_success("Sample measures plot complete")
})

#' Prepare Data for UpSet Plot
#'
#' @param object An OmicsData object.
#' @return A data frame prepared for UpSet plot.
#' @export
#' @import tidyr
#' @import dplyr
#' @examples
#' omics_data <- createOmicsData()
#' omics_data <- calculate_measures(omics_data)
#' omics_data <- flag_anomalies(omics_data)
#' upset_data <- prepare_upset_data(omics_data)
setGeneric("prepare_upset_data", function(object) standardGeneric("prepare_upset_data"))

setMethod("prepare_upset_data", "OmicsData", function(object) {
  cli::cli_h1("Preparing Data for UpSet Plot")
  anomaly_measures <- object@anomaly_measures
  upset_data <- anomaly_measures |>
    dplyr::select(Feature, Skewness_Flag, Kurtosis_Flag, Shapiro_p_Flag, Cooks_Distance_Flag, LOF_Flag) |>
    tidyr::pivot_longer(cols = Skewness_Flag:LOF_Flag, names_to = "Method", values_to = "Flag") |>
    dplyr::filter(Flag == TRUE) |>
    tidyr::pivot_wider(names_from = Method, values_from = Flag, values_fill = list(Flag = 0))
  cli::cli_alert_success("Data preparation for UpSet plot complete")
  return(upset_data)
})

#' Convert to Binary Matrix for UpSetR
#'
#' @param upset_data The data frame prepared for UpSet plot.
#' @return A binary matrix for UpSetR.
#' @export
#' @import dplyr
#' @examples
#' omics_data <- createOmicsData()
#' omics_data <- calculate_measures(omics_data)
#' omics_data <- flag_anomalies(omics_data)
#' upset_data <- prepare_upset_data(omics_data)
#' upset_matrix <- convert_to_binary_matrix(upset_data)
setGeneric("convert_to_binary_matrix", function(upset_data) standardGeneric("convert_to_binary_matrix"))

setMethod("convert_to_binary_matrix", "data.frame", function(upset_data) {
  cli::cli_h1("Converting Data to Binary Matrix for UpSetR")
  upset_matrix <- as.data.frame(lapply(upset_data[-1], as.integer))
  rownames(upset_matrix) <- upset_data$Feature
  cli::cli_alert_success("Data conversion to binary matrix complete")
  return(upset_matrix)
})

#' Ensure There Are Enough Sets for UpSet Plot
#'
#' @param upset_matrix The binary matrix for UpSetR.
#' @export
#' @import UpSetR
#' @examples
#' omics_data <- createOmicsData()
#' omics_data <- calculate_measures(omics_data)
#' omics_data <- flag_anomalies(omics_data)
#' upset_data <- prepare_upset_data(omics_data)
#' upset_matrix <- convert_to_binary_matrix(upset_data)
#' ensure_enough_sets_for_upset(upset_matrix)
setGeneric("ensure_enough_sets_for_upset", function(upset_matrix) standardGeneric("ensure_enough_sets_for_upset"))

setMethod("ensure_enough_sets_for_upset", "data.frame", function(upset_matrix) {
  cli::cli_h1("Checking Data for UpSet Plot")
  if (!requireNamespace("UpSetR", quietly = TRUE)) {
    cli::cli_alert_danger("Package 'UpSetR' is required but is not installed.")
    stop("Package 'UpSetR' is required but is not installed.")
  }
  if (ncol(upset_matrix) > 1) {
    cli::cli_alert_success("Enough sets for UpSet plot")
    UpSetR::upset(upset_matrix, sets = colnames(upset_matrix), order.by = "freq")
  } else {
    cli::cli_alert_warning("Not enough sets for UpSet plot")
    print("Not enough sets for UpSet plot")
  }
})



#' Perform Feature Selection
#'
#' This function performs feature selection using various methods such as LASSO, Elastic Net, Ridge regression, and Boruta.
#' It outputs selected features and variable importance plots.
#'
#' @param group_info A data frame containing group information.
#' @param features A data frame containing feature data.
#' @param id_col_group Column name in `group_info` to join with `features`.
#' @param id_col_features Column name in `features` to join with `group_info`.
#' @param group_col Column name indicating the group information.
#' @param outlier_col (Optional) Column name for identifying outliers.
#' @param outlier_vals (Optional) Values indicating non-outliers.
#' @param group_vals A vector of length 2 indicating the values for group comparison.
#' @param method The feature selection method to use: "lasso", "elastic_net", "ridge", "boruta".
#' @param mixture (Optional) The mixture parameter for Elastic Net, default is 0.5.
#' @param penalty_vals (Optional) Number of penalty values to try for tuning, default is 50.
#' @param seed (Optional) Random seed for reproducibility, default is 1234.
#' @param output_dir (Optional) Directory to save output files, default is "output".
#'
#' @return A tibble containing selected features and variable importances.
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom janitor clean_names
#' @importFrom Boruta Boruta TentativeRoughFix getConfirmedFormula getNonRejectedFormula getSelectedAttributes plotImpHistory
#' @importFrom randomForest randomForest
#' @importFrom caret confusionMatrix
#' @importFrom recipes recipe step_zv step_nzv step_center step_scale prep
#' @importFrom parsnip linear_reg set_engine fit
#' @importFrom workflows workflow add_recipe add_model
#' @importFrom rsample bootstraps
#' @importFrom tune tune_grid collect_metrics select_best finalize_workflow
#' @importFrom dials grid_regular penalty mixture
#' @importFrom doParallel registerDoParallel
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_line facet_wrap scale_x_log10 theme theme_classic ggsave
#' @importFrom ggsci scale_fill_npg scale_color_npg scale_fill_aaas scale_color_aaas
#' @importFrom vip vi
#' @importFrom forcats fct_reorder
#' @importFrom readr write_csv
#' @export
perform_feature_selection <- function(group_info,
                                      features,
                                      id_col_group,
                                      id_col_features,
                                      group_col,
                                      outlier_col = NULL,
                                      outlier_vals = c("No"),
                                      group_vals = c("No", "Yes"),
                                      method = c("lasso", "elastic_net", "ridge", "boruta"),
                                      mixture = 0.5,
                                      penalty_vals = 50,
                                      seed = 1234,
                                      output_dir = "output") {

  # Check if output directory exists, if not, create it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Merge group information and features
  data <- dplyr::left_join(group_info, features, by = setNames(id_col_group, id_col_features))

  # Filter outliers if specified
  if (!is.null(outlier_col)) {
    data <- dplyr::filter(data, !!rlang::sym(outlier_col) %in% outlier_vals)
  }

  # Prepare the response variable based on the method
  if (method == "boruta") {
    data <- dplyr::mutate(data, group = as.factor(dplyr::if_else(!!rlang::sym(group_col) == group_vals[2], group_vals[2], group_vals[1])))
  } else {
    data <- dplyr::mutate(data, group = dplyr::if_else(!!rlang::sym(group_col) == group_vals[2], 1, 0))
  }

  features_only <- dplyr::select(data, -dplyr::all_of(colnames(group_info)))

  if (method == "boruta") {
    # Clean column names
    prediction_data <- janitor::clean_names(features_only)

    # Update column names
    name_dict <- tibble::tibble(name = colnames(features_only), clean_name = colnames(prediction_data))

    # Boruta feature selection
    set.seed(seed)
    boruta_model <- Boruta::Boruta(group ~ ., data = prediction_data, doTrace = 3, maxRuns = 1000)
    print(boruta_model)

    # Plot variable importance
    png(file.path(output_dir, "boruta_variable_importance.png"), width = 12, height = 6, units = "in", res = 600)
    plot(boruta_model, las = 2, cex.axis = 0.35, col = c("lightblue", "blue", "darkblue"),
         main = "Boruta Feature Importance", xlab = "Features", ylab = "Importance")
    dev.off()

    # Plot importance history
    png(file.path(output_dir, "boruta_importance_history.png"), width = 12, height = 6, units = "in", res = 600)
    Boruta::plotImpHistory(boruta_model, col = "blue", main = "Boruta Importance History",
                   xlab = "Iterations", ylab = "Importance")
    dev.off()

    boruta_fixed <- Boruta::TentativeRoughFix(boruta_model)
    print(boruta_fixed)

    # Check if any important variables were detected
    if (length(Boruta::getSelectedAttributes(boruta_fixed)) == 0) {
      message("No attributes deemed important. Skipping subsequent analysis steps.")
      return(NULL)
    }

    # Extract confirmed and non-rejected formulas
    confirmed_formula <- as.formula(Boruta::getConfirmedFormula(boruta_fixed))
    nonreject_formula <- as.formula(Boruta::getNonRejectedFormula(boruta_fixed))

    # Data partition
    set.seed(seed)
    ind <- sample(2, nrow(prediction_data), replace = TRUE, prob = c(0.6, 0.4))
    train <- prediction_data[ind == 1, ]
    test <- prediction_data[ind == 2, ]

    # Random forest model with all features
    set.seed(seed)
    rf_full <- randomForest::randomForest(group ~ ., data = train)

    # Prediction & confusion matrix for test set
    p_full <- predict(rf_full, test)
    cm_full <- caret::confusionMatrix(p_full, test$group)

    # Random forest model with selected features
    set.seed(seed)
    rf_selected <- randomForest::randomForest(nonreject_formula, data = train)

    p_selected <- predict(rf_selected, test)
    cm_selected <- caret::confusionMatrix(p_selected, test$group)

    # Create Boruta result tibble
    boruta_result <- tibble::tibble(clean_name = Boruta::getSelectedAttributes(boruta_fixed),
                                    Boruta = rep("Selected", length(Boruta::getSelectedAttributes(boruta_fixed))))
    boruta_result <- dplyr::left_join(name_dict, boruta_result, by = "clean_name")

    # Save Boruta results
    readr::write_csv(boruta_result, file.path(output_dir, "boruta_variable_selected.csv"))

    # Save confusion matrices as CSV
    cm_full_df <- as.data.frame(cm_full$table)
    cm_selected_df <- as.data.frame(cm_selected$table)

    readr::write_csv(cm_full_df, file.path(output_dir, "confusion_matrix_full.csv"))
    readr::write_csv(cm_selected_df, file.path(output_dir, "confusion_matrix_selected.csv"))

    return(boruta_result)

  } else {

    # Prepare the recipe for regression methods
    rad_recipe <- recipes::recipe(group ~ ., data = features_only) %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_nzv(recipes::all_predictors()) %>%
      recipes::step_center(recipes::all_numeric_predictors()) %>%
      recipes::step_scale(recipes::all_numeric_predictors())

    rad_prep <- rad_recipe %>% recipes::prep(strings_as_factors = FALSE)

    # Specify the model type
    model_spec <- switch(method,
                         "lasso" = parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>% parsnip::set_engine("glmnet"),
                         "elastic_net" = parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) %>% parsnip::set_engine("glmnet"),
                         "ridge" = parsnip::linear_reg(penalty = tune::tune(), mixture = 0) %>% parsnip::set_engine("glmnet"))

    wf <- workflows::workflow() %>% workflows::add_recipe(rad_recipe)

    # Set up cross-validation
    set.seed(seed)
    rad_boot <- rsample::bootstraps(features_only, strata = group)

    # Create the grid for tuning
    lambda_grid <- if (method == "elastic_net") {
      dials::grid_regular(dials::penalty(), dials::mixture(), levels = penalty_vals)
    } else {
      dials::grid_regular(dials::penalty(), levels = penalty_vals)
    }

    # Register parallel backend
    doParallel::registerDoParallel()

    # Tune the model
    set.seed(seed)
    model_grid <- tune::tune_grid(
      wf %>% workflows::add_model(model_spec),
      resamples = rad_boot,
      grid = lambda_grid
    )

    # Collect metrics
    metrics <- model_grid %>% tune::collect_metrics()

    # Save metrics plot
    metrics_plot <- metrics %>%
      ggplot2::ggplot(ggplot2::aes(penalty, mean, color = .metric)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.5) +
      ggplot2::geom_line(size = 1.5) +
      ggplot2::facet_wrap(~.metric, scales = "free", nrow = 2) +
      ggplot2::scale_x_log10() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme_classic() +
      ggsci::scale_fill_npg() +
      ggsci::scale_color_npg()

    ggplot2::ggsave(file.path(output_dir, "metrics_plot.png"), metrics_plot, width = 6, height = 3)

    # Select the best model based on RMSE
    best_model <- model_grid %>% tune::select_best(metric = "rmse")

    final_wf <- tune::finalize_workflow(
      wf %>% workflows::add_model(model_spec),
      best_model
    )

    # Fit the final model
    final_fit <- final_wf %>% parsnip::fit(data)

    # Extract variable importance
    vi_data <- final_fit %>%
      workflows::pull_workflow_fit() %>%
      vip::vi(lambda = best_model$penalty) %>%
      dplyr::mutate(Importance = abs(Importance), Variable = forcats::fct_reorder(Variable, Importance))

    readr::write_csv(vi_data, file.path(output_dir, "variable_importance.csv"))

    # Plot top variable importance
    top_vi_plot <- vi_data %>%
      dplyr::slice_head(n = 10) %>%
      ggplot2::ggplot(ggplot2::aes(x = Importance, y = Variable, fill = Sign)) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::labs(y = NULL) +
      ggplot2::theme_classic() +
      ggsci::scale_fill_aaas() +
      ggsci::scale_color_aaas()

    ggplot2::ggsave(file.path(output_dir, "top_variable_importance.png"), top_vi_plot, width = 6, height = 3)

    return(vi_data)
  }
}


