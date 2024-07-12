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
