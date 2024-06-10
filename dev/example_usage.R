library(omicsTools)

# Example 1: Using generated data
omics_data <- createOmicsData()

# Define custom thresholds
custom_thresholds <- define_thresholds(skewness = 3, kurtosis = 8)

# Update the OmicsData object with custom thresholds
omics_data@thresholds <- custom_thresholds

# Calculate measures for each feature
omics_data <- calculate_measures(omics_data)

# Flag anomalies
omics_data <- flag_anomalies(omics_data)

# Plotting distribution measures
plot_distribution_measures(omics_data)

# Plotting sample measures
plot_sample_measures(omics_data)

# Prepare data for UpSet plot
upset_data <- prepare_upset_data(omics_data)

# Convert to binary matrix for UpSetR
upset_matrix <- convert_to_binary_matrix(upset_data)

# Ensure there are at least two columns for the UpSet plot
ensure_enough_sets_for_upset(upset_matrix)

# Example 2: Using user-provided data
# Assuming user_data is a data frame where rows are samples and columns are features
user_data <- generate_data_with_anomalies(n_samples = 100, n_features = 200)  # Replace with actual data loading if available
omics_data_user <- createOmicsData(data = user_data)

# Define custom thresholds
custom_thresholds_user <- define_thresholds(skewness = 4, kurtosis = 6, shapiro_p = 1e-5, cooks_distance = 0.5, lof = 10)

# Update the OmicsData object with custom thresholds
omics_data_user@thresholds <- custom_thresholds_user

# Calculate measures for each feature
omics_data_user <- calculate_measures(omics_data_user)

# Flag anomalies
omics_data_user <- flag_anomalies(omics_data_user)

# Plotting distribution measures
plot_distribution_measures(omics_data_user)

# Plotting sample measures
plot_sample_measures(omics_data_user)

# Prepare data for UpSet plot
upset_data_user <- prepare_upset_data(omics_data_user)

# Convert to binary matrix for UpSetR
upset_matrix_user <- convert_to_binary_matrix(upset_data_user)

# Ensure there are at least two columns for the UpSet plot
ensure_enough_sets_for_upset(upset_matrix_user)
