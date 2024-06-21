#' Check and Sort Columns, Compare Values
#'
#' This function checks if two tibbles have the same column names, sorts the columns of one tibble to match the order of the other,
#' and then checks if all values in both tibbles are the same.
#'
#' @param area_data A tibble containing the data to be checked and sorted.
#' @param area_txt A tibble containing the reference data for column order and value comparison.
#' @return Prints messages indicating whether the tibbles have the same column names and whether all values are the same.
#' @examples
#' \dontrun{
#' area_data <- read.delim("path/to/All_txt.txt", check.names = FALSE)
#' area_txt <- read.delim("path/to/area.txt", check.names = FALSE)
#' check_and_sort_columns(area_data, area_txt)
#' }
check_and_sort_columns <- function(area_data, area_txt) {
  selected_columns <- colnames(area_txt)
  area_data <- dplyr::select(area_data, dplyr::all_of(selected_columns))

  # Check if both tibbles have the same column names
  same_colnames <- all(sort(colnames(area_data)) == sort(colnames(area_txt)))
  if (same_colnames) {
    cli::cli_alert_success("Both tibbles have the same column names.")

    # Sort the columns of area_data to match the order of area_txt
    area_data <- dplyr::select(area_data, dplyr::all_of(colnames(area_txt)))

    # Check if all values in area_data are the same as in area_txt
    same_values <- all(area_data == area_txt, na.rm = TRUE)

    if (same_values) {
      cli::cli_alert_success("All values in area_data are the same as in area_txt.")
    } else {
      cli::cli_alert_danger("There are differences in values between area_data and area_txt.")
    }
  } else {
    cli::cli_alert_danger("The tibbles do not have the same column names.")
  }
}


#' Detect Duplicate MRM Transitions
#'
#' This function adds a column `MRM_Duplicate_Flag` to the tibble, indicating if a row is a duplicate based on the criteria:
#' same polarity, less than a 1-minute retention time difference, and the same MRM transition (Q1/Q3).
#'
#' @param data A tibble containing the MRM transition data.
#' @param polarity_col Name of the column containing polarity information.
#' @param retention_time_col Name of the column containing retention time information.
#' @param mass_info_col Name of the column containing mass information.
#' @param component_name_col Name of the column containing component name information.
#' @return The original tibble with an added `MRM_Duplicate_Flag` column.
#' @examples
#' \dontrun{
#' sample_data <- tibble::tribble(
#'   ~Polarity, ~`Retention Time`, ~`Mass Info`, ~`Component Name`,
#'   "Positive", 1.95, "61.0 / 44.0", "Urea_pos",
#'   "Positive", 8.34, "206.0 / 189.0", "Lipoamide_pos",
#'   "Positive", 2.18, "339.1 / 110.0", "AICAR_pos",
#'   "Positive", 1.76, "175.1 / 70.0", "Arginine_pos",
#'   "Positive", 1.75, "176.2 / 159.1", "Citrulline_pos",
#'   "Positive", 8.90, "198.0 / 181.0", "Dopa_pos",
#'   "Positive", 2.06, "132.1 / 86.0", "Isoleucine_pos",
#'   "Positive", 1.92, "132.1 / 43.1", "Leucine_pos",
#'   "Positive", 1.76, "150.1 / 133.0", "Methionine_pos",
#'   "Positive", 7.79, "166.1 / 103.0", "Phenylalanine_pos"
#' )
#' detect_duplicates(sample_data, "Polarity", "Retention Time", "Mass Info", "Component Name")
#' }
detect_duplicates <- function(data, polarity_col = "Polarity", retention_time_col = "Retention Time", mass_info_col = "Mass Info", component_name_col = "Component Name") {
  data <- dplyr::mutate(data, MRM_Duplicate_Flag = "")


  for (i in 1:nrow(data)) {
    current_row <- data[i, ]
    duplicates <- dplyr::filter(
      data,
      !!dplyr::sym(polarity_col) == current_row[[polarity_col]],
      abs(!!dplyr::sym(retention_time_col) - current_row[[retention_time_col]]) < 1,
      !!dplyr::sym(mass_info_col) == current_row[[mass_info_col]]
    )

    if (nrow(duplicates) > 1) {
      data$MRM_Duplicate_Flag[i] <- paste("Duplicate of:", paste(duplicates[[component_name_col]], collapse = ", "))
    }
  }

  return(data)
}

#' Process All MRM Transitions for Duplicates
#'
#' This function takes a tibble containing MRM transition data, processes each sample_id separately to detect duplicates,
#' and adds a column `MRM_Duplicate_Flag` indicating if a row is a duplicate based on the criteria:
#' same polarity, less than a 1-minute retention time difference, and the same MRM transition (Q1/Q3).
#'
#' @param mrm_data A tibble containing the MRM transition data.
#' @param sample_name_col Name of the column containing sample name information.
#' @param sample_id_col Name of the column containing sample ID information.
#' @param polarity_col Name of the column containing polarity information.
#' @param retention_time_col Name of the column containing retention time information.
#' @param mass_info_col Name of the column containing mass information.
#' @param component_name_col Name of the column containing component name information.
#' @return The original tibble with an added `MRM_Duplicate_Flag` column.
#' @examples
#' \dontrun{
#' mrm_data <- tibble::tribble(
#'   ~`Sample Name`, ~`Sample ID`, ~Polarity, ~`Retention Time`, ~`Mass Info`, ~`Component Name`,
#'   "Sample1", "ID1", "Positive", 1.95, "61.0 / 44.0", "Urea_pos",
#'   "Sample1", "ID1", "Positive", 8.34, "206.0 / 189.0", "Lipoamide_pos",
#'   "Sample2", "ID2", "Positive", 2.18, "339.1 / 110.0", "AICAR_pos",
#'   "Sample2", "ID2", "Positive", 1.76, "175.1 / 70.0", "Arginine_pos"
#' )
#' processed_data <- process_mrm_duplicates(mrm_data, "Sample Name", "Sample ID", "Polarity", "Retention Time", "Mass Info", "Component Name")
#' print(processed_data)
#' }
process_mrm_duplicates <- function(mrm_data, sample_name_col = "Sample Name", sample_id_col = "Sample ID", polarity_col = "Polarity", retention_time_col = "Retention Time", mass_info_col = "Mass Info", component_name_col = "Component Name") {
  # Add sample_id column to mrm_data
  mrm_data <- mrm_data |>
    dplyr::mutate(sample_id = paste(!!dplyr::sym(sample_name_col), !!dplyr::sym(sample_id_col), sep = "_"))

  # Extract unique sample_ids
  sample_ids <- mrm_data |>
    dplyr::select(sample_id) |>
    dplyr::distinct() |>
    dplyr::pull(sample_id)

  # Initialize an empty MRM_Duplicate_Flag column in mrm_data
  mrm_data <- dplyr::mutate(mrm_data, MRM_Duplicate_Flag = "")

  cli::cli_progress_bar("Processing all samples ➜", total = length(sample_ids))

  for (id in sample_ids) {
    current_sample_data <- mrm_data |>
      dplyr::filter(sample_id == id) |>
      dplyr::select(!!dplyr::sym(polarity_col), !!dplyr::sym(retention_time_col), !!dplyr::sym(mass_info_col), !!dplyr::sym(component_name_col), sample_id)

    # Add the MRM_Duplicate_Flag column
    current_sample_data <- detect_duplicates(current_sample_data, polarity_col, retention_time_col, mass_info_col, component_name_col)

    # Update the MRM_Duplicate_Flag in the original mrm_data
    mrm_data <- mrm_data |>
      dplyr::rows_update(current_sample_data, by = c("sample_id", component_name_col))

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  return(mrm_data)
}

#' Convert MRM Data to Wide Format
#'
#' This function converts a tibble containing MRM data to a wide format based on the specified response column.
#'
#' @param data A tibble containing the MRM transition data.
#' @param response_col Name of the column containing the response data to be spread.
#' @param sample_name_col Name of the column containing sample name information.
#' @param sample_id_col Name of the column containing sample ID information.
#' @param component_name_col Name of the column containing component name information.
#' @return A tibble in wide format with samples as rows and compounds as columns.
#' @examples
#' \dontrun{
#' all_txt <- tibble::tribble(
#'   ~`Sample Name`, ~`Sample ID`, ~`Component Name`, ~Area, ~`IS Area`,
#'   "Sample1", "ID1", "Compound1", 100, 50,
#'   "Sample1", "ID2", "Compound2", 200, 75,
#'   "Sample2", "ID1", "Compound1", 150, 60,
#'   "Sample2", "ID2", "Compound2", 250, 80
#' )
#' area_data <- convert_mrm_data(all_txt, "Area", "Sample Name", "Sample ID", "Component Name")
#' is_area_data <- convert_mrm_data(all_txt, "IS Area", "Sample Name", "Sample ID", "Component Name")
#' print(area_data)
#' print(is_area_data)
#' }
convert_mrm_data <- function(data, response_col, sample_name_col = "Sample Name", sample_id_col = "Sample ID", component_name_col = "Component Name") {
  wide_data <- data |>
    dplyr::transmute(
      sample_id = paste0(!!dplyr::sym(sample_name_col), "_", !!dplyr::sym(sample_id_col)),
      compound_name = !!dplyr::sym(component_name_col),
      response = !!dplyr::sym(response_col)
    ) |>
    tidyr::spread(sample_id, response) |>
    metan::transpose_df() |>
    dplyr::rename(sample_id = name)

  return(wide_data)
}


#' Flag Overexpressed Features in Blank Samples
#'
#' Flags features that are too abundant in blank samples. If a feature is NA in the first blank sample, it is acceptable.
#' Otherwise, for each sample and feature, the peak area must be at least 10 times the mean area for the same transition in the first blank sample.
#'
#' @param data A tibble containing the MRM transition data.
#' @param sample_id_col Name of the column containing sample ID information.
#' @param feature_cols A vector of column names representing the features.
#' @param threshold A numeric value representing the threshold multiplier (default is 10).
#' @return A tibble with the same dimensions and column names as the input data,
#'         containing an empty string if not flagged or "Too high in Blank" if flagged.
#' @examples
#' \dontrun{
#' area_data <- tibble::tibble(
#'   sample_id = c(
#'     "011_Blank", "012_sample_002", "013_NIST_Plasma", "014_Blank",
#'     "015_sample_006", "016_sample_003", "017_Blank", "018_sample_018"
#'   ),
#'   `2-Deoxyglucose-6-Phosphate_neg` = c(NA, 345423.96, NA, NA, 125889.800, 323818.25, 133188.88, 62745.31),
#'   `2-Oxoisoleucine_neg` = c(NA, 53004.06, 124669.80, NA, 23650.90, 118364.36, 62745.31, 73367.63),
#'   `3-(4-Hydroxyphenyl)propionate_neg` = c(NA, 53004.06, 124669.80, NA, 23650.90, 118364.36, 62745.31, 73367.63)
#' )
#' flagged_data <- flag_blank_overexpressed(area_data, sample_id_col = "sample_id", feature_cols = names(area_data)[-1])
#' print(flagged_data)
#' }
flag_blank_overexpressed <- function(data, sample_id_col = "sample_id", feature_cols, threshold = 10) {
  # First blank sample
  first_blank <- dplyr::filter(data, grepl("Blank", !!dplyr::sym(sample_id_col))) |> dplyr::slice(1)
  blank_means <- first_blank |> dplyr::summarise(dplyr::across(dplyr::all_of(feature_cols), ~ mean(.x, na.rm = TRUE)))

  # Flag features
  flagged <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(dplyr::all_of(feature_cols),
      ~ dplyr::if_else(is.na(blank_means[[dplyr::cur_column()]]), "",
        dplyr::if_else(.x < threshold * blank_means[[dplyr::cur_column()]], "Too high in Blank", "")
      ),
      .names = "{col}"
    )) |>
    dplyr::ungroup()

  # Clear flags in first blank sample
  flags <- names(flagged)[names(flagged) %in% feature_cols]
  flagged[flagged[[sample_id_col]] == first_blank[[sample_id_col]][1], flags] <- ""

  # Output flagged features with sample_id
  result <- dplyr::select(flagged, !!dplyr::sym(sample_id_col), dplyr::all_of(feature_cols))
  return(result)
}

#' Combine Flagged Area and Height Data
#'
#' This function combines flagged area and height data tibbles. If any feature is marked as "Too high in Blank"
#' in either tibble for a given sample, it will be marked as "Too high in Blank" in the combined tibble.
#'
#' @param flagged_area A tibble containing flagged area data.
#' @param flagged_height A tibble containing flagged height data.
#' @param sample_id_col Name of the column containing sample ID information.
#' @return A combined tibble with the same dimensions and column names as the input data,
#'         containing "Too high in Blank" if flagged in either tibble.
#' @examples
#' \dontrun{
#' combined_data <- combine_flagged_data(flagged_area_data, flagged_height_data, "sample_id")
#' print(combined_data)
#' }
combine_flagged_data <- function(flagged_area, flagged_height, sample_id_col = "sample_id") {
  # Ensure the sample IDs and feature columns are aligned
  stopifnot(identical(flagged_area[[sample_id_col]], flagged_height[[sample_id_col]]))
  feature_cols <- setdiff(names(flagged_area), sample_id_col)
  stopifnot(identical(feature_cols, setdiff(names(flagged_height), sample_id_col)))
  
  # Combine the data using a union set rule
  combined <- flagged_area
  for (col in feature_cols) {
    combined[[col]] <- dplyr::if_else(flagged_area[[col]] == "Too high in Blank" | flagged_height[[col]] == "Too high in Blank",
                                      "Too high in Blank", "")
  }
  
  return(combined)
}


