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
#' @export
#' @import dplyr cli tibble
#' @importFrom utils read.delim
#' @importFrom rlang .data
#' @author Yaoxiang Li
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
#'   ~polarity, ~`retention_time`, ~`Mass Info`, ~`component_name`,
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
#' detect_duplicates(sample_data, "polarity", "retention_time", "Mass Info", "component_name")
#' }
#' @export
#' @import dplyr tibble
#' @importFrom rlang sym
#' @author Yaoxiang Li
detect_duplicates <- function(data, polarity_col = "polarity", retention_time_col = "retention_time", mass_info_col = "Mass Info", component_name_col = "component_name") {
  data <- dplyr::mutate(data, MRM_Duplicate_Flag = "")

  for (i in 1:nrow(data)) {
    current_row <- data[i, ]
    duplicates <- dplyr::filter(
      data,
      !!rlang::sym(polarity_col) == current_row[[polarity_col]],
      abs(!!rlang::sym(retention_time_col) - current_row[[retention_time_col]]) < 1,
      !!rlang::sym(mass_info_col) == current_row[[mass_info_col]]
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
#'   ~`data_filename`, ~`sample_id`, ~polarity, ~`retention_time`, ~`Mass Info`, ~`component_name`,
#'   "Sample1", "ID1", "Positive", 1.95, "61.0 / 44.0", "Urea_pos",
#'   "Sample1", "ID1", "Positive", 8.34, "206.0 / 189.0", "Lipoamide_pos",
#'   "Sample2", "ID2", "Positive", 2.18, "339.1 / 110.0", "AICAR_pos",
#'   "Sample2", "ID2", "Positive", 1.76, "175.1 / 70.0", "Arginine_pos")
#' processed_data <- process_mrm_duplicates(
#'   mrm_data,
#'   "data_filename",
#'   "sample_id",
#'   "polarity",
#'   "retention_time",
#'   "Mass Info",
#'   "component_name"
#' )
#' print(processed_data)
#' }
#' @export
#' @import dplyr cli
#' @author Yaoxiang Li
process_mrm_duplicates <- function(mrm_data, sample_name_col = "data_filename", sample_id_col = "sample_id", polarity_col = "polarity", retention_time_col = "retention_time", mass_info_col = "Mass Info", component_name_col = "component_name") {
  # Add sample_id column to mrm_data
  mrm_data <- mrm_data |>
    dplyr::mutate(sample_id = paste(!!rlang::sym(sample_name_col), !!rlang::sym(sample_id_col), sep = "_"))

  # Extract unique sample_ids
  sample_ids <- mrm_data |>
    dplyr::select(sample_id) |>
    dplyr::distinct() |>
    dplyr::pull(sample_id)

  # Initialize an empty MRM_Duplicate_Flag column in mrm_data
  mrm_data <- dplyr::mutate(mrm_data, MRM_Duplicate_Flag = "")

  cli::cli_progress_bar("Processing all samples ->", total = length(sample_ids))

  for (id in sample_ids) {
    current_sample_data <- mrm_data |>
      dplyr::filter(sample_id == id) |>
      dplyr::select(!!rlang::sym(polarity_col), !!rlang::sym(retention_time_col), !!rlang::sym(mass_info_col), !!rlang::sym(component_name_col), sample_id)

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
#'   ~`data_filename`, ~`sample_id`, ~`component_name`, ~Area, ~`IS Area`,
#'   "Sample1", "ID1", "Compound1", 100, 50,
#'   "Sample1", "ID2", "Compound2", 200, 75,
#'   "Sample2", "ID1", "Compound1", 150, 60,
#'   "Sample2", "ID2", "Compound2", 250, 80
#' )
#' area_data <- convert_mrm_data(all_txt, "Area", "data_filename", "sample_id", "component_name")
#' is_area_data <- convert_mrm_data(all_txt, "IS Area", "data_filename", "sample_id", "component_name")
#' print(area_data)
#' print(is_area_data)
#' }
#' @export
#' @import dplyr tidyr
#' @author Yaoxiang Li
convert_mrm_data <- function(data, response_col, sample_name_col = "data_filename", sample_id_col = "sample_id", component_name_col = "component_name") {
  wide_data <- data |>
    dplyr::transmute(
      sample_id = paste0(!!rlang::sym(sample_name_col), "_", !!rlang::sym(sample_id_col)),
      compound_name = !!rlang::sym(component_name_col),
      response = !!rlang::sym(response_col)
    ) |>
    tidyr::spread(sample_id, response) |>
    transpose_df() |>
    dplyr::rename(sample_id = name)

  return(wide_data)
}

#' Flag Underexpressed Features in Samples Based on Blank Samples
#'
#' Flags features in samples based on their abundance in blank samples. If a feature is NA in the first blank sample,
#' all samples for this feature are marked as TRUE. Otherwise, for each sample and feature, if the peak area is at least
#' 10 times the area of the first blank sample, it is marked as TRUE, else FALSE. NA values in the samples remain unchanged.
#'
#' @param data A tibble containing the MRM transition data.
#' @param sample_id_col Name of the column containing sample ID information.
#' @param feature_cols A vector of column names representing the features.
#' @param threshold A numeric value representing the threshold multiplier (default is 10).
#' @return A tibble with the same dimensions and column names as the input data,
#'         containing TRUE, FALSE, or NA based on the criteria.
#' @examples
#' \dontrun{
#' area_data <- tibble::tibble(
#'   sample_id = c(
#'     "011_Blank", "012_sample_002", "013_NIST_Plasma", "014_Blank",
#'     "015_sample_006", "016_sample_003"
#'   ),
#'   `2-Deoxyglucose-6-Phosphate_neg` = c(NA, 345423.96, NA, NA, 125889.80, 323818.25),
#'   `2-Oxoisoleucine_neg` = c(NA, 53004.06, 124669.80, NA, 23650.90, 118364.36),
#'   `3-(4-Hydroxyphenyl)propionate_neg` = c(NA, 53004.06, 124669.80, NA, 23650.90, 118364.36)
#' )
#' flagged_data <- flag_underexpressed_features(
#'   area_data,
#'   sample_id_col = "sample_id",
#'   feature_cols = names(area_data)[-1]
#' )
#' print(flagged_data)
#' }
#' @export
#' @import dplyr
#' @importFrom rlang .data
#' @author Yaoxiang Li
flag_underexpressed_features <- function(data, sample_id_col = "sample_id", feature_cols, threshold = 10) {
  # First blank sample
  first_blank <- dplyr::filter(data, grepl("Blank", !!rlang::sym(sample_id_col))) |> dplyr::slice(1)

  # Flag features
  flagged <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(dplyr::all_of(feature_cols),
      ~ dplyr::if_else(is.na(.x), NA,
        dplyr::if_else(is.na(first_blank[[dplyr::cur_column()]]), TRUE,
          dplyr::if_else(.x >= threshold * first_blank[[dplyr::cur_column()]], TRUE, FALSE)
        )
      ),
      .names = "{col}"
    )) |>
    dplyr::ungroup()

  # Output flagged features with sample_id
  result <- dplyr::select(flagged, !!rlang::sym(sample_id_col), dplyr::all_of(feature_cols))
  return(result)
}


#' Combine Multiple Logical Tibbles with Intersection or Union
#'
#' @param ... Multiple tibbles to be combined. Each tibble should have the same
#' dimensions, same column names in the same order, and the first column values
#' should be identical across all tibbles.
#' @param method A string indicating the method to combine the tibbles.
#' Either "intersection" or "union". Default is "intersection".
#'
#' @return A combined tibble where each cell is TRUE based on the method:
#' - "intersection": TRUE if all corresponding cells in the input tibbles are TRUE,
#' otherwise FALSE.
#' - "union": TRUE if at least one corresponding cell in the input tibbles is TRUE,
#' otherwise FALSE.
#' @import dplyr tibble
#' @export
#'
#' @examples
#' tibble1 <- tibble::tibble(id = 1:3, A = c(TRUE, FALSE, NA), B = c(TRUE, TRUE, FALSE))
#' tibble2 <- tibble::tibble(id = 1:3, A = c(TRUE, TRUE, TRUE), B = c(FALSE, TRUE, TRUE))
#' tibble3 <- tibble::tibble(id = 1:3, A = c(TRUE, FALSE, TRUE), B = c(TRUE, TRUE, NA))
#' combine_logical_tibbles(tibble1, tibble2, tibble3, method = "intersection")
#' combine_logical_tibbles(tibble1, tibble2, tibble3, method = "union")
#' @author Yaoxiang Li
combine_logical_tibbles <- function(..., method = c("intersection", "union")) {
  method <- match.arg(method)
  tibbles <- list(...)

  # Check if all tibbles have the same dimensions, column names and first column values
  first_column_values <- lapply(tibbles, `[[`, 1)
  if (!all(sapply(first_column_values, identical, first_column_values[[1]]))) {
    stop("The first column values are not identical across all tibbles.")
  }

  column_names <- lapply(tibbles, colnames)
  if (!all(sapply(column_names, identical, column_names[[1]]))) {
    stop("The column names are not identical across all tibbles.")
  }

  dimensions <- lapply(tibbles, dim)
  if (!all(sapply(dimensions, identical, dimensions[[1]]))) {
    stop("The dimensions are not identical across all tibbles.")
  }

  # Combine tibbles based on the method
  # Author: Yaoxiang Li
  combined <- Reduce(function(x, y) {
    result <- x
    for (j in seq_along(x)) {
      if (j == 1) {
        next
      }
      if (method == "intersection") {
        result[[j]] <- x[[j]] & y[[j]]
      } else if (method == "union") {
        result[[j]] <- x[[j]] | y[[j]]
      }
    }
    result
  }, tibbles)

  return(combined)
}


#' Load and Parse SCIEX OS Exported LC-MRM-MS2 Data
#'
#' @param file_path File path of the input text file of a complete output of the
#' SCIEX OS results from a sequence. File should be tab-delimited and in the
#' 'long' format.
#' @param input_data Input tibble of raw SCIEX (pre-parsing) text file. If `NULL`
#' (default value), data will be loaded from `file_path`.
#' @param return_all_columns Logical value as to whether to return all columns (`TRUE`)
#' or just the necessary columns for downstream machine learning analysis or
#' quality control review (`FALSE`). Default value is `TRUE`.
#' When set to false, the columns included in the returned tibble include:
#' `"component_name"`, `"component_idx"`, `"precursor_mz"`, `"product_mz"`,
#' `"is_istd"`, `"istd"`, `"retention_time_expected"`, `"data_filename"`,
#' `"data_file_idx"`, `"sample_id"`, `"sample_type"`, `"component_type"`,
#' `"polarity"`, `"component_group"`, `"outlier_reasons"`,
#' `"retention_time_expected_istd"`, `"area"`, `"istd_area"`, `"area_ratio"`,
#' `"height"`, `"istd_height"`, `"height_ratio"`, `"peak_quality"`,
#' `"istd_peak_quality"`, `"retention_time"`, `"retention_time_istd"`,
#' `"rt_error"`, `"rt_delta_min"`, `"rt_start"`, `"istd_rt_start"`, `"rt_end"`,
#' `"istd_rt_end"`, `"peak_width"`, `"istd_peak_width"`, `"fwhm"`,
#' `"istd_fwhm"`, `"signal_noise"`, `"istd_signal_noise"`, `"modified"`,
#' `"relative_rt"`, `"used"`, `"tailing_factor"`, `"asymmetry_factor"`,
#' `points_across_baseline"`, `"points_across_fwhm"`).
#' @param check_negative_values Logical value as to whether to check for negative
#' values in the `area` and `height` variables (for both components and internal
#' standards). If `TRUE` (default) and there is at least one negative value in
#' the data, the minimum `area` or `height` value will be subtracted from all
#' `area` and/or `height` values by `component_name`, and 100 will then be added
#' to avoid having values below 100. `area_ratio`, `height_ratio`, and
#' `area_height_ratio` values (and their internal standard equivalent variables)
#' will also be re-calculated.
#' @param fix_istds Logical value (default `TRUE`) to identify internal standards
#' by regular expression of \code{"(\\.IS$)|(_IS$)|(_d[0-9]+_)|(\\(d[0-9]+\\))"}.
#' @return tibble with the fields appropriately renamed.
#' @import readr dplyr cli tibble
#' @export
#'
#' @examples
#' \dontrun{
#' data(sciex_mrm_ms_data)
#' data_tibble <- load_parse_sciex_txt(
#'   file_path = "path/to/file.txt",
#'   return_all_columns = FALSE,
#'   check_negative_values = TRUE
#' )
#' }
#' @author Yaoxiang Li
load_parse_sciex_txt <- function(file_path, input_data = NULL, return_all_columns = TRUE,
                                 check_negative_values = TRUE, fix_istds = TRUE) {
  cli::cli_progress_bar("Reading data", total = 5)

  if (is.null(input_data)) {
    cli::cli_alert_info("Loading data from file")
    input_data <- readr::read_tsv(file_path, na = c("N/A", "NA", ""))
  }

  # If the "Precursor Mass" column is missing, add it as NA so that the subsequent
  # renaming does not fail.
  if (!("Precursor Mass" %in% names(input_data))) {
    cli::cli_alert_warning("Column 'Precursor Mass' not found in input data; skipping.")
    input_data[["Precursor Mass"]] <- NA_real_
  }


  if (!("Fragment Mass" %in% names(input_data))) {
    cli::cli_alert_warning("Column 'Fragment Mass' not found in input data; skipping.")
    input_data[["Fragment Mass"]] <- NA_real_
  }


  if (!("Component Type" %in% names(input_data))) {
    cli::cli_alert_warning("Column 'Component Type' not found in input data; skipping.")
    input_data[["Component Type"]] <- NA_real_
  }


  if (!("Polarity" %in% names(input_data))) {
    cli::cli_alert_warning("Column 'Polarity' not found in input data; skipping.")
    input_data[["Polarity"]] <- NA_real_
  }


  if (!("Retention Time Error (%)" %in% names(input_data))) {
    cli::cli_alert_warning("Column 'Retention Time Error (%)' not found in input data; skipping.")
    input_data[["Retention Time Error (%)"]] <- NA_real_
  }


  if (!("Retention Time Delta (min)" %in% names(input_data))) {
    cli::cli_alert_warning("Column 'Retention Time Delta (min)' not found in input data; skipping.")
    input_data[["Retention Time Delta (min)"]] <- NA_real_
  }


  cli::cli_progress_update()

  cli::cli_alert_info("Renaming columns")
  input_data <- input_data |>
    dplyr::rename(
      component_name = `Component Name`,
      component_idx = `Component Index`,
      precursor_mz = `Precursor Mass`,
      product_mz = `Fragment Mass`,
      is_istd = `IS`,
      istd = `IS Name`,
      retention_time_expected = `Expected RT`,
      data_filename = `Sample Name`,
      data_file_idx = `Sample Index`,
      sample_id = `Sample ID`,
      sample_type = `Sample Type`,
      vial_number = `Vial Number`,
      dilution_factor = `Dilution Factor`,
      injection_volume_ul = `Injection Volume`,
      component_type = `Component Type`,
      polarity = `Polarity`,
      component_group = `Component Group Name`,
      outlier_reasons = `Outlier Reasons`,
      retention_time_expected_istd = `IS Expected RT`,
      area = `Area`,
      istd_area = `IS Area`,
      area_ratio = `Area Ratio`,
      height = `Height`,
      istd_height = `IS Height`,
      height_ratio = `Height Ratio`,
      area_height_ratio = `Area / Height`,
      istd_area_height_ratio = `IS Area / Height`,
      peak_quality = `Quality`,
      istd_peak_quality = `IS Quality`,
      retention_time = `Retention Time`,
      retention_time_istd = `IS Retention Time`,
      rt_error = `Retention Time Error (%)`,
      rt_delta_min = `Retention Time Delta (min)`,
      rt_start = `Start Time`,
      istd_rt_start = `IS Start Time`,
      rt_end = `End Time`,
      istd_rt_end = `IS End Time`,
      peak_width = `Total Width`,
      istd_peak_width = `IS Total Width`,
      fwhm = `Width at 50%`,
      istd_fwhm = `IS Width at 50%`,
      signal_noise = `Signal / Noise`,
      istd_signal_noise = `IS Signal / Noise`,
      modified = `Modified`,
      relative_rt = `Relative RT`,
      used = `Used`,
      tailing_factor = `Tailing Factor`,
      asymmetry_factor = `Asymmetry Factor`,
      points_across_baseline = `Points Across Baseline`,
      points_across_fwhm = `Points Across Half Height`
    ) |>
    dplyr::mutate(
      polarity = tolower(polarity),
      precursor_mz = as.numeric(precursor_mz),
      product_mz = as.numeric(product_mz)
    )
  cli::cli_progress_update()

  if (fix_istds) {
    cli::cli_alert_info("Fixing internal standards")
    int_stds <- input_data |>
      dplyr::filter(is_istd == TRUE) |>
      dplyr::pull(component_name) |>
      unique()

    potential_istds <- input_data |>
      dplyr::pull(component_name) |>
      unique() |>
      (\(x) grep("(\\.IS$)|(_IS$)|(_d[0-9]{1,}_)|(\\(d[0-9]{1,}\\))", x, value = TRUE))()


    new_istds <- base::setdiff(potential_istds, int_stds)

    if (length(new_istds) > 0) {
      input_data <- input_data |>
        dplyr::mutate(
          is_istd = dplyr::if_else(component_name %in% new_istds, TRUE, is_istd),
          component_type = dplyr::if_else(component_name %in% new_istds, "Internal Standards", component_type),
          istd = dplyr::if_else(component_name %in% new_istds, NA_character_, istd)
        )
    }
  }
  cli::cli_progress_update()

  cli::cli_alert_info("Updating internal standard related columns")
  input_data <- input_data |>
    dplyr::mutate(
      istd_area = dplyr::if_else(is_istd, area, istd_area),
      istd_height = dplyr::if_else(is_istd, height, istd_height),
      istd_peak_quality = dplyr::if_else(is_istd, peak_quality, istd_peak_quality),
      retention_time_istd = dplyr::if_else(is_istd, retention_time, retention_time_istd),
      istd_rt_start = dplyr::if_else(is_istd, rt_start, istd_rt_start),
      istd_rt_end = dplyr::if_else(is_istd, rt_end, istd_rt_end),
      istd_peak_width = dplyr::if_else(is_istd, peak_width, istd_peak_width),
      istd_fwhm = dplyr::if_else(is_istd, fwhm, istd_fwhm),
      istd_signal_noise = dplyr::if_else(is_istd, signal_noise, istd_signal_noise),
      area_ratio = dplyr::if_else(is_istd, area / istd_area, area_ratio),
      height_ratio = dplyr::if_else(is_istd, height / istd_height, height_ratio)
    )
  cli::cli_progress_update()

  if (!return_all_columns) {
    cli::cli_alert_info("Selecting necessary columns")
    keep_columns <- c(
      "component_name", "component_idx", "precursor_mz", "product_mz",
      "is_istd", "istd", "retention_time_expected", "data_filename", "data_file_idx",
      "sample_id", "sample_type", "component_type", "polarity", "component_group",
      "outlier_reasons", "retention_time_expected_istd", "area", "istd_area",
      "area_ratio", "height", "istd_height", "height_ratio", "peak_quality",
      "istd_peak_quality", "retention_time", "retention_time_istd", "rt_error",
      "rt_delta_min", "rt_start", "istd_rt_start", "rt_end", "istd_rt_end",
      "peak_width", "istd_peak_width", "fwhm", "istd_fwhm", "signal_noise",
      "istd_signal_noise", "modified", "relative_rt", "used", "tailing_factor",
      "asymmetry_factor", "points_across_baseline", "points_across_fwhm", "batch_id"
    )

    keep_columns <- intersect(keep_columns, names(input_data))
    input_data <- input_data |>
      dplyr::select(dplyr::all_of(keep_columns))
  }
  cli::cli_progress_update()

  if (check_negative_values) {
    cli::cli_alert_info("Checking for negative values")
    re_calc_area_height_ratio <- FALSE
    re_calc_istd_area_height_ratio <- FALSE

    if (any(input_data$area < 0, na.rm = TRUE)) {
      min_area <- min(input_data$area, na.rm = TRUE)
      input_data <- input_data |>
        dplyr::group_by(component_name) |>
        dplyr::mutate(area = area - min_area + 100) |>
        dplyr::ungroup()

      if (any(input_data$istd_area < 0, na.rm = TRUE)) {
        min_istd_area <- min(input_data$istd_area, na.rm = TRUE)
        input_data <- input_data |>
          dplyr::group_by(istd) |>
          dplyr::mutate(istd_area = istd_area - min_istd_area + 100) |>
          dplyr::ungroup()
        re_calc_istd_area_height_ratio <- TRUE
      }

      input_data <- input_data |>
        dplyr::mutate(area_ratio = area / istd_area)
      re_calc_area_height_ratio <- TRUE
    }

    if (any(input_data$height < 0, na.rm = TRUE)) {
      min_height <- min(input_data$height, na.rm = TRUE)
      input_data <- input_data |>
        dplyr::group_by(component_name) |>
        dplyr::mutate(height = height - min_height + 100) |>
        dplyr::ungroup()

      if (any(input_data$istd_height < 0, na.rm = TRUE)) {
        min_istd_height <- min(input_data$istd_height, na.rm = TRUE)
        input_data <- input_data |>
          dplyr::group_by(istd) |>
          dplyr::mutate(istd_height = istd_height - min_istd_height + 100) |>
          dplyr::ungroup()
        re_calc_istd_area_height_ratio <- TRUE
      }

      input_data <- input_data |>
        dplyr::mutate(height_ratio = height / istd_height)
      re_calc_area_height_ratio <- TRUE
    }

    if (re_calc_area_height_ratio) {
      input_data <- input_data |>
        dplyr::mutate(area_height_ratio = area / height)
    }

    if (re_calc_istd_area_height_ratio) {
      input_data <- input_data |>
        dplyr::mutate(istd_area_height_ratio = istd_area / istd_height)
    }
  }

  cli::cli_alert_success("Data processing complete")
  cli::cli_progress_done()

  return(input_data)
}



#' Generate Process Report for Sciex 7500/5500 Raw Data
#'
#' This function generates a comprehensive process report for Sciex 7500/5500 raw data,
#' including data normalization, missing value imputation, and optional normalization
#' and flagging steps. The results are saved in a temporary directory and then zipped
#' into a file for easy sharing.
#'
#' @param input_file The path to the input file containing raw data.
#' @param output_file The path to the output zip file.
#' @param filter_blank Logical, whether to filter out blank samples (default: TRUE).
#' @param blank_string Character, regular expression pattern to match blank sample IDs (default: 'Blank|BLANK|blank').
#' @param filter_nist Logical, whether to filter out NIST samples (default: TRUE).
#' @param nist_string Character, regular expression pattern to match NIST sample IDs (default: 'NIST|Nist|nist').
#' @param imputation_threshold Numeric, threshold for missing value imputation (default: 0.25).
#' @param imputation_method Character, method for missing value imputation (default: 'half_min').
#' @param qc_string Character, regular expression pattern to match QC sample IDs (default: 'QC').
#' @param include_is_normalization Logical, whether to include internal standard normalization (default: TRUE).
#' @param include_qc_rlsc Logical, whether to include QC-RLSC normalization (default: TRUE).
#' @param include_pqn Logical, whether to include PQN normalization (default: TRUE).
#' @param include_qc_rsd Logical, whether to include QC RSD calculation (default: TRUE).
#' @param include_snr_flag Logical, whether to include Signal-to-Noise ratio flagging (default: TRUE).
#' @param snr_threshold Numeric, threshold for Signal-to-Noise ratio flagging (default: 10).
#' @param include_area_flag Logical, whether to include area threshold flagging (default: TRUE).
#' @param include_height_flag Logical, whether to include height threshold flagging (default: TRUE).
#' @param id_col Character, name of the column containing sample IDs (default: 'sample_id').
#' @param ignore_na Logical, whether to ignore NA values in QC RSD calculation (default: TRUE).
#'
#' @return The path to the generated zip file containing the process report.
#' @importFrom dplyr filter mutate case_when
#' @importFrom readr write_csv write_tsv
#' @export
#'
#' @author Yaoxiang Li
generate_process_report <- function(input_file,
                                    output_file = NULL,
                                    filter_blank = TRUE,
                                    blank_string = 'Blank|BLANK|blank',
                                    filter_nist = TRUE,
                                    nist_string = 'NIST|Nist|nist',
                                    imputation_threshold = 0.25,
                                    imputation_method = "half_min",
                                    qc_string = "QC",
                                    include_is_normalization = TRUE,
                                    include_qc_rlsc = TRUE,
                                    include_pqn = TRUE,
                                    include_qc_rsd = TRUE,
                                    include_snr_flag = TRUE,
                                    snr_threshold = 10,
                                    include_area_flag = TRUE,
                                    include_height_flag = TRUE,
                                    id_col = "sample_id",
                                    ignore_na = TRUE) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("Package 'zip' is required to generate the process report archive. Please install it.", call. = FALSE)
  }
  # Create a temporary directory
  temp_dir <- tempfile()

  # Define folder names based on whether IS normalization is applied
  if (include_is_normalization) {
    dir1 <- "01_Raw_Data_Before_IS_Normalization"
    dir2 <- "02_Internal_Standard_Normalized_Data"
    dir3 <- "03_Missing_Value_Imputed_Data"
    step_counter <- 3
    # Create initial directory vector
    dirs <- c(dir1, dir2, dir3)
  } else {
    dir1 <- "01_Raw_Data"
    dir2 <- "02_Missing_Value_Imputed_Data"
    step_counter <- 2
    dirs <- c(dir1, dir2)
  }

  # Append optional directories with sequential numbering
  if (include_qc_rlsc) {
    step_counter <- step_counter + 1
    dirs <- c(dirs, sprintf("%02d_QC_RLSC_Normalized_Data", step_counter))
  }

  if (include_pqn) {
    step_counter <- step_counter + 1
    dirs <- c(dirs, sprintf("%02d_PQN_Normalized_Data", step_counter))
  }

  if (include_qc_rsd) {
    step_counter <- step_counter + 1
    dirs <- c(dirs, sprintf("%02d_QC_RSD_Stats", step_counter))
  }

  if (include_snr_flag) {
    step_counter <- step_counter + 1
    dirs <- c(dirs, sprintf("%02d_Signal_to_Noise_Flag", step_counter))
  }

  if (include_area_flag) {
    step_counter <- step_counter + 1
    dirs <- c(dirs, sprintf("%02d_Area_Flag", step_counter))
  }

  if (include_height_flag) {
    step_counter <- step_counter + 1
    dirs <- c(dirs, sprintf("%02d_Height_Flag", step_counter))
  }

  # Create all directories
  dir_paths <- file.path(temp_dir, dirs)
  lapply(dir_paths, dir.create, showWarnings = TRUE, recursive = TRUE)

  # 0. Load the data from the provided file
  raw_data <- omicsTools::load_parse_sciex_txt(input_file)

  # 1. Get area and IS area data
  feature_data <- omicsTools::convert_mrm_data(raw_data, "area")
  is_data <- omicsTools::convert_mrm_data(raw_data, "istd_area")

  # Save raw files to the first folder (dir1)
  readr::write_tsv(feature_data, file.path(temp_dir, dir1, "area.txt"))
  readr::write_tsv(is_data, file.path(temp_dir, dir1, "IS_area.txt"))

  # 2. Perform internal standard normalization if requested
  if (include_is_normalization) {
    normalized_data <- omicsTools::internal_standard_normalize(feature_data, is_data)
    if (is.null(normalized_data)) return(NULL)
    readr::write_csv(normalized_data, file.path(temp_dir, dir2, "Internal_Standard_Normalized_Data.csv"))
  } else {
    normalized_data <- feature_data
  }

  # 3. Perform missing value imputation
  if (filter_blank) {
    normalized_data <- normalized_data |>
      dplyr::filter(!grepl(blank_string, sample_id))
  }

  if (filter_nist) {
    normalized_data <- normalized_data |>
      dplyr::filter(!grepl(nist_string, sample_id))
  }

  imputed_data <- omicsTools::handle_missing_values(normalized_data,
                                                    threshold = imputation_threshold,
                                                    imputation_method = imputation_method)

  # Write imputed data to the appropriate folder and filename
  imputed_dir <- if (include_is_normalization) { dir3 } else { dir2 }
  imputed_file_name <- if (include_is_normalization) {
    "Internal_Standard_Normalized_and_Imputed_Data.csv"
  } else {
    "Imputed_Data.csv"
  }
  readr::write_csv(imputed_data, file.path(temp_dir, imputed_dir, imputed_file_name))

  # 4. Optional QC-RLSC normalization
  if (include_qc_rlsc) {
    qc_rlsc_normalized_data <- omicsTools::qc_normalize(imputed_data, qc_label = qc_string)
    readr::write_csv(qc_rlsc_normalized_data,
                     file.path(temp_dir,
                               sprintf("%02d_QC_RLSC_Normalized_Data", if (include_is_normalization) {4} else {3}),
                               "QC_RLSC_Normalized_Data.csv"))
  }

  # 5. Optional PQN normalization
  if (include_pqn) {
    pqn_normalized_data <- omicsTools::pqn_normalize(imputed_data)
    readr::write_csv(pqn_normalized_data,
                     file.path(temp_dir,
                               sprintf("%02d_PQN_Normalized_Data", if (include_is_normalization) {
                                 if (include_qc_rlsc) 5 else 4
                               } else {
                                 if (include_qc_rlsc) 4 else 3
                               }),
                               "PQN_Normalized_Data.csv"))
  }

  # 6. Optional QC RSD step
  if (include_qc_rsd) {
    area_data <- omicsTools::convert_mrm_data(raw_data, "area")
    qc_stats_rsd <- omicsTools::calculate_qc_rsd(area_data,
                                                 qc_string = qc_string,
                                                 nist_string = nist_string,
                                                 id_col = id_col,
                                                 ignore_na = ignore_na)
    readr::write_csv(qc_stats_rsd[["Pooled_QC_RSD"]],
                     file.path(temp_dir,
                               sprintf("%02d_QC_RSD_Stats", if (include_is_normalization) {
                                 if (include_qc_rlsc && include_pqn) 6 else if (include_qc_rlsc || include_pqn) 5 else 4
                               } else {
                                 if (include_qc_rlsc && include_pqn) 5 else if (include_qc_rlsc || include_pqn) 4 else 3
                               }),
                               "Pooled_QC_RSD.csv"))
    readr::write_csv(qc_stats_rsd[["NIST_RSD"]],
                     file.path(temp_dir,
                               sprintf("%02d_QC_RSD_Stats", if (include_is_normalization) {
                                 if (include_qc_rlsc && include_pqn) 6 else if (include_qc_rlsc || include_pqn) 5 else 4
                               } else {
                                 if (include_qc_rlsc && include_pqn) 5 else if (include_qc_rlsc || include_pqn) 4 else 3
                               }),
                               "NIST_RSD.csv"))
  }

  # 7. Optional Signal-to-noise ratio flag
  if (include_snr_flag) {
    flagged_data <- raw_data %>%
      dplyr::mutate(snr_flag = dplyr::case_when(
        signal_noise > snr_threshold ~ TRUE,
        signal_noise <= snr_threshold ~ FALSE,
        is.na(signal_noise) ~ FALSE
      ))
    flagged_snr <- omicsTools::convert_mrm_data(flagged_data, "snr_flag")
    readr::write_csv(flagged_snr,
                     file.path(temp_dir,
                               sprintf("%02d_Signal_to_Noise_Flag", if (include_is_normalization) {
                                 if (include_qc_rlsc && include_pqn && include_qc_rsd) 7 else 6
                               } else {
                                 if (include_qc_rlsc && include_qc_rsd && include_pqn) 6 else 5
                               }),
                               "flagged_snr.csv"))
  }

  # 8. Optional FLAG: Area flagging
  if (include_area_flag) {
    flagged_area <- omicsTools::flag_underexpressed_features(feature_data,
                                                             sample_id_col = id_col,
                                                             feature_cols = names(feature_data)[-1])
    readr::write_csv(flagged_area,
                     file.path(temp_dir,
                               sprintf("%02d_Area_Flag", if (include_is_normalization) {
                                 if (include_qc_rlsc && include_pqn && include_qc_rsd && include_snr_flag) 8 else 7
                               } else {
                                 if (include_qc_rlsc && include_pqn && include_qc_rsd && include_snr_flag) 7 else 6
                               }),
                               "flagged_area.csv"))
  }

  # 9. Optional FLAG: Height flagging
  if (include_height_flag) {
    height_data <- omicsTools::convert_mrm_data(raw_data, "height")
    flagged_height <- omicsTools::flag_underexpressed_features(height_data,
                                                               sample_id_col = id_col,
                                                               feature_cols = names(height_data)[-1])
    readr::write_csv(flagged_height,
                     file.path(temp_dir,
                               sprintf("%02d_Height_Flag", if (include_is_normalization) {
                                 if (include_qc_rlsc && include_pqn && include_qc_rsd && include_snr_flag && include_area_flag) 9 else 8
                               } else {
                                 if (include_qc_rlsc && include_pqn && include_qc_rsd && include_snr_flag && include_area_flag) 8 else 7
                               }),
                               "flagged_height.csv"))
  }

  # -------------------------------
  # Revised Readme.txt generation using a separate counter
  # -------------------------------
  if (include_is_normalization) {
    readme_content <- sprintf("
+---%s
|       area.txt: Raw feature response for each feature before internal standard normalization
|       IS_area.txt: Raw internal standard response (used for IS normalization)
|
+---%s
|       Internal_Standard_Normalized_Data.csv: Internal standard normalized response for each feature
|
+---%s
|       Internal_Standard_Normalized_and_Imputed_Data.csv: Post internal standard normalization, data was imputed (threshold: %.2f, method: %s)",
                              dir1, dir2, dir3, imputation_threshold, imputation_method)
  } else {
    readme_content <- sprintf("
+---%s
|       area.txt: Raw feature response for each feature
|       IS_area.txt: Raw internal standard responses (if available)
|
+---%s
|       Imputed_Data.csv: Data after missing value imputation (threshold: %.2f, method: %s)",
                              dir1, dir2, imputation_threshold, imputation_method)
  }

  # Set the starting number for optional steps
  readme_step_counter <- if (include_is_normalization) { 4 } else { 3 }

  if (include_qc_rlsc) {
    readme_content <- paste(readme_content,
                            sprintf("+---%02d_QC_RLSC_Normalized_Data\n|       QC_RLSC_Normalized_Data.csv: QC-RLSC normalized data (QC string: %s)", readme_step_counter, qc_string),
                            sep = "\n")
    readme_step_counter <- readme_step_counter + 1
  }

  if (include_pqn) {
    readme_content <- paste(readme_content,
                            sprintf("+---%02d_PQN_Normalized_Data\n|       PQN_Normalized_Data.csv: PQN normalized data", readme_step_counter),
                            sep = "\n")
    readme_step_counter <- readme_step_counter + 1
  }

  if (include_qc_rsd) {
    readme_content <- paste(readme_content,
                            sprintf("+---%02d_QC_RSD_Stats\n|       Pooled_QC_RSD.csv: CV for pooled QC samples (QC string: %s, NIST strings: %s, ID: %s, Ignore NA: %s)\n|       NIST_RSD.csv: CV for NIST samples", readme_step_counter, qc_string, nist_string, id_col, ignore_na),
                            sep = "\n")
    readme_step_counter <- readme_step_counter + 1
  }

  if (include_snr_flag) {
    readme_content <- paste(readme_content,
                            sprintf("+---%02d_Signal_to_Noise_Flag\n|       flagged_snr.csv: Features flagged by signal-to-noise (threshold: %d)", readme_step_counter, snr_threshold),
                            sep = "\n")
    readme_step_counter <- readme_step_counter + 1
  }

  if (include_area_flag) {
    readme_content <- paste(readme_content,
                            sprintf("+---%02d_Area_Flag\n|       flagged_area.csv: Features flagged based on area threshold", readme_step_counter),
                            sep = "\n")
    readme_step_counter <- readme_step_counter + 1
  }

  if (include_height_flag) {
    readme_content <- paste(readme_content,
                            sprintf("+---%02d_Height_Flag\n|       flagged_height.csv: Features flagged based on height threshold", readme_step_counter),
                            sep = "\n")
    readme_step_counter <- readme_step_counter + 1
  }

  readme_content <- paste(readme_content, "\\---Raw_Data\n        Description of the raw data used in the analysis", sep = "\n")
  writeLines(readme_content, con = file.path(temp_dir, "Readme.txt"))

  # Create zip file
  if (is.null(output_file)) {
    current_dir <- getwd()
    zip_file <- file.path(current_dir, "Process_Report.zip")
  } else {
    zip_file <- output_file
  }

  # Author: Yaoxiang Li
  files_to_zip <- unlist(lapply(dirs, function(d) {
    list.files(file.path(temp_dir, d), recursive = TRUE, full.names = TRUE)
  }))
  # Author: Yaoxiang Li
  relative_paths <- unlist(lapply(dirs, function(d) {
    file.path(d, list.files(file.path(temp_dir, d), recursive = TRUE))
  }))

  files_to_zip <- c(files_to_zip, file.path(temp_dir, "Readme.txt"))
  relative_paths <- c(relative_paths, "Readme.txt")

  old_wd <- setwd(temp_dir)
  zip::zip(zipfile = zip_file, files = relative_paths)
  setwd(old_wd)

  # Return the path to the zip file
  return(zip_file)
}

#' Plot and Analyze Lipid Class Data
#'
#' This function loads a specified data file containing lipid measurements, calculates the summary of missing values and relative standard deviation (RSD) for each lipid class, and generates plots for both summaries. It also plots and saves figures showing the distribution of internal standards across samples.
#'
#' @param file_path The path to the lipid measurement data file (e.g., "area.txt").
#' @param output_xlsx Path to save the lipid classes with names as an Excel file. Default is "lipid_classes_with_names.xlsx".
#' @param missing_plot_path Path to save the missing data percentage plot. Default is "missing_values_plot.png".
#' @param rsd_plot_path Path to save the RSD percentage plot. Default is "rsd_values_plot.png".
#' @param is_plots_dir Directory to save internal standard distribution plots. Default is "is_plots".
#' @param blank_pattern Regex pattern to identify blank samples in the "Sample ID" column. Default is "Blank|blank".
#' @param pooled_pattern Regex pattern to identify pooled QC samples in the "Sample ID" column. Default is "Pooled QC|Pooled|pool|PQC".
#' @param nist_pattern Regex pattern to identify NIST QC samples in the "Sample ID" column. Default is "NIST Plasma|NIST|nist".
#' @return Plots and saves summary figures of missing percentages, RSD percentages, and internal standard distributions.
#' @import ggplot2 dplyr readr
#' @export
#' @author Yaoxiang Li
plot_lipid_data_summary <- function(file_path,
                                    output_xlsx = "lipid_classes_with_names.xlsx",
                                    missing_plot_path = "missing_values_plot.png",
                                    rsd_plot_path = "rsd_values_plot.png",
                                    is_plots_dir = "is_plots",
                                    blank_pattern = "Blank|Control|Neg",
                                    pooled_pattern = "Pooled QC|Pooled|Pool|PQ",
                                    nist_pattern = "NIST Plasma|NIST|nist") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for writing Excel output. Please install it.", call. = FALSE)
  }
  # Load the data
  data <- readr::read_tsv(file_path, na = "N/A")

  # Grouping logic for lipid classes with storage of lipid names by class
  # Author: Yaoxiang Li
  group_lipid_classes_with_names <- function(column_names) {
    lipid_class_storage <- list()

    for (col in column_names) {
      class_name <- if (grepl("\\.IS(_\\d+)?$", col)) {
        "IS"
      } else if (grepl("^TAG", col)) {
        "TAG"
      } else if (grepl("^FFA", col)) {
        "FFA"
      } else if (grepl("(-2)?CL\\d{2}:\\d-FA\\d{2}:\\d(_\\d+)?$", col)) {
        "CL"
      } else if (grepl("(-2)?CL\\d{2}:\\d{2}-FA\\d{2}:\\d(_\\d+)?$", col)) {
        "CL"
      } else if (grepl("^PA", col)) {
        "PA"
      } else if (grepl("^PC", col)) {
        "PC"
      } else if (grepl("^LPC", col)) {
        "LPC"
      } else if (grepl("^PS", col)) {
        "PS"
      } else if (grepl("^IS_", col)) {
        "IS"
      } else if (grepl("^PG", col)) {
        "PG"
      } else if (grepl("^PI", col)) {
        "PI"
      } else if (grepl("^(d)?LCER", col)) {
        "LCER"
      } else if (grepl("carnitine|_AC$", col, ignore.case = TRUE)) {
        "Acylcarnitine"
      } else if (grepl("Cholesterol", col, ignore.case = TRUE)) {
        "Cholesterol"
      } else {
        match <- regmatches(col, regexpr("^[A-Za-z]+", col))
        if (length(match) > 0) match else col
      }

      if (!is.null(class_name) && nzchar(class_name)) {
        if (is.null(lipid_class_storage[[class_name]])) {
          lipid_class_storage[[class_name]] <- c(col)
        } else {
          lipid_class_storage[[class_name]] <- c(lipid_class_storage[[class_name]], col)
        }
      }
    }
    return(lipid_class_storage)
  }

  # Apply the function
  lipid_classes_with_names <- group_lipid_classes_with_names(colnames(data)[!grepl("^Sample", colnames(data))])
  openxlsx::write.xlsx(lipid_classes_with_names, output_xlsx)

  # Calculate missing values percentage and RSD for each lipid class
  # Author: Yaoxiang Li
  missing_values_summary <- sapply(names(lipid_classes_with_names), function(class) {
    lipid_columns <- lipid_classes_with_names[[class]]
    if (length(lipid_columns) == 0) return(NA)
    total_values <- length(unlist(data[, lipid_columns, drop = FALSE]))
    missing_count <- sum(is.na(data[, lipid_columns, drop = FALSE]))
    (missing_count / total_values) * 100
  })

  # Author: Yaoxiang Li
  rsd_summary <- sapply(names(lipid_classes_with_names), function(class) {
    lipid_columns <- lipid_classes_with_names[[class]]
    if (length(lipid_columns) == 0) return(NA)
    lipid_data <- data[, lipid_columns, drop = FALSE]
    lipid_means <- colMeans(lipid_data, na.rm = TRUE)
    lipid_sd <- apply(lipid_data, 2, stats::sd, na.rm = TRUE)
    mean(rsd <- (lipid_sd / lipid_means) * 100, na.rm = TRUE)
  })

  # Create and display summary data frame
  missing_values_df <- data.frame(
    Lipid_Class = names(missing_values_summary),
    Missing_Percentage = missing_values_summary,
    RSD_Percentage = rsd_summary
  )
  print(missing_values_df)

  # Plot missing percentage and save
  ggplot2::ggplot(missing_values_df, aes(x = stats::reorder(Lipid_Class, -Missing_Percentage), y = Missing_Percentage)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Lipid Class") +
    ggplot2::ylab("Missing Percentage (%)") +
    ggplot2::ggtitle("Missing Data Percentage by Lipid Class") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  ggplot2::ggsave(missing_plot_path, width = 12, height = 6)

  # Plot RSD percentage and save
  ggplot2::ggplot(missing_values_df, aes(x = stats::reorder(Lipid_Class, -RSD_Percentage), y = RSD_Percentage)) +
    ggplot2::geom_bar(stat = "identity", fill = "darkorange") +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Lipid Class") +
    ggplot2::ylab("RSD Percentage (%)") +
    ggplot2::ggtitle("RSD Percentage by Lipid Class") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  ggplot2::ggsave(rsd_plot_path, width = 12, height = 6)

  # Internal standard distributions
  internal_standards <- lipid_classes_with_names[['IS']]
  dir.create(is_plots_dir, showWarnings = FALSE)

  data <- dplyr::mutate(data, Sample_Group = dplyr::case_when(
    grepl(blank_pattern, `Sample ID`, ignore.case = TRUE) ~ "Blank",
    grepl(pooled_pattern, `Sample ID`, ignore.case = TRUE) ~ "Pooled QC",
    grepl(nist_pattern, `Sample ID`, ignore.case = TRUE) ~ "NIST QC",
    TRUE ~ "Actual Sample"
  ))

  if ("Sample Name" %in% colnames(data)) {
    for (standard in internal_standards) {
      standard_with_backticks <- paste0("`", standard, "`")

      na_percentage <- round((sum(is.na(data[[standard]])) / nrow(data)) * 100, 2)
      rsd <- round((stats::sd(data[[standard]], na.rm = TRUE) / mean(data[[standard]], na.rm = TRUE)) * 100, 2)

      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "seq_along(`Sample Name`)", y = standard_with_backticks, color = "Sample_Group")) +
        ggplot2::geom_point(shape = 19, alpha = 0.7) +
        ggplot2::scale_color_manual(values = c("Blank" = "red", "Pooled QC" = "blue", "NIST QC" = "green", "Actual Sample" = "purple")) +
        ggplot2::theme_minimal() +
        ggplot2::ggtitle(paste("Scatter Plot of", standard, "across Acquisition Sequence\n",
                               "NA %:", na_percentage, "| RSD:", rsd, "%")) +
        ggplot2::xlab("Acquisition Sequence") +
        ggplot2::ylab("Peak Area") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), legend.position = "bottom") +
        ggplot2::geom_smooth(method = "loess", se = FALSE, color = "black")
      ggplot2::ggsave(filename = file.path(is_plots_dir, paste0(gsub("[^a-zA-Z0-9]", "_", standard), ".png")), plot = p, width = 10, height = 6)
    }
  } else {
    stop("The 'Sample Name' column is not found in the data.")
  }
}

#' Plot and Analyze Metabolomics Data Summary
#'
#' This function loads a specified data file containing metabolomics measurements, identifies internal standards based on a naming convention,
#' and generates plots for the distribution of internal standards across samples. It also saves the internal standard information in an Excel file.
#'
#' @param file_path The path to the metabolomics measurement data file (e.g., "area_TM.txt").
#' @param output_xlsx The path where the Excel file with internal standards will be saved (default is "internal_standards.xlsx").
#' @param is_plots_dir Directory where plots for each internal standard will be saved (default is "is_plots").
#' @param blank_pattern Pattern for identifying blank samples in `Sample ID` column (default is "Blank|BLANK|blank|Control|Negative Control|Neg Control|BLK|Blk").
#' @param pooled_pattern Pattern for identifying pooled QC samples in `Sample ID` column (default is "Pooled QC|Pooled_QC|Pooled|POOL|Pool|pool|QC Mix|Mix QC|Pool QC|PQ|PQC").
#' @param nist_pattern Pattern for identifying NIST QC samples in `Sample ID` column (default is "NIST Plasma|NIST_Plasma|NIST-QC|Reference Plasma|Plasma Ref|NIST_QC|NIST QC|NIST|nist").
#' @param other_special_pattern Pattern for identifying other special cases in `Sample ID` column (default is "Special Case|Extra Sample|NonStandard QC|Other QC").
#' @return Saves internal standard plots and an Excel file with the identified internal standards.
#' @import ggplot2 dplyr readr
#' @export
#' @author Yaoxiang Li
plot_met_data_summary <- function(file_path,
                                  output_xlsx = "internal_standards.xlsx",
                                  is_plots_dir = "is_plots",
                                  blank_pattern = "Blank|Control|Neg",
                                  pooled_pattern = "Pooled QC|Pooled|Pool|PQ",
                                  nist_pattern = "NIST Plasma|NIST|nist",
                                  other_special_pattern = "Special Case|Extra Sample|Other QC") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for writing Excel output. Please install it.", call. = FALSE)
  }

  # Load the data
  data <- readr::read_tsv(file_path, na = "N/A")

  # Detect internal standards based on naming convention
  # Author: Yaoxiang Li
  detect_internal_standards <- function(column_names) {
    internal_standards <- column_names[grepl("\\.(IS|_IS)$", column_names)]
    return(internal_standards)
  }

  # Apply the detection function
  internal_standards <- detect_internal_standards(colnames(data))

  # Save internal standards list to an Excel file
  openxlsx::write.xlsx(list("Internal_Standards" = internal_standards), output_xlsx)

  # Internal standard distributions
  dir.create(is_plots_dir, showWarnings = FALSE)

  # Add sample group classification
  data <- dplyr::mutate(data, Sample_Group = dplyr::case_when(
    grepl(blank_pattern, `Sample ID`, ignore.case = TRUE) ~ "Blank",
    grepl(pooled_pattern, `Sample ID`, ignore.case = TRUE) ~ "Pooled QC",
    grepl(nist_pattern, `Sample ID`, ignore.case = TRUE) ~ "NIST QC",
    grepl(other_special_pattern, `Sample ID`, ignore.case = TRUE) ~ "Other QC",
    TRUE ~ "Actual Sample"
  ))

  # Generate plots for internal standards across samples
  if ("Sample Name" %in% colnames(data)) {
    for (standard in internal_standards) {
      na_percentage <- round((sum(is.na(data[[standard]])) / nrow(data)) * 100, 2)
      rsd <- round((stats::sd(data[[standard]], na.rm = TRUE) / mean(data[[standard]], na.rm = TRUE)) * 100, 2)

      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = "seq_along(`Sample Name`)", y = paste0("`", standard, "`"), color = "Sample_Group")) +
        ggplot2::geom_point(shape = 19, alpha = 0.7) +
        ggplot2::scale_color_manual(values = c("Blank" = "red", "Pooled QC" = "blue", "NIST QC" = "green", "Other QC" = "orange", "Actual Sample" = "purple")) +
        ggplot2::theme_minimal() +
        ggplot2::ggtitle(paste("Scatter Plot of", standard, "across Acquisition Sequence\n",
                               "NA %:", na_percentage, "| RSD:", rsd, "%")) +
        ggplot2::xlab("Acquisition Sequence") +
        ggplot2::ylab("Peak Area") +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), legend.position = "bottom") +
        ggplot2::geom_smooth(method = "loess", se = FALSE, color = "black")
      ggplot2::ggsave(filename = file.path(is_plots_dir, paste0(gsub("[^a-zA-Z0-9]", "_", standard), ".png")), plot = p, width = 10, height = 6)
    }
  } else {
    stop("The 'Sample Name' column is not found in the data.")
  }
}
