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
#'   ~`data_filename`, ~`sample_id`, ~polarity, ~`retention_time`, ~`Mass Info`, ~`component_name`,
#'   "Sample1", "ID1", "Positive", 1.95, "61.0 / 44.0", "Urea_pos",
#'   "Sample1", "ID1", "Positive", 8.34, "206.0 / 189.0", "Lipoamide_pos",
#'   "Sample2", "ID2", "Positive", 2.18, "339.1 / 110.0", "AICAR_pos",
#'   "Sample2", "ID2", "Positive", 1.76, "175.1 / 70.0", "Arginine_pos"
#' )
#' processed_data <- process_mrm_duplicates(mrm_data, "data_filename", "sample_id", "polarity", "retention_time", "Mass Info", "component_name")
#' print(processed_data)
#' }
#' @export
#' @import dplyr cli
#' @author Yaoxiang Li
process_mrm_duplicates <- function(mrm_data, sample_name_col = "data_filename", sample_id_col = "sample_id", polarity_col = "polarity", retention_time_col = "retention_time", mass_info_col = "Mass Info", component_name_col = "component_name") {
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
#' @importFrom metan transpose_df
#' @author Yaoxiang Li
convert_mrm_data <- function(data, response_col, sample_name_col = "data_filename", sample_id_col = "sample_id", component_name_col = "component_name") {
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
#'     "015_sample_006", "016_sample_003", "017_Blank", "018_sample_018"
#'   ),
#'   `2-Deoxyglucose-6-Phosphate_neg` = c(NA, 345423.96, NA, NA, 125889.800, 323818.25, 133188.88, 62745.31),
#'   `2-Oxoisoleucine_neg` = c(NA, 53004.06, 124669.80, NA, 23650.90, 118364.36, 62745.31, 73367.63),
#'   `3-(4-Hydroxyphenyl)propionate_neg` = c(NA, 53004.06, 124669.80, NA, 23650.90, 118364.36, 62745.31, 73367.63)
#' )
#' flagged_data <- flag_underexpressed_features(area_data, sample_id_col = "sample_id", feature_cols = names(area_data)[-1])
#' print(flagged_data)
#' }
#' @export
#' @import dplyr
#' @importFrom rlang .data
#' @author Yaoxiang Li
flag_underexpressed_features <- function(data, sample_id_col = "sample_id", feature_cols, threshold = 10) {
  # First blank sample
  first_blank <- dplyr::filter(data, grepl("Blank", !!dplyr::sym(sample_id_col))) |> dplyr::slice(1)

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
  result <- dplyr::select(flagged, !!dplyr::sym(sample_id_col), dplyr::all_of(feature_cols))
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
#' @import dplyr purrr tibble
#' @export
#'
#' @examples
#' tibble1 <- tibble(id = 1:3, A = c(TRUE, FALSE, NA), B = c(TRUE, TRUE, FALSE))
#' tibble2 <- tibble(id = 1:3, A = c(TRUE, TRUE, TRUE), B = c(FALSE, TRUE, TRUE))
#' tibble3 <- tibble(id = 1:3, A = c(TRUE, FALSE, TRUE), B = c(TRUE, TRUE, NA))
#' combine_logical_tibbles(tibble1, tibble2, tibble3, method = "intersection")
#' combine_logical_tibbles(tibble1, tibble2, tibble3, method = "union")
combine_logical_tibbles <- function(..., method = c("intersection", "union")) {
  method <- match.arg(method)
  tibbles <- list(...)

  # Check if all tibbles have the same dimensions, column names and first column values
  first_column_values <- purrr::map(tibbles, ~ .x[[1]])
  if (!all(sapply(first_column_values, identical, first_column_values[[1]]))) {
    stop("The first column values are not identical across all tibbles.")
  }

  column_names <- purrr::map(tibbles, colnames)
  if (!all(sapply(column_names, identical, column_names[[1]]))) {
    stop("The column names are not identical across all tibbles.")
  }

  dimensions <- purrr::map(tibbles, dim)
  if (!all(sapply(dimensions, identical, dimensions[[1]]))) {
    stop("The dimensions are not identical across all tibbles.")
  }

  # Combine tibbles based on the method
  combined <- purrr::reduce(tibbles, function(x, y) {
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
  })

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
#' by regular expression of `"(\\.IS$)|(_IS$)|(_d[0-9]{1,}_)|(\\(d[0-9]{1,}\\))"`.
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
load_parse_sciex_txt <- function(file_path, input_data = NULL, return_all_columns = TRUE,
                                 check_negative_values = TRUE, fix_istds = TRUE) {
  cli::cli_progress_bar("Reading data", total = 5)

  if (is.null(input_data)) {
    cli::cli_alert_info("Loading data from file")
    input_data <- readr::read_tsv(file_path, na = c("N/A", "NA", ""))
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
