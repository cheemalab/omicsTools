#' Available omicsTools workflows
#'
#' Lists the workflows that are reachable from the
#' `omicsTools` Shiny application. The registry is used by automated tests so
#' workflows retain their GUI entry point.
#'
#' @return A tibble with package, workflow, Shiny tab, and supporting
#'   exported functions.
#' @export
#' @author Yaoxiang Li
omics_gui_capabilities <- function() {
  tibble::tribble(
    ~project, ~workflow, ~tab, ~functions,
    "omicsTools", "Qtrap internal-standard normalization and QC review", "qtrap_qc", "internal_standard_normalize,calculate_qc_rsd",
    "omicsTools", "SCIEX parsing, duplicate transitions, wide conversion, flags, reports, and summaries", "qtrap_pipeline", "load_parse_sciex_txt,detect_duplicates,process_mrm_duplicates,convert_mrm_data,flag_underexpressed_features,generate_process_report,plot_lipid_data_summary,plot_met_data_summary",
    "omicsTools", "Validate inputs, filter blanks, preprocess, compare groups, and export QC reports", "preprocess", "validate_omics_data,filter_blank_features,process_omics_data,compare_omics_groups,write_omics_report,export_omics_results",
    "omicsTools", "MS1 annotation", "ms1_annotation", "ms1_annotation",
    "omicsTools", "Sample metadata", "sample_metadata", "generate_sample_metadata",
    "omicsTools", "Multivariate outlier detection", "sample_outliers", "detect_sample_outliers",
    "omicsTools", "PCA visualization", "vis_pca", "plot_omics_pca",
    "omicsTools", "LC-MS feature visualization", "vis_lcms", "plot_lcms_features",
    "omicsTools", "Heatmap", "vis_heatmap", "plot_omics_heatmap",
    "omicsTools", "Linear and exponential targeted calibration", "targeted_calibration", "fit_targeted_calibration,predict_targeted_calibration,plot_targeted_calibration"
  )
}
