#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import dplyr
#' @importFrom utils read.csv write.csv
#' @importFrom shiny reactive
#' @noRd
#' @author Yaoxiang Li
app_server <- function(input, output, session) {
  mod_preprocess_server("preprocess")
  mod_qtrap_qc_server("qtrap_qc")
  mod_qtrap_pipeline_server("qtrap_pipeline")
  mod_targeted_calibration_server("targeted_calibration")
  mod_sample_outliers_server("sample_outliers")
  mod_ms1_annotation_server("ms1_annotation")

  mod_sample_metadata_server("sample_metadata")

  mod_vis_heatmap_server("vis_heatmap")
  mod_visual_explorer_server("vis_pca", "pca")
  mod_visual_explorer_server("vis_lcms", "lcms")
}
