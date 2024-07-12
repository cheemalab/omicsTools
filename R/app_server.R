#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import dplyr
#' @importFrom utils read.csv write.csv
#' @importFrom shiny reactive
#' @noRd
app_server <- function(input, output, session) {
  # res_auth <- shinymanager::secure_server(
  #   check_credentials = shinymanager::check_credentials(credentials)
  # )
  mod_qtrap_qc_server("qtrap_qc")
  mod_qc_rlsc_server("qc_rlsc")
  mod_imputation_server("imputation")

  mod_sample_sheet_server("sample_sheet")
  mod_method_metadata_server("method_metadata")

  mod_vis_heatmap_server("vis_heatmap")
  mod_vis_volcano_server("vis_volcano")
  mod_vis_venn_server("vis_venn")
  mod_vis_dot_server("vis_dot")
  mod_vis_circos_server("vis_circos")

}


