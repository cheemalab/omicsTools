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

  mod_qc_rlsc_server("qc_rlsc")
  mod_imputation_server("imputation")
  mod_sample_sheet_server("sample_sheet")
  mod_method_metadata_server("method_metadata")

}


