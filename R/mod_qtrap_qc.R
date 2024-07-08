#' qtrap_qc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_qtrap_qc_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' qtrap_qc Server Functions
#'
#' @noRd 
mod_qtrap_qc_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_qtrap_qc_ui("qtrap_qc_1")
    
## To be copied in the server
# mod_qtrap_qc_server("qtrap_qc_1")
