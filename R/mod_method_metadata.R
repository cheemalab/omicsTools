#' method_metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_method_metadata_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4(
      "Method Metadata"
    ),
    fluidRow(
      bs4Dash::bs4Card(
        title = "Sample List Parameter Set Up",
        status = "success",
        width = 12,
        fluidRow(
          column(width = 3,
                 numericInput(inputId = ns("instrument_method_id"),
                              label = "Instrument Method ID",
                              value = 100),
                 numericInput(inputId = ns("analysis_method"),
                              label = "Analysis Method",
                              value = 100)),

          column(width = 3,
                 dateInput(inputId = ns("instrument_model"),
                           label = "Instrument Model",
                           value = Sys.Date()),
                 textInput(inputId = ns("instrument_software"),
                           label = "Instrument Software",
                           value = "SigSci"),
                 numericInput(inputId = ns("instrument_software_version"),
                              label = "Instrument Software Version",
                              value = 5)),

          column(width = 3,
                 textInput(inputId = ns("calibration_method"),
                           label = "Calibration Method",
                           value = "Georgetown"),
                 textInput(inputId = ns("instrument_method"),
                           label = "Instrument Method",
                           value = "LC-MRM-MS_Lipid"),
                 textInput(inputId = ns("data_processing_software"),
                           label = "Data Processing Software",
                           value = "skin disc")),
          column(width = 3,
                 textInput(inputId = ns("sequencing_technology"),
                           label = "Sequencing Technology",
                           value = "in vitro"),
                 textInput(inputId = ns("paired_single_end"),
                           label = "Paired Single End",
                           value = "/mnt/cifs/cheema"))
        ),
        bs4Dash::actionButton(
          inputId = ns("process_qc"),
          icon = icon("r-project"),
          class = "btn-success",
          "Generate"
        ),
        downloadButton(ns("download_data"), "Download Data")
      ),

      bs4Dash::bs4Card(
        title = "Sample List Preview",
        status = "success",
        width = 12,
        DT::DTOutput(
          outputId = ns("sample_list_output")
        )
      )
    )
  )
}

#' method_metadata Server Functions
#'
#' @noRd
mod_method_metadata_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
