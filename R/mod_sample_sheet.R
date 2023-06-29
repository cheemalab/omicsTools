#' sample_sheet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sample_sheet_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4(
      "Sample Sheet"
    ),
    fluidRow(
      bs4Dash::bs4Card(
        title = "Sample List Parameter Set Up",
        status = "success",
        width = 12,
        fluidRow(
          column(width = 3,
                 numericInput(inputId = ns("collect_sample_id"),
                              label = "Collect Sample ID",
                              value = 100),
                 numericInput(inputId = ns("analytical_sample_id"),
                              label = "Analytical Sample ID",
                              value = 100),
                 numericInput(inputId = ns("local_sample_id"),
                              label = "Local Sample ID",
                              value = 100),
                 textInput(inputId = ns("analytical_sample_type"),
                              label = "Analytical Sample Type",
                              value = "Sample"),
                 textInput(inputId = ns("sample_details"),
                              label = "Sample Details",
                              value = "")),
          column(width = 3,
                 dateInput(inputId = ns("preparation_date"),
                              label = "Preparation Data",
                              value = Sys.Date()),
                 textInput(inputId = ns("preparation_site"),
                              label = "Preparation Site",
                              value = "SigSci"),
                 numericInput(inputId = ns("sample_mass"),
                              label = "Sample Mass",
                              value = 5),
                 numericInput(inputId = ns("n_discs"),
                              label = "N Discs",
                              value = 4),
                 dateInput(inputId = ns("analysis_date"),
                              label = "Analysis Date",
                              value = Sys.Date())),

          column(width = 3,
                 textInput(inputId = ns("analysis_site"),
                              label = "Analysis Site",
                              value = "Georgetown"),
                 textInput(inputId = ns("analysis_method"),
                              label = "Analysis Method",
                              value = "LC-MRM-MS_Lipid"),
                 textInput(inputId = ns("instrument_method_id"),
                              label = "Instrument Method ID",
                              value = "7500-QTRAP"),
                 textInput(inputId = ns("sample_model"),
                              label = "Sample Model",
                              value = "skin tissue model"),
                 textInput(inputId = ns("sample_type"),
                              label = "Sample Type",
                              value = "skin disc")),
          column(width = 3,
                 textInput(inputId = ns("test_environment"),
                              label = "Test Environment",
                              value = "in vitro"),
                 numericInput(inputId = ns("analysis_replicate"),
                              label = "Analysis Replicate",
                              value = 3),
                 textInput(inputId = ns("data_filename"),
                              label = "Data File Name",
                              value = "Example_Data_File_Name"),
                 numericInput(inputId = ns("data_file_idx"),
                              label = "Data File Index",
                              value = 100),
                 textInput(inputId = ns("data_filepath"),
                              label = "Data File Path",
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

#' sample_sheet Server Functions
#'
#' @noRd
mod_sample_sheet_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sample_list_data <- reactive({
      tibble::tibble(
        collect_sample_id = character(),
        analytical_sample_id = character(),
        local_sample_id = character(),
        analytical_sample_type = character(),
        sample_details = character(),
        preparation_date = character(),
        preparation_site = character(),
        sample_mass = numeric(),
        n_discs = integer(),
        analysis_date = character(),
        analysis_site = character(),
        analysis_method = character(),
        instrument_method_id = character(),
        sample_model = character(),
        sample_type = character(),
        test_environment = character(),
        analysis_replicate = integer(),
        data_filename = character(),
        data_filepath = character(),
        data_file_idx = integer()
      )
    })



    processed_data <- reactiveValues(data = NULL)

    observeEvent(input$process_qc, {
      processed_data$data <- process_data(sample_list_data())
    })

    process_data <- function(data) {
      n <- input[["collect_sample_id"]]
      data[1:input[["collect_sample_id"]], ] <- NA
      data$collect_sample_id <- 1:input[["collect_sample_id"]]
      data$analytical_sample_id <- 1:input[["analytical_sample_id"]]
      data$local_sample_id <- 1:input[["local_sample_id"]]
      data$analytical_sample_type <- rep(input[["analytical_sample_type"]], n)
      data$sample_details <- rep(input[["sample_details"]], n)
      data$preparation_date <- rep(input[["preparation_date"]], n)
      data$preparation_site <- rep(input[["preparation_site"]], n)
      data$sample_mass <- rep(input[["sample_mass"]], n)
      data$n_discs <- rep(input[["n_discs"]], n)
      data$analysis_date <- rep(input[["analysis_date"]], n)
      data$analysis_site <- rep(input[["analysis_site"]], n)
      data$analysis_method <- rep(input[["analysis_method"]], n)
      data$instrument_method_id <- rep(input[["instrument_method_id"]], n)
      data$sample_model <- rep(input[["sample_model"]], n)
      data$sample_type <- rep(input[["sample_type"]], n)
      data$test_environment <- rep(input[["test_environment"]], n)
      data$analysis_replicate <- rep(input[["analysis_replicate"]], n)
      data$data_filename <- paste0(rep(input[["data_filename"]], n), "_", 1:input[["data_file_idx"]])
      data$data_file_idx <- 1:input[["data_file_idx"]]
      data$data_filepath <- rep(input[["data_filepath"]], n)
      data
    }

    output[["sample_list_output"]] <- DT::renderDataTable(
      processed_data$data,
      caption = 'Generated List of Samples',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = FALSE,
      editable = "column",
      options = list(
        autoWidth = FALSE,
        scrollX = TRUE,
        lengthMenu = list(c(5, 20, 50, -1), c('default', '20', '50', 'All')),
        pageLength = 5,
        dom = 'Blfrtip',
        buttons = list(
          'copy',
          list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "Generated List of Samples"),
              list(extend = 'excel', filename = "Generated List of Samples")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )
  })
}

## To be copied in the UI
# mod_sample_sheet_ui("sample_sheet_1")

## To be copied in the server
# mod_sample_sheet_server("sample_sheet_1")
