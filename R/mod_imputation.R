#' imputation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imputation_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4(
      "Missing Value Imputation (Feature Wise)"
    ),
    fluidRow(
      bs4Dash::bs4Card(
        title = "Data Upload",
        status = "success",
        width = 6,
        fileInput(
          inputId = ns("qc_file"),
          label = "Choose CSV File",
          accept = c(".csv")
        ),
        numericInput(ns("percent"), "Missing Value Filtering Percent", value = 0.2, min = 0, max = 1),
        bs4Dash::actionButton(
          inputId = ns("process_qc"),
          icon = icon("r-project"),
          class = "btn-success",
          "Process"
        ),
        downloadButton(ns("download_data"), "Download Data")
      ),
      bs4Dash::bs4Card(
        title = "Data Visualization",
        status = "success",
        width = 6
      ),
      bs4Dash::bs4Card(
        title = "Data Preview",
        status = "success",
        width = 12,
        DT::DTOutput(
          outputId = ns("qc_input")
        )
      ),
      bs4Dash::bs4Card(
        title = "Normalized data",
        status = "success",
        width = 12,
        DT::DTOutput(
          outputId = ns("qc_output")
        )
      )
    )
  )
}

#' imputation Server Functions
#'
#' @noRd
mod_imputation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    qtrap_data <- reactive({
      req(input$qtrap_file)
      read.csv(input$qtrap_file$datapath)
    })
    output$qtrap_output <- DT::renderDT({
      DT::datatable(qtrap_data())
    })

    qc_data <- reactive({
      req(input$qc_file)
      readr::read_csv(input$qc_file$datapath)
    })

    processed_data <- reactiveValues(data = NULL)

    observeEvent(input$process_qc, {
      processed_data$data <- process_data(qc_data(), input$percent)
    })

    output[["qc_input"]] <- DT::renderDataTable(
      qc_data(),
      caption = 'Input Data',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = FALSE,
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
              list(extend = 'csv', filename = "Input Data"),
              list(extend = 'excel', filename = "Input Data")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    output[["qc_output"]] <- DT::renderDataTable(
      processed_data$data,
      caption = 'Cleaned Data',
      extensions = 'Buttons',
      selection = "multiple",
      rownames = FALSE,
      server = FALSE,
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
              list(extend = 'csv', filename = "Cleaned Data"),
              list(extend = 'excel', filename = "Cleaned Data")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    process_data <- function(data, percent = 0.2) {


      # data <- readr::read_csv("Wave 1 Internal Standard Normalized Data.csv")
      # data <- readr::read_csv("Wave 2 Internal Standard Normalized Data.csv")
      # data <- readr::read_csv("Wave 3 Internal Standard Normalized Data.csv")

      data <- data %>% dplyr::filter(!grepl('Blank', Sample))
      data <- data %>% dplyr::filter(!grepl('BLANK', Sample))
      data <- data %>% dplyr::filter(!grepl('NIST', Sample))


      peaks   <- as.matrix(data[, -1])


      na_features <- which(colMeans(is.na(peaks)) > percent)
      if (length(na_features)) {
        # remove features if NA > threshold rate
        peaks <- peaks[ ,-na_features]
        message(paste0(
          length(na_features),
          " features removed by percent of missing values > ",
          as.character(percent)
        ))
      } else {
        message('No features removed by missing values')
      }

      for (i in 1:ncol(peaks)) {
        v <- peaks[, i]
        if (any(is.na(v))) {
          v[which(is.na(v))] <- min(v[which(!is.na(v))][-1]) / 2
          peaks[, i] <- v
        }
      }
      dplyr::bind_cols(data[, 1], peaks)
    }

    output$download_data <- downloadHandler(
      filename = function() {
        paste("QC_Normalized_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(processed_data$data, file, row.names = FALSE)
      }
    )
  })
}

## To be copied in the UI
# mod_imputation_ui("imputation_1")

## To be copied in the server
# mod_imputation_server("imputation_1")
