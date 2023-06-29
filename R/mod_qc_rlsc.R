#' qc_rlsc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_qc_rlsc_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4(
      "QC Normalization"
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

        # column(width = 6, numericInput(ns("percent"), "QC-RSD Filtering Percent", value = 0.25, min = 0, max = 1)),
        textInput(ns("qc_name"), "QC Name", "Pooled QC"),

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

#' qc_rlsc Server Functions
#'
#' @noRd
mod_qc_rlsc_server <- function(id){
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
      processed_data$data <- process_data(qc_data())
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
      caption = 'Normalized Data',
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
              list(extend = 'csv', filename = "Normalized Data"),
              list(extend = 'excel', filename = "Normalized Data")
            ),
            text = 'Download'
          )
        )
      ),
      class = 'compact hover row-border nowrap stripe'
    )

    process_data <- function(data) {
      peaks   <- as.matrix(data[, -1])
      qc_idx  <- rep(0, nrow(peaks))
      qc_idx[grep("QC", data$Sample)] <- 1
      acq_seq <- 1:nrow(peaks)
      peaks_normalized <-
        matrix(ncol = ncol(peaks), nrow = nrow(peaks))
      colnames(peaks_normalized) <- colnames(peaks)
      for (i in 1:ncol(peaks)) {
        loess_fit <-
          stats::loess(peaks[which(qc_idx == 1), i] ~ acq_seq[which(qc_idx == 1)])
        interpolation <-
          stats::approx(x    = acq_seq[which(qc_idx == 1)],
                        y    = loess_fit$fitted,
                        xout = acq_seq)
        peaks_normalized[, i] <- peaks[, i] / interpolation$y
      }
      peaks_normalized <- tibble::as_tibble(peaks_normalized)
      dplyr::bind_cols(data[, 1], peaks_normalized)
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
# mod_qc_rlsc_ui("qc_rlsc_1")

## To be copied in the server
# mod_qc_rlsc_server("qc_rlsc_1")
