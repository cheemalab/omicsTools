# Sample outlier quality control.

#' @noRd
#' @author Yaoxiang Li
mod_sample_outliers_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = "Sample outliers", width = 4, status = "primary",
      shiny::fileInput(ns("file"), "Samples-by-features table"),
      shiny::selectInput(ns("sample_id"), "Sample identifier column", choices = NULL),
      shiny::selectizeInput(ns("features"), "Numeric feature columns", choices = NULL, multiple = TRUE),
      shiny::numericInput(ns("threshold"), "Threshold (0 = automatic)", value = 0, min = 0),
      shiny::actionButton(ns("detect"), "Detect outliers"),
      shiny::downloadButton(ns("download"), "Download scores")
    ),
    bs4Dash::bs4Card(
      title = "Outlier scores", width = 8, status = "success",
      shiny::plotOutput(ns("plot"), height = 420),
      DT::DTOutput(ns("table"))
    )
  )
}

#' @noRd
#' @author Yaoxiang Li
mod_sample_outliers_server <- function(id) {
  # Author: Yaoxiang Li
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      shiny::req(input$file)
      as.data.frame(.load_gui_upload(input$file), check.names = FALSE)
    })
    shiny::observeEvent(data(), {
      columns <- names(data())
      numeric_columns <- columns[vapply(data(), is.numeric, logical(1))]
      shiny::updateSelectInput(session, "sample_id", choices = columns, selected = columns[[1L]])
      shiny::updateSelectizeInput(session, "features", choices = numeric_columns, selected = numeric_columns, server = TRUE)
    })
    result <- shiny::eventReactive(input$detect, {
      shiny::req(input$sample_id, input$features)
      values <- data()[input$features]
      rownames(values) <- make.unique(as.character(data()[[input$sample_id]]))
      threshold <- if (input$threshold > 0) input$threshold else NULL
      detect_sample_outliers(values, threshold)
    })
    output$table <- DT::renderDT(DT::datatable(result(), options = list(pageLength = 15)))
    output$plot <- shiny::renderPlot({
      value <- result()
      ggplot2::ggplot(value, ggplot2::aes(x = stats::reorder(.data$sample, .data$score), y = .data$score, fill = .data$is_outlier)) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = value$threshold[[1L]], linetype = "dashed") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Sample", y = "Outlier score", fill = "Outlier", caption = "Created by Yaoxiang Li")
    })
    output$download <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("sample-outliers-", Sys.Date(), ".csv"),
      # Author: Yaoxiang Li
      content = function(file) utils::write.csv(result(), file, row.names = FALSE)
    )
  })
}
