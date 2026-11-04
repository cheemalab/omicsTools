#' @noRd
#' @author Yaoxiang Li
mod_vis_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = "Heatmap input", width = 4, status = "primary",
      shiny::fileInput(ns("file"), "Samples-by-features table"),
      shiny::actionButton(ns("example"), "Load example"),
      shiny::selectInput(ns("id"), "Sample ID column", choices = NULL),
      shiny::selectizeInput(ns("features"), "Numeric features", choices = NULL, multiple = TRUE),
      shiny::checkboxInput(ns("scale"), "Standardize features", TRUE),
      shiny::downloadButton(ns("download"), "Download PNG")
    ),
    bs4Dash::bs4Card(title = "Heatmap", width = 8, status = "success",
                    shiny::plotOutput(ns("plot"), height = 600))
  )
}

#' @noRd
#' @author Yaoxiang Li
mod_vis_heatmap_server <- function(id) {
  # Author: Yaoxiang Li
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      if (!is.null(input$file)) return(.load_gui_upload(input$file))
      shiny::req(input$example > 0)
      load_omics_data(omics_example_file("examples/heatmap.csv"))
    })
    shiny::observeEvent(data(), {
      columns <- names(data())
      numeric <- columns[vapply(data(), is.numeric, logical(1))]
      shiny::updateSelectInput(session, "id", choices = columns, selected = columns[[1]])
      shiny::updateSelectizeInput(session, "features", choices = numeric, selected = numeric, server = TRUE)
    })
    plot <- shiny::reactive({
      shiny::req(input$id, input$features)
      plot_omics_heatmap(data(), input$id, input$features, isTRUE(input$scale))
    })
    output$plot <- shiny::renderPlot(plot())
    output$download <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() "omicsTools-heatmap.png",
      # Author: Yaoxiang Li
      content = function(file) ggplot2::ggsave(file, plot(), width = 9, height = 7, dpi = 150)
    )
  })
}
