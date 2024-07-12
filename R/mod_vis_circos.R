#' UI Function for GO Chord Plot Visualization
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vis_circos_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::bs4Card(
        title = "Load Circos Plot Data", status = 'success', width = 4,
        shinyWidgets::actionBttn(
          inputId = ns("run_example"),
          label = "Run Example",
          style = "fill",
          color = "warning",
          size = "sm"
        ),
        shiny::tags$hr(),
        shiny::tags$h5("Upload a CSV file (also supports txt, xls, xlsx)"),
        shinyWidgets::actionBttn(
          inputId = ns("view_example_file"),
          label = "View Example File",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::fileInput(ns("go_data_file"), NULL,
                         multiple = FALSE,
                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        shinyWidgets::pickerInput(
          inputId = ns("color_scheme"),
          label = "Select Colors",
          choices = paste0("color", 1:2),
          multiple = FALSE,
          selected = "color1"
        ),
        shiny::numericInput(ns("fig_width"), label = "Figure Width", value = 12),
        shiny::numericInput(ns("fig_height"), label = "Figure Height", value = 12),
        shiny::numericInput(ns("fig_resolution"), label = "Figure Resolution", value = 72),
        shinyWidgets::dropdownButton(
          shinyWidgets::downloadBttn(
            outputId = ns("download_pdf"),
            label = "PDF Figure",
            style = "fill",
            color = "success",
            size = 'sm',
            block = TRUE
          ),
          shinyWidgets::downloadBttn(
            outputId = ns("download_png"),
            label = "PNG Figure",
            style = "fill",
            color = "success",
            size = 'sm',
            block = TRUE
          ),
          shinyWidgets::downloadBttn(
            outputId = ns("download_jpeg"),
            label = "JPEG Figure",
            style = "fill",
            color = "success",
            size = 'sm',
            block = TRUE
          ),
          shinyWidgets::downloadBttn(
            outputId = ns("download_tiff"),
            label = "TIFF Figure",
            style = "fill",
            color = "success",
            size = 'sm',
            block = TRUE
          ),
          circle = FALSE,
          label = "Download Figure",
          status = "success"
        )
      ),
      bs4Dash::bs4Card(
        title = "Circos Plot", status = 'primary',
        shiny::column(12, align = "center", shiny::plotOutput(ns("chord_plot"), width = 800, height = 800) %>%
                        shinycssloaders::withSpinner(color = "#0dc5c1", type = 5, size = 0.5)),
        width = 8
      )
    )
  )
}

#' Server Functions for GO Chord Plot Visualization
#'
#' @import ggplot2 RColorBrewer GOplot
#'
#' @noRd
mod_vis_circos_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_modal <- function(failed = FALSE) {
      example_data <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_go_chord.csv",
                                        header = TRUE, sep = ",", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      shiny::modalDialog(
        shiny::span('Each row represents a gene, each column represents a GO term, value 0 or 1 represents the relationship of gene and GO term. The last column represents logFC and is optional.'),
        shiny::tags$hr(),
        shiny::renderTable(example_data[1:10, c(1, 2, 3, 4, ncol(example_data))], rownames = FALSE),
        size = "l",
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Close")
        )
      )
    }

    shiny::observeEvent(input$view_example_file, {
      shiny::showModal(data_modal())
    })

    output$chord_plot <- shiny::renderPlot({
      NULL
    })

    vals <- shiny::reactiveValues()

    plot_chord <- shiny::reactive({
      data <- if (tools::file_ext(input$go_data_file$datapath) == "csv") {
        utils::read.table(input$go_data_file$datapath, header = TRUE, row.names = 1, sep = ",", comment.char = "", quote = "", check.names = FALSE, fill = TRUE)
      } else if (tools::file_ext(input$go_data_file$datapath) == "txt") {
        utils::read.table(input$go_data_file$datapath, header = TRUE, row.names = 1, sep = "\t", comment.char = "", quote = "", check.names = FALSE, fill = TRUE)
      } else if (tools::file_ext(input$go_data_file$datapath) == "xls") {
        readxl::read_xls(input$go_data_file$datapath) %>%
          as.data.frame() %>%
          { rownames(.) <- .[, 1]; .[, -1] }
      } else if (tools::file_ext(input$go_data_file$datapath) == "xlsx") {
        readxl::read_xlsx(input$go_data_file$datapath) %>%
          as.data.frame() %>%
          { rownames(.) <- .[, 1]; .[, -1] }
      }

      color_scheme <- switch(input$color_scheme,
                             "color1" = RColorBrewer::brewer.pal(length(names(data)) - 1, "Set2"),
                             "color2" = RColorBrewer::brewer.pal(length(names(data)) - 1, "Set3")
      )

      logfc <- if (names(data)[length(names(data))] == "logFC") 1 else 0

      plot <- GOplot::GOChord(data, ribbon.col = color_scheme, nlfc = logfc)
      vals$plot <- plot
      plot
    })

    plot_example <- shiny::reactive({
      example_data <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_go_chord.csv",
                                        header = TRUE, row.names = 1, sep = ",", comment.char = "", quote = "", check.names = FALSE, fill = TRUE)
      color_scheme <- switch(input$color_scheme,
                             "color1" = RColorBrewer::brewer.pal(length(names(example_data)) - 1, "Set2"),
                             "color2" = RColorBrewer::brewer.pal(length(names(example_data)) - 1, "Set3")
      )
      plot <- GOplot::GOChord(example_data, ribbon.col = color_scheme, nlfc = 1)
      vals$plot <- plot
      plot
    })

    shiny::observeEvent(input$run_example, {
      output$chord_plot <- shiny::renderPlot({
        plot_example()
      })
    })

    shiny::observeEvent(input$go_data_file, {
      output$chord_plot <- shiny::renderPlot({
        plot_chord()
      })
    })

    output$download_pdf <- shiny::downloadHandler(
      filename = "chord_plot.pdf",
      content = function(file) {
        grDevices::pdf(file, width = input$fig_width, height = input$fig_height)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_png <- shiny::downloadHandler(
      filename = "chord_plot.png",
      content = function(file) {
        grDevices::png(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_jpeg <- shiny::downloadHandler(
      filename = "chord_plot.jpeg",
      content = function(file) {
        grDevices::jpeg(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_tiff <- shiny::downloadHandler(
      filename = "chord_plot.tiff",
      content = function(file) {
        grDevices::tiff(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
  })
}
