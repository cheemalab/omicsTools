#' UI Function for Bubble Plot Visualization
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vis_dot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::bs4Card(
        title = "Load Dot plot / Raindrop plot Data", status = "success", width = 4,
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
        shiny::fileInput(ns("bubble_data_file"), NULL,
          multiple = FALSE,
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        shinyWidgets::pickerInput(
          inputId = ns("color_scheme"),
          label = "Select Colors",
          choices = paste0("color", 1:3),
          multiple = FALSE,
          selected = "color1"
        ),
        shiny::numericInput(ns("fig_width"), label = "Figure Width", value = 8),
        shiny::numericInput(ns("fig_height"), label = "Figure Height", value = 8),
        shiny::numericInput(ns("fig_resolution"), label = "Figure Resolution", value = 72),
        shinyWidgets::dropdownButton(
          shinyWidgets::downloadBttn(
            outputId = ns("download_pdf"),
            label = "PDF Figure",
            style = "fill",
            color = "success",
            size = "sm",
            block = TRUE
          ),
          shinyWidgets::downloadBttn(
            outputId = ns("download_png"),
            label = "PNG Figure",
            style = "fill",
            color = "success",
            size = "sm",
            block = TRUE
          ),
          shinyWidgets::downloadBttn(
            outputId = ns("download_jpeg"),
            label = "JPEG Figure",
            style = "fill",
            color = "success",
            size = "sm",
            block = TRUE
          ),
          shinyWidgets::downloadBttn(
            outputId = ns("download_tiff"),
            label = "TIFF Figure",
            style = "fill",
            color = "success",
            size = "sm",
            block = TRUE
          ),
          circle = FALSE,
          label = "Download Figure",
          status = "success"
        )
      ),
      bs4Dash::bs4Card(
        title = "Dot plot / Raindrop plot", status = "primary",
        shiny::column(12, align = "center", shiny::plotOutput(ns("bubble_plot"), width = 600, height = 600) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1", type = 5, size = 0.5)),
        width = 8
      )
    )
  )
}

#' Server Functions for Bubble Plot Visualization
#' @import ggplot2
#' @noRd
mod_vis_dot_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_modal <- function(failed = FALSE) {
      example_data <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_go_bubble.csv",
        header = TRUE, sep = ",", check.names = FALSE
      )
      names(example_data) <- c("GO Term", "FDR", "Gene Ratio", "Gene Number")
      shiny::modalDialog(
        shiny::span("GO or KEGG analysis results. First column represents GO term, second column represents FDR, third column represents gene ratio, fourth column represents gene number."),
        shiny::tags$hr(),
        shiny::renderTable(example_data, rownames = FALSE),
        easyClose = TRUE,
        size = "l",
        footer = shiny::tagList(
          shiny::modalButton("Close")
        )
      )
    }

    shiny::observeEvent(input$view_example_file, {
      shiny::showModal(data_modal())
    })

    output$bubble_plot <- shiny::renderPlot({
      NULL
    })

    vals <- shiny::reactiveValues()

    plot_bubble <- shiny::reactive({
      data <- if (tools::file_ext(input$bubble_data_file$datapath) == "csv") {
        utils::read.table(input$bubble_data_file$datapath, header = TRUE, sep = ",", check.names = FALSE, quote = "", comment.char = "")
      } else if (tools::file_ext(input$bubble_data_file$datapath) == "txt") {
        utils::read.table(input$bubble_data_file$datapath, header = TRUE, sep = "\t", check.names = FALSE, quote = "", comment.char = "")
      } else if (tools::file_ext(input$bubble_data_file$datapath) == "xls") {
        readxl::read_xls(input$bubble_data_file$datapath) %>%
          as.data.frame()
      } else if (tools::file_ext(input$bubble_data_file$datapath) == "xlsx") {
        readxl::read_xlsx(input$bubble_data_file$datapath) %>%
          as.data.frame()
      }

      names(data) <- c("GO Term", "FDR", "Gene Ratio", "Gene Number")

      space <- (max(data$`Gene Ratio`) - min(data$`Gene Ratio`)) / nrow(data)

      colors <- switch(input$color_scheme,
        "color1" = c(high = "blue", low = "red"),
        "color2" = c(high = "#21908CFF", low = "orange"),
        "color3" = c(high = "navy", low = "firebrick3")
      )

      plot <- ggplot2::ggplot(data, ggplot2::aes(x = `Gene Ratio`, y = `GO Term`, size = `Gene Number`, color = FDR)) +
        ggplot2::scale_color_gradient(high = colors["high"], low = colors["low"]) +
        ggplot2::geom_point(alpha = 0.8) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::ylab("") +
        ggplot2::scale_size(range = c(5, 10)) +
        ggplot2::xlim(min(data$`Gene Ratio`) - space, max(data$`Gene Ratio`) + space)

      vals$plot <- plot
      plot
    })

    plot_example <- shiny::reactive({
      example_data <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_go_bubble.csv",
        header = TRUE, sep = ",", check.names = FALSE
      )
      names(example_data) <- c("GO Term", "FDR", "Gene Ratio", "Gene Number")

      space <- (max(example_data$`Gene Ratio`) - min(example_data$`Gene Ratio`)) / nrow(example_data)

      colors <- switch(input$color_scheme,
        "color1" = c(high = "blue", low = "red"),
        "color2" = c(high = "#21908CFF", low = "orange"),
        "color3" = c(high = "navy", low = "firebrick3")
      )

      plot <- ggplot2::ggplot(example_data, ggplot2::aes(x = `Gene Ratio`, y = `GO Term`, size = `Gene Number`, color = FDR)) +
        ggplot2::scale_color_gradient(high = colors["high"], low = colors["low"]) +
        ggplot2::geom_point(alpha = 0.8) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::ylab("") +
        ggplot2::scale_size(range = c(5, 10)) +
        ggplot2::xlim(min(example_data$`Gene Ratio`) - space, max(example_data$`Gene Ratio`) + space)

      vals$plot <- plot
      plot
    })

    shiny::observeEvent(input$run_example, {
      output$bubble_plot <- shiny::renderPlot({
        plot_example()
      })
    })

    shiny::observeEvent(input$bubble_data_file, {
      output$bubble_plot <- shiny::renderPlot({
        plot_bubble()
      })
    })

    output$download_pdf <- shiny::downloadHandler(
      filename = "gobubble_plot.pdf",
      content = function(file) {
        grDevices::pdf(file, width = input$fig_width, height = input$fig_height)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_png <- shiny::downloadHandler(
      filename = "gobubble_plot.png",
      content = function(file) {
        grDevices::png(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_jpeg <- shiny::downloadHandler(
      filename = "gobubble_plot.jpeg",
      content = function(file) {
        grDevices::jpeg(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_tiff <- shiny::downloadHandler(
      filename = "gobubble_plot.tiff",
      content = function(file) {
        grDevices::tiff(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
  })
}
