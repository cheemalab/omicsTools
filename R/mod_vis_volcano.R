#' UI Function for Volcano Plot Visualization
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shinyWidgets
#' @importFrom shiny NS tagList
mod_vis_volcano_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::bs4Card(
        title = "Load Plot Data", width = 4, status = "success",
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
        shiny::fileInput(ns("data_file"), NULL,
          multiple = FALSE,
          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("p_value"), label = "P value or FDR", value = 0.05)),
          shiny::column(6, shiny::numericInput(ns("log2_fc"), label = "log2FC", value = 1))
        ),
        shinyWidgets::pickerInput(
          inputId = ns("color_scheme"),
          label = "Select Colors",
          choices = paste0("color", 1:3),
          multiple = FALSE,
          selected = "color1"
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("point_size"), label = "Point Size", value = 2)),
          shiny::column(6, shiny::numericInput(ns("label_size"), label = "Label Size", value = 4))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("fig_width"), label = "Figure Width", value = 8)),
          shiny::column(4, shiny::numericInput(ns("fig_height"), label = "Figure Height", value = 9)),
          shiny::column(4, shiny::numericInput(ns("fig_resolution"), label = "Figure Resolution", value = 72))
        ),
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
        title = "Volcano Plot", status = "primary",
        shiny::plotOutput(ns("volcano_plot"), height = 600) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1", type = 5, size = 0.5),
        width = 8
      )
    )
  )
}

#' Server Functions for Volcano Plot Visualization
#' @import ggplot2 ggpubr ggrepel tools
#' @noRd
mod_vis_volcano_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_modal <- function(failed = FALSE) {
      example_data <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_volcano.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      example_data <- example_data[1:10, ]
      shiny::modalDialog(
        shiny::span("First column represents gene ID, second column represents log2FC, third column represents p-value or FDR, and the fourth column represents genes to be labeled on the figure (optional)."),
        shiny::tags$hr(),
        shiny::renderTable(example_data, rownames = TRUE),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Close")
        )
      )
    }

    shiny::observeEvent(input$view_example_file, {
      shiny::showModal(data_modal())
    })

    output$volcano_plot <- shiny::renderPlot({
      NULL
    })

    vals <- shiny::reactiveValues()

    plot_volcano <- shiny::reactive({
      data <- if (tools::file_ext(input$data_file$datapath) == "csv") {
        utils::read.table(input$data_file$datapath, header = TRUE, row.names = 1, sep = ",", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      } else if (tools::file_ext(input$data_file$datapath) == "txt") {
        utils::read.table(input$data_file$datapath, header = TRUE, row.names = 1, sep = "\t", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      } else if (tools::file_ext(input$data_file$datapath) == "xls") {
        readxl::read_xls(input$data_file$datapath) %>%
          as.data.frame() %>%
          {
            rownames(.) <- .[, 1]
            .[, -1]
          }
      } else if (tools::file_ext(input$data_file$datapath) == "xlsx") {
        readxl::read_xlsx(input$data_file$datapath) %>%
          as.data.frame() %>%
          {
            rownames(.) <- .[, 1]
            .[, -1]
          }
      }

      data <- data[!is.na(data[, 2]), ]
      data$log10 <- -log10(data[, 2])
      data$class <- "none"
      data[data[, 2] <= input$p_value & data[, 1] >= input$log2_fc, ]$class <- "UP"
      data[data[, 2] <= input$p_value & data[, 1] <= -input$log2_fc, ]$class <- "DOWN"
      up_num <- nrow(data[data$class == "UP", ])
      down_num <- nrow(data[data$class == "DOWN", ])
      data$class <- as.factor(data$class)
      xval <- ceiling(max(abs(data[, 1])))

      colors <- switch(input$color_scheme,
        "color1" = c("UP" = "#E64B3599", "none" = "#4DBBD599", "DOWN" = "#00A08799"),
        "color2" = c("UP" = "#FC4E07", "none" = "#E7B800", "DOWN" = "#00AFBB"),
        "color3" = c("UP" = "#CD534C99", "none" = "#86868699", "DOWN" = "#0073C299")
      )

      plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = log2FC, y = log10, color = class)) +
        ggplot2::geom_point(data = data[data$class == "UP", ], ggplot2::aes(y = log10, color = "UP"), size = input$point_size) +
        ggplot2::geom_point(data = data[data$class == "none", ], ggplot2::aes(y = log10, color = "none"), size = input$point_size) +
        ggplot2::geom_point(data = data[data$class == "DOWN", ], ggplot2::aes(y = log10, color = "DOWN"), size = input$point_size) +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::ylab(paste0("-log10 ", names(data)[2])) +
        ggplot2::xlab("Log2FoldChange") +
        ggpubr::theme_pubr(base_size = 12, border = TRUE) +
        ggplot2::geom_hline(yintercept = -log10(input$p_value), linetype = "dashed", color = "black", size = 0.5) +
        ggplot2::geom_vline(xintercept = c(-input$log2_fc, input$log2_fc), linetype = "dashed", color = "black", size = 0.5) +
        ggrepel::geom_label_repel(
          size = input$label_size,
          fontface = "bold",
          color = "purple",
          box.padding = ggplot2::unit(1, "lines"),
          point.padding = ggplot2::unit(0.5, "lines"),
          segment.colour = "purple",
          segment.size = 0.5,
          segment.alpha = 0.5,
          max.overlaps = Inf
        ) +
        ggplot2::geom_point(data = data[data$label != "", ], color = "purple") +
        ggplot2::annotate(geom = "text", label = paste0("UP_Number: ", up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5) +
        ggplot2::annotate(geom = "text", label = paste0("DOWN_Number: ", down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
        ggplot2::labs(color = "class")

      vals$plot <- plot
      plot
    })

    plot_example <- shiny::reactive({
      example_data <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_volcano.csv", header = TRUE, row.names = 1, sep = ",", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      example_data <- example_data[!is.na(example_data[, 2]), ]
      example_data$log10 <- -log10(example_data[, 2])
      example_data$class <- "none"
      example_data[example_data[, 2] <= input$p_value & example_data[, 1] >= input$log2_fc, ]$class <- "UP"
      example_data[example_data[, 2] <= input$p_value & example_data[, 1] <= -input$log2_fc, ]$class <- "DOWN"
      up_num <- nrow(example_data[example_data$class == "UP", ])
      down_num <- nrow(example_data[example_data$class == "DOWN", ])
      example_data$class <- as.factor(example_data$class)
      xval <- ceiling(max(abs(example_data[, 1])))

      colors <- switch(input$color_scheme,
        "color1" = c("UP" = "#E64B3599", "none" = "#4DBBD599", "DOWN" = "#00A08799"),
        "color2" = c("UP" = "#FC4E07", "none" = "#E7B800", "DOWN" = "#00AFBB"),
        "color3" = c("UP" = "#CD534C99", "none" = "#86868699", "DOWN" = "#0073C299")
      )

      plot <- ggplot2::ggplot(data = example_data, ggplot2::aes(x = log2FC, y = log10, color = class, label = label)) +
        ggplot2::geom_point(data = example_data[example_data$class == "UP", ], ggplot2::aes(y = log10, color = "UP"), size = input$point_size) +
        ggplot2::geom_point(data = example_data[example_data$class == "none", ], ggplot2::aes(y = log10, color = "none"), size = input$point_size) +
        ggplot2::geom_point(data = example_data[example_data$class == "DOWN", ], ggplot2::aes(y = log10, color = "DOWN"), size = input$point_size) +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::ylab(paste0("-log10 ", names(example_data)[2])) +
        ggplot2::xlab("Log2FoldChange") +
        ggpubr::theme_pubr(base_size = 12, border = TRUE) +
        ggplot2::geom_hline(yintercept = -log10(input$p_value), linetype = "dashed", color = "black", size = 0.5) +
        ggplot2::geom_vline(xintercept = c(-input$log2_fc, input$log2_fc), linetype = "dashed", color = "black", size = 0.5) +
        ggrepel::geom_label_repel(
          size = input$label_size,
          fontface = "bold",
          color = "purple",
          box.padding = ggplot2::unit(1, "lines"),
          point.padding = ggplot2::unit(0.5, "lines"),
          segment.colour = "purple",
          segment.size = 0.5,
          segment.alpha = 0.5,
          max.overlaps = Inf
        ) +
        ggplot2::geom_point(data = example_data[example_data$label != "", ], color = "purple") +
        ggplot2::annotate(geom = "text", label = paste0("UP_Number: ", up_num), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5) +
        ggplot2::annotate(geom = "text", label = paste0("DOWN_Number: ", down_num), x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5) +
        ggplot2::labs(color = "class")

      vals$plot <- plot
      plot
    })

    shiny::observeEvent(input$run_example, {
      output$volcano_plot <- shiny::renderPlot({
        plot_example()
      })
    })

    shiny::observeEvent(input$data_file, {
      output$volcano_plot <- shiny::renderPlot({
        plot_volcano()
      })
    })

    output$download_pdf <- shiny::downloadHandler(
      filename = "volcano.pdf",
      content = function(file) {
        grDevices::pdf(file, width = input$fig_width, height = input$fig_height)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_png <- shiny::downloadHandler(
      filename = "volcano.png",
      content = function(file) {
        grDevices::png(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_jpeg <- shiny::downloadHandler(
      filename = "volcano.jpeg",
      content = function(file) {
        grDevices::jpeg(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_tiff <- shiny::downloadHandler(
      filename = "volcano.tiff",
      content = function(file) {
        grDevices::tiff(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
  })
}
