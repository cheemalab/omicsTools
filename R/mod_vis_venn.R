#' UI Function for Venn Diagram Visualization
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shinyWidgets
#' @importFrom shiny NS tagList
mod_vis_venn_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::bs4Card(
        title = "Load Venn Diagram Data", status = 'success', width = 4,
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
        shiny::fileInput(ns("venn_data_file"), NULL,
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
        title = "Venn Diagram", status = 'primary',
        shiny::plotOutput(ns("venn_plot"), height = 600) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1", type = 5, size = 0.5),
        width = 8
      )
    )
  )
}

#' Server Functions for Venn Diagram Visualization
#' @importFrom ggvenn ggvenn
#' @noRd
mod_vis_venn_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_modal <- function(failed = FALSE) {
      example_data <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_venn.csv",
                                        header = TRUE, sep = ",", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      example_data[is.na(example_data)] <- ""
      shiny::modalDialog(
        shiny::span('Supports up to four groups.'),
        shiny::tags$hr(),
        shiny::renderTable(example_data, rownames = FALSE),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Close")
        )
      )
    }

    shiny::observeEvent(input$view_example_file, {
      shiny::showModal(data_modal())
    })

    output$venn_plot <- shiny::renderPlot({
      NULL
    })

    vals <- shiny::reactiveValues()

    plot_venn <- shiny::reactive({
      data <- if (tools::file_ext(input$venn_data_file$datapath) == "csv") {
        utils::read.table(input$venn_data_file$datapath, header = TRUE, sep = ",", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      } else if (tools::file_ext(input$venn_data_file$datapath) == "txt") {
        utils::read.table(input$venn_data_file$datapath, header = TRUE, sep = "\t", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      } else if (tools::file_ext(input$venn_data_file$datapath) == "xls") {
        readxl::read_xls(input$venn_data_file$datapath) %>%
          as.data.frame()
      } else if (tools::file_ext(input$venn_data_file$datapath) == "xlsx") {
        readxl::read_xlsx(input$venn_data_file$datapath) %>%
          as.data.frame()
      }

      group_names <- names(data)
      merge_gene_list <- list()

      if (is.numeric(data[, 1])) {
        for (i in seq_along(group_names)) {
          merge_gene_list[[group_names[i]]] <- data[!is.na(data[, i]), i]
        }
      } else if (is.character(data[, 1])) {
        for (i in seq_along(group_names)) {
          merge_gene_list[[group_names[i]]] <- data[data[, i] != "", i]
        }
      }

      colors <- switch(input$color_scheme,
                       "color1" = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
                       "color2" = c("blue", "yellow", "green", "red"),
                       "color3" = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3")
      )

      plot <- ggvenn::ggvenn(
        merge_gene_list,
        fill_color = colors,
        stroke_size = 0.5,
        set_name_size = 4
      )

      vals$plot <- plot
      plot
    })

    plot_example <- shiny::reactive({
      example_data <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_venn.csv",
                                        header = TRUE, sep = ",", check.names = FALSE, quote = "", comment.char = "", fill = TRUE)
      group_names <- names(example_data)
      merge_gene_list <- list()

      for (i in seq_along(group_names)) {
        merge_gene_list[[group_names[i]]] <- example_data[!is.na(example_data[, i]), i]
      }

      colors <- switch(input$color_scheme,
                       "color1" = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF"),
                       "color2" = c("blue", "yellow", "green", "red"),
                       "color3" = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3")
      )

      plot <- ggvenn::ggvenn(
        merge_gene_list,
        fill_color = colors,
        stroke_size = 0.5,
        set_name_size = 4
      )

      vals$plot <- plot
      plot
    })

    shiny::observeEvent(input$run_example, {
      output$venn_plot <- shiny::renderPlot({
        plot_example()
      })
    })

    shiny::observeEvent(input$venn_data_file, {
      output$venn_plot <- shiny::renderPlot({
        plot_venn()
      })
    })

    output$download_pdf <- shiny::downloadHandler(
      filename = "venn.pdf",
      content = function(file) {
        grDevices::pdf(file, width = input$fig_width, height = input$fig_height)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_png <- shiny::downloadHandler(
      filename = "venn.png",
      content = function(file) {
        grDevices::png(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_jpeg <- shiny::downloadHandler(
      filename = "venn.jpeg",
      content = function(file) {
        grDevices::jpeg(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
    output$download_tiff <- shiny::downloadHandler(
      filename = "venn.tiff",
      content = function(file) {
        grDevices::tiff(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(vals$plot)
        grDevices::dev.off()
      }
    )
  })
}
