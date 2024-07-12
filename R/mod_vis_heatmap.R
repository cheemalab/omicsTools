#' UI Function for Heatmap Visualization
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_vis_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::bs4Card(
        title = "Load Heatmap Data", status = "success", width = 4,
        shinyWidgets::actionBttn(
          inputId = ns("run_example"),
          label = "Run Example",
          style = "fill",
          color = "warning",
          size = "sm"
        ),
        shiny::tags$hr(),
        shiny::tags$h5("Upload a gene expression file (support csv, txt, xls, xlsx)"),
        shinyWidgets::actionBttn(
          inputId = ns("view_example_file"),
          label = "View Example File",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::fileInput(ns("gene_expr_file"), NULL,
                         multiple = FALSE,
                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        shiny::tags$h5("Upload a sample metadata file (support csv, txt, xls, xlsx, optional)"),
        shinyWidgets::actionBttn(
          inputId = ns("view_metadata_file"),
          label = "View Metadata File",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::fileInput(ns("metadata_file"), NULL,
                         multiple = FALSE,
                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
        shinyWidgets::pickerInput(
          inputId = ns("color_scheme"),
          label = "Select Colors",
          choices = paste0("color", 1:6),
          multiple = FALSE,
          selected = "color1"
        ),
        shinyWidgets::awesomeRadio(
          inputId = ns("cluster_method"),
          label = "Select Cluster Methods",
          choices = c("complete", "average", "ward.D", "centroid"),
          selected = "complete",
          inline = TRUE,
          status = "warning"
        ),
        shiny::fluidRow(
          shiny::column(6, shinyWidgets::switchInput(
            inputId = ns("show_row_names"),
            label = "Feature Name",
            labelWidth = "100px",
            value = FALSE
          )),
          shiny::column(6, shinyWidgets::switchInput(
            inputId = ns("show_col_names"),
            label = "Sample Name",
            labelWidth = "100px",
            value = FALSE
          ))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("fig_width"), label = "Figure Width", value = 9)),
          shiny::column(4, shiny::numericInput(ns("fig_height"), label = "Figure Height", value = 6)),
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
        title = "Heatmap", status = "primary",
        shiny::plotOutput(ns("heatmap_plot"), height = 600) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1", type = 5, size = 0.5),
        width = 8
      )
    )
  )
}

#' Server Functions for Heatmap Visualization
#' @importFrom viridis viridis
#' @importFrom pheatmap pheatmap
#' @noRd
mod_vis_heatmap_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_modal <- function(failed = FALSE) {
      example <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_heatmap2.csv", header = TRUE, sep = ",", row.names = 1)
      shiny::modalDialog(
        shiny::renderTable(example[1:6, 1:6], rownames = TRUE),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Close")
        )
      )
    }

    shiny::observeEvent(input$view_example_file, {
      shiny::showModal(data_modal())
    })

    metadata_modal <- function(failed = FALSE) {
      example1 <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_heatmap_group_info.csv", header = TRUE, sep = ",")
      shiny::modalDialog(
        shiny::span("Note: the samples in metadata file must keep the same order with that of gene expression file. Support two or more groups."),
        shiny::tags$hr(),
        shiny::renderTable(example1[1:6, ], rownames = FALSE),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Close")
        )
      )
    }

    shiny::observeEvent(input$view_metadata_file, {
      shiny::showModal(metadata_modal())
    })

    output$heatmap_plot <- shiny::renderPlot({
      NULL
    })

    heatmap_example <- shiny::reactive({
      example <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_heatmap2.csv", header = TRUE, sep = ",", row.names = 1)
      example1 <- utils::read.table("https://github.com/YaoxiangLi/tidyview-data/raw/main/example_heatmap_group_info.csv", header = TRUE, sep = ",")

      data <- example
      data <- data[which(apply(data, 1, sd) > 0), ]
      data <- t(scale(t(data)))
      data[data > 1.5] <- 1.5
      data[data < -1.5] <- -1.5

      metadata <- example1
      annotation_col <- data.frame(Group = factor(metadata$group))
      rownames(annotation_col) <- colnames(data)

      pheatmap::pheatmap(
        data, scale = "none", annotation_col = annotation_col,
        color = viridis::viridis(1000), show_rownames = input$show_row_names,
        show_colnames = input$show_col_names, cluster_cols = TRUE,
        border_color = NA, clustering_method = input$cluster_method
      )
    })

    shiny::observeEvent(input$run_example, {
      output$heatmap_plot <- shiny::renderPlot({
        heatmap_example()
      })
    })

    load_heatmap_data <- shiny::reactive({
      shiny::req(input$gene_expr_file)

      data <- if (tools::file_ext(input$gene_expr_file$datapath) == "csv") {
        utils::read.table(input$gene_expr_file$datapath, header = TRUE, row.names = 1, sep = ",")
      } else if (tools::file_ext(input$gene_expr_file$datapath) == "txt") {
        utils::read.table(input$gene_expr_file$datapath, header = TRUE, row.names = 1, sep = "\t")
      } else if (tools::file_ext(input$gene_expr_file$datapath) == "xls") {
        readxl::read_xls(input$gene_expr_file$datapath) %>%
          as.data.frame() %>%
          { rownames(.) <- .[, 1]; .[, -1] }
      } else if (tools::file_ext(input$gene_expr_file$datapath) == "xlsx") {
        readxl::read_xlsx(input$gene_expr_file$datapath) %>%
          as.data.frame() %>%
          { rownames(.) <- .[, 1]; .[, -1] }
      }

      data <- data[which(apply(data, 1, sd) > 0), ]
      data <- t(scale(t(data)))
      data[data > 1.5] <- 1.5
      data[data < -1.5] <- -1.5

      data
    })

    heatmap_main <- shiny::reactive({
      data <- load_heatmap_data()
      metadata <- if (!is.null(input$metadata_file$datapath)) {
        if (tools::file_ext(input$metadata_file$datapath) == "csv") {
          utils::read.table(input$metadata_file$datapath, header = TRUE, row.names = 1, sep = ",")
        } else if (tools::file_ext(input$metadata_file$datapath) == "txt") {
          utils::read.table(input$metadata_file$datapath, header = TRUE, row.names = 1, sep = "\t")
        } else if (tools::file_ext(input$metadata_file$datapath) == "xls") {
          readxl::read_xls(input$metadata_file$datapath) %>%
            as.data.frame() %>%
            { rownames(.) <- .[, 1]; .[, -1] }
        } else if (tools::file_ext(input$metadata_file$datapath) == "xlsx") {
          readxl::read_xlsx(input$metadata_file$datapath) %>%
            as.data.frame() %>%
            { rownames(.) <- .[, 1]; .[, -1] }
        }
      } else {
        NULL
      }

      if (!is.null(metadata)) {
        annotation_col <- data.frame(Group = factor(metadata$group))
        rownames(annotation_col) <- colnames(data)
      } else {
        annotation_col <- NULL
      }

      pheatmap::pheatmap(
        data, scale = "none", annotation_col = annotation_col,
        color = viridis::viridis(1000), show_rownames = input$show_row_names,
        show_colnames = input$show_col_names, cluster_cols = TRUE,
        border_color = NA, clustering_method = input$cluster_method
      )
    })

    shiny::observeEvent(input$gene_expr_file, {
      output$heatmap_plot <- shiny::renderPlot({
        heatmap_main()
      })
    })

    output$download_pdf <- shiny::downloadHandler(
      filename = "heatmap.pdf",
      content = function(file) {
        grDevices::pdf(file, width = input$fig_width, height = input$fig_height)
        print(heatmap_main())
        grDevices::dev.off()
      }
    )
    output$download_png <- shiny::downloadHandler(
      filename = "heatmap.png",
      content = function(file) {
        grDevices::png(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(heatmap_main())
        grDevices::dev.off()
      }
    )
    output$download_jpeg <- shiny::downloadHandler(
      filename = "heatmap.jpeg",
      content = function(file) {
        grDevices::jpeg(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(heatmap_main())
        grDevices::dev.off()
      }
    )
    output$download_tiff <- shiny::downloadHandler(
      filename = "heatmap.tiff",
      content = function(file) {
        grDevices::tiff(file, width = input$fig_width, height = input$fig_height, units = "in", res = input$fig_resolution)
        print(heatmap_main())
        grDevices::dev.off()
      }
    )
  })
}
