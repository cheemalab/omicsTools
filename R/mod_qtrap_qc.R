#' qtrap_qc UI Function
#'
#' @description A shiny Module for 5500/7500 Qtrap Data Quality Control Tools.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column fileInput textInput downloadButton verbatimTextOutput tableOutput helpText hr
#' @importFrom ggplot2 ggplot geom_point geom_smooth aes theme_bw labs
#' @importFrom dplyr filter mutate select %>%
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom readr read_delim write_excel_csv
#' @importFrom tibble as_tibble
#' @importFrom bs4Dash bs4Card
#' @author Yaoxiang Li
mod_qtrap_qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        bs4Card(
          title = "Data Inputs",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          helpText("Input of the peak area .txt data file"),
          fileInput(ns("file_path"), label = "Qtrap Data Area File",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          helpText("Input of the internal standard peak area .txt data file"),
          fileInput(ns("is_area_path"), label = "Internal Standard Area File",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          textInput(ns("qc_name"), "Name for pooled QC used in data file", "Pooled QC"),
          textInput(ns("nist_name"), "Name for NIST plasma used in data file", "NIST"),
          hr(),
          shiny::div(
            class = "mb-3",
            helpText("Download the Pooled QC sample report"),
            downloadButton(ns("download_qc_report"), "Download")
          ),
          shiny::div(
            class = "mb-3",
            helpText("Download the NIST Plasma sample report"),
            downloadButton(ns("download_nist_report"), "Download")
          ),
          shiny::div(
            class = "mb-3",
            helpText("Download the internal-standard normalized dataset"),
            downloadButton(ns("download_all"), "Download")
          )
        )
      ),
      column(
        width = 8,
        fluidRow(
          column(4, DT::DTOutput(ns("names_tbl"))),
          column(8,
                 plotOutput(ns("x2"), height = 400),
                 plotOutput(ns("x3"), height = 400))
        ),
        h4(""),
        verbatimTextOutput(ns("summary")),
        tableOutput(ns("preContents"))
      )
    )
  )
}


#' qtrap_qc Server Functions
#'
#' @noRd
#' @author Yaoxiang Li
mod_qtrap_qc_server <- function(id) {
  # Author: Yaoxiang Li
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dataset_raw <- reactive({
      req(input$file_path)
      req(input$is_area_path)

      file_path <- input$file_path$datapath
      is_area_path <- input$is_area_path$datapath

      loaded_tibble <- readr::read_delim(
        file = file_path, delim = "\t", na = c("N/A", "Unknown"),
        show_col_types = FALSE
      ) %>%
        filter(!grepl("Blank|BLANK", `Sample ID`)) %>%
        mutate(Sample = paste0(`Sample Name`, "_", `Sample ID`), .before = "Sample Name") %>%
        select(-c("Sample Name", "Sample ID", "Sample Type"))

      return(loaded_tibble)
    })

    dataset_all <- reactive({
      req(input$file_path)
      req(input$is_area_path)

      file_path <- input$file_path$datapath
      is_area_path <- input$is_area_path$datapath

      loaded_tibble <- readr::read_delim(
        file = file_path, delim = "\t", na = c("N/A", "Unknown"),
        show_col_types = FALSE
      )
      is_tibble <- readr::read_delim(
        file = is_area_path, delim = "\t", na = c("N/A", "Unknown"),
        show_col_types = FALSE
      )

      loaded_tibble <- loaded_tibble %>%
        filter(!grepl("Blank|BLANK", `Sample ID`))
      is_tibble <- is_tibble %>%
        filter(!grepl("Blank|BLANK", `Sample ID`))

      normalized_values <-
        loaded_tibble[, 4:ncol(loaded_tibble)] /
        is_tibble[, 4:ncol(is_tibble)]

      normalized_tibble <-
        normalized_values %>%
        bind_cols(loaded_tibble[, 1:3], .) %>%
        mutate(Sample = paste0(`Sample Name`, "_", `Sample ID`), .before = "Sample Name") %>%
        select(-c("Sample Name", "Sample ID", "Sample Type"))

      return(normalized_tibble)
    })

    dataset_names <- reactive({
      dataset_all() %>%
        colnames() %>%
        as_tibble(.name_repair = "minimal")
    })

    dataset_qc <- reactive({
      req(input$qc_name)
      .summarize_qtrap_group(dataset_all(), input$qc_name)
    })

    dataset_nist <- reactive({
      req(input$nist_name)
      .summarize_qtrap_group(dataset_all(), input$nist_name)
    })

    output$names_tbl <- DT::renderDT(
      DT::datatable(dataset_names(),
                    caption = 'List of presented features',
                    selection = "single",
                    rownames = FALSE,
                    options = list(lengthMenu = c(20, 50, 100), pageLength = 20))
    )

    output$x2 <- renderPlot({
      qc_name   <- input$qc_name
      nist_name <- input$nist_name

      selected <- input$names_tbl_rows_selected
      if (is.null(selected)) selected <- 1L

      loaded_tibble <- dataset_raw() %>%
        pivot_longer(cols = -1) %>%
        pivot_wider(names_from = "Sample", values_from = "value")

      row_data <- loaded_tibble[selected, -1, drop=FALSE]
      col_names <- colnames(row_data)

      # Identify colors for QC/NIST
      colors <- rep("yellow", length(col_names))
      colors[grep(qc_name, col_names)] <- "blue"
      colors[grep(nist_name, col_names)] <- "red"

      plot_data <- data.frame(
        acq_seq = 1:length(col_names),
        conc = as.numeric(row_data[1,]),
        type = ifelse(grepl(qc_name, col_names), "QC",
                      ifelse(grepl(nist_name, col_names), "NIST", "Sample")),
        color_fill = colors
      )

      ggplot(plot_data, aes(x = acq_seq, y = conc)) +
        theme_bw() +
        geom_point(aes(col = type),
                   shape = 21,
                   fill = plot_data$color_fill,
                   color = "black",
                   size = 3) +
        geom_smooth(method = "loess", level = 0.5, linetype = "dashed") +
        labs(y = "Raw Peak Area",
             x = "Acquisition Order",
             title = "Unprocessed Peak Area for Pooled QC/NIST Plasma and Samples")
    })

    output$x3 <- renderPlot({
      qc_name   <- input$qc_name
      nist_name <- input$nist_name

      selected <- input$names_tbl_rows_selected
      if (is.null(selected)) selected <- 1L

      loaded_tibble <- dataset_all() %>%
        pivot_longer(cols = -1) %>%
        pivot_wider(names_from = "Sample", values_from = "value")

      row_data <- loaded_tibble[selected, -1, drop=FALSE]
      col_names <- colnames(row_data)

      colors <- rep("yellow", length(col_names))
      colors[grep(qc_name, col_names)] <- "blue"
      colors[grep(nist_name, col_names)] <- "red"

      plot_data <- data.frame(
        acq_seq = 1:length(col_names),
        conc = as.numeric(row_data[1,]),
        type = ifelse(grepl(qc_name, col_names), "QC",
                      ifelse(grepl(nist_name, col_names), "NIST", "Sample")),
        color_fill = colors
      )

      ggplot(plot_data, aes(x = acq_seq, y = conc)) +
        theme_bw() +
        geom_point(aes(col = type),
                   shape = 21,
                   fill = plot_data$color_fill,
                   color = "black",
                   size = 3) +
        geom_smooth(method = "loess", level = 0.5, linetype = "dashed") +
        labs(y = "Relative Concentration",
             x = "Acquisition Order",
             title = "Normalized Pooled QC/NIST Plasma using Internal Standards")
    })

    # Download section
    output$download_qc_report <- downloadHandler(
      # Author: Yaoxiang Li
      filename = function() {
        paste0("Internal_Standard_Normalized_QC_", Sys.Date(), ".csv")
      },
      # Author: Yaoxiang Li
      content = function(file) {
        write_excel_csv(dataset_qc(), file)
      }
    )

    output$download_nist_report <- downloadHandler(
      # Author: Yaoxiang Li
      filename = function() {
        paste0("Internal_Standard_Normalized_NIST_", Sys.Date(), ".csv")
      },
      # Author: Yaoxiang Li
      content = function(file) {
        write_excel_csv(dataset_nist(), file)
      }
    )

    output$download_all <- downloadHandler(
      # Author: Yaoxiang Li
      filename = function() {
        paste0("Internal_Standard_Normalized_Data_", Sys.Date(), ".csv")
      },
      # Author: Yaoxiang Li
      content = function(file) {
        write_excel_csv(dataset_all(), file)
      }
    )
  })
}

# Author: Yaoxiang Li
.summarize_qtrap_group <- function(data, pattern) {
  summary <- data %>%
    filter(grepl(pattern, Sample)) %>%
    pivot_longer(cols = -1) %>%
    pivot_wider(names_from = "Sample", values_from = "value")

  if (ncol(summary) < 2L) {
    stop("No samples matched the requested QC pattern.", call. = FALSE)
  }
  values <- as.matrix(summary[, -1, drop = FALSE])
  means <- rowMeans(values, na.rm = TRUE)
  deviations <- apply(values, 1, stats::sd, na.rm = TRUE)
  summary %>%
    mutate(
      Mean = means,
      `Standard Deviation` = deviations,
      `Coefficient of variation` = deviations / means
    )
}
