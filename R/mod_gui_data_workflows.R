# Shiny modules for the table-processing workflow.

#' @noRd
#' @author Yaoxiang Li
mod_preprocess_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = "Prepare and analyze", width = 4, status = "primary",
      shiny::fileInput(ns("file"), "Intensity table (rows are samples)"),
      shiny::fileInput(ns("sample_info"), "Sample metadata (optional)"),
      shiny::fileInput(ns("feature_info"), "Feature metadata (optional)"),
      shiny::helpText("Metadata use the same sample ID column. Feature metadata use feature_id, with optional mz and rt (seconds)."),
      shiny::selectInput(ns("id_col"), "Sample ID column", choices = NULL),
      shiny::selectizeInput(ns("metadata"), "Metadata columns", choices = NULL, multiple = TRUE),
      shiny::tags$details(
        shiny::tags$summary("Sample roles"),
        shiny::selectInput(ns("sample_type"), "Sample type column", choices = c("None" = "")),
        shiny::textInput(ns("study_label"), "Study sample label", "Sample"),
        shiny::textInput(ns("blank_label"), "Blank label", "Blank"),
        shiny::textInput(ns("qc_label"), "QC label", "QC"),
        shiny::selectInput(ns("batch"), "Batch column", choices = c("None" = "")),
        shiny::selectInput(ns("order"), "Injection order column", choices = c("Table order" = ""))
      ),
      shiny::checkboxInput(ns("blank_filter"), "Filter against blanks", FALSE),
      shiny::conditionalPanel(
        sprintf("input['%s'] === true", ns("blank_filter")),
        shiny::numericInput(ns("blank_ratio"), "Minimum study / blank ratio", 3, min = 1)
      ),
      shiny::sliderInput(ns("missing"), "Maximum feature missingness", min = 0, max = 1, value = 0.2, step = 0.05),
      shiny::selectInput(ns("imputation"), "Imputation",
                         c("None" = "none", "Half minimum" = "half_min", "Mean" = "mean", "Median" = "median", "Mode" = "mode"),
                         selected = "half_min"),
      shiny::checkboxInput(ns("filter_qc"), "Filter by QC RSD", FALSE),
      shiny::conditionalPanel(
        sprintf("input['%s'] === true", ns("filter_qc")),
        shiny::sliderInput(ns("qc_rsd"), "Maximum QC RSD", min = 0, max = 1, value = 0.2, step = 0.05)
      ),
      shiny::selectInput(ns("normalization"), "Normalization",
                         c("None" = "none", "QC-RLSC" = "qc_rlsc", "PQN" = "pqn")),
      shiny::tags$details(
        shiny::tags$summary("Compare groups"),
        shiny::selectInput(ns("group"), "Group column", choices = c("None" = "")),
        shiny::selectInput(ns("reference"), "Reference group", choices = NULL),
        shiny::selectInput(ns("comparison"), "Comparison group", choices = NULL),
        shiny::selectInput(ns("test"), "Test", c("Welch t-test" = "welch", "Wilcoxon" = "wilcoxon"))
      ),
      shiny::actionButton(ns("process"), "Run workflow"),
      shiny::tags$hr(),
      shiny::checkboxInput(ns("transpose"), "Transpose CSV output", FALSE),
      shiny::downloadButton(ns("download"), "Processed CSV"),
      shiny::downloadButton(ns("report"), "QC report"),
      shiny::downloadButton(ns("bundle"), "All results")
    ),
    bs4Dash::bs4Card(
      title = "Workflow results", width = 8, status = "success",
      shiny::verbatimTextOutput(ns("summary")),
      shiny::tags$h5("Processing steps"), DT::DTOutput(ns("steps")),
      shiny::tags$h5("Processed data"), DT::DTOutput(ns("processed")),
      shiny::tags$h5("Group comparison"), DT::DTOutput(ns("comparison")),
      shiny::plotOutput(ns("volcano"), height = 320),
      shiny::tags$details(shiny::tags$summary("Uploaded data"), DT::DTOutput(ns("raw")))
    )
  )
}

#' @noRd
#' @author Yaoxiang Li
mod_preprocess_server <- function(id) {
  # Author: Yaoxiang Li
  shiny::moduleServer(id, function(input, output, session) {
    raw_data <- shiny::reactive({
      shiny::req(input$file)
      as.data.frame(.load_gui_upload(input$file), check.names = FALSE)
    })
    sample_info <- shiny::reactive({
      if (is.null(input$sample_info)) return(NULL)
      as.data.frame(.load_gui_upload(input$sample_info), check.names = FALSE)
    })
    shiny::observeEvent(list(raw_data(), sample_info()), {
      columns <- unique(c(names(raw_data()), names(sample_info())))
      ids <- names(raw_data())
      id <- if ("sample_id" %in% ids) "sample_id" else ids[[1L]]
      nonnumeric <- names(raw_data())[!vapply(raw_data(), is.numeric, logical(1))]
      defaults <- unique(c(id, nonnumeric, names(sample_info())))
      shiny::updateSelectInput(session, "id_col", choices = ids, selected = id)
      shiny::updateSelectizeInput(session, "metadata", choices = columns, selected = defaults, server = TRUE)
      shiny::updateSelectInput(session, "sample_type", choices = c("None" = "", columns), selected = "")
      shiny::updateSelectInput(session, "batch", choices = c("None" = "", columns), selected = "")
      shiny::updateSelectInput(session, "order", choices = c("Table order" = "", columns), selected = "")
      shiny::updateSelectInput(session, "group", choices = c("None" = "", columns), selected = "")
    })
    shiny::observeEvent(list(input$group, raw_data(), sample_info()), {
      group <- input$group
      if (is.null(group) || !nzchar(group)) return()
      source <- if (group %in% names(sample_info())) sample_info() else raw_data()
      values <- unique(as.character(source[[group]]))
      values <- values[!is.na(values) & nzchar(values)]
      shiny::updateSelectInput(session, "reference", choices = values, selected = values[1L])
      shiny::updateSelectInput(session, "comparison", choices = values, selected = values[min(2L, length(values))])
    })
    # Author: Yaoxiang Li
    selected <- function(value) {
      if (is.null(value) || !nzchar(value)) NULL else value
    }
    analysis <- shiny::eventReactive(input$process, {
      feature_info <- if (is.null(input$feature_info)) NULL else {
        as.data.frame(.load_gui_upload(input$feature_info), check.names = FALSE)
      }
      process_omics_data(
        raw_data(), id_col = input$id_col, metadata_cols = input$metadata,
        sample_info = sample_info(), feature_info = feature_info,
        sample_type_col = selected(input$sample_type), batch_col = selected(input$batch),
        order_col = selected(input$order),
        blank_filter = isTRUE(input$blank_filter), blank_value = input$blank_label %||% "Blank",
        study_value = input$study_label %||% "Sample", ratio = input$blank_ratio %||% 3,
        missing = input$missing, imputation = input$imputation, normalization = input$normalization,
        qc_value = input$qc_label %||% "QC", qc_rsd = if (isTRUE(input$filter_qc)) input$qc_rsd else NULL,
        group_col = selected(input$group), reference = input$reference, comparison = input$comparison,
        method = input$test %||% "welch"
      )
    })
    processed_data <- shiny::reactive({
      value <- analysis()$data
      if (isTRUE(input$transpose)) value <- transpose_df(value)
      tibble::as_tibble(value)
    })
    output$raw <- DT::renderDT(DT::datatable(raw_data(), options = list(pageLength = 8, scrollX = TRUE)))
    output$steps <- DT::renderDT(DT::datatable(analysis()$steps, options = list(dom = "t")))
    output$processed <- DT::renderDT(DT::datatable(processed_data(), options = list(pageLength = 12, scrollX = TRUE)))
    output$comparison <- DT::renderDT({
      value <- analysis()$comparison
      if (!nrow(value)) value <- data.frame(message = "Choose a group column and run the workflow to compare groups.")
      DT::datatable(value, options = list(pageLength = 10, scrollX = TRUE))
    })
    output$volcano <- shiny::renderPlot({
      value <- analysis()$comparison
      shiny::req(nrow(value))
      value <- value[is.finite(value$log2FC) & is.finite(value$fdr), , drop = FALSE]
      shiny::req(nrow(value))
      value$significant <- value$fdr < 0.05
      ggplot2::ggplot(value, ggplot2::aes(x = .data$log2FC, y = -log10(pmax(.data$fdr, .Machine$double.xmin)), colour = .data$significant)) +
        ggplot2::geom_point() + ggplot2::theme_bw() +
        ggplot2::labs(x = "Log2 fold change", y = "-log10 adjusted p-value", colour = "FDR < 0.05", caption = "Created by Yaoxiang Li")
    })
    output$summary <- shiny::renderPrint({
      result <- analysis()
      cat(nrow(result$data), "samples and", length(result$feature_cols), "features retained\n")
      cat("Normalization:", result$settings$normalization, "\n")
    })
    output$download <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("omicsTools-processed-", Sys.Date(), ".csv"),
      # Author: Yaoxiang Li
      content = function(file) utils::write.csv(processed_data(), file, row.names = FALSE)
    )
    output$report <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("omicsTools-QC-", Sys.Date(), ".html"),
      # Author: Yaoxiang Li
      content = function(file) write_omics_report(analysis(), file)
    )
    output$bundle <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("omicsTools-results-", Sys.Date(), ".zip"),
      # Author: Yaoxiang Li
      content = function(file) {
        if (!requireNamespace("zip", quietly = TRUE)) stop("Install the zip package to export a results bundle.", call. = FALSE)
        directory <- tempfile("omicsTools-results-")
        on.exit(unlink(directory, recursive = TRUE), add = TRUE)
        export_omics_results(analysis(), directory)
        zip::zipr(file, list.files(directory, full.names = TRUE), root = directory)
      }
    )
  })
}

# Author: Yaoxiang Li
.load_gui_upload <- function(file_input) {
  shiny::req(file_input)
  load_omics_data(file_input$datapath, format = tools::file_ext(file_input$name))
}
