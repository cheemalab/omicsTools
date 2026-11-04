# Shiny module for the complete SCIEX/Qtrap processing pipeline.

#' @noRd
#' @author Yaoxiang Li
mod_qtrap_pipeline_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = "SCIEX processing pipeline", width = 4, status = "primary",
      shiny::fileInput(ns("file"), "SCIEX exported TXT file", accept = c(".txt", ".tsv")),
      shiny::selectInput(
        ns("response"), "Response value",
        c("Peak area" = "area", "Internal-standard area" = "istd_area", "Area ratio" = "area_ratio", "Peak height" = "height", "Height ratio" = "height_ratio")
      ),
      shiny::checkboxInput(ns("duplicates"), "Detect duplicate MRM transitions", TRUE),
      shiny::actionButton(ns("process"), "Parse and convert"),
      shiny::downloadButton(ns("wide_download"), "Download wide CSV"),
      shiny::tags$hr(),
      shiny::checkboxInput(ns("is_norm"), "Include internal-standard normalization", TRUE),
      shiny::checkboxInput(ns("rlsc"), "Include QC-RLSC", TRUE),
      shiny::checkboxInput(ns("pqn"), "Include PQN", TRUE),
      shiny::checkboxInput(ns("qc_rsd"), "Include QC-RSD statistics", TRUE),
      shiny::checkboxInput(ns("snr"), "Include signal-to-noise flags", TRUE),
      shiny::checkboxInput(ns("area_flag"), "Include blank-area flags", TRUE),
      shiny::checkboxInput(ns("height_flag"), "Include blank-height flags", TRUE),
      shiny::downloadButton(ns("report"), "Build full process report"),
      shiny::tags$hr(),
      shiny::fileInput(ns("summary_file"), "Wide area table for data summary", accept = c(".txt", ".tsv")),
      shiny::selectInput(ns("summary_type"), "Summary workflow", c("Metabolomics" = "metabolomics", "Lipidomics" = "lipidomics")),
      shiny::downloadButton(ns("summary_bundle"), "Build summary bundle")
    ),
    bs4Dash::bs4Card(
      title = "SCIEX result", width = 8, status = "success",
      shiny::verbatimTextOutput(ns("summary")),
      shiny::tags$h5("Wide response matrix"),
      DT::DTOutput(ns("wide")),
      shiny::tags$hr(),
      shiny::tags$h5("Parsed transition table"),
      DT::DTOutput(ns("parsed"))
    )
  )
}

#' @noRd
#' @author Yaoxiang Li
mod_qtrap_pipeline_server <- function(id) {
  # Author: Yaoxiang Li
  shiny::moduleServer(id, function(input, output, session) {
    parsed <- shiny::eventReactive(input$process, {
      shiny::req(input$file)
      value <- load_parse_sciex_txt(input$file$datapath)
      value <- dplyr::mutate(
        value,
        mass_info = paste(.data$precursor_mz, .data$product_mz, sep = " -> ")
      )
      if (isTRUE(input$duplicates)) {
        original_sample_id <- value$sample_id
        value <- process_mrm_duplicates(value, mass_info_col = "mass_info")
        value$sample_id <- original_sample_id
      }
      value
    })
    wide <- shiny::reactive({
      shiny::req(input$response)
      convert_mrm_data(parsed(), response_col = input$response)
    })
    output$parsed <- DT::renderDT(DT::datatable(parsed(), options = list(pageLength = 10, scrollX = TRUE)))
    output$wide <- DT::renderDT(DT::datatable(wide(), options = list(pageLength = 12, scrollX = TRUE)))
    output$summary <- shiny::renderPrint({
      cat("Transitions:", nrow(parsed()), "\n")
      cat("Samples:", nrow(wide()), "\n")
      cat("Features:", max(0, ncol(wide()) - 1L), "\n")
      if ("MRM_Duplicate_Flag" %in% names(parsed())) {
        cat("Flagged duplicate rows:", sum(nzchar(parsed()$MRM_Duplicate_Flag), na.rm = TRUE), "\n")
      }
    })
    output$wide_download <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("sciex-", input$response, "-wide.csv"),
      # Author: Yaoxiang Li
      content = function(file) utils::write.csv(wide(), file, row.names = FALSE)
    )
    output$report <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("sciex-process-report-", Sys.Date(), ".zip"),
      # Author: Yaoxiang Li
      content = function(file) {
        generate_process_report(
          input_file = input$file$datapath,
          output_file = file,
          include_is_normalization = isTRUE(input$is_norm),
          include_qc_rlsc = isTRUE(input$rlsc),
          include_pqn = isTRUE(input$pqn),
          include_qc_rsd = isTRUE(input$qc_rsd),
          include_snr_flag = isTRUE(input$snr),
          include_area_flag = isTRUE(input$area_flag),
          include_height_flag = isTRUE(input$height_flag)
        )
      }
    )
    output$summary_bundle <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("sciex-", input$summary_type, "-summary-", Sys.Date(), ".zip"),
      # Author: Yaoxiang Li
      content = function(file) {
        shiny::req(input$summary_file)
        if (!requireNamespace("zip", quietly = TRUE)) stop("Install the optional `zip` package.")
        directory <- tempfile("omicsTools-sciex-summary-")
        dir.create(directory)
        if (identical(input$summary_type, "lipidomics")) {
          plot_lipid_data_summary(
            file_path = input$summary_file$datapath,
            output_xlsx = file.path(directory, "lipid_classes.xlsx"),
            missing_plot_path = file.path(directory, "missing_values.png"),
            rsd_plot_path = file.path(directory, "rsd_values.png"),
            is_plots_dir = file.path(directory, "internal_standards")
          )
        } else {
          plot_met_data_summary(
            file_path = input$summary_file$datapath,
            output_xlsx = file.path(directory, "internal_standards.xlsx"),
            is_plots_dir = file.path(directory, "internal_standards")
          )
        }
        zip::zipr(file, list.files(directory, recursive = TRUE, full.names = TRUE), root = directory)
      }
    )
  })
}
