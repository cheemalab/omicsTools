# Consolidated Shiny modules migrated from the retired standalone projects.

#' @noRd
#' @author Yaoxiang Li
mod_targeted_calibration_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = "Calibration input", width = 4, status = "primary",
      shiny::fileInput(ns("file"), "CSV, TSV, or Excel table"),
      shiny::selectInput(ns("model"), "Model", c("Linear" = "linear", "Exponential" = "exponential")),
      shiny::selectInput(ns("concentration"), "Concentration column", choices = NULL),
      shiny::selectInput(ns("response"), "Response column", choices = NULL),
      shiny::actionButton(ns("fit"), "Fit calibration"),
      shiny::downloadButton(ns("download"), "Download predictions")
    ),
    bs4Dash::bs4Card(
      title = "Calibration result", width = 8, status = "success",
      shiny::plotOutput(ns("plot")),
      shiny::verbatimTextOutput(ns("summary")),
      DT::DTOutput(ns("table"))
    )
  )
}

#' @noRd
#' @author Yaoxiang Li
mod_targeted_calibration_server <- function(id) {
  # Author: Yaoxiang Li
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      shiny::req(input$file)
      load_omics_data(input$file$datapath, tools::file_ext(input$file$name))
    })
    shiny::observeEvent(data(), {
      columns <- names(data())
      shiny::updateSelectInput(session, "concentration", choices = columns, selected = columns[[1L]])
      shiny::updateSelectInput(session, "response", choices = columns, selected = columns[[min(2L, length(columns))]])
    })
    fit <- shiny::eventReactive(input$fit, {
      shiny::req(input$concentration, input$response)
      fit_targeted_calibration(data(), input$concentration, input$response, model = input$model)
    })
    output$plot <- shiny::renderPlot(plot_targeted_calibration(fit()))
    output$summary <- shiny::renderPrint(suppressWarnings(summary(fit()$fit)))
    predictions <- shiny::reactive({
      predict_targeted_calibration(fit(), data())
    })
    output$table <- DT::renderDT(DT::datatable(predictions(), options = list(pageLength = 10)))
    output$download <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("targeted-calibration-", Sys.Date(), ".csv"),
      # Author: Yaoxiang Li
      content = function(file) utils::write.csv(predictions(), file, row.names = FALSE)
    )
  })
}

#' @noRd
#' @author Yaoxiang Li
mod_ms1_annotation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = "MS1 annotation", width = 4, status = "primary",
      shiny::textAreaInput(ns("mz"), "m/z values (comma, space, or line separated)"),
      shiny::numericInput(ns("tolerance"), "Tolerance", value = 10, min = 0),
      shiny::selectInput(ns("unit"), "Unit", c("ppm" = 2, "Da" = 1)),
      shiny::selectInput(ns("database"), "Database", c("METLIN" = "metlin", "HMDB" = "hmdb")),
      shiny::selectInput(ns("mode"), "Ion mode", c("Positive" = "pos", "Negative" = "neg", "Neutral" = "neu")),
      shiny::actionButton(ns("annotate"), "Annotate"),
      shiny::downloadButton(ns("download"), "Download results")
    ),
    bs4Dash::bs4Card(
      title = "Matches", width = 8, status = "success",
      DT::DTOutput(ns("matches"))
    )
  )
}

#' @noRd
#' @author Yaoxiang Li
mod_ms1_annotation_server <- function(id) {
  # Author: Yaoxiang Li
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::eventReactive(input$annotate, {
      values <- strsplit(input$mz %||% "", "[,;[:space:]]+", perl = TRUE)[[1L]]
      ms1_annotation(
        values,
        masstole = input$tolerance,
        toleUnit = as.integer(input$unit),
        annotaDB = input$database,
        ionMode = input$mode
      )
    })
    output$matches <- DT::renderDT(DT::datatable(result(), options = list(pageLength = 15)))
    output$download <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("ms1-annotations-", Sys.Date(), ".csv"),
      # Author: Yaoxiang Li
      content = function(file) utils::write.csv(result(), file, row.names = FALSE)
    )
  })
}

#' @noRd
#' @author Yaoxiang Li
mod_sample_metadata_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = "Sample metadata", width = 4, status = "primary",
      shiny::textAreaInput(ns("samples"), "Sample/file names (one per line)"),
      shiny::textInput(ns("separator"), "Separator regular expression", "[_-]"),
      shiny::textInput(ns("fields"), "Field names (comma separated)", "group,subject,replicate"),
      shiny::actionButton(ns("generate"), "Generate metadata"),
      shiny::downloadButton(ns("download"), "Download metadata")
    ),
    bs4Dash::bs4Card(title = "Metadata preview", width = 8, status = "success", DT::DTOutput(ns("table")))
  )
}

#' @noRd
#' @author Yaoxiang Li
mod_sample_metadata_server <- function(id) {
  # Author: Yaoxiang Li
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::eventReactive(input$generate, {
      samples <- trimws(strsplit(input$samples %||% "", "[\r\n]+", perl = TRUE)[[1L]])
      fields <- trimws(strsplit(input$fields %||% "", ",", fixed = TRUE)[[1L]])
      generate_sample_metadata(samples[nzchar(samples)], input$separator, fields[nzchar(fields)])
    })
    output$table <- DT::renderDT(DT::datatable(result(), options = list(pageLength = 15)))
    output$download <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0("sample-metadata-", Sys.Date(), ".csv"),
      # Author: Yaoxiang Li
      content = function(file) utils::write.csv(result(), file, row.names = FALSE)
    )
  })
}

#' @noRd
#' @author Yaoxiang Li
mod_visual_explorer_ui <- function(id, title = "Visualization") {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = paste(title, "input"), width = 4, status = "primary",
      shiny::fileInput(ns("file"), "CSV, TSV, or Excel table"),
      shiny::uiOutput(ns("columns")),
      shiny::downloadButton(ns("download"), "Download plot")
    ),
    bs4Dash::bs4Card(
      title = title, width = 8, status = "success",
      shiny::plotOutput(ns("plot"), height = 600)
    )
  )
}

#' @noRd
#' @author Yaoxiang Li
mod_visual_explorer_server <- function(id, type) {
  # Author: Yaoxiang Li
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      shiny::req(input$file)
      as.data.frame(load_omics_data(input$file$datapath, tools::file_ext(input$file$name)), check.names = FALSE)
    })
    output$columns <- shiny::renderUI({
      columns <- names(data())
      ns <- session$ns
      switch(
        type,
        pca = shiny::tagList(
          shiny::selectInput(ns("group"), "Optional group column", c("None" = "", columns)),
          shiny::selectizeInput(ns("features"), "Numeric feature columns", columns, multiple = TRUE)
        ),
        lcms = shiny::tagList(
          shiny::selectInput(ns("x"), "Retention-time column", columns),
          shiny::selectInput(ns("y"), "m/z column", columns),
          shiny::selectInput(ns("colour"), "Optional mode column", c("None" = "", columns))
        )

      )
    })
    plot <- shiny::reactive({
      value <- data()
      switch(
        type,
        pca = {
          feature_names <- input$features
          if (!length(feature_names)) feature_names <- names(value)[vapply(value, is.numeric, logical(1))]
          group <- if (nzchar(input$group %||% "")) value[[input$group]] else NULL
          plot_omics_pca(value[feature_names], group = group, labels = rownames(value))
        },
        lcms = plot_lcms_features(value, input$x, input$y, input$colour %||% "")

      )
    })
    output$plot <- shiny::renderPlot(plot())
    output$download <- shiny::downloadHandler(
      # Author: Yaoxiang Li
      filename = function() paste0(type, "-", Sys.Date(), ".png"),
      # Author: Yaoxiang Li
      content = function(file) ggplot2::ggsave(file, plot = plot(), width = 9, height = 7, dpi = 150)
    )
  })
}
