#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bs4Dash userMessages userMessage dashboardHeader dashboardBrand dashboardSidebar sidebarMenu menuItem dashboardBody tabItem bs4Card dropdownMenu taskItem notificationItem messageItem sidebarUserPanel sidebarHeader dashboardFooter tabItems
#' @noRd
#' @author Yaoxiang Li
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    .app_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      title = "omicsTools",
      fullscreen = TRUE,

# Header ------------------------------------------------------------------
      dashboardHeader(
        title = dashboardBrand(
          title = "omicsTools",
          color = "primary",
          href = "#",
          image = "www/logo.svg"
        ),
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        fixed = FALSE

      ),
# Sidebar -----------------------------------------------------------------
      sidebar = dashboardSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        sidebarUserPanel(
          name = "Welcome!"
        ),
        sidebarMenu(
          sidebarHeader("Data Processing"),
          menuItem(
            "Import & Preprocess",
            tabName = "preprocess",
            icon = icon("filter")
          )
        ),
        sidebarMenu(
          sidebarHeader("Quality Control Tools"),
          menuItem(
            "7500/5500 Qtrap",
            tabName = "qtrap_qc",
            icon = icon("chart-line")
          ),
          menuItem(
            "SCIEX Pipeline",
            tabName = "qtrap_pipeline",
            icon = icon("gears")
          ),
          menuItem(
            "Targeted Calibration",
            tabName = "targeted_calibration",
            icon = icon("ruler")
          ),
          menuItem(
            "Sample Outliers",
            tabName = "sample_outliers",
            icon = icon("triangle-exclamation")
          )
        ),
        sidebarMenu(
          sidebarHeader("Mass Spectrometry Tools"),
          menuItem(
            "MS1 Annotation",
            tabName = "ms1_annotation",
            icon = icon("magnifying-glass-chart")
          )
        ),
        sidebarMenu(
          sidebarHeader("Sample List Tools"),
          menuItem(
            "Sample Metadata",
            tabName = "sample_metadata",
            icon = icon("table-list")
          )
        ),
        sidebarMenu(
          sidebarHeader("Visualization Tools"),
          menuItem(
            "Heatmap",
            tabName = "vis_heatmap",
            icon = icon("braille")
          ),
          menuItem(
            "PCA",
            tabName = "vis_pca",
            icon = icon("star")
          ),
          menuItem(
            "LC-MS Features",
            tabName = "vis_lcms",
            icon = icon("chart-area")
          )
        )
      ),


# Footer ------------------------------------------------------------------
      footer = dashboardFooter(
        left = a(
          href = "https://github.com/YaoxiangLi/omicsTools",
          target = "_blank",
          rel = "noopener noreferrer",
          style = "font-size: 11px;",
          "Created by Yaoxiang Li"
        ),
        right = paste("Copyright", format(Sys.Date(), "%Y"))
      ),
# Body --------------------------------------------------------------------
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName = "preprocess",
            mod_preprocess_ui("preprocess")
          ),
          tabItem(
            tabName = "qtrap_qc",
            mod_qtrap_qc_ui("qtrap_qc")
          ),
          tabItem(
            tabName = "qtrap_pipeline",
            mod_qtrap_pipeline_ui("qtrap_pipeline")
          ),
          tabItem(
            tabName = "targeted_calibration",
            mod_targeted_calibration_ui("targeted_calibration")
          ),
          tabItem(
            tabName = "sample_outliers",
            mod_sample_outliers_ui("sample_outliers")
          ),
          tabItem(
            tabName = "ms1_annotation",
            mod_ms1_annotation_ui("ms1_annotation")
          ),
# Tab: Sample Metadata ----------------------------------------------------
          tabItem(
            tabName = "sample_metadata",
            mod_sample_metadata_ui("sample_metadata")
          ),
# Tab: Visualization ------------------------------------------------------
          tabItem(
            tabName = "vis_heatmap",
            mod_vis_heatmap_ui("vis_heatmap")
          ),
          tabItem(
            tabName = "vis_pca",
            mod_visual_explorer_ui("vis_pca", "Principal component analysis")
          ),
          tabItem(
            tabName = "vis_lcms",
            mod_visual_explorer_ui("vis_lcms", "LC-MS feature map")
          )
        )
      )
    )
  )
}

# Author: Yaoxiang Li
.app_resources <- function() {
  shiny::addResourcePath("www", system.file("app", "www", package = "omicsTools"))
  shiny::tags$head(shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "www/logo.svg"))
}
