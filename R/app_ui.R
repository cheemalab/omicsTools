#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny fluidRow tagList fileInput downloadButton
#' @importFrom bs4Dash userMessages userMessage dashboardHeader dashboardBrand dashboardSidebar sidebarMenu menuItem dashboardBody tabItem bs4Card dropdownMenu taskItem notificationItem messageItem sidebarUserPanel sidebarHeader dashboardControlbar controlbarMenu controlbarItem dashboardFooter tabItems
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
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
          image = "https://cdn-icons-png.flaticon.com/512/3655/3655580.png",
        ),
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        controlbarIcon = icon("th"),
        fixed = FALSE

      ),
# Sidebar -----------------------------------------------------------------
      sidebar = dashboardSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        sidebarUserPanel(
          # image = "https://cdn-icons-png.flaticon.com/512/3655/3655580.png",
          name = "Welcome to Cheema Lab!"
        ),
        sidebarMenu(
          sidebarHeader("Quality Control Tools"),
          menuItem(
            "7500/5500 Qtrap",
            tabName = "qtrap_qc",
            icon = icon("chart-line")
          ),
          menuItem(
            "Missing Value",
            tabName = "imputation",
            icon = icon("chart-line")
          ),
          menuItem(
            "QC-RLSC",
            tabName = "qc_rlsc",
            icon = icon("chart-gantt")
          )
        ),
        sidebarMenu(
          sidebarHeader("Sample List Tools"),
          menuItem(
            "Sample Sheet",
            tabName = "sample_sheet",
            icon = icon("list-alt")
          ),
          menuItem(
            "Method Metadata",
            tabName = "method_metadata",
            icon = icon("table")
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
            "Volcano plot",
            tabName = "vis_volcano",
            icon = icon("vine")
          ),
          # menuItem(
          #   "PCA",
          #   tabName = "vis_pca",
          #   icon = icon("star")
          # ),
          menuItem(
            "Venn diagram",
            tabName = "vis_venn",
            icon = icon("cc-mastercard")
          ),
          # menuItem(
          #   "Manhattan",
          #   tabName = "vis_manhattan",
          #   icon = icon("area-chart")
          # ),
          # menuItem(
          #   "Raindrop",
          #   tabName = "vis_raindrop",
          #   icon = icon("cloud")
          # ),
          menuItem(
            "Rain plot",
            tabName = "vis_dot",
            icon = icon("circle")
          ),
          menuItem(
            "Circos plot",
            tabName = "vis_circos",
            icon = icon("gg-circle")
          )
        )
      ),


# Controlbar --------------------------------------------------------------
      controlbar = dashboardControlbar(
        skin = "light",
        pinned = FALSE,
        collapsed = TRUE,
        overlay = TRUE,
        controlbarMenu(
          id = "controlbarmenu",
          controlbarItem(
            title = "Item 1",
            sliderInput(
              inputId = "obs",
              label = "Number of observations:",
              min = 0,
              max = 1000,
              value = 500
            ),
            column(
              width = 12,
              align = "center",
              radioButtons(
                inputId = "dist",
                label = "Distribution type:",
                c(
                  "Normal" = "norm",
                  "Uniform" = "unif",
                  "Log-normal" = "lnorm",
                  "Exponential" = "exp"
                )
              )
            )
          ),
          controlbarItem("Item 2",
                         "Simple text")
        )
      ),
# Footer ------------------------------------------------------------------
      footer = dashboardFooter(
        left = a(
          href = "#",
          target = "_blank", "Unpublished tools created by Yaoxiang Li @ Cheema Lab. All rights reserved."
        ),
        right = "©2024"
      ),
# Body --------------------------------------------------------------------
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName = "qtrap_qc",
            mod_qtrap_qc_ui("qtrap_qc")
          ),
# Tab: Imputation ---------------------------------------------------------
          tabItem(
            tabName = "imputation",
            mod_imputation_ui("imputation")
          ),
          tabItem(
            tabName = "qc_rlsc",
            mod_qc_rlsc_ui("qc_rlsc")
          ),
# Tab: Sample List Generator ----------------------------------------------
          tabItem(
            tabName = "sample_sheet",
            mod_sample_sheet_ui("sample_sheet")
          ),
          tabItem(
            tabName = "method_metadata",
            mod_method_metadata_ui("method_metadata")
          ),
# Tab: Visualization ------------------------------------------------------
          tabItem(
            tabName = "vis_heatmap",
            mod_vis_heatmap_ui("vis_heatmap")
          ),
          tabItem(
            tabName = "vis_volcano",
            mod_vis_volcano_ui("vis_volcano")
          ),
          tabItem(
            tabName = "vis_venn",
            mod_vis_venn_ui("vis_venn")
          ),
          tabItem(
            tabName = "vis_dot",
            mod_vis_dot_ui("vis_dot")
          ),
          tabItem(
            tabName = "vis_circos",
            mod_vis_circos_ui("vis_circos")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "omicsTools"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
