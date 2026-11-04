#' Run the omicsTools application
#' @inheritParams shiny::shinyApp
#' @return A Shiny application.
#' @author Yaoxiang Li
#' @export
# Author: Yaoxiang Li
run_app <- function(onStart = NULL, options = list(), enableBookmarking = NULL, uiPattern = "/") {
  shiny::shinyApp(ui = app_ui, server = app_server, onStart = onStart,
                 options = options, enableBookmarking = enableBookmarking, uiPattern = uiPattern)
}
