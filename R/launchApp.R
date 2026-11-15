#' Launch DIVIANA
#'
#' Launches DIVIANA in the default browser while keeping the R console available.
#'
#' @return Invisibly, a Shiny app handle. Use its `stop()` method to stop the app.
#'
#' @examples
#'
#' \dontrun{
#' launchApp()
#' }
#'
#' @export
launchApp = function() {
  suppressPackageStartupMessages({
    invisible(shiny::startApp(system.file("shiny", package = "diviana"), launch.browser = TRUE))
  })
}
