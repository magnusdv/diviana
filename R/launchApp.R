#' Launch DIVIANA
#'
#' This launches the DIVIANA app.
#'
#' @return No return value, called for side effects.
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
