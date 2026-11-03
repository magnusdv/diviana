app_file = function(...) {
  p = system.file("shiny", ..., package = "diviana")
  if(nzchar(p)) return(p)
  p = file.path(testthat::test_path("..", "..", "inst", "shiny"), ...)
  if(file.exists(p)) return(p)
  stop("Cannot find app file: ", file.path(...), call. = FALSE)
}

source_app = function(...) {
  for(f in c(...)) source(app_file("R", f), local = parent.frame())
}

app_for_test = function() {
  shiny::runApp(system.file("shiny", package = "diviana"))
}
