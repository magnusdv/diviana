source_app = function(...) {
  path = testthat::test_path("..", "..", "inst", "shiny", "R")
  if(!dir.exists(path))
    path = system.file("shiny", "R", package = "diviana", mustWork = TRUE)
  env = parent.frame()
  invisible(lapply(file.path(path, c(...)), sys.source, envir = env))
}
