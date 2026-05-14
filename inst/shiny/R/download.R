
downloadTables = function(results, filename) {
  nvic = nrow(results$LR)
  results$AM$LR = safeFormat(results$PM$LR)
  results$AM$GLR = safeFormat(results$PM$GLR)
  results$PM$LR = safeFormat(results$PM$LR)
  results$PM$GLR = safeFormat(results$PM$GLR)
  results$LR[] = safeFormat(results$LR)
  results$JT = results$JT[[1]] # TODO: fix to handle > 1

  names(results) = c("resultsAM", "resultsPM", "LR matrix", "Exclusions", "Joint")

  hs = openxlsx::createStyle(textDecoration = "bold")
  wb = openxlsx::buildWorkbook(results, headerStyle = hs, colWidths = "auto",
                               rowNames = list(FALSE,FALSE,TRUE,TRUE, FALSE))
  openxlsx::addStyle(wb, 3, cols = 1, rows = 1+seq_len(nvic), style = hs, stack = TRUE)
  openxlsx::addStyle(wb, 4, cols = 1, rows = 1+seq_len(nvic), style = hs, stack = TRUE)

  openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
  invisible(wb)
}


safeFormat = function(v, ...) {
  res = format(v, ...)
  res[res == "NA"] = ""
  res
}
