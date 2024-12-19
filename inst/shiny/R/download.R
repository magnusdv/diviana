
downloadTables = function(results, filename) {
  nvic = nrow(results$LR)
  #results$PM$LR = safeFormat(results$PM$LR)
  #results$PM$GLR = safeFormat(results$PM$GLR)
  #results$LR[] = safeFormat(results$LR)
  names(results) = c("resultsAM", "resultsPM", "LR matrix", "Exclusions")

  hs = createStyle(textDecoration = "bold")
  wb = buildWorkbook(results, headerStyle = hs, colWidths = "auto",
                     rowNames = list(FALSE,FALSE,TRUE,TRUE))
  addStyle(wb, 3, cols = 1, rows = 1+seq_len(nvic), style = hs, stack = TRUE)
  addStyle(wb, 4, cols = 1, rows = 1+seq_len(nvic), style = hs, stack = TRUE)

  saveWorkbook(wb, file = filename, overwrite = TRUE)
  invisible(wb)
}


safeFormat = function(v, ...) {
  res = format(v, ...)
  res[res == "NA"] = ""
  res
}
