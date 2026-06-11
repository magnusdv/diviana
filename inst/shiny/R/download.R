
downloadTables = function(results, filename) {

  results$AM$LR  = safeFormat(results$AM$LR)
  results$AM$GLR = safeFormat(results$AM$GLR)

  results$PM$LR  = safeFormat(results$PM$LR)
  results$PM$GLR = safeFormat(results$PM$GLR)

  nvic = nrow(results$LR)

  results$LR[] = safeFormat(results$LR)

  # TODO: fix to handle > 1 joint families
  if(length(JT <- results$JT)) {
    JT = JT[[1]]
    if(nrow(JT) > 1e3)
      JT = JT[1:1e3, , drop = FALSE]
    results$JT = JT
  }

  #names(results) = c("resultsAM", "resultsPM", "LR matrix", "Exclusions", "Joint")

  nLR = ncol(results$LR)
  nEX = ncol(results$EX)

  hs = openxlsx::createStyle(textDecoration = "bold")
  hsc = openxlsx::createStyle(textDecoration = "bold", halign = "center")

  wb = openxlsx::buildWorkbook(results, headerStyle = hs,
                               rowNames = list(FALSE, FALSE, TRUE, TRUE, FALSE))

  openxlsx::setColWidths(wb, 1, cols = 1:ncol(results[[1]]), widths = "auto")
  openxlsx::setColWidths(wb, 2, cols = 1:ncol(results[[2]]), widths = "auto")
  openxlsx::setColWidths(wb, 3, cols = 1:(nLR + 1), widths = 12)
  openxlsx::setColWidths(wb, 4, cols = 1:(nEX + 1), widths = 6)

  # Center matrix headers
  for(j in 3:4) {
    openxlsx::addStyle(wb, j, cols = seq_len(nLR + 1), rows = 1, style = hsc, stack = TRUE)
  }

  openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
  invisible(wb)
}


safeFormat = function(v, ...) {
  res = format(v, ...)
  res[is.na(v) | trimws(res) == "NA"] = ""
  res
}
