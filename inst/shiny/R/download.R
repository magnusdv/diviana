
downloadTables = function(results, filename, log = NULL) {

  results$AM$LR  = safeFormat(results$AM$LR)
  results$AM$GLR = safeFormat(results$AM$GLR)

  results$PM$LR  = safeFormat(results$PM$LR)
  results$PM$GLR = safeFormat(results$PM$GLR)

  results$LR[] = safeFormat(results$LR)

  # Export each joint family on a separate sheet
  joint = lapply(results$JT, \(x) head(x, 1000))
  fam = names(joint)
  if(is.null(fam)) fam = seq_along(joint)

  # Keep family labels within Excel's sheet name restrictions
  nms = sprintf("Joint %s", fam)
  nms = chartr("[]:*?/\\", "_______", nms)
  nms = sub("'+$", "", substr(nms, 1, 31))
  if(anyDuplicated(tolower(nms)))
    nms = sub("'+$", "", substr(sprintf("%d %s", seq_along(nms), nms), 1, 31))
  names(joint) = nms
  results = c(results[c("AM", "PM", "LR", "EX")], joint)

  nLR = ncol(results$LR)
  nEX = ncol(results$EX)

  hs = openxlsx::createStyle(textDecoration = "bold")
  hsc = openxlsx::createStyle(textDecoration = "bold", halign = "center")

  wb = openxlsx::buildWorkbook(results, headerStyle = hs,
                               rowNames = c(FALSE, FALSE, TRUE, TRUE, rep(FALSE, length(joint))))

  openxlsx::setColWidths(wb, 1, cols = seq_len(ncol(results$AM)), widths = "auto")
  openxlsx::setColWidths(wb, 2, cols = seq_len(ncol(results$PM)), widths = "auto")
  openxlsx::setColWidths(wb, 3, cols = seq_len(nLR + 1), widths = 12)
  openxlsx::setColWidths(wb, 4, cols = seq_len(nEX + 1), widths = 6)

  # Center matrix headers
  openxlsx::addStyle(wb, 3, cols = seq_len(nLR + 1), rows = 1, style = hsc, stack = TRUE)
  openxlsx::addStyle(wb, 4, cols = seq_len(nEX + 1), rows = 1, style = hsc, stack = TRUE)

  # Preserve the log's lines and spacing
  if(length(log)) {
    openxlsx::addWorksheet(wb, "Log", gridLines = FALSE)
    openxlsx::writeData(wb, "Log", log, colNames = FALSE)
    textStyle = openxlsx::createStyle(fontName = "Consolas", fontSize = 10)
    openxlsx::addStyle(wb, "Log", style = textStyle, rows = seq_along(log), cols = 1)
    openxlsx::setColWidths(wb, "Log", cols = 1, widths = 120)
  }

  openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
  invisible(wb)
}


safeFormat = function(v, ...) {
  res = format(v, ...)
  res[is.na(v) | trimws(res) == "NA"] = ""
  res
}
