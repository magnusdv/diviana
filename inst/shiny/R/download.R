
downloadTables = function(AM, PM, aliasAM, aliasPM, filename) {
  labsAM = data.frame(Alias = aliasAM, Original = names(aliasAM))
  labsPM = data.frame(Alias = aliasPM, Original = names(aliasPM))
  tables = list(resultsAM = AM, resultsPM = PM,
                labelsAM = labsAM, labelsPM = labsPM)
    hs = createStyle(textDecoration = "bold")
    openxlsx::write.xlsx(tables, filename, headerStyle = hs, colWidths = "auto")
}
