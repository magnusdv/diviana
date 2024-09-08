
# Temporary name waiting for better solution in dvir
plotSolutionDIVIANA = function(dvi, summary, ...) { # summaryAM

  # Ensure proper dviData object
  refs = typedMembers(dvi$am)

  miss = summary$Missing
  conc = summary$Conclusion
  #vics = summary$Sample

  groups = lapply(names(COLS_BG), function(g)
    miss[conc == g])
  names(groups) = names(COLS_BG)

  fill = list()
  for(i in seq_along(COLS_BG)) {
    cl = COLS_BG[i]
    fill[[cl]] = c(fill[[cl]], groups[[i]])
  }

  excl = groups$Excluded
  stillmiss = c(groups$Inconclusive, groups$`No match`, miss[conc == ""])

  linecol = list("red" = c(groups$Excluded, stillmiss))
  carrier = stillmiss

  plotDVI(dvi, style = 0, pm = FALSE, titles = NULL,
          fill = fill, hatched = refs, deceased = excl,
          col = linecol, carrier = carrier, lwd = list("1.5" = c(excl)), ...)
}
