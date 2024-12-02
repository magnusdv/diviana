
# Temporary name waiting for better solution in dvir
plotSolutionDIVIANA = function(dvi, summary, pednrs = NULL, ...) { # summaryAM

  if(is.null(pednrs))
    pednrs = seq_along(dvi$am)

  # Ensure proper dviData object
  refs = typedMembers(dvi$am)

  miss = summary$Missing
  conc = summary$Conclusion

  groups = lapply(names(COLS_BG), function(g)
    miss[conc == g])
  names(groups) = names(COLS_BG)

  groups$noconc = miss[conc == ""]

  fill = list()
  for(i in seq_along(COLS_BG)) {
    cl = COLS_BG[i]
    fill[[cl]] = c(fill[[cl]], groups[[i]])
  }

  excl = groups$Excluded
  stillmiss = unlist(groups[c("Nonidentifiable", "Inconclusive", "Inconclusive GLR", "No match", "noconc")])

  linecol = list("red" = c(groups$Excluded, stillmiss))
  carrier = stillmiss

  tryCatch(
    plotDVI(dvi, style = 0, pm = FALSE, am = pednrs, titles = NULL,
            fill = fill, hatched = refs, deceased = excl,
            col = linecol, carrier = carrier, lwd = list("1.5" = c(excl)), ...),
    error = function(e) {
      msg = conditionMessage(e)
      stop2(if(grepl("cex", msg)) "Cannot fit graph" else msg)
    })
}
