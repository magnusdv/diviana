
# Temporary name waiting for better solution in dvir
plotSolutionDIVIANA = function(dvi, summary, pednrs = NULL, ...) { # summaryAM

  Nped = length(dvi$am)
  if(is.null(pednrs))
    pednrs = seq_len(Nped)

  refs = typedMembers(dvi$am)
  miss = dvi$missing

  conc = summary$Conclusion

  groups = lapply(names(COLS_BG) |> .setnames(), function(g) miss[conc == g])
  groups$noconc = miss[conc == ""]

  fill = list()
  for(i in seq_along(COLS_BG)) {
    cl = COLS_BG[i]
    fill[[cl]] = c(fill[[cl]], groups[[i]])
  }
  excl = groups$Excluded
  stillmiss = setdiff(miss, unlist(groups[c("Undisputed", "Match (GLR)", "Symmetric match", "Probable", "Disputed")]))
  linecol = list("red" = c(excl, stillmiss))
  carrier = stillmiss

  labs = dvi$missing

  tryCatch(
    plotDVI(dvi, style = 0, pm = FALSE, am = pednrs, labs = labs, nrowAM = if(Nped > 4) 2 else NA,
            titles = NULL, fill = fill, hatched = refs, deceased = excl,
            col = linecol, carrier = carrier, lwd = list("1.5" = c(excl)), ...),
    error = function(e) {
      msg = conditionMessage(e)
      stop2(if(grepl("cex", msg)) "Cannot fit graph" else msg)
    })
}
