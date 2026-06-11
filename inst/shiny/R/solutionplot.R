
# Temporary name waiting for better solution in dvir
plotSolutionDIVIANA = function(dvi, summary, pednrs = NULL, labs = dvi$missing,
                               pageSize = 6, nrowAM = NULL, ncolAM = NULL, ...) {

  Nped = length(dvi$am)
  if(is.null(pednrs))
    pednrs = seq_len(Nped)

  pageN = min(Nped, pageSize)

  if(is.null(nrowAM) && is.null(ncolAM))
    nrowAM = if(pageN > 4) 2 else NA

  amDim = dvir:::amArrayDim(pageN, nrowAM %||% NA, ncolAM %||% NA)

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
  stillmiss = .mysetdiff(miss, unlist(groups[c("Undisputed", "Match (GLR)", "Symmetric match", "Probable", "Disputed")]))
  linecol = list("red" = c(excl, stillmiss))
  carrier = stillmiss

  tryCatch(
    plotDVI(dvi, style = 0, pm = FALSE, am = pednrs, labs = labs,
            nrowAM = amDim[1], ncolAM = amDim[2],
            widths = rep(1, amDim[2]), heights = rep(1, amDim[1]),
            titles = NULL, fill = fill, hatched = refs, deceased = excl,
            col = linecol, carrier = carrier, lwd = list("1.5" = c(excl)), ...),
    error = function(e) {
      msg = conditionMessage(e)
      stop2(if(grepl("cex", msg)) "Cannot fit graph" else msg)
    })
}
