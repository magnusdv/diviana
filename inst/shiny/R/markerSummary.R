
#' Summary table of marker information
#'
#' @param locAttrs A list of locus attributes.
#' @param am,pm Allele matrices from the loaded sample tables, with two columns
#'   per marker and column names ending in `.1` and `.2`.
#'
#' @return A data frame.
#'
markerSummaryDiviana = function(locAttrs, am = NULL, pm = NULL) {
  if(!length(locAttrs))
    return()

  reslist = lapply(locAttrs, function(a) {
    mut = a$mutmod
    pars = pedmut::getParams(mut, c("model", "rate", "rate2", "range"), format = 3)
    colnames(pars) = c("Model", "Rate", "Rate2", "Range")

    if(is.na(pars$Model))
      pars$Model = "No model"
    else
      pars$Model = capit(pars$Model)

    lumptxt = stattxt = NA_character_
    if(!is.null(mut)) {
      lumptxt = if(pedmut::alwaysLumpable(mut)) "Always" else "No"  #if (specLump) "Special" else
      stattxt = if(pedmut::isStationary(mut)) "Yes" else "No"
    }
    data.frame(Marker = a$name,
               Alleles = length(a$alleles),
               PIC = PIC(a$afreq) |> round(2),
               pars,
               Stationary = stattxt,
               Lumpable = lumptxt)
  })

  res = do.call(rbind, reslist)

  # Marker presence depends on loaded samples, not pedigree assignments
  markers = normaliseName(res$Marker)
  inData = function(a) {
    if(!NROW(a)) return(NA)
    nm = colnames(a)[2L * seq_len(ncol(a)/2L) - 1L]
    markers %in% normaliseName(substr(nm, 1L, nchar(nm) - 2L))
  }
  res = cbind(res["Marker"], AM = inData(am), PM = inData(pm), res[-1L])

  rownames(res) = NULL
  res
}

# Polymorphic information content
PIC = function(afr) 1 - sum(afr^2) - sum(afr^2)^2 + sum(afr^4)

# Capitalize first letter
capit = function (x) {
  notna = !is.na(x)
  if (!length(x) || !any(notna))
    return(x)
  substr(x[notna], 1, 1) = toupper(substr(x[notna], 1, 1))
  x
}
