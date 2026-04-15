#' Summary table of marker information
#'
#' @param dvi A `dvir::dviData` object.
#' @param locAttrs Optional locus attributes, typically from
#'   `pedtools::getLocusAttributes(dvi$am)`, supplied instead of `dvi`.
#'
#' @return A data frame.
#'
#' @examples
#' markerSummary(planecrash)
#'
markerSummaryDiviana = function(locAttrs = NULL, dvi = NULL) { dd <<- dvi
  if(is.null(locAttrs) && length(dvi$am)) {
    dvi = dvir:::consolidateDVI(dvi)
    locAttrs = pedtools::getLocusAttributes(dvi$am, checkComps = TRUE)
  }

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
               Stat = stattxt,
               Lump = lumptxt)
  })

  res = do.call(rbind, reslist)

  if(!is.null(dvi)) {
    usedinAM = if(length(dvi$am)) res$Marker %in% pedtools::name(dvi$am) else NA
    usedinPM = if(length(dvi$pm)) res$Marker %in% pedtools::name(dvi$pm) else NA
    res = cbind(AM = usedinAM, PM = usedinPM, res)
  }

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
