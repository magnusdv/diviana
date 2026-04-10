#' Generate table of marker data
#'
#' @param dvi A `dviData` object
#'
#' @return A data frame.
#'
#' @examples
#' markerSummary(planecrash)
#'
markerSummary = function(dvi = NULL, am = NULL, pm = NULL) { print("in marker summary...")
  if(!is.null(dvi))
    dvi = dvir:::consolidateDVI(dvi)

  am = am %||% dvi$am
  pm = pm %||% dvi$pm

  # List of lists: Marker attributes
  #print(am)

  if(is.null(am) && is.null(pm))
    return()
  locAttrsAM = pedtools::getLocusAttributes(am, checkComps = TRUE)

  reslist = lapply(locAttrsAM, function(a) {
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
               Chart = "",
               pars,
               Stat = stattxt,
               Lump = lumptxt,
               Mut = "")
  })

  res = do.call(rbind, reslist)
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
