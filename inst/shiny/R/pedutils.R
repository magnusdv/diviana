editPeddata = function(peddat, idch = character(), sxch = integer(), missing = character()) {
  ped = peddat$ped

  # Extract for this component
  id = idch[names(idch) %in% labels(ped)]
  if(length(id)) {
    ped = pedtools::relabel(ped, new = id)
    j = match(peddat$refs, names(id), nomatch = 0L)
    if(any(j > 0))
      peddat$refs[j > 0L] = unname(id[j[j > 0L]])
  }

  sx = sxch[names(sxch) %in% labels(ped)]
  if(length(sx)) {
    oldsex = pedtools::getSex(ped, names(sx))
    swap = names(sx)[oldsex * sx == 2] #  1 <-> 2 or vice versa

    if(length(swap)) {
      spouses = pedtools::spouses(ped, swap)
      if(any(spouses %in% missing))
        stop2("Cannot change sex for reference individuals with missing spouses")
      ped = pedtools::swapSex(ped, swap, verbose = FALSE)
    }

    set = oldsex == 0 & sx != 0
    if(any(set))
      ped = pedtools::setSex(ped, names(sx)[set], sex = sx[set])
  }

  peddat$ped = ped
  peddat
}
