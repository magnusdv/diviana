

# Prepare data for updating labels in plot
updateLabelsData = function(currData, old = NULL, new, .alignment = NULL) {
  ped = currData$ped
  reorder = FALSE

  # If new = "asPlot" or "generations" (hack needed since pedigree is always reordered)
  if(is.null(old)) {
    idMap = relabel(ped, old = NULL, new = new, .alignment = .alignment, returnLabs = TRUE)
    newped = relabel(ped, old = names(idMap), new = idMap, reorder = TRUE)
  }
  else {
    newped = relabel(ped, old = old, new = new, reorder = FALSE)
    idMap = .setnames(newped$ID, ped$ID)
  }

  newtw = currData$twins
  newtw$id1 = idMap[newtw$id1]
  newtw$id2 = idMap[newtw$id2]

  list(ped = newped,
       aff = idMap[currData$aff],
       carrier = idMap[currData$carrier],
       deceased = idMap[currData$deceased],
       twins = newtw)
}


# Pagination for plotting many pedigrees. Large pedigrees alone.
pedPages = function(peds, pageSize = 6L) {
  if(!length(peds))
    return(NULL)

  nind = pedsize(peds)
  small = which(nind < 25L)
  large = which(nind >= 25L)

  pages = split(small, (seq_along(small) - 1L) %/% pageSize)

  # Place each large pedigree after the page containing its preceding small pedigree
  pos = ceiling(findInterval(large, small)/pageSize)
  ord = order(c(seq_along(pages), pos),
              c(rep.int(0L, length(pages)), seq_along(large)))

  unname(c(pages, as.list(large))[ord])
}
