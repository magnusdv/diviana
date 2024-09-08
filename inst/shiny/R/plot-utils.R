

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
    idMap = setNames(newped$ID, ped$ID)
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
