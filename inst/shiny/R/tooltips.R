
tooltips = list(
  resetall = "Reset all",
  downloaddata = "Download R/dviData",
  debug = "Debugging mode",
  plotdviButton = "Overview plot of the DVI case",
  nextped = "Next pedigree",
  prevped = "Previous pedigree",
  newped = "Add a new pedigree",
  editped = "Edit the current pedigree",
  solve = "Run the complete DVI pipeline",
  LRthresh = "LR threshold for conclusive evidence",
  LRthresh2 = "LR threshold for probable evidence",
  maxIncomp = "Maximum number of incompatible markers",
  clearsel = "Clear selection"
)

addTooltips = function(session) {
  for(id in names(tooltips))
    addTooltip(id = id, session = session, options = list(title = tooltips[[id]]))
}
