
tooltips = list(
  debug = "Debugging mode",
  plotdviButton = "Overview plot of the DVI case",
  solve = "Run the complete DVI pipeline",
  nextped = "Nex pedigree",
  prevped = "Previous pedigree",
  newped = "Add a new pedigree",
  editped = "Edit the current pedigree"
)

addTooltips = function(session) {
  for(id in names(tooltips)) {
    addTooltip(id = id, session = session, options = list(title = tooltips[[id]]))
  }
}
