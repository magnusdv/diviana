
TT = list(
  usealias = "Use aliases in plots/tables",
  resetall = "Reset all",
  downloaddata = "Download R/dviData",
  debug = "Debugging mode",
  plotdviButton = "Overview plot of the DVI case",
  nextped = "Next pedigree",
  prevped = "Previous pedigree",
  newped = "Add new pedigree",
  editped = "Edit current pedigree",
  solve = "Run the complete DVI pipeline",
  LRthresh = "LR threshold for conclusive evidence",
  LRthresh2 = "LR threshold for probable evidence",
  maxIncomp = "Maximum number of incompatible markers",
  clearsel = "Clear selection",
  sex1 = "Male", sex2 = "Female", sex0 = "Unknown sex",
  remove = "Remove individual and descendants",
  missing = "Missing",
  nonmissing = "Non-missing",
  sibleft = "Sibling on left side",
  sibright = "Sibling on right side",
  untyped = "Unassign selected refs"
)

wrap_tooltip=function(tag, id, placement = "top"){
  if(id %in% names(TT)){
    tag$attribs$`data-toggle` = "tooltip"
    tag$attribs$title = TT[[id]]
    tag$attribs$`data-placement` = placement
    tag$attribs$`data-trigger`="hover"
  }
  tag
}

addTooltips = function(session, tooltipList = TT, ns = identity) {print(tooltipList)
  for(id in names(tooltipList)) { print(id)
    addTooltip(ns(id), session = session,
               options = list(title = tooltipList[[id]]))
  }
}
