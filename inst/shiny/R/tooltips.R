
TT = list(
  usealias = "Use aliases in plots/tables",
  resetall = "Reset all",
  downloaddata = "Download R/dviData",
  downloadRes = "Export results to Excel",
  debug = "Debugging mode",
  dviplot = "DVI overview plot",
  newped = "Add new pedigree",
  editped = "Edit this pedigree",
  delped = "Delete this pedigree",
  solve = "Run complete DVI pipeline",
  LRthresh = "LR threshold for conclusive evidence",
  LRthresh2 = "LR threshold for probable evidence",
  maxIncomp = "Maximum number of incompatible markers",
  clearsel = "Clear selection",
  sex1 = "Male", sex2 = "Female", sex0 = "Unknown sex",
  remove = "Remove individual and descendants",
  missing = "Missing",
  nonmissing = "Non-missing",
  sibleft = "Sibling to the left",
  sibright = "Sibling to the right",
  untyped = "Unassign selected refs",
  pedhelp = "Help"
)

wrap_tooltip = function(tag, id, placement = "top"){
  if(id %in% names(TT)){
    tag$attribs$`data-toggle` = "tooltip"
    tag$attribs$title = TT[[id]]
    tag$attribs$`data-placement` = placement
    tag$attribs$`data-trigger`="hover"
    #tag$attribs$`data-container` = "body"  # Ensure global container
  }
  tag
}

# addTooltips = function(session, tooltipList = TT, ns = identity) { #print(tooltipList)
#   for(id in names(tooltipList)) { #print(id)
#     addTooltip(ns(id), session = session,
#                options = list(title = tooltipList[[id]]))
#   }
# }
