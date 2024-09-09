### Helper functions for DVI app

stop2 = function (...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

`%||%` = function(x,y) {
  if(is.null(x)) y else x
}

setnames = function(x, nms = x) {
  names(x) = nms
  x
}

.myintersect = function(x, y)
  y[match(x, y, 0L)]

bigHeading = function(x)
  h4(strong(x), .noWS = "before")

midHeading = function(x)
  h5(strong(x), style = "margin-bottom: 0px;")

bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")

errModal = function(..., html = FALSE) {
  args = list(...)
  if(length(args) == 1 && inherits(args[[1]], "condition")) {
    mess = conditionMessage(args[[1]])
    if(grepl("reduce cex", mess))
      mess = "Plot region is too small"
  }
  else
    mess = paste(lapply(args, toString), collapse = "")
  if(html)
    mess = HTML(mess)
  showModal(modalDialog(mess, easyClose = TRUE))
}

showErr = function(..., html = TRUE) {
  args = list(...)
  if(length(args) == 1 && inherits(args[[1]], "condition"))
    mess = conditionMessage(args[[1]])
  else
    mess = paste(lapply(args, toString), collapse = "")
  if(html)
    mess = HTML(mess)
  showNotification(mess, type = "error")
}

# No used
textInput2 = function(inputId, value) {
  w  = textInput(inputId, label = NULL, value = value, width = "100%")
  w$children[[2]]$attribs[["style"]] = "padding-top: 1px; padding-bottom: 1px; height: 24px;"
  w
}

# Version of pedtools::generateLabs that allows for a prefix in existing labels
nextLab = function(x, n, avoid = NULL, prefix) {
  if(is.ped(x))
    x = x$ID
  y = sub(sprintf("^%s-", prefix), "", x)
  new = pedtools:::generateLabs(y, n, avoid = avoid)
  paste(prefix, new, sep = "-")
}

.addChild = function(x, ids, sex, prefix) {
  newids = nextLab(x, 2, prefix = prefix)
  if(length(ids) == 1) {
    ids = c(ids, newids[1])
    child = newids[2]
  } else {
    child = newids[1]
  }

  addChild(x, parents = ids, id = child, sex = sex, verbose = FALSE)
}

.addParents = function(x, id, prefix) {
  newids = nextLab(x, 2, prefix = prefix)
  addParents(x, id, father = newids[1], mother = newids[2], verbose = FALSE)
}

.addSib = function(x, id, sex, prefix) {
  newids = nextLab(x, 3, prefix = prefix)

  if(length(id) > 1)
    stop2("Too many individuals are selected. Current selection: ", sortIds(x, id), "<br><br>",
          "To add a sibling, please select exactly one individual.")

  if(id %in% founders(x)) {
    fa = newids[1]; mo = newids[2]; child = newids[3]
    x = addParents(x, id, father = fa, mother = mo, verbose = FALSE)
  }
  else {
    fa = father(x, id); mo = mother(x, id); child = newids[1]
  }

  addChildren(x, father = fa, mother = mo, ids = child, sex = sex, verbose = FALSE)
}


removeSel = function(ped, ids) {
  newped = tryCatch(
    removeIndividuals(ped, ids, verbose = FALSE),
    error = function(e) conditionMessage(e)
  )

  isEmpty = is.null(newped)
  discon = is.character(newped) && grepl("disconnected", newped, ignore.case = TRUE)

  errmsg = if(is.character(newped)) newped else NULL
  if(isEmpty || discon)
    errmsg = sprintf("Removing %s would leave a disconnected or empty pedigree",
                     ifelse(length(ids) == 1, paste("individual", ids), "these individuals"))
  if(!is.null(errmsg))
    stop2(errmsg)

  newped
}

sortIds = function(x, ids) {
  intern = internalID(x, ids)
  ids[order(intern)]
}

uniquify = function(id) {
  # Random number between 1 and 1e6
  rand = round(runif(1, 1, 1e6))

  # Combine with current time to enhance uniqueness
  time = round(as.numeric(Sys.time()), 3)

  # Concatenate the two parts
  paste(id, time, rand, sep = "_")
}


# Capture the return value of a function AND its printed messages
captureOutput = function(func, ...) {
  log = capture.output(res <- func(...))
  list(result = res, log = log)
}

prepLog = function(txt) {
  headlines = startsWith(txt, "------")
  heads = txt[headlines] |> sub("^-* ", "", x = _) |> sub(" -*$", "", x = _)
  txt[headlines] = paste0("<h5>", heads, "</h4>")
  txt |> paste0(collapse = "<br>") |> HTML()
}

naReplace = function(v, repl = 0) {
  if(length(repl) != 1)
    stop2("Replacement vector must have length 1")
  v[is.na(v)] = repl
  v
}
