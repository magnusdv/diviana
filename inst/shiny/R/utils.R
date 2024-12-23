### Helper functions for DVI app

stop2 = function (...) {
  a = lapply(list(...), toString)
  a = append(a, list(call. = FALSE))
  do.call(stop, a)
}

`%||%` = function(x,y) {
  if(is.null(x)) y else x
}

.setnames = function(x, nms = x) {
  if(is.null(x))
    return(x)
  names(x) = nms
  x
}

.flipnames = function(x) {
  .setnames(names(x), x)
}

.myintersect = function(x, y)
  y[match(x, y, 0L)]

bigHeading = function(x)
  h4(strong(x), .noWS = "before")

midHeading = function(x)
  h5(strong(x), style = "margin-bottom: 0px;")

bold = function(x) strong(x, .noWS = "outside")
ital = function(x) em(x, .noWS = "outside")

.generateLabs = pedtools:::generateLabs

.addChild = function(x, ids, sex, avoid = NULL) {
  newids = .generateLabs(x, 2, avoid = avoid)
  if(length(ids) == 1) {
    ids = c(ids, newids[1])
    child = newids[2]
  } else {
    child = newids[1]
  }

  addChild(x, parents = ids, id = child, sex = sex, verbose = FALSE)
}

.addParents = function(x, id, avoid = NULL) {
  newids = .generateLabs(x, 2, avoid = avoid)
  addParents(x, id, father = newids[1], mother = newids[2], verbose = FALSE)
}


.addSib = function(x, id, sex = 1, side = c("right", "left"), avoid = NULL) {

  if(length(id) > 1)
    stop2("To add a sibling, please select exactly one individual. Current selection: ", sortIds(x, id))

  newids = .generateLabs(x, 3, avoid = avoid)

  if(id %in% founders(x)) {
    fa = newids[1]; mo = newids[2]; child = newids[3]
    x = addParents(x, id, father = fa, mother = mo, verbose = FALSE)
  }
  else {
    fa = father(x, id); mo = mother(x, id); child = newids[1]
  }


  newped = addChildren(x, father = fa, mother = mo, ids = child, sex = sex, verbose = FALSE)

  # Reorder so that new sib comes directly before or after (default) `id`
  idInt = internalID(x, id)
  n = length(x$ID)
  ord = switch(match.arg(side),
               left = c(seq_len(idInt-1), n+1, idInt:n),
               right = c(seq_len(idInt), n+1, if(idInt < n) seq.int(idInt+1, n)))

  reorderPed(newped, ord)
}

.addSibOLD = function(x, id, sex, avoid = NULL) {
  if(length(id) > 1)
    stop2("Too many individuals are selected. Current selection: ", sortIds(x, id), "<br><br>",
          "To add a sibling, please select exactly one individual.")

  newids = .generateLabs(x, 3, avoid = avoid)

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

abbrMat = function(x)
  x[, seq_len(min(3, ncol(x))), drop = FALSE]

amel2sex = function(amel) {
  sex = integer(length(amel))
  sex[amel %in% c("X/Y", "X,Y", "XY")] = 1L
  sex[amel %in% c("X/X", "X,X", "XX")] = 2L
  sex
}

getSexFromAMEL = function(g) {
  sex = integer(nrow(g))
  if(!is.null(g$AMEL)) {
    sex[g$AMEL == "X/Y"] = 1L
    sex[g$AMEL == "X/X"] = 2L
  }
  sex
}

getAlleleSep = function(g) {
  if(ncol(g) == 0) return()
  if(any(grepl("/", g[[1]], fixed = TRUE))) return("/")
  if(any(grepl(",", g[[1]], fixed = TRUE))) return(",")
  return(" ")
}

useAlias = function(labs, alias) {
  names(labs) = ifelse(labs %in% names(alias), alias[labs], "")
  labs
}


rbindSafe = function(df1, df2) {
  if(is.null(df1))
    return(df2)
  cols = union(names(df1), names(df2))
  df1[setdiff(cols, names(df1))] = NA
  df2[setdiff(cols, names(df2))] = NA
  rbind(df1[, cols], df2[, cols])
}

getGenotypesAndSex = function(x) {
  df = x |> getGenotypes(ids = typedMembers) |> as.data.frame()
  df$Sex = getSex(x, rownames(df))
  df
}

abbreviatePedrel = function(x, width = 15) {
  x[x == "Unrelated"] = "Unrel"
  x[x == "Mother-daughter"] = "Mother-dau"
  x[x == "Father-daughter"] = "Father-dau"

  if(!any(long <- (nchar(x) > width)))
    return(x)

  x[long] = sub(" &.*", " &", x[long])
  if(!any(long <- (nchar(x) > width)))
    return(x)

  x[long] = sub("--.*", "/", x[long])
  if(!any(long <- (nchar(x) > width)))
    return(x)

  x[long] = paste0(substr(x[long], 1, width-2), "..")
  x
}


changeSex = function(ped, ids, sex) {

  if(sex == 0) {
    if(!all(ids %in% leaves(ped)))
      stop2("Only individuals without children can have unknown sex")
    newped = setSex(ped, ids, sex = 0)
    return(newped)
  }

  # By now: sex is 1 or 2
  currentSex = getSex(ped, ids)

  ped |>
    swapSex(ids[currentSex == (3-sex)], verbose = FALSE) |>
    setSex(ids[currentSex == 0], sex = sex)
}
