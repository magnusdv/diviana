.setDB = function(x, db = NULL) {
  if(is.null(db))
    stop2("No database given")

  m = name(x)

  if(length(m) == 0)
    stop2("No markers in object")

  idx = match(normaliseName(m), normaliseName(names(db)), nomatch = 0)

  if(all(idx == 0))
    stop2("None of the markers found in database")

  if(any(idx == 0)) {
    warning("Ignoring markers not found in database:", m[idx == 0])
    x = selectMarkers(x, idx > 0)
  }

  setFreqDatabase(x, db[idx])
}

normaliseName = function(x) {
  tolower(gsub("[-._ ]", "", x))
}
