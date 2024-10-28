.setDB = function(x, db = NULL) {
  if(is.null(db))
    stop2("No database given")

  m = name(x)
  not_in_db = !m %in% names(db)
  if(any(not_in_db)) {
    warning("Ignoring markers not found in database:", m[not_in_db])
    x = selectMarkers(x, !not_in_db)
  }

  if(nMarkers(x) == 0) {
    stop2("No marker")
  }
  setFreqDatabase(x, db)
}
