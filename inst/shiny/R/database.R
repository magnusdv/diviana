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

# Wrapper of pedtools::setMutmod
.setMuts = function(x, markers, params) {
  for(i in seq_along(markers)) {
    parlist = params[[i]]
    #print(parlist)
    if(is.null(parlist$model) || parlist$model == "none")
      x = setMutmod(x, i, model = NULL)
    else
      x = do.call(setMutmod, c(list(x = x, markers = i), parlist))
  }
  x
}

# Wrapper of pedmut::getParams
.getAllMutParams = function(x) {
  markers = name(x) |> .setnames()
  lapply(markers, function(m) {
    mut = mutmod(x, m)
    p = getParams(mut, format = 1)
    list(model = p$model[1],
         rate = list(female = p$rate[1], male = p$rate[2]),
         rate2 = list(female = p$rate2[1], male = p$rate2[2]),
         range = list(female = p$range[1], male = p$range[2]))
  })
}
