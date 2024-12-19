
.updateDB = function(loci, mutParams) {
  mod = mutParams$model
  if(is.null(mod) || mod == "none") {
    for(m in names(loci)) loci[[m]]$mutmod = NULL
  }
  else if(mod == "data") {
    mods = mutParams$fullmods
    for(m in names(loci)) loci[[m]]$mutmod = mods[[m]]
  }
  else {
    args = list(model = mod, rate = mutParams$rate, validate = TRUE)
    if(mod == "stepwise") { args$rate2 = 1e-6; args$range = 0.1 }
    for(m in names(loci)) {
      tryCatch({
        args$alleles = loci[[m]]$alleles
        args$afreq = loci[[m]]$afreq
        loci[[m]]$mutmod = do.call(mutationModel, args)
      }, error = function(e) {
        msg = paste0(conditionMessage(e), "<br>Disabling mutation modelling for this marker.")
        errModal(msg, html = TRUE)
        loci[[m]]$mutmod = NULL
      })
    }
  }
  loci
}


.setDB = function(x, db = NULL, tag = "") {
  if(is.null(db)) {
    stop2("No database loaded")
  }

  m = name(x)

  if(length(m) == 0)
    stop2("No markers in object")

  idx = match(normaliseName(m), normaliseName(names(db)), nomatch = 0)

  if(all(idx == 0))
    stop2("None of the markers found in database")

  if(any(idx == 0)) {
    showNotification(sprintf("%s: Ignoring markers not found in database:\n%s",
                             tag, sum(idx == 0), toString(m[idx == 0])))
    x = selectMarkers(x, idx > 0)
  }

  y = setLocusAttributes(x, markers = m[idx > 0], locusAttributes = db[idx], erase = TRUE)
   showNotification(sprintf("%s: Database attached (%d markers)", tag, sum(idx > 0)))
  y
}

normaliseName = function(x) {
  tolower(gsub("[-._ ]", "", x))
}
