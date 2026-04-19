db2locattrs = function(db, mutparams = NULL) {
  if(is.null(db))
    return(NULL)

  mnames = names(db) |> .setnames()
  standard = "model" %in% names(mutparams)
  hasoriginal = mnames %in% names(mutparams) |> .setnames(mnames)

  lapply(mnames, function(m) {
    alleles = names(db[[m]])
    afreq = unname(db[[m]])
    loc = list(name = m, alleles = alleles, afreq = afreq, mutmod = NULL)

    if(!standard && !hasoriginal[m])
      return(loc)

    args = if(standard) mutparams else mutparams[[m]]
    args$alleles = alleles
    args$afreq = afreq

    loc$mutmod = tryCatch(
      do.call(mutationModel, args),
      error = function(e) {
        msg = paste0(conditionMessage(e), "<br>Disabling the model for: ", m)
        errModal(msg, html = TRUE)
        NULL
    })

    loc
  })
}


.setLocusAttrs = function(x, loci = NULL, tag = "") {
  if(is.null(loci)) {
    stop2("No database loaded")
  }

  datamarkers = name(x)

  if(length(datamarkers) == 0)
    stop2("No markers in object")

  idx = match(normaliseName(datamarkers), normaliseName(names(loci)), nomatch = 0)

  if(all(idx == 0))
    stop2("None of the markers found in database")

  if(any(idx == 0)) {
    showNotification(sprintf("%s: Ignoring %d markers not found in database:\n%s",
                             tag, sum(idx == 0), toString(datamarkers[idx == 0])))
    x = selectMarkers(x, idx > 0)
    datamarkers = datamarkers[idx > 0]
    idx = idx[idx > 0]
  }

  loci2 = loci[idx]

  # NB: must specify `markers` when matchNames is FALSE
  y = setLocusAttributes(x, markers = datamarkers, locusAttributes = loci2,
                         erase = TRUE, matchNames = FALSE)
  showNotification(sprintf("%s: Database attached (%d markers)", tag, length(loci2)))
  y
}

normaliseName = function(x) {
  toupper(gsub("[-._ ]", "", x))
}

# Observed alleles not seen in database
# .firstcomp: Use only 1st comp of pedlist inputs
missingAlleles = function(db, ..., .firstcomp = TRUE) {
  peds = list(...)
  miss = list()
  dbnames = names(db)
  dbnorm = normaliseName(dbnames)

  for(x in peds) {

    isped = is.ped(x)

    if(.firstcomp && !isped && length(x) && is.ped(x[[1]])) {
      x = x[[1]]
      isped = TRUE
    }

    if(!isped && !is.pedList(x))
      stop("Non-pedigree input: ", x)

    xnorm = name(x) |> normaliseName()
    idx = match(xnorm, dbnorm, nomatch = 0)

    for(i in seq_along(idx)) {
      j = idx[i]
      if(j == 0) next

      nm = dbnames[j]
      new = .mysetdiff(pedtools::alleles(x, i), names(db[[j]]))
      if(length(new))
        miss[[nm]] = c(miss[[nm]], new)
    }
  }

  lapply(miss, \(v) sort.default(unique.default(v)))
}

# Improved version of `pedtools::addAllele()`
# (adds multiple alleles; uses proportional shrinkage above 0.001; takes list input (not ped)
# TODO: move to pedtools
.addAlleles = function(x, newAlleles = NULL, freq = 0.001, minim = NA) {
  if(is.na(minim))
    minim = if (any(x < 0.001)) 0 else 0.001

  if(any(freq < minim))
    stop2("New frequencies must be at least `minim`")

  newAlleles = .mysetdiff(unique.default(newAlleles), names(x))
  if(length(newAlleles) == 0)
    return(x)

  y = rep(freq, length(newAlleles))
  names(y) = newAlleles

  need = sum(y)
  avail = sum(x - minim)
  if(avail < need) stop2("Impossible: not enough mass above `minim`")

  k = length(x)
  a = (1 - need - k*minim)/(1 - k*minim)
  oldNew = minim + a*(x - minim)

  res = c(oldNew, y)

  # Natural sort
  als = names(res)
  num = suppressWarnings(as.numeric(als))
  ord = if(!any(is.na(num))) order(num) else order(als)
  res[ord]
}
