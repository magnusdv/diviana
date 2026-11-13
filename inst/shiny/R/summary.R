
# Summarise loaded samples and pedigree records, independently of analysis preparation
dviSummary = function(am, pm, peds, amAlleles, dbMarkers, dbLabel, nAdded = 0L) {
  count = function(n, singular, plural = paste0(singular, "s"))
    paste(n, if(n == 1L) singular else plural)
  warn = function(x, when = TRUE) if(when) tags$em(x, class = "text-orange") else x
  line = function(label, ...) div(strong(paste0(label, ": ")), ...)
  detail = function(...) div(..., class = "ml-3")
  sep = " · "

  sexText = function(sex) {
    if(!length(sex)) return("")
    n = tabulate(match(sex, c(1L, 2L), nomatch = 3L), nbins = 3L)
    used = which(n > 0L)
    labels = c("male", "female", "unknown sex")
    text = if(length(sex) == 1L) labels[used] else
      if(length(used) == 1L) paste("all", labels[used]) else
        toString(paste(n[used], c("male", "female", "unknown")[used]))
    paste0(" (", text, ")")
  }

  nAM = NROW(am)
  nPM = NROW(pm)
  nFam = length(peds)
  hasData = nAM + nPM + nFam > 0L

  # Count assignments against loaded sample IDs, not stale pedigree references
  refs = lapply(peds, `[[`, "refs")
  unassigned = sum(!am$Sample %in% unlist(refs, use.names = FALSE))
  noRefs = sum(vapply(refs, function(x) !any(x %in% am$Sample), logical(1)))
  assignment = if(unassigned == nAM) {
    if(nAM == 1L) "Not assigned to pedigree" else "None assigned to pedigrees"
  } else paste(unassigned, "not assigned to pedigrees")

  # Any observed allele counts as typed, including partial genotypes
  untyped = if(length(amAlleles))
    sum(.rowSums(!amAlleles %in% c("", ".", "-", "0", "NA", NA),
                 nAM, ncol(amAlleles)) == 0L) else nAM

  nMiss = lengths(lapply(peds, `[[`, "miss"))
  nMissing = sum(nMiss)
  simple = sum(nMiss == 1L)
  complex = sum(nMiss > 1L)
  noMissing = sum(nMiss == 0L)
  types = if(nFam && simple == nFam) {
    if(nFam == 1L) "simple" else "all simple"
  } else if(nFam && complex == nFam) {
    if(nFam == 1L) "complex" else "all complex"
  } else toString(c(if(simple) paste(simple, "simple"), if(complex) paste(complex, "complex")))
  missingSex = unlist(lapply(peds, function(x)
    x$ped$SEX[match(x$miss, x$ped$ID)]), use.names = FALSE)

  # Normalise marker names as in the analysis, but retain markers absent from the database
  metadata = c(".rowid", "Fam", "origFam", "Sample", "Alias", "AMEL", "Sex")
  amMarkers = unique.default(normaliseName(setdiff(names(am), metadata)))
  pmMarkers = unique.default(normaliseName(setdiff(names(pm), metadata)))
  markers = if(nAM && nPM) intersect(amMarkers, pmMarkers) else
    if(nAM) amMarkers else pmMarkers
  markerText = if(nAM && nPM) {
    if(length(markers) == length(amMarkers) && length(markers) == length(pmMarkers))
      paste(length(markers), "(AM and PM)")
    else
      sprintf("%d AM, %d PM (%d shared)", length(amMarkers), length(pmMarkers), length(markers))
  } else if(nAM || nPM) paste(length(markers), if(nAM) "(AM only)" else "(PM only)") else
    "None loaded"
  hasDB = length(dbMarkers) > 0L
  absent = if(hasDB) sum(!markers %in% normaliseName(dbMarkers)) else 0L

  content = div(style = "font-weight:normal;line-height:1.25",
    line("AM", if(nAM) tagList(
      count(nAM, "sample"), if(unassigned) tagList(sep, warn(assignment)),
      if(untyped) tagList(sep, warn(paste(untyped, "untyped")))) else
        warn("No samples loaded", hasData)),
    line("Pedigrees", if(nFam) tagList(
      count(nFam, "family", "families"),
      if(nzchar(types)) paste0(" (", types, ")")) else warn("None", hasData)),
    if(nMissing) detail(count(nMissing, "missing person"), sexText(missingSex)),
    if(noMissing) detail(warn(paste(count(noMissing, "family", "families"),
                                    "without missing persons"))),
    if(noRefs) detail(warn(paste(count(noRefs, "family", "families"), "without references"))),
    line("PM", if(nPM) tagList(count(nPM, "sample"),
      sexText(match(pm$Sex, c("M", "F"), nomatch = 0L))) else
        warn("No samples loaded", hasData)),
    line("Markers", warn(markerText, hasData && !length(markers)),
      if(absent) tagList(sep, warn(paste(absent, "absent from database")))),
    line("Frequencies", if(hasDB) dbLabel else warn("Not selected", hasData),
      if(nAdded) tagList(sep, warn(count(nAdded, "added allele"))))
  )

  bs4Dash::bs4InfoBox(title = NULL, width = 12, elevation = 1, color = "info",
                     value = content, icon = shiny::icon("info"))
}
