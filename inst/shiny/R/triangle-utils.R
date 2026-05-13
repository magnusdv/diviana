# Wrapper for the three triangle cards
triangleCard = function(title, idpref) {
  id = paste0(idpref, c("KappaCalc", "Triangle", "Msg", "Table"))

  calcBtn = actionButton(id[1], "Calculate", class = "btn-sm",
                         icon = icon("play"), style = "margin-left: 30px;")
  #if(idpref == "am")
  #  calcBtn = div(class = "aligned-row",
  #                div(checkboxInput("acrossComps", HTML("Across<br>components"), width = "auto"), style = "font-size: small;"),
  #                calcBtn)

  bs4Dash::bs4Card(
    width = 4,
    collapsible = FALSE,
    title = div(class = "aligned-row-wide", title, calcBtn),
    plotlyOutput(id[2], width = "100%", height = "350px"),
    hr(),
    uiOutput(id[3]),
    DT::DTOutput(id[4]))
}


# Messages below triangles when tables are empty
cpMessage = function(tab, type, nref = NULL, nvic = NULL, across = FALSE) {
  if(!is.null(tab) && nrow(tab) > 0)
    return(NULL)

  type = match.arg(type, c("am", "ampm", "pm"))

  nrefTot = sum(nref)

  msg = switch(type,
    am = {
      if(!length(nref))
        "Nothing to compute: no families."
      else if(nrefTot < 2)
        "Nothing to compute: fewer than 2 typed family members."
      else {
        npairs = if(across) choose(nrefTot, 2) else sum(choose(nref, 2))
        if(npairs == 0)
          "Nothing to compute: no family with >1 typed."
        else
          sprintf("Click Calculate to compute: %d pairs %s.", npairs,
                  if(across) "within & across families" else "within families")
      }
    },
    ampm = {
      if(nvic == 0)
        "Nothing to compute: no PM samples loaded."
      else if(nrefTot == 0)
        "Nothing to compute: no AM samples in families."
      else
        sprintf("Click Calculate to compute: %d AM-PM pairs.", nrefTot * nvic)
    },
    pm = {
      if(nvic == 0)
        "Nothing to compute: no PM samples loaded."
      else if(nvic < 2)
        "Nothing to compute: fewer than 2 PM samples."
      else
        sprintf("Click Calculate to compute: %d PM pairs.", choose(nvic, 2))
    })

  shiny::div(class = "text-muted", style = "font-style:italic;", msg)
}


# CheckPairwise tables ------------------------------------------------------

CPnoplot = function(x, ...) {
  res = forrel::checkPairwise(x, plotType = "none", verbose = FALSE, nsim = 1000, pvalThreshold = 0.05, ...)
  if(is.null(res)) data.frame() else res
}


formatCP = function(tab, usealias = FALSE, alias1 = NULL, alias2 = alias1, sortby = NULL) {

  if(is.null(tab) || nrow(tab) == 0)
    return(NULL)

  if(usealias && !is.null(alias1)) {
    tab$id1 = alias1[tab$id1]
    tab$id2 = alias2[tab$id2]
  }

  skipcols = c("N", "kappa0", "kappa1", "kappa2", "relgroup", "err")
  tab = tab[!names(tab) %in% skipcols]

  if("pedrel" %in% names(tab))
    tab$pedrel = abbreviatePedrel(tab$pedrel)

  .dtstyleCP(tab)
}

.dtstyleCP = function(df) {
  scrollY = if(nrow(df)>10) "220px" else NULL
  .pick = function(...) intersect(c(...), names(df))

  # Order by pval if present
  pvalIdx = match("pval", names(df), nomatch = 0)
  ord = if(pvalIdx > 0) list(pvalIdx - 1, 'asc') else NULL

  DT::datatable(df,
                class = "stripe compact nowrap",
                selection = "none",
                width = "100%",
                rownames = FALSE,
                plugins = "natural",
                options = list(dom = "ft",
                               paging = FALSE,
                               language = list(search = "Filter: "),
                               scrollY = scrollY, scrollX = TRUE,
                               order = ord,
                               columnDefs = list(
                                 list(type = "natural", targets = 0:1),
                                 list(className = "dt-left", targets = "_all"))
                               )
                ) |>
  DT::formatStyle(names(df), target = "row", lineHeight = "75%") |>
  DT::formatStyle(.pick("pedrel"), fontSize = "80%") |>
  formatRound(.pick("k0", "k1", "k2"), digits = 2) |>
  formatRound(.pick("pval"), digits = 3, zero.print = "<0.001") |>
  formatSignif(.pick("GLR"), digits = 3)
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
