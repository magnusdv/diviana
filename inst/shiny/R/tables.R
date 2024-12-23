# formatGenoTable = function(x) {
#   gt(x, rownames_to_stub = TRUE) |>
#     opt_stylize(style = 6) |>
#     opt_row_striping() |>
#     tab_options(
#       table.font.size = px(12),
#       data_row.padding = px(0),
#       container.overflow.y = TRUE,
#       table.align = "left"
#     ) |>
#     tab_style(style = cell_text(whitespace = "nowrap"),
#               locations = cells_stub())
# }



# Result tables -----------------------------------------------------------

# Fill colours in table & plot
COLS_BG = c(
  "Undisputed"      = "#61D04F", #palette()[3]
  "Match (GLR)"     = "#61D04F",
  "Symmetric match" = "greenyellow",
  "Probable"        = "orange",
  "Disputed"        = "yellow",
  "Excluded"        = "plum1",
  "Nonidentifiable" = "lightgray",
  "No match"        = NA,
  "Inconclusive"    = NA,
  "Inconclusive GLR"= NA
)

# Text colour + carrier symbols in plot
redText = c("Nonidentifiable", "No match", "Inconclusive")


formatResultTable = function(x, usealias = FALSE, aliasPM = NULL, style = 6) {

  if(is.character(x)) {
    emptyGT = gt(data.frame(Message = x)) |>
      tab_options(
        column_labels.hidden = TRUE,
        table.width = pct(100),
        table.align = "left")
    return(emptyGT)
  }

  if(usealias && !is.null(aliasPM) && !all(is.na(x$Sample)))
    x$Sample = aliasPM[x$Sample]

  tab = gt(x) |>
    #opt_interactive(use_sorting = T, use_compact_mode = T, use_resizers = T,
    #                             use_text_wrapping = F) |>
    opt_stylize(style = style,
                add_row_striping = TRUE) |>
    tab_options(
      data_row.padding = px(2),
      container.overflow.x = TRUE,
      container.overflow.y = TRUE,
      table.align = "left",
      container.width = pct(100),
      table.width = pct(100),
    )

  if(!nrow(x))
    return(tab)

  tab = tab |>
    sub_missing(missing_text = "") |>
    fmt_scientific(c("LR", "GLR"), n_sigfig = 3, exp_style = "e",
                   force_sign_n = TRUE) |>
    fmt_number("LR", decimals = 2,
               rows = LR >= 0.1 & LR < 1000) |>
    fmt_number("GLR", decimals = 2,
               rows = GLR >= 0.1 & GLR < 1000) |>
    cols_add(colour = COLS_BG[Conclusion]) |>
    cols_hide("colour") |>
    tab_style(
      style = cell_fill(color = from_column(column = "colour")),
      locations = cells_body(columns = "Conclusion", rows = !is.na(colour))
    ) |>
    tab_style(
      style = cell_text(color = "red"),
      locations = cells_body(columns = "Conclusion", rows = Conclusion %in% redText)
    ) |>
    tab_style(
      style = cell_text(whitespace = "nowrap"),
      locations = cells_body(columns = c("LR", "GLR", "Conclusion", "Comment")))

  tab
}



# CheckPairwise tables ------------------------------------------------------

CPnoplot = function(x, ...) {
  res = forrel::checkPairwise(x, plotType = "none", verbose = FALSE, nsim = 1000, pvalThreshold = 0.05, ...)
  if(is.null(res))
    res = data.frame()
  res
}


formatCP = function(tab, usealias = FALSE, alias1 = NULL, alias2 = alias1, sortby = NULL) {
  # TODO: fix this. the problem is that CPnotplot returns NULL if less than 2 indivs
  emptyMsg = NULL
  if(is.null(tab))
    emptyMsg = "No calculations to show yet"
  else if(nrow(tab) == 0)
    emptyMsg = "Less than 2 individuals"

  if(!is.null(emptyMsg)) {return(NULL)
    df = as.data.frame(.setnames(list(character(0)), emptyMsg), check.names = FALSE)
    return(df |> .dtstyleCP())
  }

  #if(ncol(tab) > 1) {
  #  sortby = if("GLR" %in% names(tab)) tab$GLR else (tab$k1/4 + tab$k2/2)
  #  tab = tab[order(sortby, decreasing = TRUE), ] # no need for drop = F!
  #}

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

  DT::datatable(df,
                class = "stripe compact nowrap",
                selection = "none",
                width = "100%",
                rownames = FALSE,
                plugins = "natural",
                options = list(dom = "ft", paging = FALSE,
                               language = list(search = "Filter: "),
                               scrollY = scrollY, scrollX = TRUE,
                               columnDefs = list(list(type = "natural", targets = 0:1)))) |>
  DT::formatStyle(names(df), target = "row", lineHeight = "75%") |>
  DT::formatStyle(.pick("pedrel"), fontSize = "80%") |>
  formatRound(.pick("k0", "k1", "k2"), digits = 2) |>
  formatRound(.pick("pval"), digits = 3, zero.print = "<0.001") |>
  formatSignif(.pick("GLR"), digits = 3)
}

# Result matrices (LR/exclusion) ----------------------------------------------

# Shared formatting options
formatMatrix = function(m) {
  nr=nrow(m); nc=ncol(m)
  wraptxt = if(any(nchar(c(rownames(m), colnames(m))) > 7)) NULL else "nowrap"

  m |>
    as.data.frame() |>
    gt(rownames_to_stub = TRUE) |>
    sub_missing(missing_text = "-") |>
    tab_style(
      style = cell_text(weight = "bold", size = "90%", whitespace = wraptxt),
      locations = list(cells_column_labels(), cells_stub())
    ) |>
    tab_style(
      style = list(cell_fill(color = "transparent"),
                   cell_borders(sides = "all", style = "hidden")),
      locations = cells_stubhead()
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = list(cells_body(), cells_column_labels(), cells_stub())
    ) |>
    tab_options(
      table.align = "left",
      table_body.hlines.style = "solid",
      table_body.vlines.style = "solid",
      column_labels.vlines.style = "solid",
      data_row.padding = if(nr > 10) px(2) else if(nr > 20) px(0),
      column_labels.padding.horizontal = if(nc > 10) px(2) else if(nc > 20) px(0),
      table.font.size = if(nr > 20 || nc > 20) "75%" else if(nr > 10 || nc > 10) "90%"
    )
}

formatExclusionMatrix = function(m, maxIncomp = 2, usealias = FALSE,
                                 aliasPM = NULL, transpose = FALSE) {
  if(usealias && !is.null(aliasPM))
    rownames(m) = aliasPM[rownames(m)]

  if (transpose) m = t(m)

  rowsEx = which(apply(m, 1, allAbove, maxIncomp))
  colsEx = which(apply(m, 2, allAbove, maxIncomp))
  naMat = is.na(m)
  naRows = rownames(m)[rowSums(naMat) == ncol(m)]
  naCols = colnames(m)[colSums(naMat) == nrow(m)]

  tbl = formatMatrix(m) |>
    tab_style(
      style = cell_text(color = "red"),
      locations = list(cells_column_labels(colsEx), cells_stub(rowsEx))
    ) |>
    tab_style(
      style = cell_text(color = "gray60"),
      locations = list(cells_column_labels(naCols), cells_stub(naRows))
    ) |>
    tab_options(
      column_labels.background.color = "#E6E6FA",
      stub.background.color = "#FFE5B4"
    )

  # Colour individual entries
  for(cl in colnames(m)) {
    tbl = tbl |>
      tab_style(
        style = cell_text(color = "red"),
        locations = cells_body(rows = m[,cl] > maxIncomp, columns = cl)
      ) |>
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(rows = m[,cl] == 0, columns = cl)
      ) |>
      tab_style(
        style = cell_fill(color = "#E8FBD7"),
        locations = cells_body(rows = m[,cl] > 0 & m[,cl] <= maxIncomp, columns = cl)
      ) |>
      tab_style(
        style = cell_text(color = "gray"),
        locations = cells_body(rows = is.na(m[,cl]), columns = cl)
      )
  }

  tbl
}


formatLRmatrix = function(m, LRthresh = 1e4, usealias = FALSE,
                          aliasPM = NULL, transpose = FALSE) {
  if(usealias && !is.null(aliasPM))
    rownames(m) = aliasPM[rownames(m)]

  if (transpose) m = t(m)

  naMat = is.na(m)
  naRows = rownames(m)[rowSums(naMat) == ncol(m)]
  naCols = colnames(m)[colSums(naMat) == nrow(m)]

  tbl = formatMatrix(m) |>
    tab_style(
      style = cell_text(color = "gray60"),
      locations = list(cells_column_labels(naCols), cells_stub(naRows))
    ) |>
    fmt_auto(-any_of(naCols)) |>
    tab_options(
      column_labels.background.color = "#E6E6FA",
      stub.background.color = "#FFE5B4"
    )

  # Colour individual entries
  for(cl in colnames(m)) {
    tbl = tbl |>
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(rows = m[,cl] >= LRthresh, columns = cl)
      ) |>
      tab_style(
        style = cell_fill(color = "#E8FBD7"),
        locations = cells_body(rows = m[,cl] < LRthresh & m[,cl] >= LRthresh/10, columns = cl)
      ) |>
      tab_style(
        style = cell_text(color = "gray"),
        locations = cells_body(rows = is.na(m[,cl]), columns = cl)
      )
  }

  tbl
}

# Used in the main app: Insert missing row/columns
completeMatrix = function(m, rownames, colnames) {
  rr = setdiff(rownames, rownames(m))
  if(length(rr))
    m = rbind(m, matrix(NA, nrow = length(rr), ncol = ncol(m), dimnames = list(rr, NULL)))

  cc = setdiff(colnames, colnames(m))
  if(length(cc))
    m = cbind(m, matrix(NA, nrow = nrow(m), ncol = length(cc), dimnames = list(NULL, cc)))

  m[rownames, colnames, drop = FALSE]
}

# Utility: Check if all elements exceed a certain value (and not all NA)
allAbove = function(x, val) {
  !all(is.na(x)) && all(x > val, na.rm = TRUE)
}
