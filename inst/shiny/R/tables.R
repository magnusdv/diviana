formatGenoTable = function(x) {
  gt(x, rownames_to_stub = TRUE) |>
    opt_stylize(style = 6) |>
    opt_row_striping() |>
    tab_options(
      table.font.size = px(12),
      data_row.padding = px(0),
      container.overflow.y = TRUE,
      table.align = "left"
    ) |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_stub())
}

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


formatResultTable = function(x, style = 6, inter = FALSE) {
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
    ) |>
    sub_missing(missing_text = "") |>
    fmt_scientific(c("LR", "GLR"), n_sigfig = 3, exp_style = "e",
                   force_sign_n = TRUE) |>
    #sub_missing(columns = "GLR", missing_text = "-") |>
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
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_body(columns = c("LR", "GLR", "Conclusion", "Comment")))

  tab
}


CPnoplot = function(x, ...) {
  forrel::checkPairwise(x, plotType = "none", verbose = FALSE, ...)
}


formatCP = function(tab, sortby = NULL) {
  # TODO: implement this
  if(is.null(tab) || nrow(tab) == 0)
    return(data.frame("No results to display" = character(0), check.names = FALSE))

  if(ncol(tab) > 1) {
    sortby = if("GLR" %in% names(tab)) tab$GLR else (tab$k1/4 + tab$k2/2)
    tab = tab[order(sortby, decreasing = TRUE), ]
  }

  skipcols = c("N", "kappa0", "kappa1", "kappa2", "relgroup", "pval", "err")
  tab[!names(tab) %in% skipcols]
}
