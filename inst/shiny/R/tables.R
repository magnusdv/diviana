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
  "Inconclusive"    = NA
)

# Text colour + carrier symbols in plot
COLS_FG = c(
  "No match"        = "red",
  "Inconclusive"    = "red"
)

formatResultTable = function(x, style = 6) {
  gt(x) |>
    opt_stylize(style = style) |>
    opt_row_striping() |>
    tab_options(
      #table.font.size = px(12),
      data_row.padding = px(2),
      container.overflow.x = TRUE,
      container.overflow.y = TRUE,
      table.align = "left",
    ) |>
    sub_missing(missing_text = "") |>
    fmt_scientific(c("LR", "GLR"), n_sigfig = 3) |>
    cols_add(colour = COLS_BG[Conclusion]) |>
    cols_hide("colour") |>
    tab_style(
      style = cell_fill(color = from_column(column = "colour", na_value = "white")),
      locations = cells_body(columns = "Conclusion", rows = !is.na(colour))
    ) |>
    tab_style(
      style = cell_text(color = "red"),
      locations = cells_body(columns = "Conclusion", rows = is.na(colour))
    ) |>
    tab_style(style = cell_text(whitespace = "nowrap"),
              locations = cells_body(columns = c("LR", "GLR", "Conclusion", "Comment")))

}
