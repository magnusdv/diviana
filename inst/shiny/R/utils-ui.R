errModal = function(..., html = FALSE) {
  args = list(...)
  if(length(args) == 1 && inherits(args[[1]], "condition")) {
    mess = conditionMessage(args[[1]])
    if(grepl("reduce cex", mess))
      mess = "Plot region is too small"
  }
  else
    mess = paste(lapply(args, toString), collapse = "")
  if(html)
    mess = HTML(mess)
  showModal(modalDialog(mess, easyClose = TRUE))
}

showErr = function(..., html = TRUE) {
  args = list(...)
  if(length(args) == 1 && inherits(args[[1]], "condition"))
    mess = conditionMessage(args[[1]])
  else
    mess = paste(lapply(args, toString), collapse = "")
  if(html)
    mess = HTML(mess)
  showNotification(mess, type = "error")
}


# Not used
textInput2 = function(inputId, value) {
  w  = textInput(inputId, label = NULL, value = value, width = "100%")
  w$children[[2]]$attribs[["style"]] = "padding-top: 1px; padding-bottom: 1px; height: 24px;"
  w
}


# Wrapper for the three triangle cards
triangleCard = function(title, idpref) {
  id = paste0(idpref, c("kappa", "triangle", "table"))
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
    DT::DTOutput(id[3]))
}


inlineRadioButtons = function(inputId, label, ...) {
  radios = radioButtons(inputId, label = NULL, inline = TRUE, ...)
  div(class = "aligned-row", strong(label, style = "padding-right:20px"), radios)
}

helpBtn = function(id) {
  actionButton(id, label = NULL, icon = icon("question-circle"),
               style = "padding: 0; margin:0; background: transparent; border: none; cursor: pointer; font-size: 20px;")
}


slimTextInput = function(id, label = NULL, value = "", height = 25, style = NULL, labelStyle = NULL,
                         textStyle = NULL, ...) {
  if(is.null(value) || length(value) != 1 || is.na(value))
    value = ""
  w = textInput(id, label = label, value = value, ...)
  w$attribs$style = paste("margin:0;", style)

  # Label
  w$children[[1]]$attribs$style = labelStyle

  # Text field
  textStyle = paste(textStyle, sprintf("height: %dpx; padding: 0 1px 0 5px", height))
  w$children[[2]]$attribs$style = textStyle

  w
}

# Not used?
slimTextOutput = function(id, height = 25, col = "white") {
  w = textOutput(id)
  style = "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; padding: 0 1px 0 5px;"
  style2 = sprintf("height: %dpx; line-height: %dpx; background-color: %s", height, height, col)
  w$attribs$style = paste(style, style2)
  w
}

# Not used?
slimCheck = function(id, height = 25) {
  w = checkboxInput(id, label = NULL)
  w$attribs$style = sprintf("height: %dpx; line-height: %dpx; margin:0;", height, height)
  w$children[[1]] $children[[1]]$attribs$style = "margin:0"
  w
}

myIcon = function(name, height = 1, align = "middle") {
  tags$img(src = sprintf("icons/%s.svg", name),
           style = sprintf("height:%gem; width:auto; vertical-align:%s;", height, align))
}

# not used
jellyBttn = function(id, label = NULL, height = 30, margin = "0 0 3px 0") {
  w = actionBttn(id, label, style = "gradient", color = "warning", size = "sm", block = T)
  style = sprintf("height: %dpx; line-height: %dpx; margin: %s;", height, height, margin)
  style = paste(style, "font-size: small; font-weight: bold; padding: 0 5px !important;")
  w$attribs$style = style
  w
}

freqRadios = function(id) {

  norstr = paste("norSTR:", c("Africa", "Europe", "Norway"))

  tagList(
    tags$style(HTML("
      .freq-radios .shiny-input-container { margin-bottom: 1rem; }
      .freq-radios .control-label { margin-bottom: 0;}
      .freq-radios label { margin-bottom: 0; }
    ")),
    tags$div(
      id = id,
      class = "form-group shiny-input-radiogroup freq-radios",
      style = "display:grid; grid-template-columns:auto 1fr; gap:0 16px; align-items:center;",

      tags$input(type = "radio", name = id, value = "builtin", checked = "checked"),
      selectInput("builtinDB", "Builtin", c(norstr, "NorwegianFrequencies (legacy)")),

      tags$input(type = "radio", name = id, value = "custom"),
      fileInput("customDB", "Custom"),

      tags$input(type = "radio", name = id, value = "original"),
      tags$div(tags$label("Original"), textOutput("database_famname"))
    )
  )
}

mutRadios = function(id) {
  tagList(
    tags$style(HTML("
      .mut-radios label { margin-bottom: 0; }
      .mut-radios .standard-rate { position: relative; flex: 1; min-width: 0; }
      .mut-radios .standard-rate .shiny-input-container { margin-bottom: 0;}
      .mut-radios .standard-rate input { height: 24px; padding: 2px 0 2px 6px; }
      .mut-radios .standard-rate .tinylabel {
        position: absolute; top: -14px; font-size: smaller; line-height: 1; text-align: center; width: 100%;
      }
      .mut-radios .bttn-jelly { padding:0 2px; }
    ")),
    tags$div(
      id = id,
      class = "form-group shiny-input-radiogroup mut-radios",
      style = "display:grid; grid-template-columns:auto 1fr; gap:20px 16px; align-items:center;",

      tags$input(type = "radio", name = id, value = "none", checked = "checked"),
      tags$div(tags$label("No model")) |> wrap_tooltip("mutradio_none"),

      tags$input(type = "radio", name = id, value = "standard"),
      tags$div(
        style = "display:flex; align-items:center; gap:16px; white-space:nowrap;",
        tags$label("Standard")|> wrap_tooltip("mutradio_standard"),
        tags$div(
          style = "display:flex; align-items:center; gap:8px; flex:1; min-width:0; margin-top:4px",
          tags$div(
            class = "standard-rate",
            tags$div("Female", class = "tinylabel"),
            numericInput("mutrateF", NULL, width = "100%", min = 0, max = 1, value = 0.001)
          ),
          tags$div(
            class = "standard-rate",
            tags$div("Male", class = "tinylabel"),
            numericInput("mutrateM", NULL, width = "100%", min = 0, max = 1, value = 0.002)
          ),
          actionBttn("mutApplyAll", label = tagList(myIcon("play", align = "-0.1em")), style = "jelly", size = "s") |>
          wrap_tooltip("mutApplyAll")
        )
      ),
      tags$input(type = "radio", name = id, value = "original"),
      tags$div(tags$label("Original")) |> wrap_tooltip("mutradio_original")
    )
  )
}
