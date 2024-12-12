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
  if(idpref == "am")
    calcBtn = div(class = "aligned-row",
                  div(checkboxInput("acrossComps", HTML("Across<br>components"), width = "auto"), style = "font-size: small;"),
                  calcBtn)

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
  textStyle = paste(textStyle, sprintf(
    "height: %dpx; padding: 0 1px 0 5px", height, height))
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
  w$children[[1]]$children[[1]]$attribs$style = "margin:0"
  w
}

# Not used? # p() for single line of given height
simplePar = function(text, height, style = NULL) {
  w = p(text)
  style1 = sprintf("height: %dpx; line-height: %dpx; margin:0; padding: 0 3px;",
                   height, height)
  w$attribs$style = paste(style1, style)
  w
}


# not used
jellyBttn = function(id, label = NULL, height = 30, margin = "0 0 3px 0") {
  w = actionBttn(id, label, style = "gradient", color = "warning", size = "sm", block = T)
  style = sprintf("height: %dpx; line-height: %dpx; margin: %s;", height, height, margin)
  style = paste(style, "font-size: small; font-weight: bold; padding: 0 5px !important;")
  w$attribs$style = style
  w
}

