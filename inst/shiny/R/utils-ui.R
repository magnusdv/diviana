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

confirmModal = function(message, backId, confirmId, confirmLabel = "Yes, replace",
                         status = "danger") {
  mod = modalDialog(
    message,
    footer = div(style = "display:flex;justify-content:center;gap:8px;width:100%",
                 actionButton(backId, "No, go back"),
                 actionButton(confirmId, confirmLabel, status = status))
  )
  mod$children[[1]]$attribs$class = paste(
    mod$children[[1]]$attribs$class, "modal-dialog-centered")
  mod$children[[1]]$attribs$style = "max-width:420px; top:-15vh;"
  mod
}


# Not used
textInput2 = function(inputId, value) {
  w  = textInput(inputId, label = NULL, value = value, width = "100%")
  w$children[[2]]$attribs[["style"]] = "padding-top: 1px; padding-bottom: 1px; height: 24px;"
  w
}


inlineRadioButtons = function(inputId, label, ...) {
  radios = radioButtons(inputId, label = NULL, inline = TRUE, ...)
  div(class = "aligned-row", strong(label, style = "padding-right:20px"), radios)
}

# Shared missing-person labels for dataset loading and AM pedigree import
missingLabelRadios = function(id, selected = "M", action = "loading") {
  div(style = "margin:12px 0 8px",
      radioButtons(id, "Naming scheme for missing persons",
        choiceNames = list(
          tagList(
            span("M1, M2, ...", style = "display:inline-block;width:100px"),
            tags$small("(sequential)", class = "text-muted")),
          tagList(
            span("[Family]-MP", style = "display:inline-block; width:100px"),
            tags$small("(family name prefix)", class = "text-muted"))
        ),
        choiceValues = c("M", "F"),
        selected = selected),
      div(class = "text-muted small", style = "font-style: italic; margin-top: 3px;",
          sprintf("Note: Labels can be changed after %s.", action)))
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
