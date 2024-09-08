# not used
jellyBttn = function(id, label = NULL, height = 30, margin = "0 0 3px 0") {
  w = actionBttn(id, label, style = "gradient", color = "warning", size = "sm", block = T)
  style = sprintf("height: %dpx; line-height: %dpx; margin: %s;", height, height, margin)
  style = paste(style, "font-size: small; font-weight: bold; padding: 0 5px !important;")
  w$attribs$style = style
  w
}

quickAction = function(id, label)
  actionButton(id, label, size = "sm", status = "warning", width = "100%",
               style = "padding-left:0; padding-right:0; font-weight: bolder;")

slimTextInput = function(id, label = NULL, height = 25, value = "", ...) {
  if(is.null(value) || length(value) != 1 || is.na(value))
    value = ""
  w = textInput(id, label = label, value = value, ...)
  w$attribs$style = "margin:0"
  w$children[[2]]$attribs$style = sprintf(
    "height: %dpx; line-height: %dpx; padding: 0 1px 0 5px", height, height)
  w
}

slimTextOutput = function(id, height = 25, col = "white") {
  w = textOutput(id)
  style = "overflow: hidden; text-overflow: ellipsis; white-space: nowrap; padding: 0 1px 0 5px;"
  style2 = sprintf("height: %dpx; line-height: %dpx; background-color: %s", height, height, col)
  w$attribs$style = paste(style, style2)
  w
}

slimCheck = function(id, height = 25) {
  w = checkboxInput(id, label = NULL)
  w$attribs$style = sprintf("height: %dpx; line-height: %dpx; margin:0;", height, height)
  w$children[[1]]$children[[1]]$attribs$style = "margin:0"
  w
}

# p() for single line of given height
simplePar = function(text, height) {
  w = p(text)
  w$attribs$style = sprintf(
    "height: %dpx; line-height: %dpx; margin:0; padding: 0 3px;", height, height)
  w
}

updateAlias = function(session, am = NULL, pm = NULL) {

  for(i in seq_along(am))
    updateTextInput(session, paste0("alias_am", i), value = am[i])

  for(i in seq_along(pm))
    updateTextInput(session, paste0("alias_pm", i), value = pm[i])
}
