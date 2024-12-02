
importUI = function(id, title = paste(id, "data")) {
  ns = NS(id)
  bs4Card(width = NULL, title = title, collapsible = FALSE,
    DT::DTOutput(ns("mainTable"), height = "300px"),
    shiny::uiOutput(ns("sourcefield")),
    footer = div(class = "btn-group",
      actionButton(ns("loadButton"), label = tagList(icon("file-upload"), "Import")),
      actionButton(ns("editButton"), label = tagList(icon("edit"), "Edit")),
      actionButton(ns("aliasButton"), label = tagList(icon("user-edit"), "Aliases"))
)

  )
}

importServer = function(id, externalData = reactiveVal(NULL)) {

  moduleServer(id, function(input, output, session) { # print(id)
    ns = session$ns

    # Reactives to be returned to main app
    data = reactiveVal(NULL)
    sources = reactiveVal(NULL)

    # In-module reactives
    dfRaw = reactiveVal(NULL)
    currentfile = reactiveVal(NULL)

    dfFiltered = reactive({
      df = req(dfRaw())
      if(input$include_hidden)
        return(df)

      blanks = rowSums(df != "/", na.rm = TRUE) == 0
      controls = grepl("C[+-]", rownames(df))
      ladder = grepl("Ladder", rownames(df))

      df[!(blanks | controls | ladder), , drop = FALSE]
    })

    observeEvent(externalData(), { #print("extern")
      df = externalData() |> processData(id = id)
      data(df)
      sources("Example")
      }, ignoreNULL = TRUE)

    observeEvent(input$loadButton, {
      dfRaw(NULL) # reset each time

      showModal(modalDialog(
        title = div("Data import", uiOutput(ns("banner")),
                    style = "display: flex; justify-content: space-between; align-items: center;"),
        size = "l",
        fluidRow(
          column(4,
            pickerInput(ns("filetype"), label = NULL, width = "100%", selected = "Genemapper",
                        choices = c("Familias", "Genemapper", "Text file", "dviData (.RData/.rds)"),
                        options = pickerOptions(style = "btn-outline-secondary"))),
          column(8,
            fileInput(ns("file"), NULL, width = "100%",
                      accept = c(".csv", ".tsv", ".txt", ".fam", ".rds", ".RData"))),
        ),
        p("Select rows (or none to import all):",
          style = "font-weight:bold; margin-bottom:0"),
        DT::DTOutput(ns("previewTable"), height = "300px"),
        checkboxInput(ns("include_hidden"), "Include blanks and control samples", width = "100%"),
        selectizeInput(ns("exclude_cols"), "Columns to be excluded:", width = "100%",
                       choices = character(), multiple = TRUE),

        footer = tagList(div(style = "width:100%; display:flex; align-items:center;justify-content: space-between; flex-wrap: nowrap;",
          radioButtons(ns("action"), NULL, inline = TRUE,
                       choices = c("Add to existing data" = "add", "Replace all" = "replace")),
          div(modalButton("Cancel"), actionButton(ns("save"), "Save"))))
      ))
    })

    observeEvent(input$file, {
      req(input$file)
      path = input$file$datapath

      raw = NULL
      tryCatch(switch(input$filetype,
        Familias = { raw = dvir::familias2dvir(path, missingFormat = "M[FAM]-[IDX]")},
        Genemapper =  { raw = readGenemapper(path)},
        "Text file" = { raw = read.table(path, header=TRUE)},
        "dviData (.RData/.rds)" = { raw = readRdvi(path) }
      ), error = errModal, warning = errModal)

      currentfile(input$file$name)

      # Move AMEL to front
      if("AMEL" %in% names(raw))
        raw = raw[c("AMEL", setdiff(names(raw), "AMEL"))]

      dfRaw(raw)

      # Update column exclusion list
      cls = names(raw)
      deflt = c("Yindel", grep("^DY", cls, value = TRUE))
      updateSelectizeInput(session, "exclude_cols", choices = cls, selected = deflt)
    })

    output$previewTable = DT::renderDT(genoDT(dfFiltered(), sel = "multiple"))

    observeEvent(input$save, {
      df = dfFiltered()
      rowsel = input$previewTable_rows_selected
      excludeCols = input$exclude_cols

      if(!is.null(rowsel))
        df = df[sort(rowsel), , drop = FALSE]

      if(length(excludeCols))
        df = df[, !names(df) %in% excludeCols, drop = FALSE]

      df = processData(df, id)

      newdat = if(input$action == "replace") df else rbindSafe(data(), df)
      data(newdat)

      src = if(input$action == "Replace") currentfile() else c(sources(), currentfile())
      sources(src)

      removeModal()
    })

    output$mainTable = DT::renderDT({
      dat = req(data())
      if(identical(dat$Alias, rownames(dat)))
        dat$Alias = NULL
      genoDT(dat)
    })

    output$sourcefield = renderUI({
      # if(is.null(sources()))        return()
      src = unlist(lapply(req(sources()), function(s) as.character(em(s))))
      HTML(paste0(c("Sources: ", src), collapse = if(length(src) == 1) " " else "<br/>"))
    })

    # Edit -------------------------------------------------------------------

    editdata = reactiveVal(NULL)

    observeEvent(input$editButton, {
      editdata(req(data()))
      showModal(modalDialog(
        title = "Edit mode", size = "l",
        p("Edit the table by double clicking on the cells. Press 'Save' to confirm changes."),
        DT::DTOutput(ns("editTable"), height = "300px"),
        footer = tagList(modalButton("Cancel"), actionButton(ns("editSave"), "Save"))
      ))
    })

    output$editTable = DT::renderDT(genoDT(editdata(), editable = TRUE))

    # Preserve manually entered aliases
    observeEvent(input$editTable_cell_edit, {
      dat = editdata()
      edit = input$editTable_cell_edit
      print(edit)
      i = edit$row
      j = edit$col
      val = edit$value
      if(names(dat)[j] == "Sex" && !val %in% c("F", "M", "?")) {
        showErr("Illegal value")
        return()
      }
      dat[i,j] = val
      editdata(dat)
    })

    observeEvent(input$editSave, {
      data(editdata())
      removeModal()
    })

    # Alias -------------------------------------------------------------------

    aliasInputs = reactiveValues(remove = "", keep = "", prefix = "")
    origs = reactiveVal(NULL)
    aliasErrorMsg = reactiveVal("")

    observeEvent(input$aliasButton, {
      origs(rownames(req(data())))

      showModal(modalDialog(
        title = "Generate Aliases",
        fluidRow(
          column(6,
            radioButtons(ns("aliasMethod"), "Choose method:",
                         choices = list("Sequence with prefix" = "prefix",
                                        "Remove strings" = "remove",
                                        "Extract string" = "extract"),
                         selected = "prefix")
          ),
          column(6,
            uiOutput(ns("conditional_inputs")),
            uiOutput(ns("aliasErrorUI"))
          )
        ),
        DT::dataTableOutput(ns("aliasTable")),
        footer = tagList(modalButton("Cancel"), actionButton(ns("aliasSave"), "Save"))
      ))
    })

    output$aliasTable = DT::renderDT({
      df = data.frame(Original = origs(), Alias = generateAliases())
      aliasDT(df)
    })

    generateAliases = reactive({
      origs = req(origs())
      aliasErrorMsg("")
      method = input$aliasMethod
      inp = input[[method]]
      if(is.null(inp) || inp == "")
        return(origs)

      res = origs
      if(method == "prefix") {
        res = paste0(inp, seq_along(origs))
      }
      else tryCatch({ # catch invalid regexpr
        if(method == "remove") {
          pattern = strsplit(inp, ",")[[1]] |> paste(collapse = "|")
          res = gsub(pattern, "", origs)
        }
        else if(method == "extract") {
          r = regexpr(inp, origs)
          al = origs
          al[r > -1] = regmatches(origs, r)
          res = al
        }
      },
        warning = function(w) NULL,
        error = function(e) aliasErrorMsg("Invalid regular expression")
      )
      if(any(res == "") || anyDuplicated.default(res) )
        aliasErrorMsg("Empty or duplicated aliases")
      res
    })

    output$conditional_inputs = renderUI({
      ns = session$ns
      switch(input$aliasMethod,
             prefix = textInput(ns("prefix"), "Prefix:", value = switch(id, PM = "V", AM = "R", "ID")),
             remove = textInput(ns("remove"), "Strings (separate w/comma):"),
             extract = textInput(ns("extract"), "Regex pattern to match:"))
    })

    output$aliasErrorUI = renderUI({
      msg = aliasErrorMsg()
      if (msg != "") tags$div(style = "color: red;", msg)
    })

    observeEvent(input$aliasSave, {
      dat = data()
      dat$Alias = generateAliases()
      data(dat)
      removeModal()
    })

    # End alias ---------------------------------------------------------------

    # Show banner with warning on shinyapps.io
  output$banner = renderUI({
    #isShinyAppsIO = grepl("shinyapps.io", session$clientData$url_hostname)
    #if(!sShinyAppsIO) {
      div(style = "display:flex; align-items: center; background-color: #fff3cd; border-radius: 10px; text-align: center; line-height: 90%; font-size:60%",
          icon("circle-exclamation", style = "margin: 6px; color:red; font-size: 20px;"),
          div(HTML("Avoid uploading<br>sensitive data online!")),
          icon("circle-exclamation", style = "margin: 6px; color:red; font-size: 20px;")
      )
      #}
    })

    # Return reactive variables to main app
    list(data = reactive(data()), sources = reactive(sources()))
  })

}


# Utils -------------------------------------------------------------------


# Format DT tables
genoDT = function(dat, sel = "none", scrollY = "250px", editable = FALSE) {

  dt = DT::datatable(
    dat,
    class = "stripe hover nowrap compact",
    colnames = c("Sample" = 1),
    plugins = "natural",
    selection = sel,
    editable = editable,
    options = list(
      dom = 't', scrollX = TRUE, scrollY = scrollY, paging = FALSE,
      columnDefs = list(list(type = "natural", targets = 0:1),
                        list(orderable = TRUE, targets = 0:1),
                        list(orderable = FALSE, targets = "_all"))
    ))

    dt = DT::formatStyle(dt, names(dat), target = "row", lineHeight = "75%")

    if("Sex" %in% names(dat))
      dt = DT::formatStyle(dt, "Sex", borderRight = '1px solid #ccc',
                           color = DT::styleEqual(c("F", "M"), c("hotpink", "steelblue")))
    dt
}

aliasDT = function(dat, scrollY = "250px") {
  res = DT::datatable(dat, rownames = FALSE, selection = "none",
                      class = "stripe hover nowrap compact",
                      options = list(dom = 't', scrollX = TRUE,
                                     scrollY = scrollY, paging = FALSE))
  res = DT::formatStyle(res, names(dat), target = "row", lineHeight = "75%")

  # Highlight empty or duplicated aliases
  bad = c("", unique.default(dat$Alias[duplicated.default(dat$Alias)]))
  cols = c("lightgray", "pink", "lightblue", "yellow", "lightgreen", "orange")[seq_along(bad)]
  DT::formatStyle(res, "Alias", backgroundColor = DT::styleEqual(bad, cols))
}

# Check Sex column, or infer from AMEL
checkSex = function(x) {
  x[x %in% c("", NA, 0)] = "?"
  x[x == 1] = "M"
  x[x == 2] = "F"
  bad = !x %in% c("F", "M", "?")
  if(any(bad)) {
    warning(paste("Illegal values in the Sex column:", toString(unique(sx[bad]))))
    x[bad] = "?"
  }
  x
}

inferSex = function(df) {
  if("AMEL" %in% names(df))
    sx = c("?", "M", "F")[amel2sex(df$AMEL) + 1]
  else
    sx = rep("?", nrow(df))
  sx
}

processData = function(df, id) {
  if(is.matrix(df))
    df = as.data.frame(df)

  if(!"Alias" %in% names(df))
    df$Alias = rownames(df)

  if("Sex" %in% names(df))
    df$Sex = checkSex(df$Sex)
  else if("AMEL" %in% names(df))
    df$Sex = c("?", "M", "F")[amel2sex(df$AMEL) + 1]
  else
    df$Sex = "?"

  # Put 'Alias' and 'Sex' first
  df[, c("Alias", "Sex", setdiff(names(df), c("Alias", "Sex", "AMEL"))), drop = FALSE]
}

readRdvi = function(fil) {
  switch(tolower(tools::file_ext(fil)),
    rds = {
      content = readRDS(fil)
      if(!inherits(content, "dviData"))
        stop2("File does not contain a `dviData` object")
      dvi = content
    },
    rdata = {
      load(fil, envir = (env <- new.env()))
      objs = mget(ls(envir = env), envir = env)
      dviDatas = Filter(function(x) inherits(x, "dviData"), objs)
      if(!length(dviDatas))
        stop2("No `dviData` objects found in this .RData file")
      dvi = dviDatas[[1]]
    },
    stop2("Illegal file extension. Accepted types are .RData and .rds")
  )
}

# Test app ------------------------------------------------------------

of = sys.frame(1)$ofile
if(!is.null(of) && basename(of) == "module-import.R") { cat("Test app for import module\n")
  library(bs4Dash)
  ui = bs4DashPage(dark = NULL, help = NULL,
    bs4DashNavbar(status = "info", title = "test import"),
    bs4DashSidebar(disable = TRUE, minified = FALSE),
    bs4DashBody(fluidRow(column(width = 6, importUI("PM")),
                         column(width = 6, importUI("AM"))))
  )

  server = function(input, output, session) {
    externalData = reactiveVal(data.frame(a=1:3, b=4:6, row.names = c("A", "B", "C")))
    importA = importServer("PM", externalData)
    importB = importServer("AM")
  }

  shinyApp(ui = ui, server = server)
}
