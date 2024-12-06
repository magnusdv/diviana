
dataUI = function(id, title = paste(id, "data")) {
  ns = NS(id)
  bs4Card(width = NULL, title = title, collapsible = FALSE,
    DT::DTOutput(ns("mainTable"), width = "auto"),#height = "300px"),
    br(),
    shiny::uiOutput(ns("sourcefield")),
    footer = div(class = "btn-group",
      actionButton(ns("importButton"), label = tagList(icon("file-upload"), "Import")),
      actionButton(ns("editButton"), label = tagList(icon("edit"), "Edit")),
      actionButton(ns("aliasButton"), label = tagList(icon("user-edit"), "Aliases"))
    )
  )
}

dataServer = function(id, externalData = reactiveVal(NULL), .debug = NULL) {
  #if(is.null(.debug))
  #  .debug = function(...) NULL

  moduleServer(id, function(input, output, session) { .debug("data server", id)
    ns = session$ns

    # Main reactives
    mainTable = reactiveValues(raw = NULL, main = NULL)
    completeDvi = reactiveValues(raw = NULL, import = NULL)
    sources = reactiveValues(current = NULL, all = NULL)

    observeEvent(externalData(), { .debug("import module: external data")
      mainTable$main = externalData() |> prepareGenoDf()
      completeDvi$raw = completeDvi$import = NULL
      }, ignoreNULL = TRUE)


    # Import ------------------------------------------------------------------

    observeEvent(input$importButton, {
      # Reset each time (but not main table)
      mainTable$raw = completeDvi$raw = completeDvi$import = NULL
      fileError(NULL)

      showModal(modalDialog(
        title = div(class = "aligned-row-wide", paste("Data import:", id), uiOutput(ns("banner"))),
        size = "l",
        fluidRow(
          column(4,
            pickerInput(ns("filetype"), label = NULL, width = "100%", selected = "fam",
                        choices = c(Familias = "fam", Genemapper = "gm",
                                    `Text file` = "txt",
                                    `dviData (.RData/.rds)` = "rdata"),
                        options = pickerOptions(style = "btn-outline-secondary"))),
          column(8,
            fileInput(ns("file"), NULL, width = "100%",
                      accept = c(".csv", ".tsv", ".txt", ".fam", ".rds", ".RData"))),
        ),
        textOutput(ns("instructions")),
        uiOutput(ns("fileError")),

        # Conditional panel showing complete DVI info
        uiOutput(ns("dviContent")),

        # Conditional panel showing raw sample genotypes
        uiOutput(ns("tableContent")),

        footer = tagList(div(class = "aligned-row-wide",
          radioButtons(ns("action"), NULL, inline = TRUE,
                       choices = c("Replace existing data" = "replace", "Add" = "add")),
          div(modalButton("Cancel"), actionButton(ns("importSave"), "Save"))))
      ))
    })

    observeEvent(input$filetype, { .debug("filetype", input$filetype)
      mainTable$raw = completeDvi$raw = NULL
      fileError(NULL)

      instr = switch(input$filetype,
        fam = "This import feature is designed for files exported from the DVI module of Familias.",
        gm = "This supports Genemapper files in 'tall format', i.e., with one genotype per line.",
        txt = "Tab-separated text files.",
        rdata = "Use this to import `dviData` objects created in R (with the dvir package) saved as either Rdata or rds."
      )
      output$instructions = renderText(instr)
    })

    fileError = reactiveVal(NULL)
    output$fileError = renderUI(
      div(style = "color: red", HTML(paste("<b>Error:</b>", req(fileError())))))

    # Conditional panel with DVI info
    output$dviContent = renderUI({
      req(completeDvi$raw)
      tagList(
        fluidRow(
          column(6, uiOutput(ns("dviInfo"))),
          column(6, style = "padding: 20px",
                 radioButtons(ns("importWhat"), label = "What do you want to import?",
                              choiceValues = c("everything", "samples"),
                              choiceNames = c("Everything", paste(id, "only"))))
        )
      )
    })

    # conditional panel of raw sample genotypes
    output$tableContent = renderUI({
      req(mainTable$raw)
      tagList(
        em("If you only want to import some of the samples, select them before clicking Save."),
        DT::DTOutput(ns("previewTable")),
        if(input$filetype == "gm") {
          div(style = "margin:5px 0;",
              checkboxInput(ns("includeHidden"), width = "100%",
                            "Include blanks, controls and ladder samples"))
        },
        div(class = "aligned-row-wide", style = "margin-top: 5px",
            p("Exclude columns:",
              style = "white-space: nowrap; font-weight: bolder; margin:0; padding-right:10px"),
            selectizeInput(ns("excludeCols"), NULL, width = "100%",
                           choices = character(), multiple = TRUE)))
    })

    output$dviInfo = renderUI({ .debug("dvi info")
      raw = req(completeDvi$raw)
      info = capture.output(print(raw, printMax = 0))
      info[1] = "<b>Contents of DVI dataset</b>"
      s = paste(info, collapse = "\n")
      s = HTML(gsub("\n ", "\n &bull; ", s))
      div(s, style = paste(
        "white-space: pre-wrap; color: RoyalBlue; line-height: 1.2em; padding: 5px;",
        "margin: 5px 0; background-color: #f9f9f9; border: 1px solid #ccc;"))
    })

    observeEvent(input$importWhat, { .debug("importWhat", input$importWhat)
      if(input$importWhat == "samples") {
        s = switch(id, AM = completeDvi$raw$am, PM = completeDvi$raw$pm)
        mainTable$raw = getGenotypesAndSex(s)
      }
      else
        mainTable$raw = NULL
    })

    observeEvent(input$file, { .debug("import file");
      req(input$file)
      path = input$file$datapath
      fileError(NULL)

      rawdvi = rawtable = NULL
      tryCatch(switch(input$filetype,
        fam   = { rawdvi = dvir::familias2dvir(path, missingFormat = "M[FAM]-[IDX]")},
        rdata = { rawdvi = readRdvi(path)},
        gm    = { rawtable = readGenemapper(path)},
        txt   = { rawtable = readGenoFromTxt(path)},
      ),
      # warning = showErr, TODO!
      error = function(e) fileError(conditionMessage(e)))

      if(!is.null(rawtable)) {
        cls = names(rawtable)
        if("AMEL" %in% cls)
          rawtable = rawtable[c("AMEL", setdiff(cls, "AMEL"))]
        mainTable$raw = rawtable
      }
      else if(!is.null(rawdvi))
        completeDvi$raw = rawdvi

      sources$current = input$file$name
    })

    # Update column exclusion list (default: Y stuff)
    observeEvent(mainTable$raw, {
      cls = names(mainTable$raw)
      updateSelectizeInput(session, "excludeCols", choices = cls,
                           selected = c("Yindel", grep("^DY", cls, value = TRUE)))
    })

    # Genemapper: Remove blanks, controls and ladder samples
    dfFiltered = reactive({ .debug("import filter genemapper/text blanks")
      df = req(mainTable$raw)
      if(input$filetype == "gm" && !input$includeHidden) {
        blanks = rowSums(df != "/", na.rm = TRUE) == 0
        controls = grepl("C[+-]", rownames(df))
        ladder = grepl("Ladder", rownames(df))
        df = df[!(blanks | controls | ladder), , drop = FALSE]
      }
      df
    })

    output$previewTable = DT::renderDT(genoDT(dfFiltered(), sel = "multiple"))

    observeEvent(input$importSave, { .debug("import save")
      if(!is.null(completeDvi$raw) && input$importWhat == "everything") {
        mainTable$main = NULL
        completeDvi$import = completeDvi$raw
      }
      else {
        df = prepareGenoDf(dfFiltered(),
                           selectRows = input$previewTable_rows_selected,
                           excludeCols = input$excludeCols)
        newdat = if(input$action == "replace") df else rbindSafe(mainTable$main, df)
        mainTable$main = newdat
        completeDvi$import = NULL
      }

      sources$all = if(input$action == "replace") sources$current else c(sources$all, sources$current)
      removeModal()
    })

    output$mainTable = DT::renderDT({ .debug("render main table", id)
      dat = req(mainTable$main)
      genoDT(dat)
    })

    output$sourcefield = renderUI({
      src = unlist(lapply(req(sources$all), function(s) as.character(em(s))))
      HTML(paste0(c("Sources: ", src), collapse = if(length(src) == 1) " " else "<br/>"))
    })

    # Edit -------------------------------------------------------------------

    editdata = reactiveVal(NULL)

    observeEvent(input$editButton, {
      editdata(req(mainTable$main))
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
      mainTable$main = editdata()
      removeModal()
    })

    # Alias -------------------------------------------------------------------

    origs = reactiveVal(NULL)
    aliasInputs = reactiveValues(remove = "", keep = "", prefix = "")
    aliasErrorMsg = reactiveVal("")

    observeEvent(input$aliasButton, {
      origs(rownames(req(mainTable$main)))

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

    generateAliases = reactive({ .debug("generate aliases")
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

    output$conditional_inputs = renderUI({ .debug("conditional alias input")
      ns = session$ns
      switch(input$aliasMethod,
             prefix = textInput(ns("prefix"), "Prefix:", value = switch(id, PM = "V", AM = "R", "ID")),
             remove = textInput(ns("remove"), "Strings (separate w/comma):"),
             extract = textInput(ns("extract"), "Regex pattern to match:"))
    })

    output$aliasErrorUI = renderUI({ .debug("alias error ui")
      msg = aliasErrorMsg()
      if (msg != "") tags$div(style = "color: red;", msg)
    })

    observeEvent(input$aliasSave, { .debug("alias save")
      dat = mainTable$main
      dat$Alias = generateAliases()
      mainTable$main = dat
      removeModal()
    })

    # End alias ---------------------------------------------------------------

    # Show warning banner. (Set session=session to restrict to shinyapps.io)
    output$banner = renderUI(banner(session = NULL))

    # Return reactive variables to main app
    list(main = reactive(mainTable$main),
         completeDvi = reactive(completeDvi$import),
         sources = reactive(sources$all))
  })

}


# Utils -------------------------------------------------------------------


# Format DT tables
genoDT = function(dat, sel = "none", scrollY = "220px", editable = FALSE) {

  # Main view (not edit): Hide alias column if identical to rownames.
  if(!editable && identical(dat$Alias, rownames(dat)))
    dat$Alias = NULL

  # Prepare DT
  dt = DT::datatable(dat,
    class = "stripe hover nowrap compact",
    colnames = c("Sample" = 1),
    plugins = "natural",
    selection = sel,
    editable = editable,
    options = list(
      dom = 't', scrollX = TRUE, paging = FALSE,
      scrollY = if(nrow(dat)>10) scrollY else NULL,
      columnDefs = list(list(type = "natural", targets = 0:1),
                        list(orderable = TRUE, targets = 0:1),
                        list(orderable = FALSE, targets = "_all"))
    )) |>
    DT::formatStyle(names(dat), target = "row", lineHeight = "75%")

    # Separate and colour Sex column if present
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

# Check sex vector
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

prepareGenoDf = function(df, selectRows = NULL, excludeCols = NULL) {
  if(is.matrix(df))
    df = as.data.frame(df)

  if(!is.null(selectRows))
    df = df[sort(selectRows), , drop = FALSE]

  if(length(excludeCols))
    df = df[, !names(df) %in% excludeCols, drop = FALSE]

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

  dvi
}


banner = function(session) {
  if(!is.null(session)) {
    isShinyAppsIO = grepl("shinyapps.io", session$clientData$url_hostname)
    if(!sShinyAppsIO) return()
  }
  div(class = "aligned-row",
    style = "background-color: #fff3cd; border-radius: 10px; text-align: center; line-height: 90%; font-size:60%",
    icon("circle-exclamation", style = "margin: 6px; color:red; font-size: 20px;"),
    div(HTML("Avoid uploading<br>sensitive data online!")),
    icon("circle-exclamation", style = "margin: 6px; color:red; font-size: 20px;")
  )
}

# Test app ------------------------------------------------------------

of = sys.frame(1)$ofile
if(!is.null(of) && basename(of) == "module-import.R") { cat("Test app for import module\n")
  library(bs4Dash)
  ui = bs4DashPage(dark = NULL, help = NULL,
    bs4DashNavbar(status = "info", title = "test import"),
    bs4DashSidebar(disable = TRUE, minified = FALSE),
    bs4DashBody(fluidRow(column(width = 6, dataUI("PM")),
                         column(width = 6, dataUI("AM"))))
  )

  server = function(input, output, session) {
    externalData = reactiveVal(data.frame(a=1:3, b=4:6, row.names = c("A", "B", "C")))
    importA = dataServer("PM", externalData)
    importB = dataServer("AM")
  }

  shinyApp(ui = ui, server = server)
}
