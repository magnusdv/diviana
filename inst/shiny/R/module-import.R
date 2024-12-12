
importUI = function(id, title = paste("Data import:", id)) {
  ns = NS(id)
  bs4Card(width = NULL, title = title,
    fluidRow(
      column(width = 6, actionButton(ns("load_data"), "Load data")),
      column(width = 6, align = "right", actionButton(ns("aliasButton"), "Alias"))
    ),
    br(),
    DT::DTOutput(ns("mainTable"), height = "300px")
  )
}

importServer = function(id, externalData = reactiveVal(NULL)) {

  moduleServer(id, function(input, output, session) {
    ns = session$ns

    # Reactives to be returned to main app
    data = reactiveVal(NULL)
    alias = reactiveVal(NULL)
    datapath = reactiveVal(NULL)

    # In-module reactives
    dfRaw = reactiveVal(NULL)
    dfFiltered = reactive({
      df = req(dfRaw())
      if(input$include_hidden)
        return(df)

      blanks = rowSums(df != "/", na.rm = TRUE) == 0
      controls = grepl("C[+-]", rownames(df))
      ladder = grepl("Ladder", rownames(df))

      df[!(blanks | controls | ladder), , drop = FALSE]
    })

    observeEvent(externalData(), {
      g = externalData()
      data(g)
      alias(aliasDefault(g, id))
      }, ignoreNULL = TRUE)

    observeEvent(input$load_data, {
      dfRaw(NULL) # reset each time

      showModal(modalDialog(title = "Data import", size = "xl",
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
        DT::DTOutput(ns("data_table"), height = "300px"),
        checkboxInput(ns("include_hidden"), "Include blanks and control samples", width = "100%"),
        selectizeInput(ns("exclude_cols"), "Columns to be excluded:", width = "100%",
                       choices = character(), multiple = TRUE),
        radioButtons(ns("action"), NULL, inline = TRUE,
                     choices = c("Add to existing data", "Replace")),
        footer = tagList(modalButton("Cancel"), actionButton(ns("save"), "Save"))
      ))
    })

    observeEvent(input$file, {
      req(input$file)
      fil = input$file$datapath
      raw = NULL
      tryCatch(switch(input$filetype,
        Familias = {
          raw = dvir::familias2dvir(fil, missingFormat = "M[FAM]-[IDX]")
        },
        Genemapper =  {
          raw = readGenemapper(fil)
        },
        "Text file" = {
          raw = read.table(input$file$datapath, header=TRUE)
        },
        "dviData (.RData/.rds)" = { .debug("-> loading .RData/.rds")
          raw = readRdvi(fil)
        }
      ), error = errModal, warning = errModal)

      datapath(input$file$datapath)
      dfRaw(raw)

      # Update column exclusion list
      cls = names(raw)
      deflt = c("Yindel", grep("^DY", cls, value = TRUE))
      updateSelectizeInput(session, "exclude_cols", choices = cls, selected = deflt)
    })

    output$data_table = DT::renderDT(genoDT(dfFiltered(), sel = "multiple"))

    observeEvent(input$save, {
      df = dfFiltered()
      if(!is.null(rowsel <- input$data_table_rows_selected))
        df = df[sort(rowsel), , drop = FALSE]

      if(length(input$exclude_cols))
        df = df[, !names(df) %in% input$exclude_cols, drop = FALSE]

      newdat = if(input$action == "Replace") df else rbindSafe(data(), df)

      # Move AMEL if needed
      cols = names(newdat)
      if(match("AMEL", names(newdat), nomatch = 0) > 1)
        newdat = newdat[c("AMEL", setdiff(names(newdat), "AMEL"))]

      data(newdat)
      alias(aliasDefault(newdat, id))
      removeModal()
    })

    output$mainTable = DT::renderDT({
      df = req(data())
      df = cbind(Alias = alias()[rownames(df)], df)
      genoDT(df, orderby = 0:1)
    })


    # Alias -------------------------------------------------------------------

    aliasPreview = reactiveVal(NULL)
    aliasInputs = reactiveValues(remove = "", keep = "")

    observeEvent(input$aliasButton, {
      dat = req(data())
      # Initialize preview with current short names or original names
      shorts = alias() %||% aliasDefault(dat, id = "") # this should not happen
      aliasPreview(shorts)

      showModal(modalDialog(
        title = "Generate aliases", size = "l",
        textInput(ns("remove"), "Remove strings (separate by comma):", width = "100%",
          value = aliasInputs$remove),
        textInput(ns("keep"), "Keep only this part (regexp):", width = "100%",
          value = aliasInputs$keep),
        actionButton(ns("preview"), "Preview"),
        DT::DTOutput(ns("aliasTable"), height = "300px"),
        footer = tagList(modalButton("Cancel"), actionButton(ns("aliasSave"), "Save"))
      ))
    })

    observeEvent(input$preview, {
      aliasInputs$remove = input$remove
      aliasInputs$keep = input$keep
      origs = names(req(alias()))
      al = generateAlias(origs, input$remove, input$keep)
      aliasPreview(al)
    })

    output$aliasTable = DT::renderDT({
      al = aliasPreview()
      df = data.frame(Original = names(al), Alias = al)
      aliasDT(df)
    })

    # Preserve manually entered aliases
    observeEvent(input$aliasTable_cell_edit, {
      edit = input$aliasTable_cell_edit
      edit = edit[edit$col == 1, , drop = FALSE]
      if(!nrow(edit))
        return()
      al = aliasPreview()
      al[edit$row] = edit$value
      aliasPreview(al)
    })

    observeEvent(input$aliasSave, {
      alias(aliasPreview())
      removeModal()
    })

    # End alias ---------------------------------------------------------------

    # Return reactive variables to main app
    list(dat = reactive(data()), short = reactive(alias()))
  })

}


# Utils -------------------------------------------------------------------

rbindSafe = function(df1, df2) {
  if(is.null(df1))
    return(df2)
  cols = union(names(df1), names(df2))
  df1[setdiff(cols, names(df1))] = NA
  df2[setdiff(cols, names(df2))] = NA
  rbind(df1[, cols], df2[, cols])
}

# Generate short names
generateAlias = function(names, remove = NULL, keep = NULL) {
  short = names
  if (!is.null(remove) && nzchar(remove) > 0) {
    remove = unlist(strsplit(remove, ",\\s*"))
    for (str in remove)
      short = sub(str, "", short)
  }
  if (!is.null(keep) && nzchar(keep)) {
    matches = regexpr(keep, short)
    has_match = matches != -1
    short[matches != -1] = regmatches(short, matches)
  }
  short
}

# Format DT tables
genoDT = function(dat, sel = "none", scrollY = "250px", orderby = 0) {

  dt = DT::datatable(
    dat,
    class = "stripe hover nowrap compact",
    colnames = c("Sample" = 1),
    plugins = "natural",
    selection = sel,
    options = list(
      dom = 't', scrollX = TRUE, scrollY = scrollY, paging = FALSE,
      columnDefs = list(list(type = "natural", targets = orderby),
                        list(orderable = TRUE, targets = orderby),
                        list(orderable = FALSE, targets = "_all"))
    ))

    dt = DT::formatStyle(dt, names(dat), target = "row", lineHeight = "75%")
    if("AMEL" %in% names(dat)) {
      dt = DT::formatStyle(dt, "AMEL",
        borderRight = '1px solid #ccc',
        fontStyle = 'italic',
        color = DT::styleEqual(c("X/X", "X/Y"), c("red", "blue")))
    }

    dt
}

aliasDT = function(dat, scrollY = "250px") {

  res = DT::datatable(dat, rownames = FALSE, editable = TRUE, selection = "none",
                      class = "stripe hover nowrap compact",
                      options = list(dom = 't', scrollX = TRUE,
                                     scrollY = scrollY, paging = FALSE))
  res = DT::formatStyle(res, names(dat), target = "row", lineHeight = "75%")

  # Highlight duplications in the 'Alias' column
  if(!anyDuplicated.default(alias <- dat$Alias))
    return(res)

  dups = unique.default(alias[duplicated.default(alias)])
  if(length(dups) > 5)
    dups = dups[1:5]
  cols = c("pink", "lightblue", "yellow", "lightgreen", "orange")[1:length(dups)]

  DT::formatStyle(res, "Alias", backgroundColor = DT::styleEqual(dups, cols))
}

# Generate default aliases. PM: V1, V2, ...; AM: R1, R2, ...
aliasDefault = function(dat, id) {
  prefix = switch(id, AM = "R", PM = "V", "")
  origs = rownames(dat)
  paste0(prefix, seq_along(origs)) |> setnames(origs)
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

# Run test app ------------------------------------------------------------

if(interactive()) {

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
