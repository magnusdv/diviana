
dataUI = function(id, title = paste(id, "data")) {
  ns = NS(id)
  bs4Card(width = NULL, title = title, collapsible = TRUE, collapsed = FALSE,
    uiOutput(ns("mainTableUI")),
    uiOutput(ns("sourcefield")),
    footer = div(class = "btn-group",
      actionButton(ns("importButton"), label = tagList(myIcon("file-arrow-up", align = "-0.1em"), "Import")),
      actionButton(ns("editButton"), label = tagList(myIcon("edit", align = "-0.1em"), "Edit")),
      actionButton(ns("aliasButton"), label = tagList(icon("user-pen", align = "-0.1em"), "Aliases"))
    )
  )
}

dataServer = function(id, externalData = reactiveVal(NULL), assignedRefs = reactiveVal(),
                      missingIDs = reactiveVal(), .debug = NULL) {

  .debug2 = if(!is.null(.debug)) function(...) .debug(sprintf("-%s data:", id), ...)

  moduleServer(id, function(input, output, session) { .debug2("server")
    ns = session$ns

    # Main reactives
    mainTable = reactiveValues(raw = NULL, main = NULL)
    completeDvi = reactiveValues(raw = NULL, import = NULL)
    sources = reactiveValues(current = NULL, all = NULL)

    observeEvent(externalData(), { .debug2("set external", externalData())
      ext = externalData()

      if(identical(ext, "reset")) {
        mainTable$main = mainTable$raw = completeDvi$raw = completeDvi$import = NULL
        sources$all = sources$current = NULL
        isolate(externalData(NULL)) # TODO
        return()
      }
      else {
        mainTable$main = externalData() |> standardiseGenoData(flavour = id)
        completeDvi$raw = completeDvi$import = NULL
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)


    # Import ------------------------------------------------------------------

    filetypeSaved = reactiveVal(NULL)
    observeEvent(input$importButton, { .debug2("open import UI")
      # Reset each time (but not main table)
      mainTable$raw = completeDvi$raw = completeDvi$import = NULL
      fileError(NULL)

      showModal(modalDialog(
        title = div(class = "aligned-row-wide", paste("Data import:", id), uiOutput(ns("banner"))),
        size = "l",
        fluidRow(
          column(4,
            pickerInput(ns("filetype"), label = NULL, width = "100%",
                        choices = c(Familias = "fam",
                                    Genemapper = "gm",
                                    `Text file` = "txt",
                                    `dviData (.RData/.rds)` = "rdata"),
                        selected = filetypeSaved(),
                        choicesOpt = list(disabled = c(FALSE, FALSE, FALSE, FALSE),
                                          style = c("", "", "", ""), # color: gray !important;
                                          icon = c("", "", "", "")), # "fa-ban"
                        options = pickerOptions(style = "btn-outline-primary",
                                                iconBase = "fas"))),
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
          div(modalButton("Cancel"), actionButton(ns("importSave"), "Save", status = "success"))))
      ))
    })

    observeEvent(input$filetype, { .debug2("filetype =", input$filetype)
      mainTable$raw = completeDvi$raw = NULL
      fileError(NULL)
      filetypeSaved(input$filetype)

      instr = switch(input$filetype,
        fam = "A file exported from the DVI module of Familias.",
        gm = "A genemapper file in 'tall format', i.e., with one genotype per line.",
        txt = "A tab-separated file with one row per individual. Alleles may be split in two columns per marker, or joined as genotypes ('a/b' or 'a,b').",
        rdata = "A `dviData` object created in R with the dvir package, saved as either RData or rds."
      )
      output$instructions = renderText(paste("Expected format:", instr))
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
    output$tableContent = renderUI({ .debug2("update raw table")
      req(mainTable$raw)
      tagList(
        em("If you only want to import some of the samples, select them before clicking Save.",
           style = "color: tomato;"),
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

    output$dviInfo = renderUI({ .debug2("render dvi info")
      raw = req(completeDvi$raw)
      info = capture.output(print(raw, printMax = 0))
      info[1] = "<b>Contents of DVI dataset</b>"
      s = paste(info, collapse = "\n")
      s = HTML(gsub("\n ", "\n &bull; ", s))
      div(s, style = paste(
        "white-space: pre-wrap; color: RoyalBlue; line-height: 1.2em; padding: 5px;",
        "margin: 5px 0; background-color: #f9f9f9; border: 1px solid #ccc;"))
    })

    observeEvent(input$importWhat, { .debug2("importWhat =", input$importWhat)
      if(input$importWhat == "samples") {
        s = switch(id, AM = completeDvi$raw$am, PM = completeDvi$raw$pm)
        mainTable$raw = genosWithAttrs(s, addCols = c("Sample", "Sex"))
      }
      else
        mainTable$raw = NULL
    })

    observeEvent(input$file, { .debug2("import file", input$file$name);
      req(input$file)
      path = input$file$datapath
      fileError(NULL)
      rawdvi = rawtable = NULL
      tryCatch(switch(input$filetype,
        fam   = { rawdvi = dvir::familias2dvir(path, missingFormat = "M[FAM]-[IDX]", verbose = FALSE)},
        rdata = { rawdvi = readRdvi(path)},
        gm    = { rawtable = readGenemapper(path)},
        txt   = { rawtable = readGenoFromTxt(path)},
      ),
      warning = function(e) print(conditionMessage(e)), #showErr, TODO!
      error = function(e) fileError(conditionMessage(e)))

      if(!is.null(rawtable))
        mainTable$raw = rawtable |> moveColsFirst("AMEL")
      else if(!is.null(rawdvi)) {
        if(id == "AM" && !length(rawdvi$am))
          fileError("No AM samples found in this DVI dataset")
        else if(id == "PM" && !length(rawdvi$pm))
          fileError("No PM samples found in this DVI dataset")
        else
          completeDvi$raw = rawdvi
      }

      sources$current = input$file$name
    })

    # Update column exclusion list (default: Y stuff)
    observeEvent(mainTable$raw, {
      cls = names(mainTable$raw)
      updateSelectizeInput(session, "excludeCols", choices = cls,
                           selected = c("Yindel", grep("^DY", cls, value = TRUE)))
    })

    # Genemapper: Remove blanks, controls and ladder samples
    dfFiltered = reactive({ .debug2("import filter genemapper/text blanks")
      df = req(mainTable$raw)
      if(input$filetype == "gm" && !input$includeHidden) {
        blanks = rowSums(df != "/", na.rm = TRUE) == 0
        controls = grepl("C[+-]", rownames(df))
        ladder = grepl("Ladder", rownames(df))
        df = df[!(blanks | controls | ladder), , drop = FALSE]
      }
      df
    })

    output$previewTable = DT::renderDT(previewGenoDT(dfFiltered()))

    observeEvent(input$importSave, { .debug2("import save", input$importWhat)
      if(!is.null(completeDvi$raw) && input$importWhat == "everything") {
        completeDvi$import = completeDvi$raw
        sources$all = sources$current
        removeModal()
        return()
      }
      # If not complete DVI
      df = standardiseGenoData(dfFiltered(), flavour = id,
                               selectRows = input$previewTable_rows_selected,
                               excludeCols = input$excludeCols)
      if(input$action == "replace") {
        mainTable$main = df
        sources$all = sources$current
      }
      else {
        mainTable$main = rbindSafe(mainTable$main, df)
        sources$all = c(sources$all, sources$current)
      }
      removeModal()
    })

    output$mainTableUI = renderUI(
      if(!is.null(mainTable$main)) DT::DTOutput(ns("mainTable")) else NULL
    )

    output$mainTable = DT::renderDT({
      genoDT(mainTable$main, mode = "main", flavour = id, assigned = assignedRefs(), scrollY = "370px")
    }, server = FALSE)

    output$sourcefield = renderUI({
      src = vapply(sources$all, \(s) as.character(em(s)), character(1))
      if(!length(src))
        return(NULL)

      tags$div(
        style = "margin-top: 10px; line-height: 1.1;",
        HTML(paste(c("Sources:", src), collapse = if(length(src) == 1) " " else "<br>"))
      )
    })

    # Edit -------------------------------------------------------------------

    editdata = reactiveVal(NULL)
    idEdits = reactiveVal(NULL)

    observeEvent(input$editButton, { .debug2("open edit UI")
      editdata(req(mainTable$main))
      showModal(modalDialog(
        title = "Edit mode", size = "l",
        p("Edit the table by double clicking on the cells. Press 'Save' to confirm changes."),
        DT::DTOutput(ns("editTable"), height = "320px"),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("editSave"), "Save", status = "success"))
      ))
    })

    output$editTable = DT::renderDT({ .debug2("render edit table")
      genoDT(editdata(), mode = "edit", flavour = id, scrollY = "300px")
    })

    # Preserve manually entered aliases
    observeEvent(input$editTable_cell_edit, { .debug2("edit cell")
      dat = editdata()
      missing = missingIDs()
      ed = input$editTable_cell_edit
      i = input$editTable_rows_current[ed$row]
      j = ed$col + 1L # 0 = rownames
      val = trimws(ed$value)
      cl = names(dat)[j]

      tryCatch({
        if(cl %in% c("Sample", "Alias") && val == "")
          stop2("Name cannot be empty")
        if(cl == "Sample" && val %in% dat$Sample[-i])
          stop2("Duplicate sample name: ", val)
        if(cl == "Alias" && val %in% dat$Alias[-i])
          stop2("Duplicate alias: ", val)
        if(cl == "Sample" && val %in% missing)
          stop2("Name is already in use by a missing person:", val)
        if(cl == "Alias" && val %in% missing)
          stop2("Alias is already in use by a missing person:", val)

        # Update row name to match Sample
        if(cl == "Sample")
          rownames(dat)[i] = val

        # Perform edit
        dat[i, j] = val
        editdata(dat)
      }, error = showErr)
    })

    observeEvent(input$editTable_sex_edit, { .debug2("edit sex")
      dat = editdata()
      ed = input$editTable_sex_edit
      i = match(ed$key, dat$.rowid)
      dat$Sex[i] = ed$value
      editdata(dat)
    })

    observeEvent(input$editSave, {  .debug2("edit save")
      old = mainTable$main
      new = editdata()

      old = old[order(old$.rowid), , drop = FALSE]
      new = new[order(new$.rowid), , drop = FALSE]

      idch = old$Sample != new$Sample
      sexch = old$Sex != new$Sex
      rows = which(idch | sexch)

      sex012 = match(new$Sex, c("M", "F"), nomatch = 0L)

      ids = if(any(idch)) .setnames(new$Sample[idch], old$Sample[idch]) else NULL
      sex = if(any(sexch)) .setnames(sex012[sexch], new$Sample[sexch]) else NULL

      changes = if(length(ids) || length(sex)) list(ids = ids, sex = sex) else NULL

      rownames(new) = new$Sample
      mainTable$main = new
      idEdits(changes)
      removeModal()
    })

    # Alias -------------------------------------------------------------------

    origs = reactiveVal(NULL)
    aliasInputs = reactiveValues(remove = "", keep = "", prefix = "")
    aliasErrorMsg = reactiveVal("")

    observeEvent(input$aliasButton, { .debug2("open Alias UI")
      origs(rownames(req(mainTable$main)))

      showModal(modalDialog(
        title = "Generate Aliases",
        fluidRow(
          column(6,
            radioButtons(ns("aliasMethod"), "Choose method:",
                         choices = list("Sequence with prefix" = "prefix",
                                        "Keep until first" = "cut",
                                        "Remove text" = "remove",
                                        "Extract text" = "extract"),
                         selected = "prefix")
          ),
          column(6,
            uiOutput(ns("conditional_inputs")),
            uiOutput(ns("aliasErrorUI"))
          )
        ),
        div(style = "margin-top: 10px;", DT::DTOutput(ns("aliasTable"))),
        footer = tagList(
          tagAppendAttributes(
            actionButton(ns("aliasReplace"), HTML("Replace originals<br>with aliases"), warning = "info"),
            class = "btn-xs",
            style = "float:left; margin-right:auto; color:tomato; font-size:80%; font-weight:550; line-height:1.1; border-radius: 12px; padding: 2px 6px;"
          ),
          modalButton("Cancel"),
          actionButton(ns("aliasSave"), "Save",status = "success"))
      ))
    })

    output$aliasTable = DT::renderDT({
      df = data.frame(Original = origs(), Alias = generateAliases())
      aliasDT(df)
    })

    generateAliases = reactive({ .debug2("generate aliases")
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
        if(method == "cut") {
          r = regexpr(inp, origs, fixed = TRUE)
          res = origs
          res[r > 0] = substr(origs[r > 0], 1, r[r > 0] - 1)
        }
        else if(method == "remove") {
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

    output$conditional_inputs = renderUI({ .debug2("conditional alias input")
      ns = session$ns
      switch(input$aliasMethod,
             prefix = textInput(ns("prefix"), "Prefix:", value = switch(id, PM = "V", AM = "R", "ID")),
             cut = textInput(ns("cut"), "Any character or space:"),
             remove = textInput(ns("remove"), "Strings (separate w/comma):"),
             extract = textInput(ns("extract"), "Regex pattern to match:"))
    })

    output$aliasErrorUI = renderUI({ .debug2("alias error ui")
      msg = aliasErrorMsg()
      if (msg != "") tags$div(style = "color: red;", msg)
    })

    observeEvent(input$aliasSave, { .debug2("alias save")
      dat = mainTable$main
      dat$Alias = generateAliases()
      mainTable$main = dat
      removeModal()
    })

    observeEvent(input$aliasReplace, { .debug2("alias replace")
      dat = mainTable$main
      al = generateAliases()

      changes = list(ids = .setnames(al, dat$Sample))
      idEdits(changes)

      dat$Alias = dat$Sample = al
      rownames(dat) = al
      mainTable$main = dat
      removeModal()
    })

    # End alias ---------------------------------------------------------------

    # Show warning banner. (Set session=session to restrict to shinyapps.io)
    output$banner = renderUI(banner(session = NULL))

    # Return reactive variables to main app
    list(main = reactive(mainTable$main),
         completeDvi = reactive(completeDvi$import),
         idEdits = reactive(idEdits()),
         sources = reactive(sources$all))
  })

}


# Utils -------------------------------------------------------------------

previewGenoDT = function(dat) {
  DT::datatable(
    dat,
    class = "stripe hover nowrap compact",
    rownames = "Sample" %notin% colnames(dat),
    escape = TRUE,
    plugins = "natural",
    selection = "multiple",
    options = list(
      dom = "t",
      ordering = FALSE,
      paging = FALSE,
      scrollX = TRUE,
      scrollY = if(nrow(dat) > 10) "220px" else NULL
    )
  ) |>
    DT::formatStyle(names(dat), target = "row", lineHeight = "75%")
}


formatGenoView = function(dat, mode, flavour = NULL, fam = NULL) {

  if(mode == "main" && flavour == "AM") {
    if(is.null(fam))
      dat$Fam = ""
    else {
      f = fam[dat$Sample]
      dat$Fam = ifelse(is.na(f), "", f)
    }
    dat = moveColsFirst(dat, c("Fam", "Sample", "Alias", "Sex"))
  }

  if(mode == "main" && identical(dat$Alias, rownames(dat)))
    dat$Alias = NULL

  if(mode == "edit" && "Sex" %in% names(dat)) {
    dat$Sex = sprintf(
      "<select class='sex-edit' data-key='%s'><option value='F'%s>Female</option><option value='M'%s>Male</option></select>",
      dat$.rowid,
      ifelse(dat$Sex == "F", " selected", ""),
      ifelse(dat$Sex == "M", " selected", "")
    )
  }

  dat
}

genoDT = function(dat, mode = c("main", "edit"), flavour = NULL, assigned = NULL, scrollY = NULL) {
  if(is.null(dat))
    return(NULL)

  mode = match.arg(mode)
  dat = formatGenoView(dat, mode, flavour = flavour, fam = assigned)

  nms = names(dat)
  .cols = function(cols) .myintersect(cols, nms)
  .colIdx = function(cols) {
    i = match(cols, nms, nomatch = 0L)
    i[i > 0L] - 1L
  }

  editopt = switch(mode, main = FALSE,
    edit = list(target = "cell", disable = list(columns = .colIdx(c("Fam", "Sex"))))
  )

  callback = if(mode == "edit") DT::JS("sexEditCallback(table);") else DT::JS("return table;")

  dt = DT::datatable(
    dat,
    class = "stripe hover nowrap compact",
    rownames = FALSE,
    escape = if(mode == "edit") .mysetdiff(nms, "Sex") else TRUE,
    callback = callback,
    plugins = "natural",
    selection = "none",
    editable = editopt,
    options = list(
      dom = "t",
      paging = FALSE,
      scrollX = TRUE,
      scrollY = scrollY,
      scrollCollapse = TRUE,
      columnDefs = list(
        list(type = "natural", targets = .cols(c("Fam", "Sample", "Alias"))),
        list(orderable = TRUE, targets = .cols(c("Fam", "Sample", "Alias"))),
        list(orderable = FALSE, targets = "_all"),
        list(visible = FALSE, targets = .cols(".rowid"))
      )
    )
  ) |>
    DT::formatStyle(names(dat), target = "row", lineHeight = "75%") |>
    DT::formatStyle(.cols("Sex"), borderRight = "1px solid #ccc") |>
    DT::formatStyle(.cols("Fam"), fontWeight = "bold", color = "darkblue")

  if(mode == "main")
    dt = dt |> DT::formatStyle(.cols("Sex"), color = DT::styleEqual(c("F", "M"),
                                                                    c("hotpink", "steelblue")))
  dt
}

standardiseGenoData = function(df, flavour, selectRows = NULL, excludeCols = NULL) {
  if(is.null(df))
    return()

  if(is.matrix(df))
    df = as.data.frame(df)

  if(length(selectRows))
    df = df[sort(selectRows), , drop = FALSE]

  if(length(excludeCols))
    df = df[, names(df) %notin% excludeCols, drop = FALSE]

  nms = names(df)
  rownms = rownames(df)

  # Don't include fam here; taken care of in formatGenoView
  #if(flavour == "AM" && !"Fam" %in% nms)
  #  df$Fam = ""

  if("Sample" %notin% nms && !is.null(rownms))
    df$Sample = rownms

  if(!identical(rownms, df$Sample)) {
    rownames(df) = rownms = df$Sample
  }

  if("Alias" %notin% nms) {
    al = switch(flavour, PM = "V", AM = "R") |> paste0(seq_len(nrow(df)))
    df$Alias = if(!any(al %in% rownms)) al else rownms
  }

  # AMEL: First column starting with amel (case insensitive). NA if empty
  amel = grep("^amel", nms, ignore.case = TRUE, value = TRUE)[1]

  if("Sex" %in% nms)
    df$Sex = checkSex(df$Sex)
  else if(!is.na(amel)) {
    df$Sex = c("?", "M", "F")[amel2sex(df[[amel]]) + 1]
    df[[amel]] = NULL
  }
  else
    df$Sex = "?"

  # Key column for DT editing (not visible)
  df$.rowid = seq_len(nrow(df))

  moveColsFirst(df, c(".rowid", "Fam", "Sample", "Alias", "Sex"))
}


aliasDT = function(dat, scrollY = "250px") {
  res = DT::datatable(dat,
                      rownames = FALSE,
                      selection = "none",
                      class = "stripe hover compact",
                      options = list(dom = 't',
                                     scrollX = FALSE,
                                     scrollY = scrollY,
                                     scrollCollapse = TRUE,
                                     paging = FALSE)) |>
    DT::formatStyle(names(dat), target = "row", lineHeight = "80%")

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
  bad = x %notin% c("F", "M", "?")
  if(any(bad)) {
    warning("Illegal values in the Sex column:", toString(unique(sx[bad])))
    x[bad] = "?"
  }
  x
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
