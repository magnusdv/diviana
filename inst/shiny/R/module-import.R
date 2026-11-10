
# Register text import controls within the existing data module
registerImport = function(id, input, output, session, mainTable, setData, sources,
                           existingFams, existingIDs, missingIDs, muted,
                           identifications, currentModal, .debug2) {
  ns = session$ns

  raw = reactiveVal(NULL)
  builtAM = reactiveVal(NULL)
  importedAM = reactiveVal(NULL)
  importType = reactiveVal(NULL)
  previewRows = reactiveVal(NULL)
  fileError = reactiveVal(NULL)
  pendingImport = reactiveVal(NULL)

  # Preserve import settings separately for AM and PM
  importOpts = reactiveValues(action = "add", longNames = TRUE, createAM = TRUE,
                              missingFmt = "M", includeHidden = FALSE, excludeCols = NULL)

  observeEvent(input$action, {importOpts$action = input$action})
  observeEvent(input$longNames, {importOpts$longNames = input$longNames})
  observeEvent(input$createAM, {importOpts$createAM = input$createAM})
  observeEvent(input$missingFmt, {importOpts$missingFmt = input$missingFmt})
  observeEvent(input$includeHidden, {importOpts$includeHidden = input$includeHidden})
  observeEvent(input$excludeCols, {
    if(!is.null(raw()) && is.null(pendingImport()))
      importOpts$excludeCols = input$excludeCols %||% character()
  }, ignoreNULL = FALSE)

  clearImport = function(reset = FALSE) {
    raw(NULL)
    builtAM(NULL)
    importedAM(NULL)
    importType(NULL)
    previewRows(NULL)
    fileError(NULL)
    pendingImport(NULL)

    if(reset) {
      importOpts$action = "add"
      importOpts$longNames = TRUE
      importOpts$createAM = TRUE
      importOpts$missingFmt = "M"
      importOpts$includeHidden = FALSE
      importOpts$excludeCols = NULL
    }
  }

  # Import dialog -----------------------------------------------------------

  showImport = function(restored = FALSE) {
    mod = modalDialog(
      title = div(class = "aligned-row-wide", paste("Import", id, "samples"),
                  uiOutput(ns("banner")), helpBtn("help-import")),
      size = "l",
      p("Import sample data from a plain text file. See help page for supported formats.",
        style = "margin-bottom:6px"),
      fileInput(ns("file"), NULL, width = "100%", accept = c(".txt", ".tsv", ".csv"),
                placeholder = if(restored) isolate(sources$current) else "No file selected"),
      uiOutput(ns("fileError")),
      uiOutput(ns("tableContent")),

      conditionalPanel("output.showHiddenControls", ns = ns,
        checkboxInput(ns("includeHidden"), "Include blanks, controls and ladder samples",
                      value = isolate(importOpts$includeHidden), width = "auto")),
      if(id == "AM")
        conditionalPanel("output.showFamilyControls", ns = ns,
          div(
            style = paste("background:#f8f9fa;border:1px solid #dee2e6;",
                          "border-radius:6px;padding:8px 10px 2px 12px;margin:10px 0 0"),
            fluidRow(
              column(7,
                checkboxInput(ns("longNames"), "Add family and relationship to sample IDs",
                              value = isolate(importOpts$longNames), width = "auto"),
                checkboxInput(ns("createAM"), "Create pedigrees automatically",
                              value = isolate(importOpts$createAM), width = "auto"),
                div(style = "padding-right:30px", uiOutput(ns("buildIssues")))
              ),
              column(5,
                conditionalPanel("input.createAM", ns = ns,
                  div(style = "margin-top:-14px",
                      missingLabelRadios(ns("missingFmt"), action = "importing",
                                         selected = isolate(importOpts$missingFmt))))
              )
            )
          )
        ),

      footer = div(class = "aligned-row-wide",
        radioButtons(ns("action"), NULL, inline = TRUE,
                     selected = isolate(importOpts$action),
                     choices = c("Add samples" = "add", "Replace" = "replace")),
        div(modalButton("Cancel"),
            actionButton(ns("importSave"), "Save", status = "success")))
    )
    if(!is.null(currentModal)) currentModal(mod)
    showModal(mod)
  }

  observeEvent(input$importButton, { .debug2("open import UI")
    clearImport()
    sources$current = NULL
    showImport()
  })

  output$fileError = renderUI({
    msg = req(fileError())
    div(style = "color:red", strong("Error: "), msg)
  })

  observeEvent(list(input$longNames, input$createAM, input$missingFmt,
                  input$includeHidden, input$excludeCols,
                  input$previewTable_rows_selected, input$action),
               fileError(NULL), ignoreInit = TRUE)

  output$showFamilyControls = reactive(id == "AM" && !is.null(raw()) && !is.null(builtAM()))
  outputOptions(output, "showFamilyControls", suspendWhenHidden = FALSE)

  output$showHiddenControls = reactive(identical(importType(), "gm"))
  outputOptions(output, "showHiddenControls", suspendWhenHidden = FALSE)


  # Genotype preview and import controls
  output$tableContent = renderUI({ .debug2("update raw table")
    cols = names(req(raw()))
    excluded = isolate(importOpts$excludeCols)
    if(is.null(excluded))
      excluded = c("Yindel", grep("^DY", cols, value = TRUE))

    tagList(
      DT::DTOutput(ns("previewTable")),
      tags$style(HTML("
        .import-exclude .selectize-input {
          min-height:26px; padding:2px 6px; font-size:0.8rem;
        }
      ")),
      div(
        style = "display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:4px 12px;margin:4px 0 6px",
        span(
          strong(textOutput(ns("sampleCount"), inline = TRUE)),
          " · Select rows to import only those samples.",
          class = "text-muted", style = "font-size:0.82rem"
        ),
        div(class = "import-exclude",
            style = "display:flex;align-items:center;gap:8px;margin-left:auto",
            span("Exclude columns:", class = "text-muted",
                 style = "font-size:0.8rem;white-space:nowrap"),
            tagAppendAttributes(
              selectizeInput(ns("excludeCols"), NULL, width = "210px", choices = cols,
                             selected = intersect(excluded, cols), multiple = TRUE),
              style = "margin:0"))
      )
    )
  })

  output$sampleCount = renderText({
    n = if(length(previewRows())) length(previewRows()) else nrow(dfFiltered())
    sprintf("%d sample%s", n, if(n == 1L) "" else "s")
  })

  output$buildIssues = renderUI({
    req(isTRUE(input$createAM))
    tryCatch(pedIssueSummary(selectedAM()$issues), error = function(e) NULL)
  })

  # Read uploaded file ------------------------------------------------------

  observeEvent(input$file, {
    raw(NULL)
    builtAM(NULL)
    importedAM(NULL)
    importType(NULL)
    previewRows(NULL)
    sources$current = NULL
    fileError(NULL)
    req(input$file)
    .debug2("import file", input$file$name)

    path = input$file$datapath
    ext = tolower(tools::file_ext(input$file$name))
    if(ext %notin% c("txt", "tsv", "csv")) {
      fileError("Only .txt, .tsv and .csv files are accepted for sample import")
      return()
    }

    tryCatch({
      res = readTextImport(path, flavour = id)
      if(!ncol(res$data)) stop2("No genotype columns found in file")
      builtAM(res$built)
      importType(res$type)
      raw(moveColsFirst(res$data, "AMEL"))
      sources$current = input$file$name
    }, error = function(e) {
      fileError(gsub(path, input$file$name, conditionMessage(e), fixed = TRUE))
    })
  })

  # Prepare sample names and show original family information for short names
  dfFiltered = reactive({ .debug2("import filter genemapper/text blanks")
    df = req(raw())
    if(id == "AM" && !is.null(builtAM())) {
      if(isFALSE(input$longNames %||% importOpts$longNames)) {
        src = builtAM()$source
        df$Family = trimws(src[[1]])
        df$Relationship = trimws(src[[2]])
        df$Sample = trimws(src[[3]])
        df = moveColsFirst(df, c("Family", "Relationship", "Sample"))
      } else {
        df$Sample = rownames(df)
        df = moveColsFirst(df, "Sample")
      }
    }
    if(identical(importType(), "gm") &&
       !isTRUE(input$includeHidden %||% importOpts$includeHidden)) {
      blanks = rowSums(df != "", na.rm = TRUE) == 0
      controls = grepl("C[+-]", rownames(df))
      ladder = grepl("Ladder", rownames(df))
      df = df[!(blanks | controls | ladder), , drop = FALSE]
    }
    df
  })

  # Build pedigrees from selected rows only
  selectedAM = reactive({
    b = req(builtAM())
    sel = previewRows()
    fmt = input$missingFmt %||% importOpts$missingFmt
    long = input$longNames %||% importOpts$longNames
    if(length(sel) || fmt != "M" || !long) {
      source = b$source
      if(length(sel))
        source = source[sort(sel), , drop = FALSE]
      dvir::buildAM(source, mpFormat = fmt, longNames = long)
    }
    else b$result
  })

  output$previewTable = DT::renderDT(
    previewGenoDT(dfFiltered(), selected = isolate(previewRows())))

  observeEvent(input$previewTable_rows_selected, {
    if(is.null(pendingImport()))
      previewRows(input$previewTable_rows_selected)
  }, ignoreNULL = TRUE)

  # Save and confirmation ---------------------------------------------------

  commitImport = function(z) {
    tryCatch({
      setData(rbindSafe(z$keep, z$df))
      sources$all = if(z$action == "replace" && !NROW(z$keep)) z$source else
        unique.default(c(sources$all, z$source))
      if(z$create)
        importedAM(list(result = z$build, action = z$action))
      pendingImport(NULL)
      removeModal()
    }, error = function(e) {
      fileError(conditionMessage(e))
      if(!is.null(pendingImport())) {
        z = pendingImport()
        pendingImport(NULL)
        previewRows(z$selected)
        showImport(restored = TRUE)
      }
    })
  }

  observeEvent(input$importSave, { .debug2("import save")
    req(input$action, raw())
    fileError(NULL)

    if(id == "AM" && input$action == "replace" && NROW(identifications())) {
      fileError("Reset the dataset before replacing AM data with fixed identifications")
      return()
    }

    create = id == "AM" && !is.null(builtAM()) && isTRUE(input$createAM)
    df = tryCatch({
      b = if(id == "AM" && !is.null(builtAM())) selectedAM()
      x = if(is.null(b))
        standardiseGenoData(dfFiltered(), selectRows = previewRows(),
                            excludeCols = input$excludeCols)
      else standardiseGenoData(b$geno, excludeCols = input$excludeCols)

      # Check genotypes before saving any data
      geno = x[setdiff(names(x), c(".rowid", "Fam", "Sample", "Alias", "AMEL", "Sex"))]
      if(!ncol(geno)) stop2("No genotype columns selected")
      splitGenotypes(geno)
      x
    }, error = function(e) {
      fileError(conditionMessage(e))
      NULL
    })

    if(is.null(df)) return()

    # Replacement affects active samples only; keep identified PM rows
    old = mainTable$main
    keep = if(input$action == "add") old else if(id == "PM" && NROW(old))
      old[old$Sample %in% muted(), , drop = FALSE]
    dup = intersect(df$Sample, keep$Sample)
    if(length(dup)) {
      fileError(paste("Existing sample IDs:", toString(dup)))
      return()
    }

    tryCatch({
      if(create && input$action == "add") {

        # Continue sequential missing labels from existing maximum
        if((input$missingFmt %||% "M") == "M") {
          m = grep("^M[0-9]+$", c(existingIDs(), keep$Sample), value = TRUE)
          k = if(length(m)) max(as.integer(substring(m, 2))) else 0L
          mp = paste0("M", k + seq_along(b$missing))
          dup = intersect(mp, c(keep$Sample, df$Sample))
          if(length(dup))
            stop2("Missing person IDs already used by samples: ", toString(dup))
          b$am = Map(\(p, old, new) relabel(p, old = old, new = new),
                     b$am, b$missing, mp)
          b$missing = mp
        }

        # Reject duplicate family and individual IDs
        dup = c(intersect(names(b$am), existingFams()),
                intersect(c(rownames(b$geno), b$missing), existingIDs()),
                intersect(unlist(lapply(b$am, labels), use.names = FALSE),
                          c(keep$Sample, missingIDs())))
        if(length(dup)) {
          fileError(paste("Existing family or individual IDs:", toString(unique(dup))))
          return()
        }
      }

      z = list(df = df, keep = keep, build = b, create = create,
               action = input$action, source = sources$current,
               selected = previewRows())
      replacing = z$action == "replace" && (NROW(old) ||
        (id == "AM" && create && length(existingFams())))

      if(!replacing) {
        commitImport(z)
        return()
      }

      # Cache the selection and settings before replacing the modal
      importOpts$action = input$action
      importOpts$excludeCols = input$excludeCols %||% character()
      pendingImport(z)

      msg = switch(id,
        AM = sprintf("Replace existing AM samples%s?", if(create) " and pedigrees" else ""),
        PM = "Replace existing PM samples? Manually identified samples will be kept."
      )
      showModal(confirmModal(msg, ns("replaceBack"), ns("replaceConfirm")))
    }, error = function(e) fileError(conditionMessage(e)))
  })

  observeEvent(input$replaceBack, {
    z = req(pendingImport())
    previewRows(z$selected)
    pendingImport(NULL)
    showImport(restored = TRUE)
  })

  observeEvent(input$replaceConfirm, commitImport(req(pendingImport())))

  list(constructedAM = reactive(importedAM()), clear = clearImport)
}


previewGenoDT = function(dat, selected = NULL) {
  DT::datatable(
    dat,
    class = "stripe hover nowrap compact",
    rownames = "Sample" %notin% colnames(dat),
    escape = TRUE,
    plugins = "natural",
    selection = list(mode = "multiple", selected = selected),
    options = list(
      dom = "t",
      ordering = FALSE,
      paging = FALSE,
      scrollX = TRUE,
      scrollY = if(nrow(dat) > 8) "176px" else NULL
    )
  ) |>
    DT::formatStyle(names(dat), target = "row", lineHeight = "75%")
}
