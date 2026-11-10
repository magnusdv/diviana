# Convert import issues to reasons indexed by family
pedIssueReasons = function(x) {
  if(!length(x)) return(character())
  cause = names(x)
  cause[cause == "mpSexChanged"] = "check sexes"
  cause[cause == "nonSpouseRepair"] = "repaired disconnected family"
  y = split(rep(tolower(cause), lengths(x)), unlist(x, use.names = FALSE))
  vapply(y, function(z) paste(unique.default(z), collapse = "; "), "")
}

pedIssueSummary = function(x, when = "saving") {
  n = length(pedIssueReasons(x))
  if(!n) return(NULL)
  div(class = "alert alert-warning py-1 px-2 mt-1 mb-0",
      style = "background:#fff3cd;border-color:#ffe69c;color:#664d03",
      sprintf("%d %s manual inspection after %s", n,
              if(n == 1L) "pedigree needs" else "pedigrees need", when))
}

dataUI = function(id, title = paste(id, "data")) {
  ns = NS(id)
  bs4Card(width = NULL, title = title, collapsible = TRUE, collapsed = FALSE,
    uiOutput(ns("mainTableUI")),
    uiOutput(ns("sourcefield")),
    footer = div(class = "btn-group",
      actionButton(ns("importButton"), label = tagList(myIcon("file-arrow-up", align = "-0.1em"), "Import")),
      actionButton(ns("editButton"), label = tagList(myIcon("edit", align = "-0.1em"), "Edit")),
      actionButton(ns("aliasButton"), label = tagList(icon("user-pen", align = "-0.1em"), "Aliases")),
      actionButton(ns("exportButton"), label = tagList(icon("file-export"), "Export")),
      if(id == "PM") tagList(
        actionButton(ns("mergeButton"), label = tagList(icon("object-group"), "Merge PM")),
        actionButton(ns("pairingButton"), "PM \u2192 AM"))
    )
  )
}

dataServer = function(id, externalData = reactiveVal(NULL), assignedRefs = reactiveVal(),
                      missingIDs = reactiveVal(), existingFams = reactive(character()),
                      muted = reactive(character()), currentModal = NULL, .debug = NULL,
                      identifications = reactive(NULL), existingIDs = reactive(character())) {

  .debug2 = if(is.null(.debug)) function(...) NULL else function(...)
    .debug(sprintf("-%s data:", id), ...)

  moduleServer(id, function(input, output, session) { .debug2("server")
    ns = session$ns

    # Main reactives
    mainTable = reactiveValues(main = NULL)
    sources = reactiveValues(current = NULL, all = NULL)

    # Commit the complete table, including muted PM samples.
    setData = function(dat) {
      if(NROW(dat)) {
        if(id == "PM")
          dat = dat[order(dat$Sample), , drop = FALSE]
        al = dat$Alias
        empty = is.na(al)
        if(any(empty)) {
          candidates = paste0(switch(id, PM = "V", AM = "R"), seq_len(nrow(dat)))
          al[empty] = head(setdiff(candidates, al), sum(empty))
        }
        if(any(!nzchar(al)))
          stop2("Aliases must be nonempty")
        if(id == "PM" && anyDuplicated(al))
          stop2("PM aliases must be unique")
        dat$Alias = al
        dat$.rowid = seq_len(nrow(dat))
        rownames(dat) = dat$Sample
        dat = moveColsFirst(dat, ".rowid")
      }
      else dat = NULL
      mainTable$main = dat
    }

    # Register module for text import
    import = registerImport(id, input, output, session, mainTable, setData, sources,
                            existingFams, existingIDs, missingIDs, muted,
                            identifications, currentModal, .debug2)

    # AM table: Clicking first column opens pedigree
    famClick = reactive({
      z = input$mainTable_cell_clicked
      req(id == "AM", z$col == 0)
      z$value
    })

    observeEvent(externalData(), { .debug2("set external", externalData())
      ext = externalData()

      if(identical(ext, "reset")) {
        mainTable$main = NULL
        sources$all = sources$current = NULL
        updateRadioButtons(session, "missingFmt", selected = "M")
        import$clear(reset = TRUE)
        isolate(externalData(NULL)) # TODO
        return()
      }
      else {
        setData(standardiseGenoData(ext))
        import$clear()
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    output$mainTableUI = renderUI(
      if(!is.null(mainTable$main)) DT::DTOutput(ns("mainTable")) else NULL
    )

    output$mainTable = DT::renderDT({
      genoDT(mainTable$main, mode = "main", flavour = id, assigned = assignedRefs(),
             muted = muted(), scrollY = "370px", identifications = identifications())
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

    showEdit = function() showModal(modalDialog(
      title = "Edit mode", size = "l",
      p("Double-click cells to edit, or select rows to delete samples. Press 'Save' to confirm changes."),
      DT::DTOutput(ns("editTable"), height = "320px"),
      footer = div(class = "aligned-row-wide",
                   actionLink(ns("editDelete"), NULL, icon = icon("trash-can", class = "fa-lg"),
                              title = "Delete selected samples", style = "color:inherit"),
                   div(modalButton("Cancel"),
                       actionButton(ns("editSave"), "Save", status = "success")))
    ))

    observeEvent(input$editButton, { .debug2("open edit UI")
      editdata(req(mainTable$main))
      showEdit()
    })

    output$editTable = DT::renderDT({ .debug2("render edit table")
      genoDT(editdata(), mode = "edit", flavour = id, scrollY = "300px")
    })

    deleteRows = reactiveVal(integer())
    observeEvent(input$editDelete, {
      rows = input$editTable_rows_selected
      req(length(rows))
      dat = editdata()
      old = mainTable$main
      samples = old$Sample[match(dat$.rowid[rows], old$.rowid)]

      # Keep references that belong to an AM pedigree
      if(id == "AM") {
        assigned = intersect(samples, names(assignedRefs()))
        if(length(assigned)) {
          showErr("Cannot delete references assigned to pedigrees: ", toString(assigned))
          return()
        }
      }
      if(id == "PM" && any(samples %in% muted())) {
        showErr("Cannot delete manually identified PM samples")
        return()
      }
      deleteRows(dat$.rowid[rows])
      confirm = modalDialog(
        sprintf("Delete %d selected %s?", length(rows),
                if(length(rows) == 1L) "sample" else "samples"),
        tags$ul(style = "margin:0;padding-left:20px;max-height:250px;overflow-y:auto",
                lapply(dat$Sample[rows], tags$li)),
        footer = div(style = "display:flex; justify-content:center; gap:8px; width:100%;",
                     actionButton(ns("editDeleteBack"), "No, go back"),
                     actionButton(ns("editDeleteConfirm"), "Yes, delete", status = "danger"))
      )
      confirm$children[[1]]$attribs$class = paste(
        confirm$children[[1]]$attribs$class, "modal-dialog-centered")
      confirm$children[[1]]$attribs$style = "max-width:420px; top:-15vh;"
      showModal(confirm)
    })

    observeEvent(input$editDeleteBack, showEdit())
    observeEvent(input$editDeleteConfirm, {
      dat = editdata()
      editdata(dat[!dat$.rowid %in% deleteRows(), , drop = FALSE])
      deleteRows(integer())
      saveEdit()
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
        if(cl == "Sample" && val %in% missing)
          stop2("Name is already in use by a missing person:", val)

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

    saveEdit = function() { .debug2("edit save")
      new = editdata()
      old = mainTable$main
      old = old[match(new$.rowid, old$.rowid), , drop = FALSE]

      idch = old$Sample != new$Sample
      sexch = old$Sex != new$Sex

      sex012 = match(new$Sex, c("M", "F"), nomatch = 0L)

      ids = if(any(idch)) .setnames(new$Sample[idch], old$Sample[idch]) else NULL
      sex = if(any(sexch)) .setnames(sex012[sexch], new$Sample[sexch]) else NULL

      changes = if(length(ids) || length(sex)) list(ids = ids, sex = sex) else NULL

      tryCatch({
        fixed = old$Sample %in% muted()
        cols = setdiff(names(new), c(".rowid", "Sample", "Alias"))
        if(id == "PM" && any(fixed) &&
           !identical(unname(as.matrix(old[fixed, cols, drop = FALSE])),
                      unname(as.matrix(new[fixed, cols, drop = FALSE]))))
          stop2("Edit identified profiles in the AM table")
        setData(new)
        idEdits(changes)
        removeModal()
      }, error = showErr)
    }
    observeEvent(input$editSave, saveEdit())


    # Aliases -----------------------------------------------------------------

    registerAliases(id, input, output, session, mainTable, setData,
                    missingIDs, idEdits, .debug2)

    # Export ------------------------------------------------------------------

    observeEvent(input$exportButton, {
      req(mainTable$main)
      showModal(modalDialog(
        title = paste("Export", id, "data"),
        textInput(ns("exportName"), "File name:", value = paste0(id, "-data")),
        radioButtons(ns("exportFormat"), "Format:", inline = TRUE,
                     choices = c("Tab-separated text (.txt)" = "txt", "Excel (.xlsx)" = "xlsx")),
        footer = tagList(modalButton("Cancel"),
                         downloadButton(ns("exportDownload"), "Download", class = "btn-success"))
      ))
    })

    output$exportDownload = downloadHandler(
      filename = function() {
        name = trimws(input$exportName %||% "")
        if(!nzchar(name)) name = paste0(id, "-data")
        name = sub("\\.(txt|xlsx)$", "", name, ignore.case = TRUE)
        paste0(basename(name), ".", input$exportFormat %||% "txt")
      },
      content = function(file) {
        dat = req(mainTable$main)
        dat$.rowid = NULL
        if(identical(input$exportFormat, "xlsx"))
          openxlsx::write.xlsx(dat, file, sheetName = id, rowNames = FALSE)
        else
          utils::write.table(dat, file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
      }
    )

    # Show warning banner. (Set session=session to restrict to shinyapps.io)
    output$banner = renderUI(banner(session = NULL))

    # Return reactive variables to main app
    list(main = reactive(mainTable$main),
         set = setData,
         idEdits = reactive(idEdits()),
         merge = reactive(input$mergeButton),
         pairing = reactive(input$pairingButton),
         sources = reactive(sources$all),
         sourceValues = sources,
         famClick = famClick,
         constructedAM = import$constructedAM)
  })

}


# Utils -------------------------------------------------------------------

formatGenoView = function(dat, mode, flavour = NULL, fam = NULL, identifications = NULL) {

  if(mode == "main" && flavour == "AM") {
    if(is.null(fam))
      dat$Fam = ""
    else {
      f = fam[dat$Sample]
      dat$Fam = ifelse(is.na(f), "", f)
    }
    dat = dat[order(dat$Fam, dat$Sample), , drop = FALSE]
    dat = moveColsFirst(dat, c("Fam", "Sample", "Alias", "Sex"))

    # Show the original PM sample without changing the stored AM ID.
    j = match(dat$Sample, identifications$Missing)
    fixed = !is.na(j)
    if(any(fixed))
      dat$Sample[fixed] = sprintf("%s (%s)", dat$Sample[fixed], identifications$Sample[j[fixed]])
  }

  if(mode == "main" && identical(dat$Alias, rownames(dat)))
    dat$Alias = NULL

  if(mode == "edit" && "Sex" %in% names(dat)) {
    dat$Sex = sprintf(
      "<select class='sex-edit' data-key='%s'><option value='?'%s%s>?</option><option value='F'%s>Female</option><option value='M'%s>Male</option></select>",
      dat$.rowid,
      ifelse(!dat$Sex %in% c("F", "M"), " selected", ""),
      if(identical(flavour, "PM")) "" else " disabled",
      ifelse(dat$Sex == "F", " selected", ""),
      ifelse(dat$Sex == "M", " selected", "")
    )
  }

  dat
}

genoDT = function(dat, mode = c("main", "edit"), flavour = NULL, assigned = NULL,
                   muted = NULL, scrollY = NULL, identifications = NULL) {
  if(is.null(dat))
    return(NULL)

  mode = match.arg(mode)
  dat = formatGenoView(dat, mode, flavour = flavour, fam = assigned,
                       identifications = identifications)

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
    selection = if(mode == "edit") "multiple" else "none",
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
    DT::formatStyle(.cols("Fam"), fontWeight = "bold", color = "darkblue",
                cursor = "pointer")

  if(mode == "main")
    dt = dt |> DT::formatStyle(.cols("Sex"), color = DT::styleEqual(c("F", "M"),
                                                                    c("hotpink", "steelblue")))
  if(mode == "main" && length(muted))
    dt = dt |> DT::formatStyle("Sample", target = "row",
      backgroundColor = DT::styleEqual(muted, rep("#eeeeee", length(muted))),
      color = DT::styleEqual(muted, rep("#999999", length(muted))))
  dt
}

standardiseGenoData = function(df, selectRows = NULL, excludeCols = NULL) {
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

  if("Sample" %notin% nms && !is.null(rownms))
    df$Sample = rownms

  if(!identical(rownms, df$Sample)) {
    rownames(df) = rownms = df$Sample
  }

  # Aliases are allocated after combining imported and retained rows.
  if("Alias" %notin% nms)
    df$Alias = rep(NA_character_, nrow(df))
  df$Alias[df$Alias == "" & !is.na(df$Alias)] = NA_character_

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

  # Standardise genotype separators
  genoCols = setdiff(names(df), c("Fam", "Sample", "Alias", "AMEL", "Sex", ".rowid"))
  df[genoCols] = lapply(df[genoCols], \(x) gsub(",", "/", x, fixed = TRUE))

  moveColsFirst(df, c("Fam", "Sample", "Alias", "Sex"))
}


# Check sex vector
checkSex = function(x) {
  x[x %in% c("", NA, 0)] = "?"
  x[x == 1] = "M"
  x[x == 2] = "F"
  bad = x %notin% c("F", "M", "?")
  if(any(bad)) {
    warning("Illegal values in the Sex column:", toString(unique(x[bad])))
    x[bad] = "?"
  }
  x
}

readDviFile = function(fil, name = basename(fil)) {
  ext = tolower(tools::file_ext(name))
  dvi = switch(ext,
    fam = dvir::familias2dvir(fil, missingFormat = "M[MIDX]", verbose = FALSE),
    rds = readRdvi(fil, name = name),
    rdata = readRdvi(fil, name = name),
    stop2("Unsupported DVI file. Use .fam, .rds or .RData")
  )
  if(!inherits(dvi, "dviData")) stop2("File does not contain a valid dviData object")
  dvi
}

readRdvi = function(fil, name = basename(fil)) {
  ext = tolower(tools::file_ext(name)) # use 'name' here (temporary path has unstable extension)
  switch(ext,
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

  if(!is.null(dvi$identifications))
    dvi$identifications$Alias = NULL
  dvi
}


banner = function(session) {
  if(!is.null(session)) {
    isShinyAppsIO = grepl("shinyapps.io", session$clientData$url_hostname)
    if(!isShinyAppsIO) return()
  }
  div(class = "aligned-row",
    style = "background-color: #fff3cd; border-radius: 10px; text-align: center; line-height: 90%; font-size:60%",
    icon("circle-exclamation", style = "margin: 6px; color:red; font-size: 20px;"),
    div(HTML("Avoid uploading<br>sensitive data online!")),
    icon("circle-exclamation", style = "margin: 6px; color:red; font-size: 20px;")
  )
}

