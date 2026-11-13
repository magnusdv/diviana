databaseUI = function() {
  fluidRow(
    tags$style(HTML("
      .marker-summary-scroll { width: fit-content; max-width: 100%; max-height: 500px; overflow: auto; }
      #markersummary table.dataTable { width: auto !important; margin: 0; }
      #markersummary table.dataTable th, #markersummary table.dataTable td { padding: 2px 12px; }
      #markersummary table.dataTable thead th { position: sticky; top: 0; z-index: 1; background: #fff; padding: 6px 12px; }
    ")),
    column(3,
      bs4Card(width = 12, collapsible = FALSE, title = "Allele frequencies",
        freqRadios("dbtype")),
      bs4Card(width = 12, collapsible = FALSE, title = "Mutation model",
        mutRadios("muttype"))
    ),
    column(9,
      bs4Card(width = 12, collapsible = FALSE, title = "Marker summary",
        label = span(textOutput("markerInfo", inline = TRUE),
                     style = "font-size:0.9rem;white-space:nowrap"),
        uiOutput("databaseStatus"),
        div(class = "marker-summary-scroll", DT::DTOutput("markersummary", width = "auto")),
        uiOutput("addedAllelesNote"),
        footer = div(class = "btn-group",
          actionButton("chartButton",
            label = tagList(myIcon("simple-chart", align = "-0.1em"), " Frequencies")),
          actionButton("mutMatrixButton",
            label = tagList(myIcon("bolt", align = "-0.1em"), "Mutation matrix")))
      )
    )
  )
}

registerDatabase = function(input, output, session, am, pm, settings, loading) {
  original = reactiveVal(NULL)
  dbChoice = reactiveVal("")
  mutChoice = reactiveVal("none")
  ignoreDbInput = reactiveVal(FALSE)
  ignoreMutInput = reactiveVal(FALSE)

  # Update the server choice before the radio change returns from the browser
  syncRadio = function(id, value, flag) {
    flag(!identical(isolate(input[[id]]) %||% "", value))
    freezeReactiveValue(input, id)
    updateRadioButtons(session, id, selected = if(nzchar(value)) value else character())
  }

  # Clearing all radios sends NULL, which must also clear the ignore flag
  observeEvent(input$dbtype, {
    if(ignoreDbInput()) {
      ignoreDbInput(FALSE)
      return()
    }
    dbChoice(input$dbtype %||% "")
  }, ignoreInit = TRUE, ignoreNULL = FALSE, priority = 100)

  observeEvent(input$muttype, {
    if(ignoreMutInput()) {
      ignoreMutInput(FALSE)
      return()
    }
    mutChoice(input$muttype)
  }, ignoreInit = TRUE, priority = 100)

  # Choosing or uploading a database also selects its source
  observeEvent(input$builtinDB, {
    if(!nzchar(input$builtinDB) && dbChoice() != "builtin") return()
    choice = if(nzchar(input$builtinDB)) "builtin" else ""
    dbChoice(choice)
    syncRadio("dbtype", choice, ignoreDbInput)
  }, ignoreInit = TRUE)

  observeEvent(input$customDB, {
    dbChoice("custom")
    syncRadio("dbtype", "custom", ignoreDbInput)
  }, ignoreInit = TRUE)

  setChoices = function(db = "", mut = "none") {
    dbChoice(db)
    mutChoice(mut)
    syncRadio("dbtype", db, ignoreDbInput)
    syncRadio("muttype", mut, ignoreMutInput)
    if(db == "") {
      freezeReactiveValue(input, "builtinDB")
      updateSelectInput(session, "builtinDB", selected = "")
    }
  }

  loadDatabase = function(dat, source) {
    original(list(db = dat$db, mutparams = dat$mutparams,
                  hasMut = dat$hasMut, source = basename(source)))
    setChoices(if(length(dat$db)) "original" else "",
               if(isTRUE(dat$hasMut)) "original" else "none")
  }

  resetDatabase = function() {
    original(NULL)
    setChoices()
    updateNumericInput(session, "mutrateF", value = 0.001)
    updateNumericInput(session, "mutrateM", value = 0.002)
  }

  customDB = reactive({
    f = input$customDB
    if(is.null(f)) return(NULL)

    tryCatch({
      if(tolower(tools::file_ext(f$name)) == "fam") {
        y = pedFamilias::readFam(f$datapath, verbose = FALSE)
        if(length(y) && is.ped(y[[1]]))
          getFreqDatabase(y[[1]])
        else if(length(y) && length(y[[1]]) && is.ped(y[[1]][[1]]))
          getFreqDatabase(y[[1]][[1]])
        else {
          names(y) = vapply(y, function(loc) loc$name, "")
          lapply(y, function(loc) .setnames(loc$afreq, loc$alleles))
        }
      }
      else
        readFreqDatabase(f$datapath, sep = "\t")
    }, error = function(e) {
      errModal(e)
      NULL
    })
  })

  selectedDB = reactive({
    if(loading()) return(NULL)
    switch(dbChoice(),
      builtin = switch(input$builtinDB %||% "",
        "norSTR: Norway" = norSTR::norwayDB,
        "norSTR: Europe" = norSTR::europeDB,
        "norSTR: Africa" = norSTR::africaDB,
        "norSTR: South America" = norSTR::southAmericaDB,
        "norSTR: West Asia" = norSTR::westAsiaDB,
        "norSTR: Middle Asia" = norSTR::midAsiaDB,
        "norSTR: East Asia" = norSTR::eastAsiaDB,
        NULL),
      custom = customDB(),
      original = original()$db,
      NULL)
  })

  ready = reactive(length(selectedDB()) > 0L)
  label = reactive({
    if(!ready()) return("not selected")
    switch(dbChoice(), builtin = input$builtinDB, custom = input$customDB$name,
           original = original()$source)
  })

  checkDatabase = function() {
    if(ready()) return(TRUE)
    showNotification("Select a frequency database in MARKERS before calculating", type = "error")
    FALSE
  }

  output$database_famname = renderText(original()$source)
  observe({
    toggleState(selector = "input[name='dbtype'][value='builtin']",
                condition = nzchar(input$builtinDB %||% ""))
    toggleState(selector = "input[name='dbtype'][value='custom']",
                condition = !is.null(input$customDB))
    toggleState(selector = "input[name='dbtype'][value='original']",
                condition = length(original()$db) > 0L)
    toggleState(selector = "input[name='muttype'][value='original']",
                condition = isTRUE(original()$hasMut))
  })

  mutParams = reactive({
    switch(mutChoice(),
      none = NULL,
      standard = {
        prm = list(model = settings$standardmodel,
                   rate = list(female = input$mutrateF, male = input$mutrateM))
        if(prm$model == "stepwise") {
          prm$rate2 = 1e-6
          prm$range = 0.1
        }
        prm
      },
      original = original()$mutparams)
  })

  observedAlleles = reactive({
    obs = lapply(list(am(), pm()), function(a) {
      if(!length(a)) return(list())
      j = 2L*seq_len(ncol(a)/2L) - 1L
      als = lapply(j, function(k)
        .mysetdiff(unique.default(as.character(a[, k + 0:1])), c("", ".", "-", "0", "NA", NA)))
      nm = colnames(a)[j]
      names(als) = substr(nm, 1L, nchar(nm) - 2L)
      als
    })
    obs = c(obs[[1]], obs[[2]])
    if(!length(obs)) return(obs)

    # Pool equivalent marker names across AM and PM before assigning frequencies
    nm = normaliseName(names(obs))
    groups = split(obs, factor(nm, levels = unique.default(nm)))
    res = lapply(groups, function(x) sort.default(unique.default(unlist(x, use.names = FALSE))))
    names(res) = names(obs)[!duplicated.default(nm)]
    res[lengths(res) > 0L]
  })

  DBext = reactive({
    db = selectedDB()
    obs = observedAlleles()
    if(!length(db))
      return(lapply(obs, function(a) .setnames(rep.int(1/length(a), length(a)), a)))

    idx = match(normaliseName(names(db)), normaliseName(names(obs)))
    added = lapply(seq_along(db), function(i) {
      if(is.na(idx[i])) return(character())
      .mysetdiff(obs[[idx[i]]], names(db[[i]]))
    })
    names(added) = names(db)
    use = which(lengths(added) > 0L)
    db[use] = Map(.addAlleles, db[use], added[use])
    attr(db, "added") = added
    db
  })

  locusAttrs = reactive({
    if(loading()) return(NULL)
    # Apply mutation settings only after a frequency source is selected
    db2locattrs(DBext(), if(ready()) mutParams())
  })

  output$databaseStatus = renderUI({
    if(!ready())
      p(class = "text-danger",
        "No frequency database selected. Equal frequencies are used temporarily.")
  })

  output$addedAllelesNote = renderUI({
    if(any(lengths(attr(DBext(), "added"))))
      p(class = "text-muted mt-2",
        "Added alleles have frequency 0.001. Existing frequencies are adjusted.")
  })

  markerTable = reactive({
    loc = locusAttrs()
    if(!length(loc)) return(NULL)
    mtab = markerSummaryDiviana(loc, am = am(), pm = pm())
    if(!ready()) mtab$PIC = NULL
    added = attr(DBext(), "added")
    if(any(lengths(added)))
      mtab[["Added alleles"]] = vapply(added[mtab$Marker], function(a)
        toString(a[order(suppressWarnings(as.numeric(a)), a)]), "")
    mtab
  })

  output$markerInfo = renderText({
    tab = markerTable()
    n = NROW(tab)
    if(!n) return("No markers")
    count = sprintf("%d marker%s", n, if(n == 1L) "" else "s")

    if(all(is.na(tab$AM)) && all(is.na(tab$PM))) return(count)
    if(all(is.na(tab$AM)) || all(is.na(tab$PM)) || identical(tab$AM, tab$PM)) {
      used = sum(tab$AM %in% TRUE | tab$PM %in% TRUE)
      return(if(used == n) count else paste(count, paste(used, "in data"), sep = " · "))
    }
    paste(count, paste(sum(tab$AM), "in AM"), paste(sum(tab$PM), "in PM"), sep = " · ")
  })

  output$markersummary = DT::renderDT({
    tab = markerTable()
    if(is.null(tab)) return(NULL)
    formatDatabaseTable(tab)
  })

  showFrequencies = function(i) {
    req(i)
    loc = req(locusAttrs()[i][[1]])
    freqs = .setnames(loc$afreq, loc$alleles)
    added = attr(DBext(), "added")[[loc$name]]
    output$freqPlot = renderPlot(plotFreqs(freqs, loc$name))
    output$freqTable = DT::renderDT(formatFreqTable(freqs, added = added))

    showModal(modalDialog(
      title = paste("Allele frequencies:", loc$name), size = "l", easyClose = TRUE,
      if(!ready()) p(class = "text-danger", "Temporary equal frequencies"),
      div(style = "display:flex;align-items:flex-start;gap:15px;margin-right:20px",
        div(style = "flex:3;min-width:0", plotOutput("freqPlot", height = 350)),
        div(style = "flex:1;min-width:0", DT::DTOutput("freqTable", width = "100%")))
    ))
  }

  observeEvent(input$chartButton, showFrequencies(input$markersummary_rows_selected))
  # A double-click may clear the selection, so use its row directly
  observeEvent(input$markersummary_dblclick, showFrequencies(input$markersummary_dblclick))

  observeEvent(input$mutMatrixButton, {
    i = req(input$markersummary_rows_selected)
    loc = req(locusAttrs()[i][[1]])
    mutmod = req(loc$mutmod)
    malemat = mutmod$male |> as.matrix() |> formatMatrix() |> gt::fmt_auto()
    femalemat = mutmod$female |> as.matrix() |> formatMatrix() |> gt::fmt_auto()
    output$mutmatrix = gt::render_gt({
      switch(req(input$mutsex), Female = femalemat, Male = malemat)
    }, height = 400)

    showModal(modalDialog(
      title = paste("Mutation matrix:", loc$name), size = "l", easyClose = TRUE,
      radioButtons("mutsex", NULL, choices = c("Female", "Male"), selected = "Female", inline = TRUE),
      gt::gt_output("mutmatrix")
    ))
  })

  list(locusAttrs = locusAttrs, markers = reactive(names(selectedDB())),
       nAdded = reactive(sum(lengths(attr(DBext(), "added")))),
       ready = ready, label = label, check = checkDatabase,
       load = loadDatabase, reset = resetDatabase)
}

freqRadios = function(id) {
  norstr = paste("norSTR:", c("Norway", "Europe", "Africa", "South America",
                             "West Asia", "Middle Asia", "East Asia"))
  tagList(
    tags$style(HTML("
      .freq-radios .shiny-input-container { margin-bottom: 1rem; }
      .freq-radios .control-label { margin-bottom: 0; }
      .freq-radios label { margin-bottom: 0; }
    ")),
    div(id = id, class = "form-group shiny-input-radiogroup freq-radios",
      style = "display:grid;grid-template-columns:auto 1fr;gap:0 16px;align-items:center;",
      tags$input(type = "radio", name = id, value = "builtin"),
      selectInput("builtinDB", "Builtin", choices = c("Select database" = "", norstr),
                  selected = ""),
      tags$input(type = "radio", name = id, value = "custom"),
      fileInput("customDB", "Custom"),
      tags$input(type = "radio", name = id, value = "original"),
      div(tags$label("Original", style = "margin-right:0.5em;"),
          textOutput("database_famname", inline = TRUE), style = "white-space:nowrap;")
    )
  )
}

mutRadios = function(id) {
  tagList(
    tags$style(HTML("
      .mut-radios label { margin-bottom: 0; }
      .mut-radios .standard-rate { position: relative; flex: 1; min-width: 0; }
      .mut-radios .shiny-input-container { margin-bottom: 0; }
      .mut-radios .standard-rate .shiny-input-container { margin-bottom: 0; }
      .mut-radios .standard-rate input { height: 24px; padding: 2px 0 2px 6px; }
      .mut-radios .standard-rate .tinylabel {
        position: absolute; top: -14px; font-size: smaller; line-height: 1; text-align: center; width: 100%;
      }
    ")),
    div(id = id, class = "form-group shiny-input-radiogroup mut-radios",
      style = "display:grid;grid-template-columns:auto 1fr;gap:20px 16px;align-items:center;",
      tags$input(type = "radio", name = id, value = "none", checked = "checked"),
      div(tags$label("No model")) |> wrap_tooltip("mutradio_none"),
      tags$input(type = "radio", name = id, value = "standard"),
      div(style = "display:flex;align-items:center;gap:16px;white-space:nowrap;",
        tags$label("Standard") |> wrap_tooltip("mutradio_standard"),
        div(style = "display:flex;align-items:center;gap:8px;flex:1;min-width:0;margin-top:4px",
          div(class = "standard-rate",
            div("Female", class = "tinylabel"),
            numericInput("mutrateF", NULL, width = "100%", min = 0, max = 1, value = 0.001)),
          div(class = "standard-rate",
            div("Male", class = "tinylabel"),
            numericInput("mutrateM", NULL, width = "100%", min = 0, max = 1, value = 0.002))
        )
      ),
      tags$input(type = "radio", name = id, value = "original"),
      div(tags$label("Original")) |> wrap_tooltip("mutradio_original")
    )
  )
}

db2locattrs = function(db, mutparams = NULL) {
  if(is.null(db)) return(NULL)
  mnames = names(db) |> .setnames()
  standard = "model" %in% names(mutparams)
  hasoriginal = mnames %in% names(mutparams) |> .setnames(mnames)

  lapply(mnames, function(m) {
    alleles = names(db[[m]])
    afreq = unname(db[[m]])
    loc = list(name = m, alleles = alleles, afreq = afreq, mutmod = NULL)
    if(!standard && !hasoriginal[m]) return(loc)

    args = if(standard) mutparams else mutparams[[m]]
    args$alleles = alleles
    args$afreq = afreq
    loc$mutmod = tryCatch(do.call(mutationModel, args), error = function(e) {
      errModal(paste0(conditionMessage(e), "<br>Disabling the model for: ", m), html = TRUE)
      NULL
    })
    loc
  })
}

# Add alleles by shrinking existing frequencies above the lower bound
.addAlleles = function(x, newAlleles = NULL, freq = 0.001, minim = NA) {
  if(is.na(minim))
    minim = if(any(x < 0.001)) 0 else 0.001
  if(any(freq < minim))
    stop2("New frequencies must be at least `minim`")

  newAlleles = .mysetdiff(unique.default(newAlleles), names(x))
  if(!length(newAlleles)) return(x)
  y = rep(freq, length(newAlleles))
  names(y) = newAlleles
  need = sum(y)
  avail = sum(x - minim)
  if(avail < need) stop2("Impossible: not enough mass above `minim`")

  k = length(x)
  a = (1 - need - k*minim)/(1 - k*minim)
  res = c(minim + a*(x - minim), y)

  # Sort numeric allele labels numerically
  als = names(res)
  num = suppressWarnings(as.numeric(als))
  ord = if(!anyNA(num)) order(num) else order(als)
  res[ord]
}

# Marker summary table ----------------------------------------------------

styleBool = function(x, keep = TRUE) ifelse(
  x,
  "<span style='color:green;font-weight:600'>✓</span>",
  ifelse(
    keep,
    "<span style='color:red;font-weight:500'>✗</span>",
    "<span style='color:red;font-weight:500;font-size:90%'>✗</span>"
  )
)

formatDatabaseTable = function(df) {
  req(!is.null(df))

  cols = intersect(c("Rate", "Rate2", "Range", "Stationary", "Lumpable"), names(df))
  df[cols[vapply(df[cols], function(x) all(is.na(x)), logical(1))]] = NULL

  df$keep = if(all(is.na(df$AM)) && all(is.na(df$PM))) TRUE else
    df$AM %in% TRUE | df$PM %in% TRUE
  df$AM = styleBool(df$AM, keep = df$keep)
  df$PM = styleBool(df$PM, keep = df$keep)

  numericCols = intersect(c("Alleles", "PIC", "Rate", "Rate2", "Range"), names(df))
  tab = DT::datatable(df,
    class = "stripe nowrap",
    rownames = FALSE,
    escape = .mysetdiff(names(df), c("AM", "PM")),
    selection = "single",
    callback = DT::JS("
      table.on('dblclick', 'tbody tr', function() {
        const i = table.row(this).index();
        if(i !== undefined)
          Shiny.setInputValue('markersummary_dblclick', i + 1, {priority: 'event'});
      });
    "),
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      autoWidth = FALSE,
      columnDefs = list(list(targets = "keep", visible = FALSE),
                        list(className = "dt-center", targets = c("AM", "PM")),
                        list(className = "dt-right", targets = numericCols))
    )) |>
    DT::formatStyle(
      names(df) |> .mysetdiff(c("AM", "PM")),
      valueColumns = "keep",
      target = "row",
      color = DT::styleEqual(c(TRUE, FALSE), c(NA, "#999")),
      fontStyle = DT::styleEqual(c(TRUE, FALSE), c("normal", "italic")),
      fontSize = DT::styleEqual(c(TRUE, FALSE), c(NA, "90%"))
    ) |>
    DT::formatStyle(names(df), target = "row", lineHeight = "1rem")

  if("PIC" %in% names(df))
    tab = DT::formatRound(tab, "PIC", digits = 2, mark = "")
  if("Added alleles" %in% names(df))
    tab = DT::formatStyle(tab, "Added alleles", color = "#dc3545")
  tab
}


formatFreqTable = function(freqs, added = character()) {
  # Sort
  als = names(freqs)
  alsnum = suppressWarnings(as.numeric(als))
  freqs = freqs[order(alsnum, als)]

  # Update vars after sort
  als = names(freqs)
  alsnum = suppressWarnings(as.numeric(als))

  # Ad hoc padding to align on "." for alleles < 10
  singleDigit = !is.na(alsnum) & alsnum < 10
  als[singleDigit] = paste0(" ", als[singleDigit])

  # Hack to avoid long allele name
  als[tolower(als) == "rest allele"] = "Rest"

  df = data.frame(Allele = als, Frequency = unname(freqs),
                  Added = names(freqs) %in% added, check.names = FALSE)

  DT::datatable(
        df,
        class = "stripe compact nowrap",
        rownames = FALSE,
        selection = "none",
        #width = "100%",
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          autoWidth = FALSE,
          scrollX = TRUE,
          scrollY = "310px", #if(nrow(df) > 15) "310px" else NULL,
          scrollCollapse = TRUE,
          columnDefs = list(list(className = 'dt-left', targets = "_all"),
                            list(targets = "Added", visible = FALSE))
        )
      ) |>
      DT::formatStyle(names(df), target = "row", lineHeight = "90%") |>
      DT::formatStyle(c("Allele", "Frequency"), valueColumns = "Added",
                      color = DT::styleEqual(TRUE, "#dc3545")) |>
      DT::formatStyle("Allele", fontFamily = "monospace", whiteSpace = "pre", fontSize = "95%")
}

