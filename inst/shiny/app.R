suppressMessages(suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(shinyjs)
  library(DT)
  library(gt)
  library(norSTR)
  library(pedtools)
  library(pedmut)
  library(forrel)
  library(diviana)
  library(dvir)
  library(verbalisr)
  library(plotly)
  library(openxlsx)
}))

VERSION = packageDescription("diviana")$Version

# Add path to icons
addResourcePath("icons", "www/static_icons")

# TODO----------------------------------------------------------------
#
# MARKERS
# * indicate markers with added alleles
# * store original mutmods for faster rendering of original+original
# * Handle cases where custom db is missing some markers (warn, but allow)
#
# PED
# * Reference list ugly when long
# * Slimmer family label; fatter buttons
# * Fix width: !important
# * Use default MP labels (settings)
#
# TRIANGLES
# * Triangle plots: Latex labels
# * Triangle AM: Select family
#
#---------------------------------------------


DEVMODE = F

DATASETS = c("example1", "example2", "exclusionExample", "fire", "grave", "icmp", "planecrash")

# UI ----------------------------------------------------------------------

ui = bs4Dash::bs4DashPage(

  dark = NULL,
  help = NULL,

  # Header
  bs4DashNavbar(
    status = "info",
    title = div(class = "aligned-row",
      img(src = "diviana-moth-logo.png", style = "height: 80px; padding:5px 10px"),
      h2("DIVIANA", style = "font-family: 'Montserrat', sans-serif; margin: 0")
    ),
    navbarMenu(
         id = "navmenu",
         navbarTab(tabName = "data", text = "DATA"),
         navbarTab(tabName = "database", text = "MARKERS"),
         navbarTab(tabName = "relatedness", text = "RELATEDNESS"),
         navbarTab(tabName = "analysis", text = "ANALYSIS")
    ),
    rightUi = tagList(tags$li(class = "nav-item dropdown",
      div(class = "aligned-row", style = "margin-right: 22.5px; gap: 15px;",
        actionBttn("settings", icon("gear"), style = "jelly", color = "success", size = "m") |>
          wrap_tooltip("settings", "bottom"),
        downloadBttn("downloaddata", NULL, style = "jelly", color = "warning", size = "m")|>
          wrap_tooltip("downloaddata", "bottom"),
        actionBttn("resetall", icon("redo"), style = "jelly", color = "danger", size = "m") |>
          wrap_tooltip("resetall", "bottom"),
        selectInput("example", NULL, choices = c("Load example" = "", DATASETS), width = "200px"),
      )
    ))
  ),

  # Sidebar
  bs4DashSidebar(disable = TRUE, minified = FALSE),


  # Main panel
  bs4DashBody(
    includeCSS("www/custom.css"),
    tags$head(includeHTML("www/GA.html")),
    tags$head(tags$script(src = "scripts.js")),

    useShinyjs(),
    useBusyIndicators(spinners = FALSE, pulse = TRUE, fade = FALSE),
    busyIndicatorOptions(pulse_height = "10px"),

   tabItems(

   # Tab: DATA -- AM, PM, pedigrees -----------------------------------------------

   tabItem("data",
      fluidRow(
        column(width = 7, div(id = "data-accordion", dataUI("AM"), dataUI("PM"))),
        column(
          width = 5, id = "pedcol",
          bs4Card(width = NULL, collapsible = FALSE,
            title = cardNavigation("pedcard", "Pedigrees"),
            plotOutput("pedplot", width = "auto", height = "auto"),
            footer = div(class = "aligned-row-wide",
              div(class = "btn-group",
                actionButton("newped", label = tagList(icon("plus"), "New")) |> wrap_tooltip("newped"),
                actionButton("editped", label = tagList(icon("edit"), "Edit")) |> wrap_tooltip("editped"),
                actionButton("delped", label = tagList(icon("trash-can"), "Delete") ) |> wrap_tooltip("delped")
              ),
              actionBttn("plotdviButton", "Overview", style = "jelly", size = "s", color = "success") |>
                wrap_tooltip("dviplot")
            )
          ),
          bs4InfoBoxOutput("dvisummary", width = 12)
        ),
      ),
      p(sprintf("This is DIVIANA version %s.", VERSION),
        #"(", mylink("changelog", "https://github.com/magnusdv/diviana/blob/master/NEWS.md"), ").",
        "Bug reports and issues are welcome ",
        mylink("here", "https://github.com/magnusdv/diviana/issues"), ".")
   ),

   # Tab: Marker database -------------------------------------------

   tabItem("database",
    fluidRow(
      column(width = 3,

        # Frequency source- -----------------------------------------

        bs4Card(width = 12, collapsible = FALSE, title = "Allele frequencies",
          freqRadios("dbtype")
        ),

        # Mutation model --------------------------------------------

        bs4Card(width = 12, collapsible = FALSE, title = "Mutation model",
          mutRadios("muttype")
        )
      ),

      # Main marker table ---------------------------------------------------------------------------

      column(width = 9,
        bs4Card(width = 12, collapsible = FALSE, title = "Marker summary",
          DT::DTOutput("markersummary", width = "fit-content"),
          footer = div(class = "btn-group",
            actionButton("chartButton",
                         label = tagList(myIcon("simple-chart", align = "-0.1em"), " Frequencies")),
            actionButton("mutMatrixButton",
                         label = tagList(myIcon("bolt", align = "-0.1em"), "Mutation matrix"))
          )
        ),
      ),
    ),
   ),


  # Tab: Relatedness ---------------------------------------------------------

  tabItem("relatedness", fluidRow(
    triangleCard("AM - AM", idpref = "am"),
    triangleCard("AM - PM", idpref = "ampm"),
    triangleCard("PM - PM", idpref = "pm")
  )),

  # Tab: Analysis-------------------------------------------------------------

  tabItem("analysis",
    fluidRow(

      # Buttons: Solve! --------------------------------------------
      column(width = 2, class = "col-xl-1",
             actionBttn("solve", label = "SOLVE", icon = icon("calculator"),
                        color = "primary", block = TRUE, style = "material-flat") |>
               wrap_tooltip("solve"),
             hr(),
             h4("Settings"),
             numericInput("LRthresh", "LR threshold", value = 10000,min = 1) |>
               wrap_tooltip("LRthresh"),
             br(),
             numericInput("maxIncomp", "Exclusion limit", min = 0, step = 1, value = 2)|>
               wrap_tooltip("maxIncomp"),
             br(),
             numericInput("pairLRmin", "Pairing LRmin", value = 0, min = 0)|>
               wrap_tooltip("pairLRmin"),
             br(),
             checkboxInput("ignoresex", "Ignore Sex", value = FALSE),
             hr(),
             downloadBttn('downloadTables', "Results", style = "material-flat", block = TRUE,
                          color = "warning") |> wrap_tooltip("downloadRes")
      ),

      # Report table: AM and PM tabs --------------------------------------------
      column(5, class = "col-xl-6",
         bs4TabCard(title = div("Identifications", style = "padding-right:10px;"),
                    width = NULL, type = "tabs", side = "right",
                    collapsible = FALSE,
                    tabPanel(title = "AM", gt::gt_output("amcentric")),
                    tabPanel(title = "PM", gt::gt_output("pmcentric")),
                    tabPanel(title = "LR", gt::gt_output("lrmatrix")),
                    tabPanel(title = "EX", gt::gt_output("exmatrix")),
                    tabPanel(title = "Joint", uiOutput("jointtabs")),
                    tabPanel(title = "Log", div(style = "max-height: 600px; overflow: auto",
                                                verbatimTextOutput("solvelog")))
         )
      ),

      # Solution plots-----------------------------------------------------

      column(width = 5,
        bs4Card(width = NULL, collapsible = FALSE,
          title = cardNavigation("solutioncard", "Solution overview"),
          plotOutput("solutionplot", width = "auto", height = "auto")
        ),
      ),
    )
  ))
))



# SERVER ------------------------------------------------------------------


server = function(input, output, session) {
  #addTooltips(session)

  .debug = function(...) {
    if(!DEVMODE) return()
    args = lapply(list(...), function(a) {
      if(is.null(a)) "NULL"
      else if(is.matrix(a) || is.data.frame(a)) sprintf("[%d,%d]", nrow(a), ncol(a))
      else if (length(a)>5) sprintf("n = %d", length(a))
      else toString(a)
    })
    do.call(cat, args)
    cat("\n")
  }

  # Close app when browser closes
  observeEvent(input$browserClosed, stopApp())

  # Main reactive variables -------------------------------------------------

  pedigrees = reactiveVal(NULL)
  DB = reactiveVal(NULL) # list of freq vectors
  mainMissing = reactiveVal(NULL)

  # Import data ------------------------------------------------------------

  externalAM = reactiveVal(NULL)
  externalPM = reactiveVal(NULL)
  assignedRefs = reactiveVal(NULL)
  externalLoci = reactiveValues(db = NULL, mut = NULL, hasMut = FALSE) # `mut` becomes named list

  dataServerAM = dataServer("AM", externalAM, assignedRefs, missingIDs = mainMissing, .debug = .debug)
  dataServerPM = dataServer("PM", externalPM, missingIDs = mainMissing, .debug = .debug)

  genoAM = reactive({ .debug("genoAM", dataServerAM$main())
    g = dataServerAM$main(); g$.rowid = g$Fam = g$Sample = g$Alias = g$AMEL = g$Sex = NULL; g})
  genoPM = reactive({ .debug("genoPM", dataServerPM$main())
    g = dataServerPM$main(); g$.rowid = g$Fam = g$Sample = g$Alias = g$AMEL = g$Sex = NULL; g})

  markersAM = reactive(names(genoAM()))
  markersPM = reactive(names(genoPM()))

  sexAM = reactive({g = dataServerAM$main(); .setnames(match(g$Sex, c("M", "F"), 0L), rownames(g))})
  sexPM = reactive({g = dataServerPM$main(); .setnames(match(g$Sex, c("M", "F"), 0L), rownames(g))})

  aliasAM = reactive({g = dataServerAM$main(); .setnames(g$Alias, rownames(g))})
  aliasPM = reactive({g = dataServerPM$main(); .setnames(g$Alias, rownames(g))})

  alleleMatAM = reactive(splitCols(genoAM()))
  alleleMatPM = reactive(splitCols(genoPM()))

  # React to ID edits ---------------------------------------------------------------------------

  observeEvent(dataServerAM$idEdits(), { .debug("ID edits")
    changes = req(dataServerAM$idEdits())
    missing = mainMissing()
    if(sum(lengths(changes)) == 0)
      return()

    tryCatch({
      peds = lapply(pedigrees(), function(peddat)
        editPeddata(peddat, idch = changes$ids, sxch = changes$sex, missing = missing))

      pedigrees(peds)
    }, error = showErr)
  })

  # State for loaded/imported DVI data -------------------------------------

  setCompleteDVI = reactiveVal(NULL)
  appData = reactiveVal(NULL)
  loading = reactiveVal(FALSE)

  dbChoice = reactiveVal("builtin")
  mutChoice = reactiveVal("none")

  ignoreDbInput = reactiveVal(FALSE)
  ignoreMutInput = reactiveVal(FALSE)

  normalizeLoadedDvi = function(dvi) { .debug("normalizeLoadedDvi")
    dvi = dvir:::consolidateDVI(dvi, dedup = TRUE)
    peds = db = mutparams = NULL

    if(length(dvi$am))
      db = getFreqDatabase(dvi$am)
    else if(length(dvi$pm))
      db = getFreqDatabase(dvi$pm)

    if(length(dvi$am)) {
      peds = lapply(dvi$am, function(a) list(ped = a,
                                             miss = .myintersect(dvi$missing, a$ID),
                                             refs = typedMembers(a)))

      mutmods = getLocusAttributes(dvi$am, attribs = "mutmod", simplify = TRUE)
      mutparams = lapply(mutmods, \(mut) pedmut::getParams(mut, format = 4))
    }

    # appData containing all info to reconstruct the dvi state
    list(am = genosWithAttrs(dvi$am, addCols = c("Sample", "Sex")),
         pm = genosWithAttrs(dvi$pm, addCols = c("Sample", "Sex")),
         peds = peds,
         db = db,
         mutparams = mutparams,
         hasMut = sum(lengths(mutparams)) > 0)
  }

  syncRadio = function(id, value, flag) {
    flag(TRUE)
    freezeReactiveValue(input, id)
    updateRadioButtons(session, id, selected = value)
  }

  observeEvent(input$dbtype, {
    if(ignoreDbInput()) {
      ignoreDbInput(FALSE)
      return()
    }
    dbChoice(input$dbtype)
  }, ignoreInit = TRUE, priority = 100)

  observeEvent(input$muttype, {
    if(ignoreMutInput()) {
      ignoreMutInput(FALSE)
      return()
    }
    mutChoice(input$muttype)
  }, ignoreInit = TRUE, priority = 100)

  loadDvi = function(dvi) { .debug("loadDvi")
    dat = normalizeLoadedDvi(dvi)
    loading(TRUE)

    appData(dat)
    pedigrees(dat$peds)
    externalAM(dat$am)
    externalPM(dat$pm)

    dbChoice("original")
    mutChoice(if(dat$hasMut) "original" else "none")

    syncRadio("dbtype", "original", ignoreDbInput)
    syncRadio("muttype", if(dat$hasMut) "original" else "none", ignoreMutInput)

    session$onFlushed(function() loading(FALSE), once = TRUE)
  }

  observeEvent(setCompleteDVI(), {
    loadDvi(setCompleteDVI())
    setCompleteDVI(NULL)
  }, ignoreNULL = TRUE)

  observeEvent(input$example, {
    loadDvi(get(req(input$example)))
  }, ignoreInit = TRUE)

  # Database and mutation settings ----------------------------------------

  customDB = reactive({ .debug("customDB")
    path = req(input$customDB$datapath)
    tryCatch({
      if(identical(tools::file_ext(path), "fam")) {
        y = pedFamilias::readFam(path, verbose = FALSE)
        if(length(y) && is.ped(y[[1]]))
          getFreqDatabase(y[[1]])
        else if(length(y[[1]]) && is.ped(y[[1]][[1]]))
          getFreqDatabase(y[[1]][[1]])
        else { # if database only
          names(y) = vapply(y, function(lst) lst$name, FUN.VALUE = "")
          lapply(y, function(lst) .setnames(lst$afreq, lst$alleles))
        }
      }
      else {
        readFreqDatabase(path, sep = "\t")
      }
    }, error = errModal)
  })

  selectedDB = reactive({ .debug("selectedDB")
    if(loading())
      return(NULL)

    switch(dbChoice(),
      builtin = switch(req(input$builtinDB),
        "norSTR: Africa" = norSTR::africaDB,
        "norSTR: Europe" = norSTR::europeDB,
        "norSTR: Norway" = norSTR::norwayDB,
        "NorwegianFrequencies (legacy)" = forrel::NorwegianFrequencies,
        NULL
      ),
      custom = customDB(),
      original = appData()$db, # NULL ok
      NULL
    )
  })

  mutParams = reactive({ .debug("mutParams")
    if(loading())
      return(NULL)

    choice = mutChoice() %||% "none"

    switch(choice,
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
      original = appData()$mutparams # NULL ok
    )
  })

  observedAlleles = reactive({ .debug("observedAlleles")
    amatAM = alleleMatAM()
    amatPM = alleleMatPM()

    am = pm = NULL

    if(!is.null(amatAM)) {
      am = lapply(seq_len(ncol(amatAM)/2), function(k)
        unique.default(as.character(amatAM[, (2*k-1):(2*k)]) |> .mysetdiff(0)))
      names(am) = markersAM()
    }

    if(!is.null(amatPM)) {
      pm = lapply(seq_len(ncol(amatPM)/2), function(k)
        unique.default(as.character(amatPM[, (2*k-1):(2*k)]) |> .mysetdiff(0)))
      names(pm) = markersPM()
    }

    allmarkers = unique.default(c(names(am), names(pm))) |> .setnames()
    lapply(allmarkers, function(m) sort.default(unique.default(c(am[[m]], pm[[m]]))))
  })

  DBext = reactive({  .debug("DBext")
    db = selectedDB()
    if(is.null(db))
      return(NULL)

    obs = observedAlleles()
    if(!length(obs))
      return(db)

    dbnms = names(db)
    midx = match(normaliseName(names(obs)), normaliseName(dbnms), nomatch = 0L)
    allmiss = vector("list", length(obs))
    names(allmiss) = names(obs)

    for(i in seq_along(obs)) {
      j = midx[i]
      if(j == 0L)
        next

      miss = .mysetdiff(obs[[i]], names(db[[j]]))
      if(length(miss)) {
        dbname = dbnms[j]
        db[[dbname]] = .addAlleles(db[[j]], miss)
        allmiss[[i]] = miss
      }
    }

    attr(db, "added") = allmiss
    db
  })

  observeEvent(DBext(), { .debug("DBext observer")
    db = DBext()
    n = sum(lengths(attr(db, "added")))
    if(n > 0)
      showNotification(sprintf("Added %d missing alleles to database", n))
  }, ignoreInit = TRUE)

  locusAttrs = reactive({ .debug("locusAttrs")
    if(loading())
      return(NULL)

    db2locattrs(DBext(), mutParams())
  })

  # Derived DVI objects ----------------------------------------------------

  observeEvent(pedigrees(), {
    assigned = lapply(pedigrees(), `[[`, "refs")
    assVec = .setnames(rep(names(assigned), lengths(assigned)),
                       unlist(assigned, use.names = FALSE))
    .debug("assignedRefs:", assVec)
    assignedRefs(assVec)

    miss = unlist(lapply(pedigrees(), `[[`, "miss"), use.names = FALSE)
    mainMissing(miss)
  })

  mainAM = reactive({ .debug("mainAM", loading())
    if(loading())
      return(NULL)

    a = alleleMatAM()
    peddata = pedigrees()
    loci = locusAttrs()

    if(is.null(a) || !length(peddata) || !length(loci))
      return(NULL)

    idx = match(normaliseName(markersAM()), normaliseName(names(loci)), nomatch = 0L)
    if(any(idx == 0L)) {
      showErr("Aborting - database does not contain these AM markers: ", markersAM()[idx == 0L])
      return(NULL)
    }

    peds = lapply(peddata, `[[`, "ped")

    tryCatch(
      setMarkersDiviana(peds, alleleMatrix = a, loci = loci[idx]),
      error = showErr
    )
  })

  mainPM = reactive({ .debug("mainPM", loading())
    if(loading())
      return(NULL)

    a = alleleMatPM()
    loci = locusAttrs()

    if(is.null(a) || is.null(loci))
      return(NULL)

    idx = match(normaliseName(markersPM()), normaliseName(names(loci)), nomatch = 0L)
    if(any(idx == 0L)) {
      showErr("Aborting - database does not contain these PM markers: ", markersPM()[idx == 0L])
      return(NULL)
    }
    s = pedtools::singletons(rownames(a), sex = sexPM()) |> .setnames(rownames(a))

    tryCatch(
      setMarkersDiviana(s, alleleMatrix = a, loci = loci[idx]),
      error = showErr
    )
  })

  currentDviData = reactive({ .debug("currentDviData")
    if(loading())
      return(NULL)

    am = mainAM()
    pm = mainPM()
    missing = mainMissing()

    if(is.null(am) && is.null(pm))
      return(NULL)

    if(length(missing) && !all(missing %in% labels(am)))
      message("Something wrong: missing person not in AM!", toString(missing))

    tryCatch(dviData(am = am, pm = pm, missing = missing),
      error = function(e) {showErr(conditionMessage(e)); NULL})
  })

  observeEvent(currentDviData(), {
    resetAnalysis(resetAnalysis() + 1)
  })

  observeEvent(dataServerAM$completeDvi(), { .debug("AM complete dvi")
    setCompleteDVI(dataServerAM$completeDvi())
  }, ignoreNULL = TRUE)

  observeEvent(dataServerPM$completeDvi(), { .debug("PM complete dvi:")
    setCompleteDVI(dataServerPM$completeDvi())
  }, ignoreNULL = TRUE)

  observeEvent(c(dataServerAM$sources(), dataServerPM$sources()), { .debug("source changed")
    src = c(dataServerAM$sources(), dataServerPM$sources())
    if(!all(src == "Example"))
      isolate(updateSelectInput(session, "example", selected = ""))
  })

  observe(shinyjs::toggleState("mutApplyAll", condition = input$muttype == "standard"))

  observe({
    dat = appData()
    toggleState(
      selector = "input[name='muttype'][value='original']",
      condition = !is.null(dat) && isTRUE(dat$hasMut)
    )
  })

  # Database: Marker summary table ----------------------------------------

  output$markersummary = DT::renderDT({ .debug("render marker table")
    loc = locusAttrs()
    if(is.null(loc))
      return(NULL)
    mtab = markerSummaryDiviana(loc, dvi = currentDviData())
    formatDatabaseTable(mtab)
  })

  observeEvent(input$chartButton, { .debug("freq chart")
    i = req(input$markersummary_rows_selected)
    loc = req(locusAttrs()[[i]])
    mname = loc$name
    freqs = .setnames(loc$afreq, loc$alleles)

    output$freqPlot = renderPlot(plotFreqs(freqs, mname))
    output$freqTable = DT::renderDT(formatFreqTable(freqs))

    showModal(modalDialog(
      title = paste("Allele frequencies:", mname),
      size = "l",
      easyClose = TRUE,
      tags$div(
        style = "display:flex;align-items:flex-start;gap:15px;",
        tags$div(style = "flex:0 0 75%;", plotOutput("freqPlot", height = 350)),
        tags$div(style = "flex:0 0 25%;", DT::DTOutput("freqTable"))
      )
    ))
  })

  observeEvent(input$mutMatrixButton, { .debug("mutation matrix button")
    i = req(input$markersummary_rows_selected)
    loc = req(locusAttrs()[[i]])
    mutmod = req(loc$mutmod)
    malemat = mutmod$male |> as.matrix() |> formatMatrix() |> gt::fmt_auto()
    femalemat = mutmod$female |> as.matrix() |> formatMatrix() |> gt::fmt_auto()

    output$mutmatrix = gt::render_gt({
      switch(req(input$mutsex), Female = femalemat, Male = malemat)
    }, height = 400)

    showModal(modalDialog(
      title = paste("Mutation matrix:", loc$name),
      size = "l",
      radioButtons("mutsex", NULL, choices = c("Female", "Male"), selected = "Female",
                   inline = TRUE),
      gt::gt_output("mutmatrix"),
      easyClose = TRUE
    ))
  })

  # Pedigrees -----------------------------------------------------------

  pedFromModule = reactiveVal()
  isNewPed = reactiveVal(FALSE)
  nPed = reactive(length(pedigrees()))
  curPed = cardCounter("pedcard", nPed)

  observeEvent(input$newped, { .debug("new pedigree")
    isNewPed(TRUE)
    uniqueID = uniquify("quickpedModule")

    avoidLabs = list(famids = names(mainAM()),
                     vics = names(mainPM()),
                     refs = if(nPed() > 0) typedMembers(mainAM()) else NULL, # TODO: use assignedRefs()?
                     miss = mainMissing(),
                     labs = labels(mainAM()))

    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = NULL,
                   famid = paste0("F", nPed() + 1),
                   allrefs = rownames(genoAM()),
                   refsex = sexAM(),
                   avoidLabs = avoidLabs,
                   currentModal = currentModal,
                   .debug = .debug)
  })

  observeEvent(input$editped, { .debug("edit current pedigree")

    curped = curPed()
    if(curped == 0) {
      showErr("No pedigree to edit.")
      return()
    }

    isNewPed(FALSE)
    curr = req(pedigrees()[[curped]])
    uniqueID = uniquify("quickpedModule")
    famids = names(mainAM())

    otherFams = mainAM()[-curped]
    avoidLabs = list(famids = famids[-curped],
                     vics = names(mainPM()),
                     refs = if(nPed() > 1) typedMembers(otherFams) else NULL,
                     miss = .mysetdiff(mainMissing(), curr$ped$ID),
                     labs = labels(otherFams))

    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = curr,
                   famid = famids[curped],
                   allrefs = rownames(genoAM()),
                   refsex = sexAM(),
                   avoidLabs = avoidLabs,
                   currentModal = currentModal,
                   .debug = .debug)
  })

  observeEvent(input$delped, { .debug("delete pedigree")
    curped = curPed()
    if(curped == 0) {
      showErr("No pedigree to delete.")
      return()
    }

    peds = pedigrees()
    peds[[curped]] = NULL
    pedigrees(peds)
  })

  observeEvent(pedFromModule(), { .debug("pedigree returned from module")
    newdat = req(pedFromModule())
    peds = pedigrees()
    if(isNewPed()) {
      idx = nPed() + 1
      peds[[newdat$famid]] = newdat
    }
    else {
      idx = curPed()
      peds[[idx]] = newdat
      names(peds)[idx] = newdat$famid
    }
    pedigrees(peds)
    curPed(idx)
    isolate(updateSelectInput(session, "example", selected = ""))
  })

  output$pedplot = renderPlot({ .debug("plot current pedigree:", curPed());
    curped = curPed()
    req(curped > 0)
    peds = pedigrees()
    peddat = peds[[curped]]

    title = names(peds)[curped]
    ped = peddat$ped
    miss = peddat$miss
    refs = peddat$refs

    labs = if(settings$hideUnimportantLabs) c(refs, miss) else ped$ID
    if(settings$useAliases)
      labs = useAlias(labs, aliasAM())

    ngen = generations(ped)
    topmar = switch(ngen, 5,5,3,2)
    botmar = topmar - 1
    sidemar = switch(ngen, 6,6,4,2)
    margins = c(botmar, sidemar, topmar, sidemar)

    plot(ped, title = title, hatched = refs, cex = 1.2, cex.main = 1.5,
         margins = margins, labs = labs, foldLabs = 8,
         carrier = miss, col = list("red" = miss), lwd = list("1.2" = miss))
    box("outer", col = 1)
  },
  execOnResize = TRUE, res = 72, height = 440)

  output$dvisummary = renderInfoBox({ .debug("update summary info box")
    s = NULL
    col = "warning"
    if(!length(mainAM()))
      s = c(s, "No AM data loaded")
    if(!length(mainPM()))
      s = c(s, "No PM data loaded")
    if(!length(mainMissing()))
      s = c(s, "No missing persons indicated")

    if(is.null(s)) {
      dvi = currentDviData()
      s = capture.output(print(dvi))[-1]
      s[1:4] = sub(":.*", "", s[1:4]) |> trimws("left")
      col = "success"
    }

    bs4InfoBox(
      title = NULL,
      width = 12,
      elevation = 1,
      color = col,
      value = HTML(paste0(s, collapse = "<br>")),
      icon = shiny::icon("info")
    )
  })


  # Overview plot -----------------------------------------------------------

  observeEvent(input$plotdviButton, { .debug("click Overview button")
    if(nPed() == 0)
      showErr("No pedigrees to show")
    req(nPed() > 0)
    w = if(nPed() > 5) "1000px" else "800px"

    showModal(modalDialog(
      title = "DVI overview",
      tags$style(HTML("
        #shiny-modal .modal-dialog {width: fit-content; max-width: 95vw; margin: 1.75rem auto;}
        #shiny-modal .modal-body {overflow-x: auto;}
      ")),
      plotOutput("plotdvi", width = plotdims()$wpx, height = plotdims()$hpx),
      easyClose = TRUE
    ))
  })

  plotdims = reactive({ .debug("plot dims")
    dvi = currentDviData()
    k = sum(pedsize(dvi$am))
    dims = dvir:::findPlotDims(dvi$am, npm = length(dvi$pm))
    amdims = dims$amSize
    w = min(1000, max(400 + 50*amdims[1], 400 + 20*k))
    h = min(600, max(200 + 50*amdims[2], 250 + 10*k))
    list(w = w, h = h, wpx = paste0(w, "px"), hpx = paste0(h, "px"))
  })

  output$plotdvi = renderPlot({ .debug("render Overview plot")
    dvi = currentDviData()
    req(length(dvi$am) > 0, length(dvi$pm) > 0, length(dvi$missing) > 0)
    labs = c(rownames(genoAM()), rownames(genoPM()), dvi$missing)
    labfun = getLabfun(dvi, settings$hideUnimportantLabs, settings$useAliases,
                       c(aliasAM(), aliasPM()))
    tryCatch(plotDVI(dvi, style = 2, labs = labfun),
             error = function(e) {
               msg = conditionMessage(e)
               if(grepl("Cannot fit|no room", msg))
                 msg = "Sorry - the plot is too big!"
               else
                 msg = c("Sorry, cannot plot this.", msg)
               plot.new()
               text(0.5, 0.5, paste(strwrap(msg, width = 60), collapse = "\n"), cex = 1.2)
             })
  }, res = 96,
  width = function() plotdims()$w,
  height = function() plotdims()$h,
  execOnResize = FALSE)


  # Tab: Triangles ----------------------------------------------------------

  kappa = reactiveValues(am = NULL, pm = NULL, ampm = NULL)
  observeEvent(input$acrossComps, {kappa$am = NULL}, ignoreInit = TRUE)

  # Number of typed in each fam (NULL -> integer0)
  nRefs = reactive(lengths(lapply(mainAM(), pedtools::typedMembers)))

  output$amMsg   = renderUI(cpMessage(kappa$am, "am", nref = nRefs(), across = input$acrossComps))
  output$pmMsg   = renderUI(cpMessage(kappa$pm, "pm", nvic = length(mainPM())))
  output$ampmMsg = renderUI(cpMessage(kappa$ampm, "ampm", nref = nRefs(), nvic = length(mainPM())))

  output$amTable = DT::renderDT(
    formatCP(kappa$am, settings$useAliases, alias1 = aliasAM()), server = FALSE)

  output$pmTable = DT::renderDT(
    formatCP(kappa$pm, settings$useAliases, alias1 = aliasPM()), server = FALSE)

  output$ampmTable = DT::renderDT(
    formatCP(kappa$ampm, settings$useAliases, alias1 = aliasAM(), alias2 = aliasPM()),
    server = FALSE)

  observeEvent(input$amKappaCalc, { .debug("AM kappa calculate")
    kappa$am = CPnoplot(req(mainAM()), acrossComps = input$acrossComps) })

  observeEvent(input$pmKappaCalc, { .debug("PM kappa calculate")
    kappa$pm = CPnoplot(req(mainPM())) })

  observeEvent(input$ampmKappaCalc, { .debug("AM-PM kappa calculate")
    am = req(mainAM())
    pm = req(mainPM())
    idMatr = pedtools:::fast.grid(list(typedMembers(am), names(pm))) |> req()
    commonMarkers = .myintersect(name(am), name(pm)) |> req()
    allcmps = c(selectMarkers(am, commonMarkers), selectMarkers(pm, commonMarkers))
    k = forrel::ibdEstimate(allcmps, ids = idMatr, verbose = FALSE)
    kappa$ampm = as.data.frame(k)
  })

  output$amTriangle = renderPlotly({ .debug("am triangle")
    input$amTable_search # trigger on search/filter
    df = kappa$am[isolate(input$amTable_rows_all), , drop = FALSE]
    forrel::plotCP(df, plotType = "plotly", xlab = "", ylab = "")
  })

  output$pmTriangle = renderPlotly({ .debug("pm-triangle")
    input$pmTable_search # trigger on search/filter
    df = kappa$pm[isolate(input$pmTable_rows_all), , drop = FALSE]
    forrel::plotCP(df, plotType = "plotly", xlab = "", ylab = "", errtxt = "Potential relationship")
  })

  output$ampmTriangle = renderPlotly({ .debug("ampm-triangle");
    input$ampmTable_search # trigger on search/filter
    df = kappa$ampm[isolate(input$ampmTable_rows_all), , drop = FALSE]
    forrel::showInTriangle(df, plotType = "plotly", xlab = "", ylab = "", pch = 16, col = "pink", cex = 1.2)
  })

  # Tab: Analysis ---------------------------------------------------------------

  solutionTable = reactiveValues(AM = NULL, PM = NULL, LR = NULL, EX = NULL, JT = NULL)
  logMessage = reactiveVal("")

  observeEvent(input$solve, { .debug("solve")
    dvi = currentDviData()
    req(length(dvi$am) > 0, length(dvi$pm) > 0, length(dvi$missing) > 0)
    res = NULL
    tryCatch({
      res = captureOutput(dviSolve, dvi, threshold = input$LRthresh,
                          maxIncomp = input$maxIncomp,
                          limit = input$pairLRmin,
                          maxAssign = settings$maxAssign,
                          ignoreSex = input$ignoresex, verbose = TRUE,
                          debug = DEVMODE, detailedOutput = TRUE)
      },
      error = function(e) {
        msg = conditionMessage(e)
        if(grepl("Impossible initial data: AM component ([0-9]+)", msg)) {
          family = sub(".*AM component ([0-9]+).*", "\\1", msg)
          msg = paste0(sprintf("The reference data for family %s has likelihood 0!<br>", family),
                  "A mutation model must be applied in order to proceed.")
        }
        errModal(msg, html = TRUE)
    })

    req(res)
    solutionTable$AM = res$result$AM
    solutionTable$PM = res$result$PM
    solutionTable$LR = res$result$LRmatrix
    solutionTable$EX = res$result$exclusionMatrix
    solutionTable$JT = res$result$jointTable
    logMessage(res$log)
  })

  observeEvent(currentDviData(), {  .debug("reset result tables")
    dvi = currentDviData()

    # AM and PM data frames
    e1 = character(length(miss <- dvi$missing))
    solutionTable$AM = data.frame(Family = getFamily(dvi, miss), Missing = miss,
      Sample = e1, LR = e1, GLR = e1, Conclusion = e1, Comment = e1)

    e2 = character(length(vics <- names(dvi$pm)))
    solutionTable$PM = data.frame(Sample = vics, Missing = e2, Family = e2,
                                  LR = e2, GLR = e2, Conclusion = e2, Comment = e2)
    # LR and exclusion matrices
    solutionTable$LR = solutionTable$EX = solutionTable$JT = NULL
  })

  output$amcentric = gt::render_gt({  .debug("render result AM")
    formatResultTable(req(solutionTable$AM), title = "AM-centered result table",
                      settings$useAliases, aliasPM = aliasPM())
  }, height = 600)

  output$pmcentric = gt::render_gt({  .debug("render result PM")
    formatResultTable(req(solutionTable$PM), title = "PM-centered result table",
                      settings$useAliases, aliasPM = aliasPM())
  }, height = 600)

  output$lrmatrix = gt::render_gt({ .debug("render LR matrix")
    dvi = currentDviData()
    m = req(solutionTable$LR) |> completeMatrix(names(dvi$pm), dvi$missing)
    formatLRmatrix(m, title = "Pairwise LR matrix",
                   input$LRthresh, settings$useAliases, aliasPM = aliasPM())
  }, height = 600)

  output$exmatrix = gt::render_gt({ .debug("render exclusion matrix")
    dvi = currentDviData()
    m = req(solutionTable$EX) |> completeMatrix(names(dvi$pm), dvi$missing)
    formatExclusionMatrix(m, title = "Pairwise exclusion matrix",
                          input$maxIncomp, settings$useAliases, aliasPM = aliasPM())
  }, height = 600)

  output$jointtabs = renderUI({ .debug("render joint tables")
    dvi = currentDviData()
    JT = solutionTable$JT

    if(is.null(JT))
      return(NULL)

    if(!length(JT))
      return(tags$em("No joint tables"))

    labs = names(JT)

    tabs = lapply(seq_along(JT), \(i) {
      id = paste0("joint-", i)
      local({
        ii = i
        gttab = formatJointTab(JT[[ii]], vics = names(dvi$pm), miss = dvi$missing,
                               title = sprintf("Joint likelihood of %d assignments", nrow(JT[[ii]])),
                               usealias = settings$useAliases, aliasPM = aliasPM())
        output[[id]] = gt::render_gt(gttab)
      })
      tabPanel(labs[i], div(style = "height: 560px; overflow: auto", gt::gt_output(id)))
    })

    do.call(tabsetPanel, tabs)
  })

  output$solvelog = renderText({ .debug("render result log")
    logMessage()[-1] |> paste0(collapse = "\n")
  })

  curSols = cardCounter("solutioncard", reactive(ceiling(nPed()/6)))

  output$solutionplot = renderPlot({ .debug("plot solution")
    dvi = currentDviData()
    req(length(dvi$am) > 0, length(dvi$pm) > 0, length(dvi$missing) > 0)
    pednr = seq(curSols() * 6 - 5, min(nPed(), curSols() * 6))
    labfun = getLabfun(dvi, settings$hideUnimportantLabs, settings$useAliases, aliasAM())
    plotSolutionDIVIANA(dvi, solutionTable$AM, pednr = pednr, labs = labfun)
  }, res = 96, width = "auto", height = 600, execOnResize = TRUE)

  # Download solution tables ------------------------------------------------

  output$downloadTables = downloadHandler(
    filename = function() sprintf("diviana-%s.xlsx", Sys.Date()),
    content = function(file) { .debug("download")
      req(solutionTable$AM)
      dvi = req(currentDviData())
      tables = reactiveValuesToList(solutionTable)
      tables$LR = tables$LR |> completeMatrix(names(dvi$pm), dvi$missing)
      tables$EX = tables$EX |> completeMatrix(names(dvi$pm), dvi$missing)
      downloadTables(tables, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )


  # Settings dialog ----------------------------------------------------------

  settings = reactiveValues(hideUnimportantLabs = TRUE,
                            useAliases = TRUE,
                            maxAssign = 1e5,
                            standardmodel = "equal")

  observeEvent(input$settings, {
    showModal(modalDialog(
      tags$style(HTML("
        #settingsModal label { font-weight: 400 !important; }
      ")),
      tags$div(
        id = "settingsModal",
        h3("Settings"),
        h4("Names and aliases"),
        awesomeCheckbox("hideUnimportantLabs", "Hide irrelevant names in pedigree plots",
                        value = settings$hideUnimportantLabs, width = "auto"),
        awesomeCheckbox("useAliases", "Use aliases (short names) in plots and tables",
                        value = settings$useAliases, width = "auto"),
        br(),
        h4("Mutation model"),
        awesomeRadio("standardmodel", "Model family for `Standard` models",
                     choices = c("Equal" = "equal", "Proportional" = "proportional",
                                 "Stepwise" = "stepwise"),
                     selected = settings$standardmodel, inline = TRUE, width = "auto"),
        br(),
        h4("Analysis"),
        numericInput("maxAssign", "Maximum number of assignment in joint analysis",
                     value = settings$maxAssign, width = "auto"),
      ),
      easyClose = TRUE,
      footer = modalButton("Save and close"),
    ))
  })

  observeEvent(input$hideUnimportantLabs, {settings$hideUnimportantLabs = input$hideUnimportantLabs})
  observeEvent(input$useAliases, {settings$useAliases = input$useAliases})
  observeEvent(input$maxAssign, {settings$maxAssign = input$maxAssign})
  observeEvent(input$standardmodel, {settings$standardmodel = input$standardmodel})

  # Reset -------------------------------------------------------------------

  resetAnalysis = reactiveVal(0)

  observeEvent(input$resetall, { .debug("reset all")
    session$sendCustomMessage("clearOutputs", c(
      "amcentric", "pmcentric", "lrmatrix", "exmatrix", "jointtabs", "solvelog",
      "solutionplot", "pedplot", "dvisummary", "AM-mainTableUI", "PM-mainTableUI"
    ))
    externalAM("reset")
    externalPM("reset")
    externalLoci$db = externalLoci$mut = NULL
    externalLoci$hasMut = FALSE
    pedigrees(NULL)
    mainMissing(NULL)
    DB(NULL)
    isolate(updateSelectInput(session, "example", selected = ""))
    updateRadioButtons(session, "dbtype", selected = character(0))
    updateRadioButtons(session, "muttype", selected = character(0))
    updateNumericInput(session, "mutrateF", value = 0.001)
    updateNumericInput(session, "mutrateM", value = 0.002)
    updateNumericInput(session, "LRthresh", value = 10000)
    updateNumericInput(session, "maxIncomp", value = 2)
    updateCheckboxInput(session, "ignoresex", value = FALSE)
    updateCheckboxInput(session, "acrossComps", value = FALSE)
    resetAnalysis(resetAnalysis() + 1)
    # Settings dialog
    settings$hideUnimportantLabs = TRUE
    settings$useAliases = TRUE
    settings$standardmodel = "equal"
    settings$maxAssign = 1e5
  })

  observeEvent(resetAnalysis(), { .debug("reset results")
    kappa$am = kappa$pm = kappa$ampm = NULL
    solutionTable$AM = solutionTable$PM = solutionTable$LR = solutionTable$EX = solutionTable$JT = NULL
    logMessage(NULL)
  }, ignoreInit = TRUE)


  observe({
    if(DEVMODE) { .debug("devmode!")
      updateSelectInput(session, "AM-filetype", selected = "gm")
      #updateSelectInput(session, "example", selected = "icmp")
      #updateNavbarTabs(session, "navmenu", selected = "analysis")
    }
  })

  # Debug -------------------------------------------------------------------

  debuglog = reactiveVal("")
  output$debugOutput = renderText(debuglog())

  # Reset when debug is toggled
  #observeEvent(input$debug, debuglog("### DEBUG LOG ###"))

  #.debug = function(...) {
  #  if(DEVMODE) print(paste(..., collapse = " "))
    #if(isTRUE(input$debug))
    #  isolate(debuglog(paste(debuglog(), paste(..., collapse = " "), sep = "\n")))
  #}

  output$debugElements = renderPrint({
    req(input$debug)
    cat("### CURRENT OBJECTS ###\n")
    print(mainAM(), verbose = F)
    print(mainPM(), verbose = F)
    print(mainMissing(), verbose = F)
    #cat("\n")
    #print(abbrMat(genoTable$am))
    #cat("\n")
    #print(abbrMat(genoTable$pm))
  })


  output$downloaddata = downloadHandler(
    filename = function() "dviData.rds",
    content = function(file) { .debug("download data")
      dvi = currentDviData()
      saveRDS(dvi, file)
    }
  )


  # Help pages --------------------------------------------------------------

  # All help files
  helpDir = system.file("shiny/www/static_docs", package = "diviana")
  helpFiles = list.files(helpDir, pattern = "^help-.*\\.html$", full.names = FALSE)
  helpIds = sub("\\.html$", "", helpFiles)

  currentModal = reactiveVal()

  showInstructions = function(id) { .debug("help:", id)
    helpfile = file.path(helpDir, sprintf("%s.html", id))
    req(file.exists(helpfile))

    showModal(modalDialog(
      title = NULL,
      div(
        includeHTML(helpfile),
        style = "max-height:80vh; overflow-y:auto; font-size: 90%; padding: 0"),
      tags$style(HTML("code {
        background-color: #f8f8f8;
        color: #333;
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 1px 4px;
        font-size: 90%;}")),
      footer = actionButton("close_instructions", "Back"),
      easyClose = FALSE
    ))
  }

  observeEvent(input$close_instructions, { .debug("close help")
    removeModal()
    if(!is.null(m <- currentModal()))
      showModal(m)
  })

  # Create observers for each help button
  lapply(helpIds, function(id) observeEvent(input[[id]], showInstructions(id)))
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
