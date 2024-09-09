suppressMessages(suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(shinyjs)
  library(gt)
  library(pedtools)
  library(pedmut)
  library(dvir)
  library(verbalisr)
  library(plotly)
  library(openxlsx)
}))


# --------------------------------------------------------------------
#'
#' DATA
#' *
#' * Save (as RData) button
#' * Genemapper tall import (gives plot errors)
#' * Genemapper wide + Relationship
#'
#' TRIANGLES
#' * Triangle plots: Latex labels
#' * Triangle AM: Select family
#'
#'
# -------------------------------------------------------------------------


DEVMODE = FALSE

DATASETS = c("example1", "example2", "exclusionExample", "fire", "grave", "icmp", "planecrash")

# UI ----------------------------------------------------------------------

ui = bs4DashPage(

  dark = NULL,
  help = NULL,

  # Header
  bs4DashNavbar(
    status = "info",
    title = div(
      img(src = "diviana-moth-logo.png", style = "height: 80px; margin-left:0px; padding:5px 10px"),
      h2("DIVIANA", id = "appname", style = "display: inline"),
    ),
    navbarMenu(
         id = "navmenu",
         navbarTab(tabName = "data", text = "Data"),
         navbarTab(tabName = "relatedness", text = "Relatedness"),
         navbarTab(tabName = "analysis", text = "Analysis")
    ),
    rightUi = tagList(tags$li(
      class = "nav-item dropdown",
      style = "margin-right: 20px; width: 200px",
      selectInput("example", NULL, choices = c("Load example" = "", DATASETS))))
  ),

  # Sidebar
  bs4DashSidebar(disable = TRUE, minified = FALSE),


  # Main panel
  bs4DashBody(
    includeCSS("www/custom.css"),
    tags$head(tags$script(src = "scripts.js")),

    useShinyjs(),
    useBusyIndicators(spinners = FALSE, pulse = TRUE),

   tabItems(

   # Tab: Data ---------------------------------------------------------------

   tabItem("data",
    fluidRow(column(width = 5, class = "col-xs-12 col-lg-4",

      bs4Card(width = 12,
        title = "AM data",
        fluidRow(
          column(4,
            pickerInput("filetypeAM", label = NULL, width = "100%",
                        choices = c("Familias", "Genemapper", "dviData (.RData/.rds)"),
                        options = pickerOptions(style = "btn-outline-secondary")),
          ),
          column(8,
            fileInput("amfile", NULL,
                      accept = c(".csv", ".tsv", ".txt", ".fam", ".rds", ".RData"))
          ),
        ),
        gt::gt_output("amdata")
      ),

      bs4Card(width = 12,collapsed = TRUE,
        title = "PM data",
        checkboxInput("pm_in_am", label = "Included in AM data"),
        conditionalPanel(condition = "!input.pm_in_am",
          fluidRow(
            column(4,
              pickerInput("filetypePM", label = NULL, width = "100%",
                          choices = c("Familias", "Genemapper"),
                          options = pickerOptions(style = "btn-outline-secondary")),
            ),
            column(8,
              fileInput("pmfile", NULL, accept = c(".csv", ".tsv", ".txt", ".fam"))
            ),
          ),
        ),
        gt::gt_output("pmdata")
      ),

      bs4Card(width = 12, collapsed = TRUE, class = "mutation-inputs", style = "padding-top:0",
        title = "Mutation model",
        fluidRow(style = "margin-top: 10px; margin-bottom: 10px",
          column(3, align = "right",
            actionBttn("prevmark", label = NULL, style = "jelly", size = "xs", icon = icon("backward-step")),
          ),
          column(6, align = "middle", uiOutput("markName"),
                 style = "font-size: bigger; background-color: lightyellow; border: 1px solid #dee2e6; border-radius: 0.25rem;"),
          column(3, align = "left",
            actionBttn("nextmark", label = NULL, style = "jelly", size = "xs", icon = icon("forward-step")),
          )
        ),

        radioButtons("mutmodel", NULL, inline = TRUE, width = "100%", selected = character(0),
                     choices = c("No model" = "none", Equal = "equal", Prop = "proportional",
                                 Stepwise = "stepwise")),
        fluidRow(
          column(3, offset = 3, em("Rate")),
          column(3, em("Rate2")),
          column(3, em("Range")),
        ),
        fluidRow(
          column(3, "Female:"),
          column(3, numericInput("rateF", label = NULL, value = NA, max = 1)),
          column(3, numericInput("rate2F", label = NULL, value = NA, max = 1)),
          column(3, numericInput("rangeF", label = NULL, value = NA, max = 1)),
        ),
        fluidRow(
          column(3, "Male:"),
          column(3, numericInput("rateM", label = NULL, value = NA, max = 1)),
          column(3, numericInput("rate2M", label = NULL, value = NA, max = 1)),
          column(3, numericInput("rangeM", label = NULL, value = NA, max = 1))
        ),
        fluidRow(style = "margin-top: 10px;",
          column(6, actionButton("mutApply1", "Apply to this marker", status = "info", size = "sm")),
          column(6, actionButton("mutApplyAll", "Apply to all markers", status = "info", size = "sm", style="float:right"))
        )
      ),

      bs4Card(width = 12, collapsed = TRUE,
        title = "Frequency database",
        radioButtons("dbtype", NULL, inline = TRUE, width = "100%",
                     choices = c("Built-in" = "builtin",
                                 "In dataset" = "data",
                                 "Custom" = "custom")),
        conditionalPanel(
          condition = "input.dbtype == 'builtin'",
          pickerInput("dbselect", NULL, choices = "NorwegianFrequencies",
                      options = pickerOptions(title = "Builtin database",
                                              style = "btn-outline-secondary"))
        ),
        conditionalPanel(
          condition = "input.dbtype == 'custom'",
          fileInput("dbcustom", NULL, accept = c("text/tab-separated-values", "text/plain", ".txt"))
        )

        # pickerInput("freqmarker", label = "Marker", choices = NULL,
        #             options = pickerOptions(title = "Select marker",
        #                                     style = "btn-outline-secondary")),
        # plotOutput("freqhist", height = "220")
      ),
    ),

    column(width = 5, id = "pedcol", class = "col-xs-12 col-lg-4",

      bs4Card(
        width = NULL,
        title = div("Pedigrees", style = "display: flex; align-items: center;",
                    div(style = "margin-left: 50px;",
                        actionButton("plotdviButton", "Overview", class = "btn-sm"))),
        fluidRow(
          column(2, align = "left", actionButton("newped", "New")),
          column(2, align = "right",
            actionBttn("prevped", label = NULL, style = "jelly", size = "s", icon = icon("backward-step")),
          ),
          column(4, align = "middle", uiOutput("pedTitle"),
                 style = "font-size: larger; padding-top:3px; background-color: lightcyan; border: 1px solid #dee2e6; border-radius: 0.25rem;"),
          column(2, align = "left",
            actionBttn("nextped", label = NULL, style = "jelly", size = "s", icon = icon("forward-step")),
          ),
          column(2, align = "right", actionButton("editped", "Edit")),
        ),
        plotOutput("pedplot", width = "auto", height = "auto")
      ),

      bs4InfoBoxOutput("dvisummary", width = 12)
    ),

    column(width = 2, id = "labcol", class = "col-xs-12 col-lg-4",
      bs4Card(width = 12, id = "labels-card",
        title = "Labels",
        fluidRow(
          column(9, style = "line-height: 1.2rem",
            p("Edit names manually below or use Quick Action buttons.
              Original labels are retained and included in downloads.")),
          column(3,
            actionButton("saveAlias", "Save", status = "info", size = "lg", width = "100%"))
        ),
        fluidRow(
          column(3, quickAction("labels_restore", "Original")),
          column(3, quickAction("labels_simple", "Simple M/R/V")),
          column(3, quickAction("labels_remove", "Delete part:")),
          column(3, slimTextInput("labels_substr", NULL, value = "", height = 30, placeholder = "<part>")),
          ),
        hr(style = "margin-top: 10px; margin-bottom: 0px"),
        fluidRow(
          column(6,  tags$strong("AM", style = "display: block; text-align: center;"),
            fluidRow(column(7, "Original"), column(5, "Alias"))),
          column(6, tags$strong("PM", style = "display: block; text-align: center;"),
            fluidRow(column(7, "Original"), column(5, "Alias")))
        ),
        uiOutput("lab_rows")
      )
    )
  )
  ),

  # Tab: Relatedness ---------------------------------------------------------

  tabItem("relatedness",
    fluidRow(bs4Card(
      width = 8,
      title = div(style = "display: flex; align-items: center;",
                  "Reference individuals",
                  div(style = "margin-left: 30px;",
                      actionButton("amkappa", "Check pairwise", class = "btn-sm"))),
      fluidRow(
        column(width = 6, plotlyOutput("amtriangle", width = "100%", height = "400px")),
        column(width = 6, plotOutput("ampairped", width = "auto", height = "400px"))
      )
    ),
    bs4Card(
      width = 4,
      title = div(style = "display: flex; align-items: center;",
                  "PM samples",
                  div(style = "margin-left: 30px;",
                      actionButton("pmkappa", "Check pairwise", class = "btn-sm"))),
      plotlyOutput("pmtriangle", width = "100%", height = "400px")
    ),
  )),

  # Tab: Analysis-------------------------------------------------------------

  tabItem("analysis",
    fluidRow(

      # Buttons: Solve! --------------------------------------------
      column(width = 2, class = "col-xl-1",
             actionBttn("solve", label = "SOLVE", icon = icon("calculator"), color = "primary", style = "jelly", ),
             br(),hr(),br(),
             h4("Settings"),
             numericInput("LRthresh", "LR threshold", value = 10000,min = 1),
             br(), hr(), br(),
             downloadButton('downloadTables', "Download", class = "btn btn-warning",
                            style = "width:100%")
      ),

      # Report table: AM and PM tabs --------------------------------------------
      column(5, class = "col-xl-6",
         bs4TabCard(title = "Identifications", width = NULL, type = "tabs", side = "right",
                    collapsible = FALSE,
                    tabPanel(title = "AM", gt::gt_output("amcentric")),
                    tabPanel(title = "PM", gt::gt_output("pmcentric")),
                    tabPanel(title = "log", verbatimTextOutput("solvelog"))
         )
      ),

      # Pedigrees with inferred matches ------------------------------------------

      column(width = 5,
        bs4Card(title = "Result plot", width = NULL, collapsible = FALSE,
          plotOutput("solutionplot", width = "auto", height = "auto")
        ),
      ),
    )
  )))
)



# SERVER ------------------------------------------------------------------


server = function(input, output, session) {  print("starting")

  # Close app when browser closes
  observeEvent(input$browserClosed, stopApp())

  # Main reactive variables -------------------------------------------------
  mainDvi = reactiveValues(pm = NULL, am = NULL, missing = NULL)
  origLabs = reactiveValues(pm = NULL, am = NULL)
  pedigrees = reactiveVal(NULL)
  genoTable = reactiveValues(am = NULL, pm = NULL)
  DB = reactiveVal(NULL)
  resetTrigger = reactiveVal(0)

  observeEvent(resetTrigger(), { print("resetting")
    kappa$am = NULL; kappa$pm = NULL
    trianglePlot$am = NULL; trianglePlot$pm = NULL
    solutionTable$AM = NULL; solutionTable$PM = NULL
    datapathAM(NULL)
    logMessage(NULL)
  }, ignoreInit = TRUE)


  observe({
    if(DEVMODE) { print("devmode!")
      updateSelectInput(session, "example", selected = "icmp")
    }
  })

  # Load example ------------------------------------------------------------

  observeEvent(input$example, { print("loading example")
    dat = get(req(input$example)) |>
      dvir:::consolidateDVI(dedup = TRUE)

    resetTrigger(resetTrigger() + 1)
    shinyjs::reset("amfile")

    mainDvi$am = am = dat$am
    mainDvi$pm = pm = dat$pm
    mainDvi$missing = miss = dat$missing
    origLabs$am = labels(am)
    origLabs$pm = labels(pm)
    markernames$all = name(am)
    markernames$currIdx = 1

    genoTable$am = getGenotypes(am, ids = typedMembers(am))
    genoTable$pm = getGenotypes(pm)

    peds = lapply(am, function(a)
      list(ped = a, miss = intersect(labels(a), miss), refs = typedMembers(a)))
    pedigrees(peds)

    updateRadioButtons(session, "dbtype", selected = "data")
    DB(getFreqDatabase(am))
  })

  # AM data -----------------------------------------------------------------

  datapathAM = reactiveVal()
  observeEvent(input$amfile, datapathAM(input$amfile$datapath))

  observeEvent(datapathAM(), {
    fil = req(datapathAM())

    resetTrigger(resetTrigger() + 1)
    updateSelectInput(session, "example", selected = "")

    dvi = NULL
    tryCatch(switch(input$filetypeAM,
      Familias = {  print("loading Familias AM file")
        dvi = dvir::familias2dvir(fil, missingFormat = "M[FAM]-[IDX]")
      },
      Genemapper = {   print("loading Genemapper file")
        g = readGenemapper(fil)
        genoTable$am = g
        #origLabs$am = rownames(g)
        pedigrees(NULL)
      },
      "dviData (.RData/.rds)" = { print("loading dviData")
        switch(tolower(tools::file_ext(fil)),
          rds = {
            dvi = readRDS(fil)
            if(!inherits(dvi, "dviData"))
              stop2("File does not contain a `dviData` object")
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
    ), error = errModal, warning = errModal)

    # If complete data is loaded: Attach everywhere
    if(!is.null(dvi)) { print("dvi loaded")
      dvi = dvir:::consolidateDVI(dvi, dedup = TRUE)
      tryCatch({
        genoTable$am = getGenotypes(dvi$am, ids = typedMembers(dvi$am))
        genoTable$pm = getGenotypes(dvi$pm)
        origLabs$am = labels(dvi$am, unlist = TRUE)
        origLabs$pm = labels(dvi$pm, unlist = TRUE)
        updateCheckboxInput(session, "pm_in_am", value = TRUE)
        peds = lapply(dvi$am, function(a)
          list(ped = a, miss = intersect(labels(a), dvi$missing),
               refs = typedMembers(a)))
        pedigrees(peds)
        mainDvi$am = dvi$am
        mainDvi$pm = dvi$pm
        markernames$all = name(dvi$am)
        markernames$currIdx = 1
      },
      error = errModal, warning = errModal)
    }
  })

  output$amdata = gt::render_gt({ print("Rendering AM data")
    tab = as.data.frame(req(genoTable$am))
    hasAlias = rownames(tab) %in% names(currentAlias)
    rownames(tab)[hasAlias] = currentAlias$am[rownames(tab)[hasAlias]]
    formatGenoTable(tab)
  })

  # PM data -----------------------------------------------------------------

  observeEvent(input$pmfile, {
    fil = req(input$pmfile$datapath)
    tryCatch({
      switch(input$filetypePM,
      Familias = {
        famdata = dvir::familias2dvir(fil, missingFormat = "M[FAM]-[IDX]")
        genoTable$pm = getGenotypes(famdata$pm)
        mainDvi$pm = famdata$pm
      },
      Genemapper = {
        genoTable$pm = g = readGenemapper(fil)
        mainDvi$pm = rownames(g) |> singletons() |>
          setMarkers(alleleMatrix = g, locusAttributes = DB())
      })
      origLabs$pm = labels(mainDvi$pm)
    }, error = errModal, warning = errModal)
  })

  output$pmdata = gt::render_gt({  print("Rendering PM data")
    tab = as.data.frame(req(genoTable$pm))
    rownames(tab) = currentAlias$pm[rownames(tab)]
    formatGenoTable(tab)
  })


  # Mutation frame --------------------------------------------------------

  markernames = reactiveValues(all = NULL, currIdx = 0)

  observeEvent(input$prevmark, {
    markernames$currIdx = (markernames$currIdx - 2) %% length(markernames$all) + 1
  })

  observeEvent(input$nextmark, {
    markernames$currIdx = markernames$currIdx %% length(markernames$all) + 1
  })

  output$markName = renderUI({ print("Update marker name")
    idx = markernames$currIdx
    req(idx > 0, length(markernames$all))
    nam = markernames$all[idx]
    HTML(sprintf("Marker %d/%d: &nbsp; <b>%s</b>", idx, length(markernames$all), nam))
  })

  # Apply mutation model ----------------------------------------------------

  markersForMutEdit = reactiveVal(NULL)

  observeEvent(input$mutApplyAll, { print("confirm apply mut all")
    ask_confirmation("confirmMutApply", type = "warning",
      title = "Click 'Confirm' to apply this mutation model to all markers")
    markersForMutEdit(NULL)
  })

  observeEvent(input$mutApply1, { print("confirm apply mut 1")
    m = markernames$all[markernames$currIdx]
    ask_confirmation("confirmMutApply", type = "warning",
      title = sprintf("Click 'Confirm' to apply this mutation model to %s", m))
    markersForMutEdit(m)
  })

  observeEvent(req(input$confirmMutApply), { print("apply mut model to all")
    am = req(mainDvi$am)
    model = if(input$mutmodel == "none") NULL else input$mutmodel
    markers = markersForMutEdit()

    tryCatch({
      mainDvi$am = setMutmod(am, markers = markers, model = model,
        rate = list(female = input$rateF, male = input$rateM),
        rate2 = list(female = input$rate2F, male = input$rate2M),
        range = list(female = input$rangeF, male = input$rangeM))
    }, error = errModal)

    show_alert(title = "Mutation model applied!", type = "success")
  })

  observeEvent(input$mutmodel, { print("update mutmodel fields")
    mut = mutmod(req(mainDvi$am), markernames$currIdx)
    params = getParams(mut, format = 1)
    if(is.null(mut))
      params = list(rate = c(NA, NA), rate2 = c(NA, NA), range = c(NA, NA))

    setfields = switch(input$mutmodel, none = character(), equal = , proportional = "rate",
                       stepwise = c("rate", "rate2", "range"))
    disfields = setdiff(c("rate", "rate2", "range"), setfields)

    for(p in setfields) { #print(paste("set", p))
      shinyjs::enable(paste0(p, "F")); shinyjs::enable(paste0(p, "M"))
      updateNumericInput(session, paste0(p, "F"), value = params[[p]][1])
      updateNumericInput(session, paste0(p, "M"), value = params[[p]][2])
    }
    for(p in disfields) { #print(paste("disable", p))
      updateNumericInput(session, paste0(p, "F"), value = NA)
      updateNumericInput(session, paste0(p, "M"), value = NA)
      shinyjs::disable(paste0(p, "F")); shinyjs::disable(paste0(p, "M"))
    }
  })

  # Update model name when marker changes
  observeEvent(markernames$currIdx , { print("update model name")
    mut = mutmod(req(mainDvi$am), markernames$currIdx)
    model = if(is.null(mut)) "none" else getParams(mut, params = "model")$model[1]
    updateNumericInput(session, "mutmodel", value = model)
  })

  # Frequency database ------------------------------------------------------

  observeEvent(input$dbselect, { print("dbselect")
    if(input$dbselect == "")
      newdb = NULL
    else if(input$dbselect == "NorwegianFrequencies")
      newdb = forrel::NorwegianFrequencies

    DB(newdb)
  })

  observeEvent(DB(), { print("update marker names")
    m = names(req(DB()))
    updatePickerInput(session, "freqmarker", choices = m)
  })

  # Pedigrees -----------------------------------------------------------

  pedFromModule = reactiveVal()
  currentPed = reactiveVal()
  isNewPed = reactiveVal(FALSE)
  pedNr = reactiveValues(current = 0, total = 0)

  observeEvent(pedigrees(), { print("pedigrees modified")
    nped = length(pedigrees())
    if(pedNr$current == 0 || pedNr$current > nped)
      pedNr$current = 1
    pedNr$total = nped
    mainDvi$am = lapply(pedigrees(), `[[`, "ped")
    mainDvi$missing = unlist(lapply(pedigrees(), `[[`, "miss"))

    # Labels
    aliases = currentAlias$am
    aliasRev = setnames(names(aliases), aliases)

    newalias = labels(mainDvi$am)
    neworig = aliasRev[newalias] # may have missing values
    neworig[is.na(neworig)] = newalias[is.na(neworig)]
    origLabs$am = unname(neworig)
  })

  observeEvent(input$prevped, {
    pedNr$current = (pedNr$current - 2) %% pedNr$total + 1
  })

  observeEvent(input$nextped, {
    pedNr$current = pedNr$current %% pedNr$total + 1
  })

  output$pedTitle = renderUI({ print("Update ped title")
    nms = names(req(pedigrees()))
    nr = pedNr$current
    req(nr > 0)
    tit = sprintf("Family %d/%d", nr, pedNr$total)
    tit
  })

  observeEvent(input$newped, { print("newped-click")
    isNewPed(TRUE)
    uniqueID = uniquify("quickpedModule")
    refs = rownames(genoTable$am)
    hasAlias = refs %in% names(currentAlias$am)
    refs[hasAlias] = currentAlias$am[refs[hasAlias]]
    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = NULL,
                   famid = paste0("F", pedNr$total + 1), references = refs)
  })

  observeEvent(input$editped, { print("editped-click")
    isNewPed(FALSE)
    curr = req(pedigrees()[[pedNr$current]])
    uniqueID = uniquify("quickpedModule")
    refs = rownames(genoTable$am)
    hasAlias = refs %in% names(currentAlias$am)
    refs[hasAlias] = currentAlias$am[refs[hasAlias]]
    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = curr,
                   famid = paste0("F", pedNr$current), references = refs)
  })

  observeEvent(pedFromModule(), { print("pedmodule-result")
    newdat = req(pedFromModule())

    # Transfer marker data
    g = NULL
    if(length(newdat$refs)) {
      aliasAm = currentAlias$am
      aliasRev = setnames(names(aliasAm), aliasAm)

      # Get original ref names
      refs = newdat$refs
      isAlias = refs %in% aliasAm
      refs[isAlias] = aliasRev[refs[isAlias]]

      # Extract genotypes
      g = genoTable$am[refs, , drop = FALSE]
    }

    newdat$ped = setMarkers(newdat$ped, alleleMatrix = g, locusAttributes = DB())

    peds = pedigrees()
    if(isNewPed()) {
      idx = pedNr$total + 1
      peds[[paste0("F", idx)]] = newdat
    }
    else {
      idx = pedNr$current
      peds[[idx]] = newdat
    }
    pedigrees(peds)
    pedNr$current = idx

  })

  output$pedplot = renderPlot({ print("pedplot-data");
    req(pedNr$current > 0)
    peddat = pedigrees()[[pedNr$current]]
    plot(peddat$ped, hatched = peddat$refs, cex = 1.2,
         col = list("red" = peddat$miss), carrier = peddat$miss,
         lwd = list("1.2" = peddat$miss), foldLabs = 10, margins = 2)
    box("outer", col = 1)
  },
  execOnResize = TRUE, res = 72, height = 440)

  # output$freqhist = renderPlot({ print("freqhist-data")
  #   db = req(DB())
  #   marker = req(input$freqmarker)
  #   fr = db[[marker]]
  #   par(mar = c(3,0,0,0))
  #   barplot(fr)
  # })

  output$dvisummary = renderInfoBox({ print("summary-data")
    s = NULL
    col = "warning"
    if(!length(mainDvi$am))
      s = c(s, "No AM data loaded")
    if(!length(mainDvi$pm))
      s = c(s, "No PM data loaded")
    if(!length(mainDvi$missing))
      s = c(s, "No missing persons indicated")

    if(is.null(s)) {
      dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing, generatePairings = FALSE)
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

  observeEvent(input$plotdviButton, {
    showModal(modalDialog(style = "width: fit-content !important; height: 600px",
      title = "Main DVI plot",
      plotOutput("plotdvi"),
      size = "xl",
      easyClose = TRUE
    ))
  })

  plotdims = reactive(dvir:::findPlotDims(mainDvi$am, npm = length(mainDvi$pm)))

  output$plotdvi = renderPlot({  # print(plotdims())
    dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing, generatePairings = FALSE)
    plotDVI(dvi, style = 2)
  }, res = 96,
  width = function() if(length(mainDvi$am) > 5) 1000 else 800,
  height = function() 600,
  execOnResize = TRUE)


  # Edit labels -------------------------------------------------------------

  currentAlias = reactiveValues(am = NULL, pm = NULL)

  observeEvent(origLabs$am, {currentAlias$am = setnames(origLabs$am)})
  observeEvent(origLabs$pm, {currentAlias$pm = setnames(origLabs$pm)})

  output$lab_rows = renderUI({
    Nam = length(origLabs$am)
    Npm = length(origLabs$pm)
    N = max(Nam, Npm)
    origsAM = isolate(origLabs$am)
    origsPM = isolate(origLabs$pm)
    h = if(N < 10) 25 else if(N < 20) 21 else 18
    m = if(N < 10) 5 else if(N < 20) 3 else 2

    lapply(seq_len(N), function(i) {
      fluidRow(
        style = sprintf("margin-bottom: %dpx;", m),
        column(6, if(i<=Nam)  fluidRow( style = "margin-left: 0px",
                 column(7, slimTextOutput(paste0("origlab_am", i), height = h, col = "whitesmoke"), style = "padding:0"),
                 column(5, slimTextInput(paste0("alias_am", i), value = origsAM[i], height = h))
        )),
        column(6, if(i<=Npm) fluidRow( style = "margin-left: 0px",
                 column(7, slimTextOutput(paste0("origlab_pm", i), height = h, col = "whitesmoke"),style = "padding:0"),
                 column(5, slimTextInput(paste0("alias_pm", i), value = origsPM[i], height = h))
        ))
      )
    })
  })

  # Original labels
  observe(lapply(seq_along(origLabs$am), function(i) {
      output[[paste0("origlab_am", i)]] = renderText(origLabs$am[i])
  }))
  observe(lapply(seq_along(origLabs$pm), function(i) {
      output[[paste0("origlab_pm", i)]] = renderText(origLabs$pm[i])
  }))

  # Remove substrings
  observeEvent(input$labels_remove, {
    alias_am = sapply(seq_along(origLabs$am), function(i) input[[paste0("alias_am", i)]])
    alias_pm = sapply(seq_along(origLabs$pm), function(i) input[[paste0("alias_pm", i)]])
    substr = input$labels_substr
    tryCatch(
      updateAlias(session, am = gsub(substr, "", alias_am), pm = gsub(substr, "", alias_pm)),
      error = errModal)
  })

  # Restore originals
  observeEvent(input$labels_restore, {print("restore labels")
    updateAlias(session, am = origLabs$am, pm = origLabs$pm)
  })

  # Simple labels
  observeEvent(input$labels_simple, {print("simple labels")
    dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing, generatePairings = FALSE)
    newdvi = relabelDVI(dvi, victimPrefix = "V", missingPrefix = "M", refPrefix = "R", othersPrefix = "")
    updateAlias(session, am = labels(newdvi$am), pm = labels(newdvi$pm))
  })

  observeEvent(input$saveAlias, { print("save alias")
    # New aliases read from the input fields
    newAliasAm = sapply(seq_along(origLabs$am), function(i) input[[paste0("alias_am", i)]])
    newAliasPm = sapply(seq_along(origLabs$pm), function(i) input[[paste0("alias_pm", i)]])

    # Map from current to new
    mapAm = setNames(newAliasAm, currentAlias$am)
    mapPm = setNames(newAliasPm, currentAlias$pm)

    # For am/missing: Go via `pedigrees`, which triggers update of mainDvi
    oldpeds = pedigrees()

    tryCatch({
      amNew = relabel(mainDvi$am, new = mapAm)
      peddat = lapply(seq_along(oldpeds), function(i) {
        oldp = oldpeds[[i]]
        list(ped = amNew[[i]], miss = mapAm[oldp$miss], refs = mapAm[oldp$refs])
      })
      pedigrees(peddat)

      # Update PM
      mainDvi$pm = relabel(mainDvi$pm, new = mapPm)
    }, error = errModal)

    # Update current aliases
    currentAlias$am = setnames(newAliasAm, origLabs$am)
    currentAlias$pm = setnames(newAliasPm, origLabs$pm)
  })

  # Tab: Triangle plots ----------------------------------------------------------

  kappa = reactiveValues(am = NULL, pm = NULL)
  trianglePlot = reactiveValues(am = NULL, pm = NULL)

  observeEvent(input$amkappa, { # button click
    am = req(mainDvi$am)
    kappa$am = forrel::checkPairwise(am, plotType = "none", verbose = FALSE)
    trianglePlot$am = forrel::checkPairwise(am, plotType = "plotly", verbose = FALSE)
  })

  output$amtriangle = renderPlotly({ print("amtriangle");
    p = trianglePlot$am %||% ribd::ibdTriangle(plotType = "plotly")
    p$x$source = "amtriangle"
    p
  })

  lastClick = reactiveVal(NULL)

  output$ampairped = renderPlot({ print("am-pedigree")
    p = req(event_data("plotly_click", source = "amtriangle"))
    if(identical(p, isolate(lastClick())))
      return(NULL)
    lastClick(p)
    idx = req(p$customdata)
    dat = kappa$am[idx, ]
    ids = c(dat$id1, dat$id2)

    # TODO: simplify everything below
    fams = unique(getComponent(mainDvi$am, ids))
    peddata = pedigrees()[fams]
    peds = lapply(peddata, function(d) d$ped)
    miss = unlist(lapply(peddata, function(d) d$miss))
    refs = unlist(lapply(peddata, function(d) d$refs))
    tit = sprintf("%s vs. %s:\n%s", ids[1], ids[2],
                  verbalise(peds, ids) |> format(simplify = TRUE))

    # Colours (from forrel::checkpairwise)
    cols = c("Parent-offspring" = "2", "Full siblings" = "3",
             "Half/Uncle/Grand" = "4", "First cousins/etc" = "5",
             "Other" = "6", "Unrelated" = "7") # "Other" = "#C8A2C8"
    idscol = setNames(list(ids), cols[as.character(dat$relgroup)])

    plot(peds, cex = 1.2, title = tit, foldLabs = 10, hatched = refs,
         fill = c(idscol, list("gray50" = setdiff(refs, ids))), col = idscol,
         lwd = list("2.2" = ids), margins = c(2,1.5,5,1.5), autoScale = TRUE)
    box("outer")
  },
  execOnResize = TRUE, res = 72)

  observeEvent(input$pmkappa, { # button click
    pm = req(mainDvi$pm)
    kappa$pm = forrel::checkPairwise(pm, plotType = "none", verbose = FALSE)
    trianglePlot$pm = forrel::checkPairwise(pm, plotType = "plotly", verbose = FALSE)
  })

  output$pmtriangle = renderPlotly({ print("pmtriangle");
    trianglePlot$pm %||% ribd::ibdTriangle(plotType = "plotly")
  })

  # Tab: Analysis ---------------------------------------------------------------

  solutionTable = reactiveValues(AM = NULL, PM = NULL)
  logMessage = reactiveVal("")


  observeEvent(input$solve, { print("solve")
    dvi = dviData(am = req(mainDvi$am), pm = req(mainDvi$pm), missing = req(mainDvi$missing))

    res = tryCatch(
      captureOutput(dviSolve, dvi, threshold = input$LRthresh),
      error = errModal)

    solutionTable$AM = res$result$AM
    solutionTable$PM = res$result$PM
    logMessage(res$log)
  })

  observeEvent(mainDvi$am, {  print("reset result tables")
    dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing)
    miss = dvi$missing
    fams = getFamily(dvi, miss)
    vics = names(dvi$pm)

    solutionTable$AM = data.frame(Family = fams, Missing = miss, Sample = "", LR = "", GLR = "", Conclusion = "", Comment = "")
    solutionTable$PM = data.frame(Sample = vics, Missing = "", Family = "", LR = "", GLR = "", Conclusion = "", Comment = "")
  })

  output$amcentric = gt::render_gt({  print("render result table AM")
    formatResultTable(req(solutionTable$AM))
  })

  output$pmcentric = gt::render_gt({  print("render result table PM")
    formatResultTable(req(solutionTable$PM))
  })

  output$solvelog = renderText({ print("render log")
    logMessage()[-1] |> paste0(collapse = "\n") |> sub("\n\n\n", "\n\n", x = _)
  })

  output$solutionplot = renderPlot({ print("solutionsplot")
    req(mainDvi$am)
    dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing)
    plotSolutionDIVIANA(dvi, solutionTable$AM)
  }, res = 96, width = "auto", height = 600, execOnResize = TRUE)



  # Download solution tables ------------------------------------------------

  output$downloadTables = downloadHandler(
    filename = function() sprintf("foo_%s.xlsx", Sys.Date()),
    content = function(file) { print("download")
      downloadTables(req(solutionTable$AM), solutionTable$PM, currentAlias$am, currentAlias$pm, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
