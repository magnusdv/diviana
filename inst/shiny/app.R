suppressMessages(suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(shinyjs)
  #library(shinyBS)
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


DEVMODE = F

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
      selectInput("example", NULL, choices = c("Load example" = "", DATASETS)))
    ),
  div(id = "debugdiv", style = "position:absolute; right:0; top:0",
      checkboxInput("debug", NULL, value = DEVMODE))
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

      bs4Card(width = 12, collapsed = TRUE,
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

      bs4Card(width = NULL,
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
      conditionalPanel(condition = "input.debug",
        div(style = "font-size:smaller; line-height:normal; background-color:white; height:300px; overflow-y:auto; border: 1px solid #ccc",
          verbatimTextOutput("debugOutput")),
        div(style = "font-size:smaller; line-height:normal; border: 1px solid #ccc; margin-top: 5px; position:relative;",
            div(downloadButton("downloaddata", label = "dviData", width = "100%", class = "btn btn-primary btn-sm", style = "background-color: lightblue;"),
                style = "position:absolute; top: 5px; right: 5px;"),
            verbatimTextOutput("debugElements")
            ),
      )
    )
  )
  ),

  # Tab: Relatedness ---------------------------------------------------------

  tabItem("relatedness", fluidRow(
    bs4Card(width = 4,
      title = div(style = "display: flex; align-items: center;",
                  "AM - AM",
                  div(style = "margin-left: 30px;",
                      actionButton("amkappa", "Calculate", class = "btn-sm"))),
      plotlyOutput("amtriangle", width = "100%", height = "400px"),
      tableOutput("amtable")
      #plotOutput("ampairped", width = "auto", height = "400px") # TODO
    ),
    bs4Card(width = 4,
      title = div(style = "display: flex; align-items: center;",
                  "AM - PM",
                  div(style = "margin-left: 30px;",
                      actionButton("ampmkappa", "Calculate", class = "btn-sm"))),
      plotlyOutput("ampmtriangle", width = "100%", height = "400px"),
      tableOutput("ampmtable")
    ),
    bs4Card(width = 4,
      title = div(style = "display: flex; align-items: center;",
                  "PM - PM",
                  div(style = "margin-left: 30px;",
                      actionButton("pmkappa", "Calculate", class = "btn-sm"))),
      plotlyOutput("pmtriangle", width = "100%", height = "400px"),
      tableOutput("pmtable")
    ),
  )),

  # Tab: Analysis-------------------------------------------------------------

  tabItem("analysis",
    fluidRow(

      # Buttons: Solve! --------------------------------------------
      column(width = 2, class = "col-xl-1",
             actionBttn("solve", label = "SOLVE", icon = icon("calculator"), color = "primary", style = "jelly", ),
             br(),hr(),
             h4("Settings"),
             numericInput("LRthresh", "LR threshold", value = 10000,min = 1),
             br(),
             numericInput("maxIncomp", "Exclusion limit", min = 0, step = 1, value = 2),
             br(),
             checkboxInput("ignoresex", "Ignore Sex", value = FALSE),
             hr(), br(),
             downloadButton('downloadTables', "Download", class = "btn btn-warning",
                            style = "width:100%")
      ),

      # Report table: AM and PM tabs --------------------------------------------
      column(5, class = "col-xl-6",
         bs4TabCard(title = "Identifications", width = NULL, type = "tabs", side = "right",
                    collapsible = FALSE,
                    tabPanel(title = "AM", gt::gt_output("amcentric")),
                    tabPanel(title = "PM", gt::gt_output("pmcentric")),
                    tabPanel(title = "LR matrix", div(tableOutput("lrmatrix"), id = "lrmatrix")),
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


server = function(input, output, session) {
  addTooltips(session)

  # Close app when browser closes
  observeEvent(input$browserClosed, stopApp())

  # Debugging
  debuglog = reactiveVal("")

  # Reset when debug is toggled
  observeEvent(input$debug, debuglog("### DEBUG LOG ###"))

  .debug = function(...) {
    if(DEVMODE) print(paste(..., collapse = " "))
    if(!input$debug) return()
    mess = paste(..., collapse = " ")
    isolate(debuglog(paste(debuglog(), mess, sep = "\n")))
  }

  output$debugOutput = renderText(debuglog())

  # Main reactive variables -------------------------------------------------
  mainDvi = reactiveValues(pm = NULL, am = NULL, missing = NULL)
  origLabs = reactiveValues(pm = NULL, am = NULL)
  pedigrees = reactiveVal(NULL)
  genoTable = reactiveValues(am = NULL, pm = NULL)
  DB = reactiveVal(NULL)
  resetTrigger = reactiveVal(0)

  currentDviData = reactive({
    am = mainDvi$am; pm = mainDvi$pm; missing = mainDvi$missing
    if(is.null(am) || is.null(pm))
      return(NULL)
    dviData(am = am, pm = pm, missing = missing)
  })

  observeEvent(resetTrigger(), { .debug("reset all")
    kappa$am = kappa$pm = kappa$ampm = NULL
    solutionTable$AM = NULL; solutionTable$PM = NULL
    LRmatrix(NULL)
    datapathAM(NULL)
    logMessage(NULL)
  }, ignoreInit = TRUE)


  observe({
    if(DEVMODE) { .debug("devmode!")
      updateSelectInput(session, "example", selected = "icmp")
    }
  })

  # Load example ------------------------------------------------------------

  observeEvent(input$example, { .debug("load example:", input$example)
    dat = get(req(input$example)) |>
      dvir:::consolidateDVI(dedup = TRUE)

    resetTrigger(resetTrigger() + 1)
    shinyjs::reset("amfile")

    am = dat$am
    miss = dat$missing
    mainDvi$pm = pm = dat$pm
    origLabs$am = labels(am)
    origLabs$pm = labels(pm)
    markernames$all = name(am)
    markernames$currIdx = 1

    genoTable$am = getGenotypes(am, ids = typedMembers(am))
    genoTable$pm = getGenotypes(pm)

    peds = lapply(am, function(a)
      list(ped = a, miss = intersect(miss, labels(a)), refs = typedMembers(a)))
    pedigrees(peds)

    updateRadioButtons(session, "dbtype", selected = "data")
    DB(getFreqDatabase(am))
  }, ignoreInit = T)

  # AM data -----------------------------------------------------------------

  datapathAM = reactiveVal()
  observeEvent(input$amfile, { .debug("AM file selected")
    datapathAM(input$amfile$datapath)
  })

  observeEvent(datapathAM(), { .debug("datapathAM changed: ", datapathAM())
    fil = req(datapathAM())

    resetTrigger(resetTrigger() + 1)
    updateSelectInput(session, "example", selected = "")

    dvi = NULL
    tryCatch(switch(input$filetypeAM,
      Familias = {  .debug("-> loading Familias AM file")
        dvi = dvir::familias2dvir(fil, missingFormat = "M[FAM]-[IDX]")
      },
      Genemapper = {   .debug("-> loading Genemapper file")
        stop2("Genemapper import not yet implemented")
        g = readGenemapper(fil)
        genoTable$am = g
        #origLabs$am = rownames(g)
        pedigrees(NULL)
      },
      "dviData (.RData/.rds)" = { .debug("-> loading .RData/.rds")
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
    ), error = errModal, warning = errModal)

    # If complete data is loaded: Attach everywhere
    if(!is.null(dvi)) { .debug("dvi loaded; update everything")
      dvi = dvir:::consolidateDVI(dvi, dedup = TRUE)
      tryCatch({
        genoTable$am = getGenotypes(dvi$am, ids = typedMembers(dvi$am))
        genoTable$pm = getGenotypes(dvi$pm)
        origLabs$am = labels(dvi$am, unlist = TRUE)
        origLabs$pm = labels(dvi$pm, unlist = TRUE)
        updateCheckboxInput(session, "pm_in_am", value = TRUE)
        DB(getFreqDatabase(dvi$am))
        updateRadioButtons(session, "dbtype", selected = "data")
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

  output$amdata = gt::render_gt({ .debug("render AM table")
    tab = as.data.frame(req(genoTable$am))
    hasAlias = rownames(tab) %in% names(currentAlias)
    rownames(tab)[hasAlias] = currentAlias$am[rownames(tab)[hasAlias]]
    formatGenoTable(tab)
  })

  # PM data -----------------------------------------------------------------

  observeEvent(input$pmfile, { .debug("PM file selected")
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

  output$pmdata = gt::render_gt({  .debug("render PM table")
    tab = as.data.frame(req(genoTable$pm))
    rownames(tab) = currentAlias$pm[rownames(tab)]
    formatGenoTable(tab)
  })

  # Mutation frame --------------------------------------------------------

  markernames = reactiveValues(all = NULL, currIdx = 0)

  observeEvent(input$prevmark, { .debug("mutation frame: prev marker")
    markernames$currIdx = (markernames$currIdx - 2) %% length(markernames$all) + 1
  })

  observeEvent(input$nextmark, { .debug("mutation frame: next marker")
    markernames$currIdx = markernames$currIdx %% length(markernames$all) + 1
  })

  output$markName = renderUI({ .debug("mutation frame: update marker name")
    idx = markernames$currIdx
    req(idx > 0, length(markernames$all))
    nam = markernames$all[idx]
    HTML(sprintf("Marker %d/%d: &nbsp; <b>%s</b>", idx, length(markernames$all), nam))
  })

  # Apply mutation model ----------------------------------------------------

  markersForMutEdit = reactiveVal(NULL)

  observeEvent(input$mutApplyAll, { .debug("mutation frame: confirm all")
    ask_confirmation("confirmMutApply", type = "warning",
      title = "Click 'Confirm' to apply this mutation model to all markers")
    markersForMutEdit(NULL)
  })

  observeEvent(input$mutApply1, { .debug("mutation frame: confirm 1")
    m = markernames$all[markernames$currIdx]
    ask_confirmation("confirmMutApply", type = "warning",
      title = sprintf("Click 'Confirm' to apply this mutation model to %s", m))
    markersForMutEdit(m)
  })

  observeEvent(req(input$confirmMutApply), { .debug("mutation frame: apply!")
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

  observeEvent(input$mutmodel, { .debug("mutation frame: update parameter fields")
    mut = mutmod(req(mainDvi$am), markernames$currIdx)
    params = getParams(mut, format = 1)
    if(is.null(mut))
      params = list(rate = c(NA, NA), rate2 = c(NA, NA), range = c(NA, NA))

    setfields = switch(input$mutmodel, none = character(), equal = , proportional = "rate",
                       stepwise = c("rate", "rate2", "range"))
    disfields = setdiff(c("rate", "rate2", "range"), setfields)

    for(p in setfields) {
      shinyjs::enable(paste0(p, "F")); shinyjs::enable(paste0(p, "M"))
      updateNumericInput(session, paste0(p, "F"), value = params[[p]][1])
      updateNumericInput(session, paste0(p, "M"), value = params[[p]][2])
    }
    for(p in disfields) {
      updateNumericInput(session, paste0(p, "F"), value = NA)
      updateNumericInput(session, paste0(p, "M"), value = NA)
      shinyjs::disable(paste0(p, "F")); shinyjs::disable(paste0(p, "M"))
    }
  })

  # Update model name when marker changes
  observeEvent(markernames$currIdx , { .debug("mutation frame: update model name")
    mut = mutmod(req(mainDvi$am), markernames$currIdx)
    model = if(is.null(mut)) "none" else getParams(mut, params = "model")$model[1]
    updateNumericInput(session, "mutmodel", value = model)
  }, ignoreInit = TRUE)

  # Frequency database ------------------------------------------------------

  observeEvent(input$dbselect, { .debug("database: selected", input$dbselect)
    if(input$dbselect == "")
      newdb = NULL
    if(input$dbselect == "NorwegianFrequencies")
      newdb = forrel::NorwegianFrequencies
    DB(newdb)
  })

  observeEvent(DB(), { .debug("database: update freqmarker selector")
    m = names(req(DB()))
    updatePickerInput(session, "freqmarker", choices = m)
  })

  # Pedigrees -----------------------------------------------------------

  pedFromModule = reactiveVal()
  isNewPed = reactiveVal(FALSE)
  pedNr = reactiveValues(current = 0, total = 0)

  observeEvent(pedigrees(), { .debug("pedigree list")
    nped = length(pedigrees())
    if(pedNr$current == 0 || pedNr$current > nped)
      pedNr$current = 1
    pedNr$total = nped
    mainDvi$am = lapply(pedigrees(), `[[`, "ped")
    mainDvi$missing = as.character(unlist(lapply(pedigrees(), `[[`, "miss")))

    # Labels
    aliases = currentAlias$am
    aliasRev = setnames(names(aliases), aliases)

    newalias = labels(mainDvi$am)
    neworig = aliasRev[newalias] # may have missing values
    neworig[is.na(neworig)] = newalias[is.na(neworig)]
    origLabs$am = unname(neworig)
  })

  observeEvent(input$prevped, { .debug("go to previous pedigree")
    req(pedNr$total > 0)
    pedNr$current = (pedNr$current - 2) %% pedNr$total + 1
  })

  observeEvent(input$nextped, { .debug("go to next pedigree")
    req(pedNr$total > 0)
    pedNr$current = pedNr$current %% pedNr$total + 1
  })

  output$pedTitle = renderUI({ .debug("update ped title")
    nms = names(req(pedigrees()))
    nr = pedNr$current
    req(nr > 0)
    tit = sprintf("Family %d/%d", nr, pedNr$total)
    tit
  })

  observeEvent(input$newped, { .debug("new pedigree")
    if(is.null(genoTable$am)) {
      showErr("Reference data must be loaded before creating pedigrees.")
      return()
    }

    isNewPed(TRUE)
    uniqueID = uniquify("quickpedModule")
    refs = rownames(genoTable$am)
    hasAlias = refs %in% names(currentAlias$am)
    refs[hasAlias] = currentAlias$am[refs[hasAlias]]
    avoidLabs = list(vics = names(mainDvi$pm),
                     refs = if(pedNr$total > 0) typedMembers(mainDvi$am) else NULL,
                     miss = mainDvi$missing,
                     labs = labels(mainDvi$am))


    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = NULL,
                   famid = paste0("F", pedNr$total + 1),
                   allrefs = refs, avoidLabs = avoidLabs, .debug = .debug)
  })

  observeEvent(input$editped, { .debug("edit current pedigree")
    if(pedNr$total == 0)
      showErr("No pedigree to edit.")
    req(pedNr$total > 0)

    isNewPed(FALSE)
    curr = req(pedigrees()[[pedNr$current]])
    uniqueID = uniquify("quickpedModule")
    refs = rownames(genoTable$am)
    hasAlias = refs %in% names(currentAlias$am)
    refs[hasAlias] = currentAlias$am[refs[hasAlias]]

    avoidLabs = list(vics = names(mainDvi$pm),
                     refs = if(pedNr$total > 1) typedMembers(mainDvi$am[-pedNr$current]) else NULL,
                     miss = setdiff(mainDvi$missing, curr$ped$ID),
                     labs = labels(mainDvi$am[-pedNr$current]))

    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = curr,
                   famid = paste0("F", pedNr$current),
                   allrefs = refs, avoidLabs = avoidLabs, .debug = .debug)
  })

  observeEvent(pedFromModule(), { .debug("pedigree returned from module")
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

  output$pedplot = renderPlot({ .debug("plot current pedigree:", pedNr$current);
    req(pedNr$current > 0)
    peddat = pedigrees()[[pedNr$current]]
    plot(peddat$ped, hatched = peddat$refs, cex = 1.2,
         col = list("red" = peddat$miss), carrier = peddat$miss,
         lwd = list("1.2" = peddat$miss), foldLabs = 10, margins = 2)
    box("outer", col = 1)
  },
  execOnResize = TRUE, res = 72, height = 440)

  # output$freqhist = renderPlot({ .debug("freqhist-data")
  #   db = req(DB())
  #   marker = req(input$freqmarker)
  #   fr = db[[marker]]
  #   par(mar = c(3,0,0,0))
  #   barplot(fr)
  # })

  output$dvisummary = renderInfoBox({ .debug("update summary info box")
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

  observeEvent(input$plotdviButton, { .debug("click Overview button")
    if(pedNr$total == 0)
      showErr("No pedigrees to show")
    req(pedNr$total > 0)
    showModal(modalDialog(style = "width: fit-content !important; height: 600px",
      title = "Main DVI plot",
      plotOutput("plotdvi"),
      size = "xl",
      easyClose = TRUE
    ))
  })

  plotdims = reactive(dvir:::findPlotDims(mainDvi$am, npm = length(mainDvi$pm)))

  output$plotdvi = renderPlot({ .debug("render Overview plot")
    dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing, generatePairings = FALSE)
    plotDVI(dvi, style = 2)
  }, res = 96,
  width = function() if(length(mainDvi$am) > 5) 1000 else 800,
  height = function() 600,
  execOnResize = TRUE)


  # Edit labels -------------------------------------------------------------

  currentAlias = reactiveValues(am = NULL, pm = NULL)

  observeEvent(origLabs$am, {.debug("origLabs$am"); currentAlias$am = setnames(origLabs$am)})
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
  observeEvent(input$labels_remove, { .debug("labels: remove substrings")
    alias_am = sapply(seq_along(origLabs$am), function(i) input[[paste0("alias_am", i)]])
    alias_pm = sapply(seq_along(origLabs$pm), function(i) input[[paste0("alias_pm", i)]])
    substr = input$labels_substr
    tryCatch(
      updateAlias(session, am = gsub(substr, "", alias_am), pm = gsub(substr, "", alias_pm)),
      error = errModal)
  })

  # Restore originals
  observeEvent(input$labels_restore, { .debug("labels: restore")
    updateAlias(session, am = origLabs$am, pm = origLabs$pm)
  })

  # Simple labels
  observeEvent(input$labels_simple, { .debug("labels: simple")
    dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing, generatePairings = FALSE)
    newdvi = relabelDVI(dvi, victimPrefix = "V", missingPrefix = "M", refPrefix = "R", othersPrefix = "")
    updateAlias(session, am = labels(newdvi$am), pm = labels(newdvi$pm))
  })

  observeEvent(input$saveAlias, { .debug("labels: save")
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

  kappa = reactiveValues(am = NULL, pm = NULL, ampm = NULL)

  # TODO: settings button with 'across comps'
  observeEvent(input$amkappa, { .debug("am-kappa")
    kappa$am = CPnoplot(req(mainDvi$am), acrossComps = FALSE)
  })

  output$amtriangle = renderPlotly({ .debug("am triangle");
    p = forrel::plotCP(kappa$am, plotType = "plotly", xlab = "", ylab = "")
    p$x$source = "amtriangle"
    p
  })

  lastClick = reactiveVal(NULL)

  # TODO: show peds in modal!
  output$ampairped = renderPlot({ .debug("am-triangle-pedigree")
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

  observeEvent(input$ampmkappa, { .debug("ampm-kappa")
    am = req(mainDvi$am)
    pm = mainDvi$pm
    idMatr = pedtools:::fast.grid(list(typedMembers(am), names(pm)))
    commonMarkers = intersect(name(am), name(pm))
    allcmps = c(selectMarkers(am, commonMarkers), selectMarkers(pm, commonMarkers))
    kappa$ampm = forrel::ibdEstimate(allcmps, ids = idMatr, verbose = FALSE)
  })

  observeEvent(input$pmkappa, { .debug("pm-kappa")
    kappa$pm = CPnoplot(req(mainDvi$pm))
  })

  output$ampmtriangle = renderPlotly({ .debug("ampm-triangle");
    # NB: results come from ibdEstimate()
    forrel::showInTriangle(kappa$ampm, plotType = "plotly", xlab = "", ylab = "",
                           pch = 16, col = "pink", cex = 1.2)
  })

  output$pmtriangle = renderPlotly({ .debug("pm-triangle");
    forrel::plotCP(kappa$pm, plotType = "plotly", xlab = "", ylab = "",
                   errtxt = "Potential relationship")
  })

  output$amtable = renderTable(formatCP(req(kappa$am)), width = "100%", spacing = "xs")

  output$ampmtable = renderTable(formatCP(req(kappa$ampm)), width = "100%", spacing = "xs")

  output$pmtable = renderTable(formatCP(req(kappa$pm)), width = "100%", spacing = "xs")

  # Tab: Analysis ---------------------------------------------------------------

  solutionTable = reactiveValues(AM = NULL, PM = NULL)
  logMessage = reactiveVal("")
  LRmatrix = reactiveVal(NULL)

  observeEvent(input$solve, { .debug("solve")
    dvi = dviData(am = req(mainDvi$am), pm = mainDvi$pm, missing = mainDvi$missing)

    res = tryCatch(
      captureOutput(dviSolve, dvi, threshold = input$LRthresh, maxIncomp = input$maxIncomp,
                    ignoreSex = input$ignoresex, verbose = TRUE, debug = input$debug,
                    detailedOutput = TRUE),
      error = errModal)

    solutionTable$AM = res$result$AM
    solutionTable$PM = res$result$PM
    LRmatrix(res$result$LRmatrix)
    logMessage(res$log)
  })

  observeEvent(mainDvi$am, {  .debug("reset result tables")
    dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing)
    miss = dvi$missing
    fams = getFamily(dvi, miss)
    vics = names(dvi$pm)

    if(length(miss))
      amTab = data.frame(Family = fams, Missing = miss, Sample = "", LR = "", GLR = "", Conclusion = "", Comment = "")
    else
      amTab = "No missing persons in dataset"

    if(length(vics))
      pmTab = data.frame(Sample = vics, Missing = "", Family = "", LR = "", GLR = "", Conclusion = "", Comment = "")
    else
      pmTab = "No victim samples in dataset"
    solutionTable$AM = amTab
    solutionTable$PM = pmTab
  })

  output$amcentric = gt::render_gt({  .debug("render result AM")
    formatResultTable(req(solutionTable$AM), inter = input$debug)
  })

  output$pmcentric = gt::render_gt({  .debug("render result PM")
    formatResultTable(req(solutionTable$PM))
  })

  output$solvelog = renderText({ .debug("render result log")
    logMessage()[-1] |> paste0(collapse = "\n")
  })

  output$lrmatrix = renderTable({ .debug("render LR matrix")
    lrm = req(LRmatrix())
    lrm[!is.na(lrm)] = sprintf("%.3g", lrm[!is.na(lrm)])
    rownames(lrm) = paste0("<b>", rownames(lrm), "</b>")
    lrm
  }, rownames = TRUE, spacing = "xs", na = "-", sanitize.text.function = identity)

  output$solutionplot = renderPlot({ .debug("plot solution")
    req(mainDvi$am)
    dvi = dviData(am = mainDvi$am, pm = mainDvi$pm, missing = mainDvi$missing)
    plotSolutionDIVIANA(dvi, solutionTable$AM)
  }, res = 96, width = "auto", height = 600, execOnResize = TRUE)



  # Download solution tables ------------------------------------------------

  output$downloadTables = downloadHandler(
    filename = function() sprintf("foo_%s.xlsx", Sys.Date()),
    content = function(file) { .debug("download")
      downloadTables(req(solutionTable$AM), solutionTable$PM,
                     currentAlias$am, currentAlias$pm, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )


  # Debug -------------------------------------------------------------------

  output$debugElements = renderPrint({
    req(input$debug)
    cat("### CURRENT OBJECTS ###\n")
    print(mainDvi$am, verbose = F)
    print(mainDvi$pm, verbose = F)
    print(mainDvi$missing, verbose = F)
    cat("\n")
    print(abbrMat(genoTable$am))
    cat("\n")
    print(abbrMat(genoTable$pm))
  })

  output$downloaddata = downloadHandler(
    filename = function() "dviData.rds",
    content = function(file) { .debug("download data")
      dvi = dviData(pm = req(mainDvi$pm), am = mainDvi$am, missing = mainDvi$missing)
      saveRDS(dvi, file)
    }
  )
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
