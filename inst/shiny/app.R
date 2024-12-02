suppressMessages(suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(shinyjs)
  #library(shinyBS)
  library(gt)
  library(pedtools)
  library(pedmut)
  library(forrel)
  library(dvir)
  library(verbalisr)
  library(plotly)
  library(openxlsx)
}))


# TODO----------------------------------------------------------------
#
# DATA
# * Reset all button
# * Genemapper wide + Relationship
# * Download dviData (not only debug)
# * Plot fires twice!!!
#
# PED
# * Select references: no line wrap
# * DELETE ped!
#
#
# TRIANGLES
# * Triangle plots: Latex labels
# * Triangle AM: Select family
#---------------------------------------------


DEVMODE = T

DATASETS = c("example1", "example2", "exclusionExample", "fire", "grave", "icmp", "planecrash")

# UI ----------------------------------------------------------------------

ui = bs4Dash::bs4DashPage(

  dark = NULL,
  help = NULL,

  # Header
  bs4DashNavbar(
    status = "info",
    title = div(
      style = "display: flex; align-items: center; flex-wrap: nowrap;",
      img(src = "diviana-moth-logo.png", style = "height: 80px; padding:5px 10px"),
      h2("DIVIANA", style = "font-family: 'Montserrat', sans-serif; margin: 0")
    ),
    navbarMenu(
         id = "navmenu",
         navbarTab(tabName = "pmdata", text = "PM DATA"),
         navbarTab(tabName = "amdata", text = "AM DATA"),
         navbarTab(tabName = "relatedness", text = "RELATEDNESS"),
         navbarTab(tabName = "analysis", text = "ANALYSIS")
    ),
    rightUi = tagList(
      tags$li(
        class = "nav-item dropdown",
        actionBttn("resetall", "Reset all", size = "sm", style = "simple", color = "danger")
      ),
      tags$li(
        class = "nav-item dropdown",
        style = "margin: 0 22.5px 0 10px; width: 200px",
        selectInput("example", NULL, choices = c("Load example" = "", DATASETS))
      )
    ) #,
    #div(id = "debugdiv", style = "position:absolute; right:0; top:0",
    #    checkboxInput("debug", NULL, value = DEVMODE))
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

   # Tab: PM data (and other input) -------------------------------------------

   tabItem("pmdata",
      fluidRow(column(width = 6, importUI("PM")),

      # Data tab - right column -----------------------------------------------

      column(width = 6,

        bs4Card(width = 12, collapsible = FALSE,
                title = "Frequency database",
          radioButtons("dbtype", NULL, inline = TRUE, width = "100%",
                       choices = c("Built-in" = "builtin",
                                   "In dataset" = "data",
                                   "Custom" = "custom")),
          conditionalPanel(
            condition = "input.dbtype == 'builtin'",
            pickerInput("dbselect", NULL, choices = "NorwegianFrequencies", selected = "NorwegianFrequencies",
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
        bs4Card(width = 12, collapsible = FALSE, class = "mutation-inputs", style = "padding-top:0",
          title = cardNavigation("mutcard", "Mutation model"),
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
      ),
    )),

    # Tab: AM data and pedigrees -----------------------------------------------

   tabItem("amdata",
      fluidRow(
        column(width = 6, importUI("AM")),
        column(
          width = 6, id = "pedcol",
          bs4Card(width = NULL, collapsible = FALSE,
            title = cardNavigation("pedcard", "Pedigrees"),
            plotOutput("pedplot", width = "auto", height = "auto"),
            footer = div(style = "display: flex; justify-content: space-between;",
              div(class = "btn-group",
                actionButton("newped", label = tagList(icon("plus"), "New")),
                actionButton("editped", label = tagList(icon("edit"), "Edit")),
                actionButton("delped", label = tagList(icon("trash"), "Delete"))
              ),
              actionBttn("plotdviButton", "Overview", style = "jelly", size = "s", color = "success")
            )
          ),
          bs4InfoBoxOutput("dvisummary", width = 12)
        ),
      )
   ),

    # column(width = 2, id = "labcol", class = "col-xs-12 col-lg-4",
    #   conditionalPanel(condition = "input.debug",
    #     div(style = "font-size:smaller; line-height:normal; background-color:white; height:300px; overflow-y:auto; border: 1px solid #ccc",
    #       verbatimTextOutput("debugOutput")),
    #     div(style = "font-size:smaller; line-height:normal; border: 1px solid #ccc; margin-top: 5px; position:relative;",
    #         div(downloadButton("downloaddata", label = "dviData", width = "100%", class = "btn btn-primary btn-sm", style = "background-color: lightblue;"),
    #             style = "position:absolute; top: 5px; right: 5px;"),
    #         verbatimTextOutput("debugElements")
    #         ),
    #   )
    # )


  # Tab: Relatedness ---------------------------------------------------------

  tabItem("relatedness", fluidRow(
    triangleCard("AM - AM", idpref = "am"), # TODO: plotOutput("ampairped", width = "auto", height = "400px")
    triangleCard("AM - PM", idpref = "ampm"),
    triangleCard("PM - PM", idpref = "pm")
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
                    tabPanel(title = "LR matrix", gt::gt_output("lrmatrix")),
                    tabPanel(title = "Exclusions", gt::gt_output("exmatrix")),
                    tabPanel(title = "log", verbatimTextOutput("solvelog"))
         )
      ),

      # Solution plots-----------------------------------------------------

      column(width = 5,
        bs4Card(width = NULL, collapsible = FALSE,
          title = cardNavigation("solutioncard", "Solution plots"),
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

  # Main reactive variables -------------------------------------------------

  pedigrees = reactiveVal(NULL)
  DB = reactiveVal(NULL)

  # Import data ------------------------------------------------------------

  externalPM = reactiveVal(NULL)
  externalAM = reactiveVal(NULL)

  importPM = importServer("PM", externalPM)
  importAM = importServer("AM", externalAM)

  genoPM = reactive({g = importPM$data(); g$Alias = g$AMEL = g$Sex = NULL; g})
  genoAM = reactive({g = importAM$data(); g$Alias = g$AMEL = g$Sex = NULL; g})

  sexPM = reactive(match(importPM$data()$Sex, c("M", "F"), nomatch = 0L))
  sexAM = reactive(match(importAM$data()$Sex, c("M", "F"), nomatch = 0L))

  mainPM = reactive({ .debug("mainPM")
    if(is.null(g <- genoPM()))
      return(NULL)

    s = pedtools::singletons(rownames(g), sex = sexPM())
    pm = tryCatch(
      s |> setMarkers(alleleMatrix = g) |> .setDB(DB()),
      warning = showErr, error = showErr)

    if(is.character(pm)) return(NULL)
    pm
  })

  mainAM = reactive({ .debug("mainAM")
    if(is.null(g <- genoAM()) || is.null(peddata <- pedigrees()))
      return(NULL)

    peds = lapply(peddata, `[[`, "ped")
    am = tryCatch(
      peds |> setMarkers(alleleMatrix = g) |> .setDB(DB()),
      warning = showErr, error = showErr)

    if(is.character(am)) return(NULL)
    am
  })

  mainMissing = reactive({ .debug("mainMissing");
    if(is.null(peddata <- pedigrees()))
      return(NULL)
    unlist(lapply(peddata, `[[`, "miss"), use.names = FALSE)
  })

  currentDviData = reactive({ .debug("update currentDviData")
    dviData(am = mainAM(), pm = mainPM(), missing = mainMissing())
  })

  #----------------------
  observeEvent(src <- c(importAM$source, importPM$source), { .debug("source changed")
    if(!all(c(importAM$source, importPM$source) == "Example"))
      isolate(updateSelectInput(session, "example", selected = ""))
  })

  # If complete data is loaded: Attach everywhere
    if(!is.null(NULL)) { .debug("dvi loaded; update everything")
      dvi = dvir:::consolidateDVI(dvi, dedup = TRUE)
      tryCatch({
        updateCheckboxInput(session, "pm_in_am", value = TRUE)
        DB(getFreqDatabase(dvi$am))
        updateRadioButtons(session, "dbtype", selected = "data")
        peds = lapply(dvi$am, function(a)
          list(ped = a, miss = intersect(labels(a), dvi$missing),
               refs = typedMembers(a)))
        pedigrees(peds)
        #markernames$all = name(dvi$am)
        #markernames$currIdx = 1
      },
      error = errModal, warning = errModal)
    }

  # Load example ------------------------------------------------------------

  observeEvent(input$example, { .debug("load example:", input$example)
    dat = get(req(input$example)) |>
      dvir:::consolidateDVI(dedup = TRUE)
    resetTrigger(resetTrigger() + 1)

    amdat = getGenotypes(dat$am, ids = typedMembers) |> as.data.frame()
    amdat$Sex = getSex(dat$am, rownames(amdat))
    externalAM(amdat)

    pmdat = getGenotypes(dat$pm) |> as.data.frame()
    pmdat$Sex = getSex(dat$pm, rownames(pmdat))
    externalPM(pmdat)

    peds = lapply(dat$am, function(a)
      list(ped = a, miss = .myintersect(dat$missing, a$ID), refs = typedMembers(a)))
    pedigrees(peds)

    updateRadioButtons(session, "dbtype", selected = "data")
    DB(getFreqDatabase(dat$am))
  }, ignoreInit = T)

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
    am = req(mainAM())
    model = if(input$mutmodel == "none") NULL else input$mutmodel
    markers = markersForMutEdit()

    #TODO!!
    # tryCatch({
    #   mainDvi$am = setMutmod(am, markers = markers, model = model,
    #     rate = list(female = input$rateF, male = input$rateM),
    #     rate2 = list(female = input$rate2F, male = input$rate2M),
    #     range = list(female = input$rangeF, male = input$rangeM))
    # }, error = errModal)
    #
    # show_alert(title = "Mutation model applied!", type = "success")
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
  nPed = reactive(length(pedigrees()))
  curPed = cardCounter("pedcard", nPed)

  observeEvent(input$newped, { .debug("new pedigree")
    allrefs = rownames(genoAM())
    if(is.null(allrefs)) {
      showErr("Reference data must be loaded before creating pedigrees.")
      return()
    }
    isNewPed(TRUE)
    uniqueID = uniquify("quickpedModule")

    avoidLabs = list(vics = names(mainPM()),
                     refs = if(nPed() > 0) typedMembers(mainAM()) else NULL,
                     miss = mainMissing(),
                     labs = labels(mainAM()))

    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = NULL,
                   famid = paste0("F", nPed() + 1),
                   allrefs = allrefs, avoidLabs = avoidLabs, .debug = .debug)
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
    allrefs = rownames(genoAM())

    otherFams = mainAM()[-curped]
    avoidLabs = list(vics = names(mainPM()),
                     refs = if(nPed() > 1) typedMembers(otherFams) else NULL,
                     miss = setdiff(mainMissing(), curr$ped$ID),
                     labs = labels(otherFams))

    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = curr,
                   famid = paste0("F", curped),
                   allrefs = allrefs, avoidLabs = avoidLabs, .debug = .debug)
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
      peds[[paste0("F", idx)]] = newdat
    }
    else {
      idx = curPed()
      peds[[idx]] = newdat
    }
    pedigrees(peds)
    curPed(idx)
    isolate(updateSelectInput(session, "example", selected = ""))
  })

  output$pedplot = renderPlot({ .debug("plot current pedigree:", curPed());
    req(curPed() > 0)
    peds = pedigrees()
    peddat = peds[[curPed()]]

    plot(peddat$ped, title = names(peds)[curPed()], hatched = peddat$refs,
         cex = 1.2, cex.main = 1.5,
         col = list("red" = peddat$miss), carrier = peddat$miss,
         lwd = list("1.2" = peddat$miss), foldLabs = 10, margins = c(1,3,2,3))
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
    showModal(modalDialog(style = "width: fit-content !important; height: 600px",
      title = "Main DVI plot",
      plotOutput("plotdvi"),
      size = "xl",
      easyClose = TRUE
    ))
  })

  plotdims = reactive(dvir:::findPlotDims(mainAM(), npm = length(mainPM())))

  output$plotdvi = renderPlot({ .debug("render Overview plot")
    dvi = currentDviData()
    req(length(dvi$am) > 0, length(dvi$pm) > 0, length(dvi$missing) > 0)
    plotDVI(dvi, style = 2)
  }, res = 96,
  width = function() if(nPed() > 5) 1000 else 800,
  height = function() 600,
  execOnResize = TRUE)


  # Tab: Triangle plots ----------------------------------------------------------

  kappa = reactiveValues(am = NULL, pm = NULL, ampm = NULL)

  # TODO: settings button with 'across comps'
  observeEvent(input$amkappa, { .debug("am-kappa")
    kappa$am = CPnoplot(req(mainAM()), acrossComps = FALSE)
  })

  output$amtriangle = renderPlotly({ .debug("am triangle")
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
    fams = unique(getComponent(mainAM(), ids))
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
    idscol = .setnames(list(ids), cols[as.character(dat$relgroup)])

    plot(peds, cex = 1.2, title = tit, foldLabs = 10, hatched = refs,
         fill = c(idscol, list("gray50" = setdiff(refs, ids))), col = idscol,
         lwd = list("2.2" = ids), margins = c(2,1.5,5,1.5), autoScale = TRUE)
    box("outer")
  },
  execOnResize = TRUE, res = 72)

  observeEvent(input$ampmkappa, { .debug("ampm-kappa")
    am = req(mainAM())
    pm = mainPM()
    idMatr = pedtools:::fast.grid(list(typedMembers(am), names(pm)))
    commonMarkers = intersect(name(am), name(pm))
    allcmps = c(selectMarkers(am, commonMarkers), selectMarkers(pm, commonMarkers))
    kappa$ampm = forrel::ibdEstimate(allcmps, ids = idMatr, verbose = FALSE)
  })

  observeEvent(input$pmkappa, { .debug("pm-kappa")
    kappa$pm = CPnoplot(req(mainPM()))
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

  output$amtable = renderTable(formatCP(kappa$am), width = "100%", spacing = "xs")

  output$ampmtable = renderTable(formatCP(kappa$ampm), width = "100%", spacing = "xs")

  output$pmtable = renderTable(formatCP(kappa$pm), width = "100%", spacing = "xs")

  # Tab: Analysis ---------------------------------------------------------------

  solutionTable = reactiveValues(AM = NULL, PM = NULL)
  logMessage = reactiveVal("")
  LRmatrix = reactiveVal(NULL)
  exclusionMatrix = reactiveVal(NULL)

  observeEvent(input$solve, { .debug("solve")
    dvi = currentDviData()
    req(length(dvi$am) > 0, length(dvi$pm) > 0, length(dvi$missing) > 0)
    res = NULL
    tryCatch({
      res = captureOutput(dviSolve, dvi, threshold = input$LRthresh, maxIncomp = input$maxIncomp,
                    ignoreSex = input$ignoresex, verbose = TRUE, debug = DEVMODE,#,input$debug,
                    detailedOutput = TRUE)
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
    LRmatrix(res$result$LRmatrix)
    exclusionMatrix(res$result$exclusionMatrix)
    logMessage(res$log)
  })

  observeEvent(currentDviData(), {  .debug("reset result tables")
    dvi = currentDviData()
    miss = dvi$missing
    fams = getFamily(dvi, miss)
    vics = names(dvi$pm)

    e1 = character(length(miss))
    solutionTable$AM = data.frame(Family = fams, Missing = miss, Sample = e1,
                                  LR = e1, GLR = e1, Conclusion = e1, Comment = e1)

    e2 = character(length(vics))
    solutionTable$PM = data.frame(Sample = vics, Missing = e2, Family = e2,
                                  LR = e2, GLR = e2, Conclusion = e2, Comment = e2)
  })

  output$amcentric = gt::render_gt({  .debug("render result AM")
    formatResultTable(req(solutionTable$AM))
  })

  output$pmcentric = gt::render_gt({  .debug("render result PM")
    formatResultTable(req(solutionTable$PM))
  })

  output$lrmatrix = gt::render_gt({ .debug("render LR matrix")
    dvi = currentDviData()
    m = completeMatrix(req(LRmatrix()), names(dvi$pm), dvi$missing)
    formatLRmatrix(m, input$LRthresh)
  })

  output$exmatrix = gt::render_gt({ .debug("render exclusion matrix")
    dvi = currentDviData()
    m = completeMatrix(req(exclusionMatrix()), names(dvi$pm), dvi$missing)
    formatExclusionMatrix(m, input$maxIncomp)
  })

  output$solvelog = renderText({ .debug("render result log")
    logMessage()[-1] |> paste0(collapse = "\n")
  })

  curSols = cardCounter("solutioncard", reactive(ceiling(nPed()/6)))

  output$solutionplot = renderPlot({ .debug("plot solution")
    dvi = currentDviData()
    req(length(dvi$am) > 0, length(dvi$pm) > 0, length(dvi$missing) > 0)
    pednr = seq(curSols() * 6 - 5, min(nPed(), curSols() * 6))
    plotSolutionDIVIANA(dvi, solutionTable$AM, pednr = pednr)
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

  # Reset -------------------------------------------------------------------

  resetTrigger = reactiveVal(0)

  observeEvent(input$resetall, { .debug("reset all")
    externalAM(NULL)
    externalPM(NULL)
    pedigrees(NULL)
    isolate(updateSelectInput(session, "example", selected = ""))
    updateNumericInput(session, "LRthresh", value = 10000)
    updateNumericInput(session, "maxIncomp", value = 2)
    updateCheckboxInput(session, "ignoresex", value = FALSE)
    resetTrigger(resetTrigger() + 1)
  })

  observeEvent(resetTrigger(), { .debug("reset downstream")
    kappa$am = kappa$pm = kappa$ampm = NULL
    solutionTable$AM = NULL; solutionTable$PM = NULL
    LRmatrix(NULL)
    logMessage(NULL)
  }, ignoreInit = TRUE)


  observe({
    if(DEVMODE) { .debug("devmode!")
      #updateSelectInput(session, "example", selected = "icmp")
    }
  })

  # Debug -------------------------------------------------------------------

  debuglog = reactiveVal("")
  output$debugOutput = renderText(debuglog())

  # Reset when debug is toggled
  #observeEvent(input$debug, debuglog("### DEBUG LOG ###"))

  .debug = function(...) {
    if(DEVMODE) print(paste(..., collapse = " "))
    #if(isTRUE(input$debug))
    #  isolate(debuglog(paste(debuglog(), paste(..., collapse = " "), sep = "\n")))
  }

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
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
