suppressMessages(suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)
  library(shinyjs)
  library(DT)
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
         navbarTab(tabName = "pmdata", text = "PM DATA"),
         navbarTab(tabName = "amdata", text = "AM DATA"),
         navbarTab(tabName = "relatedness", text = "RELATEDNESS"),
         navbarTab(tabName = "analysis", text = "ANALYSIS")
    ),
    rightUi = tagList(tags$li(class = "nav-item dropdown",
      div(class = "aligned-row", style = "margin-right: 22.5px; gap: 15px;",
        awesomeCheckbox("usealias", "Alias", value = TRUE, width = "auto", status = "success"),
        downloadBttn("downloaddata", NULL, style = "jelly", color = "warning", size = "m"),
        actionBttn("resetall", icon("redo"), style = "jelly", color = "danger", size = "m"),
        selectInput("example", NULL, choices = c("Load example" = "", DATASETS), width = "200px")
      )
    ))
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
      fluidRow(column(width = 7, dataUI("PM")),

      # Data tab - right column -----------------------------------------------

      column(width = 5,

        bs4Card(width = 12, collapsible = FALSE,
                title = "Frequency database",
          radioButtons("dbtype", NULL, inline = TRUE, width = "100%",
                       selected = character(0),
                       choices = c("Built-in" = "builtin",
                                   "In dataset" = "data",
                                   "Custom" = "custom")),
          conditionalPanel(
            condition = "input.dbtype == 'builtin'",
            pickerInput("dbselect", NULL, choices = "NorwegianFrequencies", selected = NULL,
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
          title = cardNavigation("mutcard", "Mutation models"),
          uiOutput("markName"),
          radioButtons("mutmodel", NULL, inline = TRUE, width = "100%", selected = character(0),
                       choices = c("No model" = "none", Equal = "equal", Prop = "proportional",
                                   Stepwise = "stepwise", Custom = "custom")),
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
          footer = tags$div(class = "btn-group",
            actionButton("mutApply1", label = tagList(icon("check-circle"), "Apply Here")),
            actionButton("mutApplyAll", label = tagList(icon("globe"), "Apply All"))
          )
        ),
      ),
    )),

    # Tab: AM data and pedigrees -----------------------------------------------

   tabItem("amdata",
      fluidRow(
        column(width = 7, dataUI("AM")),
        column(
          width = 5, id = "pedcol",
          bs4Card(width = NULL, collapsible = FALSE,
            title = cardNavigation("pedcard", "Pedigrees"),
            plotOutput("pedplot", width = "auto", height = "auto"),
            footer = div(class = "aligned-row-wide",
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
          title = cardNavigation("solutioncard", "Solution overview"),
          plotOutput("solutionplot", width = "auto", height = "auto")
        ),
      ),
    )
  )))
)



# SERVER ------------------------------------------------------------------


server = function(input, output, session) {
  addTooltips(session)

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
  DB = reactiveVal(NULL)

  # Import data ------------------------------------------------------------

  externalPM = reactiveVal(NULL)
  externalAM = reactiveVal(NULL)

  dataServerPM = dataServer("PM", externalPM, .debug = .debug)
  dataServerAM = dataServer("AM", externalAM, .debug = .debug)

  genoPM = reactive({ .debug("genoPM", dataServerPM$main())
    g = dataServerPM$main(); g$Alias = g$AMEL = g$Sex = NULL; g})
  genoAM = reactive({ .debug("genoAM", dataServerAM$main())
    g = dataServerAM$main(); g$Alias = g$AMEL = g$Sex = NULL; g})

  sexPM = reactive(match(dataServerPM$main()$Sex, c("M", "F"), nomatch = 0L))
  sexAM = reactive(match(dataServerAM$main()$Sex, c("M", "F"), nomatch = 0L))

  aliasPM = reactive({g = dataServerPM$main(); .setnames(g$Alias, rownames(g))})
  aliasAM = reactive({g = dataServerAM$main(); .setnames(g$Alias, rownames(g))})

  alleleSepPM = reactive(getAlleleSep(req(genoPM())))
  alleleSepAM = reactive(getAlleleSep(req(genoAM())))

  mainPM = reactive({ .debug("mainPM")
    if(is.null(g <- genoPM()))
      return(NULL)
    g[g==""] = NA

    s = pedtools::singletons(rownames(g), sex = sexPM())
    names(s) = rownames(g)

    pm = NULL
    tryCatch({
      pm = setMarkers(s, alleleMatrix = g, sep = alleleSepPM())
    }, error = showErr)

    db = DB()
    cat(length(db), "markers\n")
    tryCatch({pm = pm |> .setDB(db)}, error = showErr)

    pm
  })

  mainAM = reactive({ .debug("mainAM")
    if(is.null(g <- genoAM()) || is.null(peddata <- pedigrees()))
      return(NULL)

    peds = lapply(peddata, `[[`, "ped")

    g[g==""] = NA
    am = tryCatch({
      peds |>
        setMarkers(alleleMatrix = g, sep = alleleSepAM()) |>
        .setDB(DB()) |>
        .setMuts(mutModels())
    }, warning = showErr, error = showErr)

    req(!is.character(am))
    if(is.character(am))
      return(NULL)
    am
  })

  mainMissing = reactive(unlist(lapply(pedigrees(), `[[`, "miss"), use.names = FALSE))

  currentDviData = reactive({ .debug("update currentDviData; miss =", mainMissing())
    dviData(am = mainAM(), pm = mainPM(), missing = mainMissing())
  })

  observeEvent(currentDviData(), resetAnalysis(resetAnalysis() + 1))

  # Set complete DVI --------------------------------------------------------

  setCompleteDVI = reactiveVal(NULL)

  observeEvent(dataServerAM$completeDvi(), { .debug("AM complete dvi")
    setCompleteDVI(dataServerAM$completeDvi())}, ignoreNULL = TRUE)
  observeEvent(dataServerPM$completeDvi(), { .debug("PM complete dvi")
    setCompleteDVI(dataServerPM$completeDvi())}, ignoreNULL = TRUE)

  observeEvent(setCompleteDVI(), { .debug("set complete dvi")
    dvi = setCompleteDVI() |> dvir:::consolidateDVI(dedup = TRUE)

    externalAM(getGenotypesAndSex(dvi$am))
    externalPM(getGenotypesAndSex(dvi$pm))

    peds = lapply(dvi$am, function(a)
      list(ped = a, miss = .myintersect(dvi$missing, a$ID), refs = typedMembers(a)))
    pedigrees(peds)

    updateRadioButtons(session, "dbtype", selected = "data")
    DB(getFreqDatabase(dvi$am))

    mutModels(.getAllMutModels(dvi$am))

    # Reset
    setCompleteDVI(NULL)
  }, ignoreNULL = TRUE)

  # Load example
  observeEvent(input$example, { .debug("load example:", input$example)
    dvi = get(req(input$example))
    setCompleteDVI(dvi)
  }, ignoreInit = TRUE)

  # Empty example when other data is loaded
  observeEvent(c(dataServerAM$sources(), dataServerPM$sources()), { .debug("source changed")
    src = c(dataServerAM$sources(), dataServerPM$sources())
    if(!all(src == "Example"))
      isolate(updateSelectInput(session, "example", selected = ""))
  })

  # Mutation models --------------------------------------------------------

  # Main storage for mutation parameters (list of lists)
  mutModels = reactiveVal(NULL)
  mutParams = reactive({
    lapply(mutModels(), function(mut) {
      p = getParams(mut, format = 1)
      list(model = p$model[1],
        rate = list(female = p$rate[1], male = p$rate[2]),
        rate2 = list(female = p$rate2[1], male = p$rate2[2]),
        range = list(female = p$range[1], male = p$range[2]))
    })
  })

  markernames = reactive(colnames(genoAM()))
  currentMutIdx = cardCounter("mutcard", reactive(length(markernames())))
  currentMutMarker = reactive(markernames()[currentMutIdx()])
  output$markName = renderUI(HTML(sprintf("<b>%s</b>", currentMutMarker())))

  inputParams = reactive({
    list(
      model = input$mutmodel,
      rate  = list(female = input$rateF,  male = input$rateM),
      rate2 = list(female = input$rate2F, male = input$rate2M),
      range = list(female = input$rangeF, male = input$rangeM))
  })

  observeEvent(input$mutApply1, { .debug("mutations: apply 1")
    m = currentMutMarker()
    if(is.null(m)) {
      showErr("No markers loaded")
      return()
    }
    mut = NULL
    if(input$mutmodel != "none") {
      args = c(list(afreq = req(DB()[[m]]), validate = TRUE), inputParams())
      mut = tryCatch(do.call(mutationModel, args), error = errModal)
      req(isMutationModel(mut))
    }
    mods = mutModels()
    mods[m] = list(mut)
    mutModels(mods)
    showNotification(paste("Updated mutation model of", m), type = "message")
  })

  observeEvent(input$mutApplyAll, { .debug("mutations: apply all")
    #ask_confirmation("confirmMutApply", type = "warning",
    #  title = "Click 'Confirm' to apply this mutation model to all markers")
    mods = mutModels()
    if(!length(mods)) {
      showErr("No markers loaded")
      return()
    }

    allms = markernames()
    if(input$mutmodel == "none") {
      mutModels(lapply(.setnames(allms), function(m) NULL))
      showNotification("Removed all mutation models", type = "message")
      return()
    }

    args = c(list(afreq = NULL, validate = TRUE), inputParams())
    for(m in intersect(markernames(), names(DB()))) {
      args$afreq = DB()[[m]]
      mut = tryCatch(do.call(mutationModel, args), error = errModal)
      if(isMutationModel(mut))
        mods[m] = list(mut)
    }
    mutModels(mods)
    showNotification("Updated all mutation models", type = "message")
  })

  observeEvent({currentMutIdx();mutModels()}, { .debug("mutations: update input fields")
    idx = currentMutIdx()
    params = if(idx > 0) mutParams()[[idx]] else NULL
    model = params$model %||% "none"

    for(p in c("rate", "rate2", "range")) {
      shinyjs::enable(paste0(p, "F")); shinyjs::enable(paste0(p, "M"))
      updateNumericInput(session, paste0(p, "F"), value = params[[p]]$female)
      updateNumericInput(session, paste0(p, "M"), value = params[[p]]$male)
    }

    updateRadioButtons(session, "mutmodel", selected = model)
  })

  # Enable/disable mutation fields
  observeEvent(input$mutmodel, { .debug("mutations: dis/enable fields")
    setfields = switch(input$mutmodel,
                       none  = , custom = character(),
                       equal = , proportional = "rate",
                       stepwise = c("rate", "rate2", "range"))
    disfields = setdiff(c("rate", "rate2", "range"), setfields)
    for(p in setfields) {
      shinyjs::enable(paste0(p, "F"))
      shinyjs::enable(paste0(p, "M"))
    }
    for(p in disfields) {
      updateNumericInput(session, paste0(p, "F"), value = NA)
      updateNumericInput(session, paste0(p, "M"), value = NA)
      shinyjs::disable(paste0(p, "F"))
      shinyjs::disable(paste0(p, "M"))
    }
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
                   allrefs = allrefs, avoidLabs = avoidLabs,
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
    allrefs = rownames(genoAM())

    otherFams = mainAM()[-curped]
    avoidLabs = list(vics = names(mainPM()),
                     refs = if(nPed() > 1) typedMembers(otherFams) else NULL,
                     miss = setdiff(mainMissing(), curr$ped$ID),
                     labs = labels(otherFams))

    pedigreeServer(uniqueID, resultVar = pedFromModule, initialDat = curr,
                   famid = paste0("F", curped),
                   allrefs = allrefs, avoidLabs = avoidLabs,
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
    title = names(peds)[curPed()]
    miss = peddat$miss
    refs = peddat$refs
    labs = c(refs, miss)
    if(input$usealias)
      labs = useAlias(labs, aliasAM())

    plot(peddat$ped, title = title, hatched = refs, cex = 1.2, cex.main = 1.5,
         margins = c(1,3,2,3), labs = labs, foldLabs = 8,
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
    showModal(modalDialog(
      style = "height: 600px",
      title = "Main DVI plot",
      plotOutput("plotdvi"),
      easyClose = TRUE,
      class = "autowide"
    ))
  })

  plotdims = reactive(dvir:::findPlotDims(mainAM(), npm = length(mainPM())))

  output$plotdvi = renderPlot({ .debug("render Overview plot")
    dvi = currentDviData()
    req(length(dvi$am) > 0, length(dvi$pm) > 0, length(dvi$missing) > 0)
    labs = c(rownames(genoAM()), rownames(genoPM()), dvi$missing)
    if(input$usealias)
      labs = useAlias(labs, c(aliasAM(), aliasPM()))

    tryCatch(plotDVI(dvi, style = 2, labs = labs),
             error = function(e) {
               msg = conditionMessage(e)
               if(grepl("Cannot fit|no room", msg)) msg = "Sorry - the plot is too big!"
               stop2(msg)
             })
  }, res = 96,
  width = function() if(nPed() > 5) 1000 else 800,
  height = function() 600,
  execOnResize = TRUE)


  # Tab: Triangle plots ----------------------------------------------------------

  kappa = reactiveValues(am = NULL, pm = NULL, ampm = NULL)

  observeEvent(input$acrossComps, {kappa$am = NULL}, ignoreInit = TRUE)

  # TODO: settings button with 'across comps'
  observeEvent(input$amkappa, { .debug("am-kappa")
    kappa$am = CPnoplot(req(mainAM()), acrossComps = input$acrossComps)
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

  output$amtable = DT::renderDT(formatCP(kappa$am, input$usealias, alias1 = aliasAM()), server = FALSE)
  output$ampmtable = DT::renderDT(formatCP(kappa$ampm, input$usealias, alias1 = aliasAM(), alias2 = aliasPM()), server = FALSE)
  output$pmtable = DT::renderDT(formatCP(kappa$pm, input$usealias, alias1 = aliasPM()), server = FALSE)

  # Tab: Analysis ---------------------------------------------------------------

  solutionTable = reactiveValues(AM = NULL, PM = NULL, LR = NULL, EX = NULL)
  logMessage = reactiveVal("")

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
    solutionTable$LR = res$result$LRmatrix
    solutionTable$EX = res$result$exclusionMatrix
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
    solutionTable$LR = solutionTable$EX = NULL
  })

  output$amcentric = gt::render_gt({  .debug("render result AM")
    formatResultTable(req(solutionTable$AM), input$usealias, aliasPM = aliasPM())
  }, height = 600)

  output$pmcentric = gt::render_gt({  .debug("render result PM")
    formatResultTable(req(solutionTable$PM), input$usealias, aliasPM = aliasPM())
  }, height = 600)

  output$lrmatrix = gt::render_gt({ .debug("render LR matrix")
    dvi = currentDviData()
    m = req(solutionTable$LR) |> completeMatrix(names(dvi$pm), dvi$missing)
    #mm <<- solutionTable$LR; dd <<- dvi; al <<- aliasPM()
    formatLRmatrix(m, input$LRthresh, input$usealias, aliasPM = aliasPM())
  }, height = 600)

  output$exmatrix = gt::render_gt({ .debug("render exclusion matrix")
    dvi = currentDviData()
    m = req(solutionTable$EX) |> completeMatrix(names(dvi$pm), dvi$missing)
    formatExclusionMatrix(m, input$maxIncomp, input$usealias, aliasPM = aliasPM())
  }, height = 600)

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

  resetAnalysis = reactiveVal(0)

  observeEvent(input$resetall, { .debug("reset all")
    externalAM(NULL)
    externalPM(NULL)
    pedigrees(NULL)
    isolate(updateSelectInput(session, "example", selected = ""))
    updateNumericInput(session, "LRthresh", value = 10000)
    updateNumericInput(session, "maxIncomp", value = 2)
    updateCheckboxInput(session, "ignoresex", value = FALSE)
    resetAnalysis(resetAnalysis() + 1)
  })

  observeEvent(resetAnalysis(), { .debug("reset results")
    kappa$am = kappa$pm = kappa$ampm = NULL
    solutionTable$AM = solutionTable$PM = solutionTable$LR = solutionTable$EX = NULL
    logMessage(NULL)
  }, ignoreInit = TRUE)


  observe({
    if(DEVMODE) { .debug("devmode!")
      updateSelectInput(session, "example", selected = "example1")
      # updateTabItems(session, "tabmenu", "amdata")
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

  currentModal = reactiveVal()
  # Ensure the www directory is added as a resource path
  addResourcePath("www", "www")

  showInstructions = function(id) { .debug("help:", id)
    helpfile = paste0("www/", id, ".html")
    req(file.exists(helpfile))

    showModal(modalDialog(
      title = NULL,
      tags$iframe(src = helpfile,
                  style = "width:100%; height:80vh; border:none; font-size: 90%"),
      footer = actionButton("close_instructions", "Back"),
      easyClose = FALSE,
      tags$style(HTML("
       .modal-content { width:fit-content;}
      "))
    ))
  }

  observeEvent(input$close_instructions, { .debug("close help")
    removeModal()
    if(!is.null(m <- currentModal()))
      showModal(m)
  })

  # Identify all help-*.md files in www/
  helpFiles = list.files("www", pattern = "^help-.*\\.html$", full.names = FALSE)
  helpIds = sub("\\.html$", "", helpFiles)

  # Create observers for each help button
  lapply(helpIds, function(id) observeEvent(input[[id]], showInstructions(id)))
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
