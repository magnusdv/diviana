# LR power module ---------------------------------------------------------

powerUI = function(id) {
  ns = NS(id)
  fluidRow(
    column(3,
      bs4Card(width = 12, collapsible = FALSE, title = "Simulation settings",
        selectInput(ns("family"), "Family", choices = NULL),
        uiOutput(ns("missingUI")),
        radioButtons(ns("mode"), "Simulation mode",
                     choices = c("Conditional" = "conditional", "Unconditional" = "unconditional")),
        radioButtons(ns("truth"), "Simulate under",
                     choices = c("Both" = "both", "True missing person" = "H1",
                                 "Unrelated person" = "H2")),
        numericInput(ns("nsim"), "Simulations per hypothesis", value = 500, min = 1, step = 100),
        numericInput(ns("threshold"), "LR threshold", value = 1e4, min = 1e-10),
        textInput(ns("seed"), "Seed (optional)", value = ""),
        actionButton(ns("calculate"), "Calculate", class = "btn-primary"),
        div(class = "text-muted small mt-3",
            "Each LR compares the selected missing person with an unrelated candidate.",
            "Other missing persons remain untyped; this does not measure discrimination between missing relatives.")
      )
    ),
    column(9,
      bs4Card(width = 12, collapsible = FALSE, title = "LR distribution",
              textOutput(ns("status")), plotOutput(ns("plot"), height = "450px")),
      bs4Card(width = 12, collapsible = FALSE, title = "Power summary",
              DT::DTOutput(ns("summary")))
    )
  )
}

powerServer = function(id, am, missing, reset = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    analysis = reactiveVal(NULL)
    selected = reactiveVal(1L)

    # Follow the AM pedigree and MP list, independently of PM data.
    observeEvent(list(am(), missing()), {
      analysis(NULL)
      fams = names(am())
      updateSelectInput(session, "family",
                        choices = c("All families" = "*", setNames(fams, fams)),
                        selected = if(length(fams)) fams[1] else "*")
    }, ignoreInit = FALSE)

    output$missingUI = renderUI({
      fam = req(input$family)
      if(fam == "*")
        return(div(class = "text-muted small", "All missing persons will be analysed."))
      a = req(am())
      ids = intersect(missing(), a[[fam]]$ID)
      selectInput(ns("person"), "Missing person", choices = ids)
    })

    observeEvent(reset(), {
      analysis(NULL)
      selected(1L)
    }, ignoreInit = TRUE)

    observeEvent(input$calculate, {
      a = am()
      miss = missing()
      if(!length(a) || !length(miss)) {
        showNotification("AM pedigrees and missing persons are required", type = "warning")
        return()
      }
      fam = req(input$family)
      mp = if(fam == "*") NULL else req(input$person)
      nsim = input$nsim
      if(length(nsim) != 1L || !is.finite(nsim) || nsim < 1 || nsim != as.integer(nsim)) {
        showNotification("Simulations must be a positive integer", type = "error")
        return()
      }
      seed = trimws(input$seed %||% "")
      if(nzchar(seed)) {
        seedInt = suppressWarnings(as.integer(seed))
        if(is.na(seedInt) || !identical(seed, as.character(seedInt))) {
          showNotification("Seed must be an integer", type = "error")
          return()
        }
        seed = seedInt
      } else seed = NULL

      # A hard limit prevents a large batch from blocking the Shiny session indefinitely.
      np = if(fam == "*") length(miss) else 1L
      nh = if(input$truth == "both") 2L else 1L
      if(fam == "*" && nsim * np * nh > 20000) {
        showNotification("Batch exceeds 20,000 simulations; reduce the simulation count",
                         type = "warning", duration = NULL)
        return()
      }

      z = tryCatch(withProgress(message = "Simulating LR power", value = 0, {
        d = dvir::dviData(pm = NULL, am = a, missing = miss)
        dvir::dviPower(d, fams = if(fam == "*") NULL else fam,
                       missing = mp, conditional = input$mode == "conditional",
                       simulate = input$truth, nsim = nsim,
                       threshold = input$threshold, seed = seed, verbose = FALSE)
      }), error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        NULL
      })
      if(is.null(z)) return()
      analysis(z)
      selected(1L)
    })

    observeEvent(input$summary_rows_selected, {
      i = input$summary_rows_selected
      if(length(i)) selected(i[1])
    })

    output$status = renderText({
      z = analysis()
      if(is.null(z)) return("No calculations yet")
      sprintf("%d simulations per hypothesis | %s", z$params$nsim,
              if(z$params$conditional) "Conditional" else "Unconditional")
    })

    output$plot = renderPlot({
      z = req(analysis())
      i = req(selected())
      req(i <= length(z$results), is.finite(input$threshold), input$threshold > 0)
      forrel::LRpowerPlot(data = z$results[[i]], threshold = input$threshold,
                          labels = z$labels,
                          title = sprintf("%s: %s", z$summary$Family[i], z$summary$Missing[i]))
    })

    # Recalculate threshold statistics from stored LRs, without new simulations.
    output$summary = DT::renderDT({
      z = req(analysis())
      thr = req(input$threshold)
      req(is.finite(thr), thr > 0)
      tab = z$summary[, c("Family", "Missing", "MedianLog10LR_H1", "MedianLog10LR_H2")]
      colnames(tab)[3:4] = c("Median log10 LR (true)", "Median log10 LR (unrelated)")
      for(h in c("H1", "H2")) {
        nms = if(h == "H1") "True MP" else "Unrelated"
        tab[[sprintf("P(LR >= %g) %s", thr, nms)]] = vapply(z$results, function(r) {
          if(is.null(r[[h]])) return(NA_real_)
          mean(r[[h]]$log10LRperSim >= log10(thr))
        }, numeric(1))
      }
      dt = DT::datatable(tab, rownames = FALSE, selection = "single",
                         options = list(pageLength = 12, scrollX = TRUE))
      dt = DT::formatRound(dt, 3:4, digits = 2)
      DT::formatPercentage(dt, 5:6, digits = 1)
    }, server = FALSE)
  })
}
