
pedigreeUI = function(id, famid = "F1", references = NULL) { print("ui")
  ns = NS(id)

  fluidRow(
    useShinyjs(),  # Set up shinyjs

    column(width = 3,
           actionButton(ns("child"), "Add child", width = "100%"),
           actionButton(ns("sibling"), "Sibling", width = "100%"),
           actionButton(ns("parents"), "Parents", width = "100%"),
           actionButton(ns("swapsex"), "Swap sex", width = "100%"),
           actionButton(ns("remove"), "Remove", width = "100%"),
           hr(),
           p("Edit role:"),
           actionButton(ns("missing"), "Missing", width = "100%"),
           actionButton(ns("untyped"), "Untyped", width = "100%"),
           selectInput(ns("setref"), "Reference", width = "100%",
                       choices = c("Select" = "", references)),
           hr(),
           fluidRow(
             column(width = 6,
                actionButton(ns("reset"), "Reset", width = "100%", status = "danger",
                             style = "font-size: smaller; padding: 0")),
             column(width = 6,
                actionButton(ns("undo"), "Undo", width = "100%", status = "warning",
                                      style = "font-size: smaller; padding: 0"))
           ),

    ),

    # Pedigree plot
    column(width = 9,
           plotOutput(ns("plot"), click = ns("ped_click"), width = "auto", height = "auto"),
    ),
  )
}

pedigreeServer = function(id, resultVar, initialDat = NULL, famid = "F1", references = NULL) {

  if(is.null(initialDat))
    initialDat = list(ped = nuclearPed() |> relabel(\(x) paste(famid, x, sep = "-")), miss = character(), refs = character())

  moduleServer(id, function(input, output, session) {
    ns = session$ns

    currData = reactiveValues(ped = initialDat$ped,
                              miss = initialDat$miss,
                              refs = initialDat$refs)
    previousStack = reactiveVal(list(initialDat))

    plotdat = reactiveValues(alignment = NULL, scaling = NULL, annotation = NULL)
    sel = reactiveVal(character(0))

    # Modal creation and invocation
    showModal(
      modalDialog(
        title = "Pedigree builder",
        tags$head(tags$style(HTML("@media (min-width: 576px) {.modal-dialog { max-width: 600px !important; }}"))),
        pedigreeUI(id, famid, references),   # use the UI function here
        footer = tagList(
          actionButton(ns("save"), "Save"),
          actionButton(ns("cancel"), "Cancel")
        )
      )
    )


    # Return (or cancel) ------------------------------------------------------

    observeEvent(input$save, { print("save")
      removeModal()
      resultVar(reactiveValuesToList(currData))
    })

    observeEvent(input$cancel, { print("cancel module")
      removeModal()
      resultVar(NULL)
    })

    # Update pedigree ---------------------------------------------------------

    updatePedData = function(ped = currData$ped, miss = currData$miss,
                             refs = currData$refs, clearSel = TRUE) { print("update")
      currData$ped = ped
      currData$miss = .myintersect(ped$ID, miss)
      currData$refs = .myintersect(ped$ID, refs)

      # Update (or clear) selection
      sel(if(clearSel) character(0) else .myintersect(ped$ID, sel()))

      # Update stack
      previousStack(c(previousStack(), list(reactiveValuesToList(currData))))
    }

    # Modify pedigree ---------------------------------------------------------

    observeEvent(input$child, {
      newped = tryCatch(
        .addChild(currData$ped, req(sel()), sex = 1, prefix = famid),
        error = showErr)

      req(is.ped(newped))
      updatePedData(ped = newped, clearSel = FALSE)
    })

    observeEvent(input$sibling, {
      newped = tryCatch(
        .addSib(currData$ped, req(sel()), sex = 1, prefix = famid),
        error = showErr)

      req(is.ped(newped))
      updatePedData(ped = newped)
    })

    observeEvent(input$parents, {
      newped = tryCatch(
        .addParents(currData$ped, req(sel()), prefix = famid),
        error = showErr)

      req(is.ped(newped))
      updatePedData(ped = newped)
    })

    observeEvent(input$swapsex, {
      id = req(sel())
      newped = swapSex(currData$ped, id, verbose = FALSE)
      updatePedData(ped = newped, clearSel = length(id) > 1)
    })

    observeEvent(input$remove, {
      ids = req(sel())
      newped = tryCatch(removeSel(currData$ped, ids), error = showErr)
      req(is.ped(newped))
      updatePedData(ped = newped)
    })

    #observeEvent(input$clearselection, sel(character(0)))


    # Set status --------------------------------------------------------------

    observeEvent(input$missing, {
      ids = req(sel())

      # Refs cannot be missing
      ids = req(setdiff(ids, currData$refs))

      oldmiss = currData$miss
      newmiss = c(setdiff(oldmiss, ids), setdiff(ids, oldmiss))
      updatePedData(miss = newmiss)
    })

    observeEvent(input$untyped, {
      ids = req(sel())
      oldped = currData$ped
      ids = sortIds(oldped, ids) |> intersect(currData$refs) |> req()

      newids = nextLab(oldped, n = length(ids),
                       avoid = references, prefix = famid)
      newped = relabel(oldped, old = ids, new = newids)
      newrefs = setdiff(currData$refs, ids)
      updatePedData(ped = newped, refs = newrefs)
    })

    observeEvent(input$setref, { print("setref")
      id = req(sel())
      ref = req(input$setref)

      if(length(id) > 1)
        showErr("Please select a single individual")
      if(id %in% currData$miss)
        showErr("A missing person cannot be a reference")

      req(!ref %in% currData$refs)
      newped = relabel(currData$ped, old = id, new = ref)
      newrefs = c(currData$refs, ref)

      updatePedData(ped = newped, refs = newrefs)
    })

    observe({ print("ref list")
      remainingRefs = setdiff(references, currData$refs)
      updateSelectInput(session, "setref", choices = c("Select" = "", remainingRefs))
    })

    # Undo/reset --------------------------------------------------------------

    observeEvent(input$undo, {
      stack = previousStack()
      len = length(stack)

      last = stack[[len - 1]]
      for(nm in names(last))
        currData[[nm]] = last[[nm]]

      sel(character(0))
      previousStack(stack[-len])
    })

    observeEvent(input$reset, {
      first = previousStack()[[1]]

      for(nm in names(first))
        currData[[nm]] = first[[nm]]

      sel(character(0))
      previousStack(list(first))
    })

    observeEvent(previousStack(), {
      len = length(previousStack())
      if(len > 1) enable("undo") else disable("undo")
    })

    # Plot --------------------------------------------------------------------

    output$plot = renderPlot({ print("plot")
      x = req(currData$ped)
      miss = currData$miss
      dat = tryCatch(
        plot(x, title = famid, hatched = currData$refs, cex = 1.2, cex.main = 1.5,
             col = list(red = miss, blue = sel()),
             lwd = list(`1.2` = miss, `2` = sel()),
             carrier = miss, foldLabs = 10),
        error = showErr)
      box("outer", col = 1)
      req(dat) # if unsuccessful, return gracefully
      plotdat$alignment = dat$alignment
      plotdat$scaling = dat$scaling
      plotdat$annotation = dat$annotation
    },
    execOnResize = TRUE, res = 72, height = 420, width = 420)

    positionDf = reactive({ print("pos")
      align = req(plotdat$alignment)
      scale = plotdat$scaling
      mat = cbind(x = align$xall,
                  y = align$yall + scale$boxh/2,
                  idInt = align$plotord)
      as.data.frame(mat)
    })

    observeEvent(input$ped_click, {
      posDf = positionDf()
      idInt = nearPoints(posDf, input$ped_click, xvar = "x", yvar = "y",
                         threshold = 20, maxpoints = 1)$idInt
      if(length(idInt) == 0)
        return()

      id = currData$ped$ID[idInt]

      currSel = sel()
      if(id %in% currSel)
        sel(setdiff(currSel, id))
      else
        sel(c(currSel, id))
    })
  })
}
