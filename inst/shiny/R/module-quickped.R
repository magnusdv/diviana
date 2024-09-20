
pedigreeUI = function(id, famid = "F1", references = NULL) {
  ns = NS(id)

  fluidRow(
    useShinyjs(),  # Set up shinyjs
#tags$script(sprintf("
#      Shiny.addCustomMessageHandler('selectText', function(message) {
#        $('#%s').select();
#      });
#    ", ns("customlabel"))),

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
           plotOutput(ns("plot"), click = ns("ped_click"), dblclick = ns("ped_dblclick"),
                      width = "auto", height = "auto"),
           uiOutput(ns("editlabel")),
    ),
  )
}

pedigreeServer = function(id, resultVar, initialDat = NULL, famid = "F1",
                          allrefs = NULL, avoidLabs = NULL, .debug = NULL) {

  avoid = c(allrefs, avoidLabs$vics, avoidLabs$labs)

  if(is.null(initialDat)) {
    ids = .generateLabs(character(0), 3, avoid = avoid)
    initialDat = list(ped = nuclearPed(father = ids[1], mother = ids[2], children = ids[3]),
                      miss = character(), refs = character())
  }

  moduleServer(id, function(input, output, session) {
    ns = session$ns

    currData = reactiveValues(ped = initialDat$ped,
                              miss = initialDat$miss,
                              refs = initialDat$refs,
                              refOrigName = initialDat$refOrigName)

    previousStack = reactiveVal(list(initialDat))

    plotdat = reactiveValues(alignment = NULL, scaling = NULL, annotation = NULL)
    sel = reactiveVal(character(0))

    # Modal creation and invocation
    showModal(modalDialog(
        title = "Pedigree builder",
        tags$head(tags$style(HTML("@media (min-width: 576px) {.modal-dialog { max-width: 600px !important; }}"))),
        pedigreeUI(id, famid, allrefs),   # use the UI function here
        footer = tagList(
          actionButton(ns("save"), "Save"),
          actionButton(ns("cancel"), "Cancel")
        )
    ))

    # Return (or cancel) ------------------------------------------------------

    observeEvent(input$save, { .debug("--ped module: save")
      removeModal()
      resultVar(reactiveValuesToList(currData))
    })

    observeEvent(input$cancel, { .debug("--ped module: cancel")
      removeModal()
      resultVar(NULL)
    })

    # Update pedigree ---------------------------------------------------------

    updatePedData = function(ped = currData$ped, miss = currData$miss,
                             refs = currData$refs, clearSel = TRUE) {
      .debug("--ped module: update data")
      currData$ped = req(ped)
      currData$miss = .myintersect(ped$ID, miss)
      currData$refs = .myintersect(ped$ID, refs)

      # Update (or clear) selection
      sel(if(clearSel) character(0) else .myintersect(ped$ID, sel()))

      # Update stack
      previousStack(c(previousStack(), list(reactiveValuesToList(currData))))
    }

    # Modify pedigree ---------------------------------------------------------

    observeEvent(input$child, { .debug("--ped module: add child")
      newped = tryCatch(
        .addChild(currData$ped, req(sel()), sex = 1, avoid = avoid),
        error = showErr)

      req(is.ped(newped))
      updatePedData(ped = newped, clearSel = FALSE)
    })

    observeEvent(input$sibling, { .debug("--ped module: add sibling")
      newped = tryCatch(
        .addSib(currData$ped, req(sel()), sex = 1, avoid = avoid),
        error = showErr)

      req(is.ped(newped))
      updatePedData(ped = newped)
    })

    observeEvent(input$parents, { .debug("--ped module: add parents")
      newped = tryCatch(
        .addParents(currData$ped, req(sel()), avoid = avoid),
        error = showErr)

      req(is.ped(newped))
      updatePedData(ped = newped)
    })

    observeEvent(input$swapsex, { .debug("--ped module: swap sex")
      id = req(sel())
      newped = swapSex(currData$ped, id, verbose = FALSE)
      updatePedData(ped = newped, clearSel = length(id) > 1)
    })

    observeEvent(input$remove, { .debug("--ped module: remove")
      ids = req(sel())
      newped = tryCatch(removeSel(currData$ped, ids), error = showErr)
      req(is.ped(newped))
      updatePedData(ped = newped)
    })

    #observeEvent(input$clearselection, sel(character(0)))


    # Set status --------------------------------------------------------------

    observeEvent(input$missing, { .debug("--ped module: set missing")
      ids = req(sel())
      if(any(ids %in% currData$refs)) {
        showErr("Reference cannot be set as missing: ", intersect(currData$refs, ids))
        return()
      }

      oldmiss = currData$miss
      newmiss = c(setdiff(oldmiss, ids), setdiff(ids, oldmiss))
      updatePedData(miss = newmiss)
    })

    observeEvent(input$untyped, { .debug("--ped module: set untyped")
      ids = req(sel())
      oldped = currData$ped
      refs = currData$refs
      ron = currData$refOrigName

      ids = sortIds(oldped, ids) |> intersect(refs) |> req()

      newids = character(length(ids)) |> setnames(ids)
      orig = ids[ids %in% names(ron)]
      orig = orig[!ron[orig] %in% oldped$ID]
      newids[orig] = ron[orig]
      empt = newids == ""
      newids[empt] = .generateLabs(oldped, n = length(empt), avoid = avoid)
      newped = relabel(oldped, new = newids)

      # TODO: Move to updatepeddata
      newron = ron[!names(ron) %in% ids]
      currData$refOrigName = newron

      updatePedData(ped = newped, refs = setdiff(refs, ids))
    })

    observeEvent(input$setref, { .debug("--ped module: set reference")
      id = req(sel())
      ref = req(input$setref)

      if(length(id) > 1)
        showErr("Please select a single individual")
      if(id %in% currData$miss)
        showErr("A missing person cannot be a reference")

      req(!ref %in% currData$refs)
      newped = relabel(currData$ped, old = id, new = ref)
      newrefs = c(currData$refs, ref)

      ron = currData$refOrigName
      ron[ref] = id
      currData$refOrigName = ron

      updatePedData(ped = newped, refs = newrefs)
    })

    observe({ .debug("--ped module: update reference selector")
      remainingRefs = setdiff(allrefs, c(avoidLabs$refs, currData$refs))
      updateSelectInput(session, "setref", choices = c("Select" = "", remainingRefs))
    })

    # Undo/reset --------------------------------------------------------------

    observeEvent(input$undo, { .debug("--ped module: undo")
      stack = previousStack()
      len = length(stack)

      last = stack[[len - 1]]
      for(nm in names(last))
        currData[[nm]] = last[[nm]]

      sel(character(0))
      previousStack(stack[-len])
    })

    observeEvent(input$reset, { .debug("--ped module: reset")
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

    output$plot = renderPlot({ .debug("--ped module: plot")
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

    positionDf = reactive({
      align = req(plotdat$alignment)
      scale = plotdat$scaling
      mat = cbind(x = align$xall,
                  y = align$yall + scale$boxh/2,
                  idInt = align$plotord)
      as.data.frame(mat)
    })

    observeEvent(input$ped_click, { .debug("--ped module: click")
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

    editId = reactiveVal(NULL)

    observeEvent(input$ped_dblclick, { .debug("--ped module: dblclick")
      posDf = positionDf()
      idInt = nearPoints(posDf, input$ped_dblclick, xvar = "x", yvar = "y",
                         threshold = 20, maxpoints = 1)$idInt
      if(length(idInt) == 0)
        return()
      id = currData$ped$ID[idInt]

      if(id %in% currData$refs) {
        showErr("Cannot edit reference names in the plot window")
        id = NULL
      }
      editId(id)
      req(id)

      click_x = input$ped_dblclick$coords_css$x
      click_y = input$ped_dblclick$coords_css$y
      plotW = session$clientData[[sprintf("output_%s_width", ns("plot"))]]
      plotH = session$clientData[[sprintf("output_%s_height", ns("plot"))]]

      output$editlabel = renderUI(div(
        slimTextInput(ns("customlabel"), label = "Enter new name:", value = id, style = "width:auto",
                      labelStyle = "font-size: small; margin-bottom: 0; padding-left: 3px",
                      textStyle = "font-size: smaller; color: black; width: auto;"),
        fluidRow(
          column(6, align = "left", editLabBttn(ns("savelabel"), "Save", "primary")),
          column(6, align = "right", editLabBttn(ns("cancellabel"), "Cancel", "secondary"))
        ),
        style = paste("position: absolute; z-index: 1000; background-color:lightyellow; padding:6px; width:auto;",
                      sprintf("left: %fpx;", click_x + 40),
                      sprintf("top: %fpx;", click_y - 40))
        )
      )
      #session$sendCustomMessage("selectText", "select")
      #runjs(sprintf('console.log("JavaScript is running for customlabel focus: %s");', ns("customlabel")))
      #runjs(sprintf('$("#%s").focus();', ns("customlabel")))
    })


    observeEvent(input$cancellabel, { output$editlabel = renderUI(NULL) })

    observeEvent(input$savelabel, { .debug("--ped module: savelabel")
      id = req(editId())
      newlab = input$customlabel
      ped = currData$ped
      miss = currData$miss

      # Check if label should be avoided
      err = NULL
      if(newlab %in% allrefs)
        err = showErr(sprintf("The name '%s' is already in use by a reference individual", newlab))
      else if(newlab %in% avoidLabs$vics)
        err = showErr(sprintf("The name '%s' is already in use by a victim sample", newlab))
      else if(newlab %in% avoidLabs$miss)
        err = showErr(sprintf("The name '%s' is used by a missing person in another family", newlab))
      else if(id %in% miss && newlab %in% avoidLabs$labs)
        err = showErr(sprintf("The name '%s' is already in use in another family", newlab))

      if(!is.null(err) || id == newlab)
        return()

      newped = tryCatch(relabel(ped, old = id, new = newlab),
                        error = showErr)
      req(is.ped(newped))

      if(id %in% miss)
        miss[miss == id] = newlab

      updatePedData(ped = newped, miss = miss)
      output$editlabel = renderUI(NULL)
    })
  })
}

editLabBttn = function(id, label, status) {
  actionButton(id, label, size = "sm", status = status, width = "auto",
               style = "height: 30px; line-height:30px; padding: 0 5px 0 5px; margin-top: 5px;")
}

