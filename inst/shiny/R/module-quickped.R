
pedigreeUI = function(id, famid = "F1", references = NULL) {
  ns = NS(id)

  ### Button factory
  myBtn = function(id, label, mar = "2px", side = NULL, style = NULL,
                   width = "100%", tt = NULL, ...) {
    sty = paste("padding:2px; line-height:100%;",
               sprintf("margin: %s;", mar), style)
    if(!is.null(side))
      sty = paste(sty, sprintf("margin-%s: 0;", side))

    b = actionButton(ns(id), label, style = sty, width = width, ...)
    if(!is.null(tt))
      b = wrap_tooltip(b, id, placement = match.arg(tt, c("top", "bottom", "left", "right")))
    b
  }

  fluidRow(
    useShinyjs(),
    column(width = 3,

       p("Add", style = "font-weight:bold; margin-bottom: 0; margin-top:-5px"),
       div(class = "aligned-row-wide",
           myBtn("child", "Child", side = "left"),
           myBtn("parents", "Parents", side = "right"),
       ),
       div(class = "aligned-row-wide",
           myBtn("sibleft", tagList(myIcon("arrow-left", align = "-0.1em"), "Sib"),
                 side = "left", tt = "b"),
           myBtn("sibright", tagList("Sib", myIcon("arrow-right", align = "-0.1em")),
                 side = "right", tt = "b"),
       ),
       p("Sex", style = "font-weight:bold; margin-bottom: 0; margin-top: 10px"),
       div(class = "aligned-row-wide",
           myBtn("sex1", icon("square"), side = "left", tt = "b"),
           myBtn("sex2", icon("circle"), side = NULL, tt = "b"),
           myBtn("sex0", "?", side = "right", tt = "b")
        ),

       #--------
       p("Missing person", style = "font-weight:bold; margin-bottom: 0; margin-top: 10px"),
       div(class = "aligned-row-wide",
           myBtn("missing", myIcon("circle-dot-red", height = 1.2, align = "text-bottom"),
                 side = "left", tt = "b"),
           myBtn("nonmissing", myIcon("circle", height = 1.2, align = "text-bottom"),
                 side = "right", tt = "b"),
       ),

       div(class = "aligned-row-wide", style = "font-weight: bold; margin-bottom: -8px; margin-top: 12px;",
           p("Reference", style = "margin-bottom:0"),
           myBtn("untyped", myIcon("trash-can", align = "-0.1em"),
                 width = "auto", tt = "right",
                 style = "padding:0 3px 0 0; background-color:inherit; border: none"),
       ),
       DTOutput(ns("refTable"), width = "100%"),
       #-------
       br(),
       div(class = "aligned-row-wide",
           myBtn("remove", "Remove", side = "left", tt = "top"),
           myBtn("clearsel", myIcon("hand-pointer-strikethrough"), tt = "right",
                 style = "padding:5px; border:none; background-color: inherit;")
       ),
       div(class = "aligned-row-wide",
          myBtn("reset", "RESET", status = "danger", side = "left", size = "sm"),
          myBtn("undo", "UNDO", status = "warning", side = "right", size = "sm")#      style = "font-size: small")
       )
       #--------
    ),

    # Pedigree plot
    column(width = 9,
           plotOutput(ns("plot"), click = ns("ped_click"), dblclick = ns("ped_dblclick"),
                      width = "auto", height = "auto"),
           p("Double-click pedigree symbols to edit names",
             style = "font-size:small; margin-top: 0; margin-bottom: 0"),
           uiOutput(ns("editlabel")),
    ),
  )
}

pedigreeServer = function(id, resultVar, initialDat = NULL, famid = "F1",
                          allrefs = NULL, avoidLabs = NULL, currentModal = NULL, .debug = NULL) {

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
    showModal({
      mod = modalDialog(
        title = div(class = "aligned-row-wide", "Pedigree builder", helpBtn("help-ped") |> wrap_tooltip("pedhelp", "left")),
        tags$head(tags$style(HTML("@media (min-width: 576px) {.modal-dialog { max-width: 600px !important; }}"))),
        pedigreeUI(id, famid, allrefs),   # use the UI function here
        footer = tagList(
          actionButton(ns("cancel"), "Cancel"),
          actionButton(ns("save"), "Save")
        )
      )
      currentModal(mod)
      mod
    })

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

    observeEvent(input$sibleft, { .debug("--ped: left sibling")
      id = req(sel())
      tryCatch({
        updatePedData(ped = .addSib(currData$ped, id, side = "left", avoid = avoid))
      }, error = showErr)
    })

    observeEvent(input$sibright, { .debug("--ped: right sibling")
      id = req(sel())
      tryCatch({
        updatePedData(ped = .addSib(currData$ped, id, side = "right", avoid = avoid))
      }, error = showErr)
    })

    observeEvent(input$parents, { .debug("--ped module: add parents")
      id = req(sel())
      newped = tryCatch({
        updatePedData(ped = .addParents(currData$ped, id, avoid = avoid))
      }, error = showErr)
    })

    observeEvent(input$sex1, {    .debug("--ped: sex1")
      ids = req(sel())
      tryCatch({
        updatePedData(ped = changeSex(currData$ped, ids, sex = 1))
      }, error = showErr)
    })

    observeEvent(input$sex2, {    .debug("--ped: sex2")
      ids = req(sel())
      tryCatch({
        updatePedData(ped = changeSex(currData$ped, ids, sex = 2))
      }, error = showErr)
    })

    observeEvent(input$sex0, {    .debug("--ped: sex0")
      ids = req(sel())
      tryCatch({
        updatePedData(ped = changeSex(currData$ped, ids, sex = 0))
      }, error = showErr)
    })

    observeEvent(input$remove, { .debug("--ped module: remove")
      ids = req(sel())
      newped = tryCatch(removeSel(currData$ped, ids), error = showErr)
      req(is.ped(newped))
      updatePedData(ped = newped)
    })

    observeEvent(input$clearsel, sel(character(0)))


    # Set status --------------------------------------------------------------

    observeEvent(input$missing, { .debug("--ped module: set missing")
      ids = req(sel())
      if(any(ids %in% currData$refs)) {
        showErr("Reference cannot be set as missing: ", intersect(currData$refs, ids))
        return()
      }
      updatePedData(miss = c(currData$miss, ids)) # dups ok here
    })

    observeEvent(input$nonmissing, { .debug("--ped module: set nonmissing")
      ids = req(sel())
      updatePedData(miss = setdiff(currData$miss, ids))
    })

    observeEvent(input$untyped, { .debug("--ped module: set untyped")
      ids = req(sel())
      oldped = currData$ped
      refs = currData$refs
      ron = currData$refOrigName

      ids = sortIds(oldped, ids) |> intersect(refs) |> req()

      newids = character(length(ids)) |> .setnames(ids)
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

    remainingRefs = reactive(setdiff(allrefs, c(avoidLabs$refs, currData$refs)))

    output$refTable = renderDT({
      df = data.frame(Refs = remainingRefs())
      datatable(df, rownames = FALSE,
                class = "compact stripe",
                selection = "single",
                colnames = "",
                options = list(dom = 't', pageLength = -1, scrollY = "200px",
                             scrollCollapse = TRUE, ordering = FALSE,
                             language = list(zeroRecords = "No available refs"))) |>
        DT::formatStyle(names(df), target = "row", lineHeight = "75%") |>
        DT::formatStyle(names(df), fontSize = "85%")
    })

    observeEvent(input$refTable_rows_selected, {
      id = req(sel())
      if(length(id) > 1) {
        showErr("Multiple pedigree members are selected")
        return()
      }
      if(id %in% currData$miss) {
        showErr("A missing person cannot be a reference")
        return()
      }

      ref = remainingRefs()[input$refTable_rows_selected]
      #req(!ref %in% currData$refs)
      newped = relabel(currData$ped, old = id, new = ref)
      newrefs = c(currData$refs, ref)

      ron = currData$refOrigName
      ron[ref] = id
      currData$refOrigName = ron

      updatePedData(ped = newped, refs = newrefs)
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
      refs = currData$refs
      dat = tryCatch(
        plot(x, title = famid, margins = 2, labs = c(miss, refs),
             hatched = refs, cex = 1.2, cex.main = 1.5,
             col = list(red = miss, blue = sel()),
             lwd = list(`1.2` = miss, `2` = sel()),
             carrier = miss, foldLabs = 10),
        error = showErr)

      # If unsuccessful, return gracefully
      req(dat)
      graphics::box("outer", col = 1)
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
        div(class = "aligned-row-wide", style = "padding: 0",
          editLabBttn(ns("cancellabel"), "Cancel", "warning"),
          editLabBttn(ns("savelabel"), "Save", "success")
        ),

        # Bind Enter key to savelabels button
        tags$script(HTML(sprintf("
          (function() {
            const input = document.getElementById('%s');
            const saveBtn = document.getElementById('%s');
            if (input && saveBtn) {
              input.addEventListener('keyup', function(e) {
                if (e.key === 'Enter') {
                  saveBtn.click();
                }
              });
              input.focus();
              input.select();
            }
          })();
        ", ns("customlabel"), ns("savelabel")))),

        style = paste("position: absolute; z-index: 1000; background-color:lightyellow; padding:6px; width:auto; border: 1px solid gray;",
                      sprintf("left: %fpx;", click_x + 40),
                      sprintf("top:  %fpx;", click_y - 40))
        )
      )
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

