
pedigreeUI = function(id, famid = "F1") {
  ns = NS(id)

  ### Button factory
  myBtn = function(id, label, mar = "2px", side = NULL, style = NULL,
                   width = "100%", tt = NULL, ...) {

    sty = paste("padding:3px; line-height:100%;",
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

       p("Family label", style = "font-weight:bold; margin-bottom: 0; margin-top:-5px"),
       textInput(ns("famid"), label = NULL, value = famid, width = "100%"),

       p("Add", style = "font-weight:bold; margin-bottom: 0; margin-top:10px"),
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
           p("References", style = "margin-bottom:0"),
           myBtn("untyped", myIcon("circle-left", align = "-0.1em"),
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
           div(
             style = "display:flex; align-items:center; justify-content:space-between; font-size:small;",
             p("Double-click symbols to edit names", style = "margin-top: 0; margin-bottom: 0"),
             checkboxInput(ns("showall"), "Show all names", value = FALSE, width = "auto")
           ),
           uiOutput(ns("editlabel")),
    ),
  )
}

pedigreeServer = function(id, resultVar, initialDat = NULL, famid = "F1",
                          allrefs = NULL, refsex = NULL, avoidLabs = NULL, currentModal = NULL,
                          .debug = NULL) {

  .debug2 = if(is.null(.debug)) function(...) NULL else function(...) .debug("-ped module:", ...)

  avoid = c(allrefs, avoidLabs$vics, avoidLabs$labs)
  avoidFamids = .mysetdiff(avoidLabs$famids, famid)

  if(is.null(initialDat)) {
    ids = .generateLabs(character(0), 3, avoid = avoid)
    initialDat = list(ped = nuclearPed(father = ids[1], mother = ids[2], children = ids[3]),
                      famid = famid, miss = character(), refs = character(), refOrigName = character())
  }

  moduleServer(id, function(input, output, session) {
    ns = session$ns

    currData = reactiveValues(ped = initialDat$ped,
                              famid = initialDat$famid,
                              miss = initialDat$miss,
                              refs = initialDat$refs,
                              refOrigName = initialDat$refOrigName)

    previousStack = reactiveVal(list(initialDat))

    plotdat = reactiveValues(alignment = NULL, scaling = NULL, annotation = NULL)
    sel = reactiveVal(character(0))

    # Modal creation and invocation
    showModal({
      mod = modalDialog(
        title = div(class = "aligned-row-wide", "Pedigree builder",
                    helpBtn("help-ped") |> wrap_tooltip("pedhelp", "left")),
        class = "pedmodal", # TODO!
        tags$head(tags$style(HTML("
          @media (min-width: 576px) {
            .modal-dialog { max-width: 600px !important; }
          }"))),
        pedigreeUI(id, famid),
        footer = tagList(
          actionButton(ns("cancel"), "Cancel"),
          actionButton(ns("save"), "Save")
        )
      )
      currentModal(mod)
      mod
    })

    # Return (or cancel) ------------------------------------------------------

    observeEvent(input$save, { .debug2("save")
      if(currData$famid %in% avoidFamids) {
        showErr("Family label is already in use by another family")
        return()
      }
      if(currData$famid == "") {
        showErr("Family label cannot be empty")
        return()
      }
      removeModal()
      resultVar(reactiveValuesToList(currData))
    })

    observeEvent(input$cancel, { .debug2("cancel")
      removeModal()
      resultVar(NULL)
    })

    # Update pedigree ---------------------------------------------------------

    updatePedData = function(ped = currData$ped,
                             famid = currData$famid,
                             miss = currData$miss,
                             refs = currData$refs,
                             refOrigName = currData$refOrigName,
                             clearSel = FALSE) {

      .debug2("update data")
      currData$ped = req(ped)
      currData$famid = trimws(famid)
      currData$miss = .myintersect(ped$ID, miss)
      currData$refs = .myintersect(ped$ID, refs)
      currData$refOrigName = refOrigName

      # Update (or clear) selection
      sel(if(clearSel) character(0) else .myintersect(ped$ID, sel()))

      # Update stack
      previousStack(c(previousStack(), list(reactiveValuesToList(currData))))
    }

    # Modify pedigree ---------------------------------------------------------

    famidDeb = reactive(input$famid) |> debounce(200)

    observeEvent(famidDeb(), { .debug2("edit family name")
      updatePedData(famid = famidDeb(), clearSel = FALSE)
    }) |> debounce(200)

    observeEvent(input$child, { .debug2("add child")
      newped = tryCatch(
        .addChild(currData$ped, req(sel()), sex = 1, avoid = avoid),
        error = showErr)

      req(is.ped(newped))
      updatePedData(ped = newped, clearSel = FALSE)
    })

    observeEvent(input$sibleft, { .debug2("left sibling")
      id = req(sel())
      tryCatch({
        updatePedData(ped = .addSib(currData$ped, id, side = "left", avoid = avoid))
      }, error = showErr)
    })

    observeEvent(input$sibright, { .debug2("right sibling")
      id = req(sel())
      tryCatch({
        updatePedData(ped = .addSib(currData$ped, id, side = "right", avoid = avoid))
      }, error = showErr)
    })

    observeEvent(input$parents, { .debug2("add parents")
      id = req(sel())
      newped = tryCatch({
        updatePedData(ped = .addParents(currData$ped, id, avoid = avoid), clearSel = TRUE)
      }, error = showErr)
    })

    observeEvent(input$sex1, {    .debug2("sex1")
      ids = req(sel())
      tryCatch({
        updatePedData(ped = changeSex(currData$ped, ids, sex = 1, refuse = currData$refs))
      }, error = showErr)
    })

    observeEvent(input$sex2, {    .debug2("sex2")
      ids = req(sel())
      tryCatch({
        updatePedData(ped = changeSex(currData$ped, ids, sex = 2, refuse = currData$refs))
      }, error = showErr)
    })

    observeEvent(input$sex0, {    .debug2("sex0")
      ids = req(sel())
      tryCatch({
        updatePedData(ped = changeSex(currData$ped, ids, sex = 0, refuse = currData$refs))
      }, error = showErr)
    })

    observeEvent(input$remove, { .debug2("remove")
      ids = req(sel())
      newped = tryCatch(removeSel(currData$ped, ids), error = showErr)
      req(is.ped(newped))
      updatePedData(ped = newped)
    })

    observeEvent(input$clearsel, sel(character(0)))


    # Set status --------------------------------------------------------------

    observeEvent(input$missing, { .debug2("set missing")
      ids = req(sel())
      if(any(ids %in% currData$refs)) {
        showErr("Reference cannot be set as missing: ", intersect(currData$refs, ids))
        return()
      }
      updatePedData(miss = c(currData$miss, ids), clearSel = TRUE) # dups ok here
    })

    observeEvent(input$nonmissing, { .debug2("set nonmissing")
      ids = req(sel())
      updatePedData(miss = .mysetdiff(currData$miss, ids))
    })

    observeEvent(input$untyped, { .debug2("set untyped")
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
      newrefs = .mysetdiff(refs, ids)
      newron = ron[!names(ron) %in% ids]

      updatePedData(ped = newped, refs = newrefs, refOrigName = newron)
    })

    remainingRefs = reactive(.mysetdiff(allrefs, c(avoidLabs$refs, currData$refs)))

    output$refTable = renderDT({ .debug2("render ref table:", remainingRefs())
      df = data.frame(Refs = remainingRefs() %||% character())
      datatable(df, rownames = FALSE,
                class = "compact stripe",
                selection = "single",
                colnames = "",
                options = list(dom = 't', pageLength = -1, scrollY = "200px",
                             scrollCollapse = TRUE, ordering = FALSE,
                             language = list(zeroRecords = "No unassigned refs!"))) |>
        DT::formatStyle(names(df),
                        target = "row",
                        lineHeight = "80%",
                        fontSize = "90%",
                        whiteSpace = "nowrap"
        )
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

      sex = getSex(currData$ped, id)
      if(sex != refsex[ref]) {
        showErr(sprintf("Sex mismatch! Reference '%s' is %s", ref, c("male", "female")[refsex[ref]]))
        return()
      }

      newped = relabel(currData$ped, old = id, new = ref)
      newrefs = c(currData$refs, ref)

      ron = currData$refOrigName
      ron[ref] = id

      updatePedData(ped = newped, refs = newrefs, refOrigName = ron)
    })


    # Undo/reset --------------------------------------------------------------

    observeEvent(input$undo, { .debug2("undo")
      stack = previousStack()
      len = length(stack)
      req(len > 1)

      last = stack[[len - 1]]
      for(nm in names(last))
        currData[[nm]] = last[[nm]]

      sel(character(0))
      previousStack(stack[-len])
    })

    observeEvent(input$reset, { .debug2("reset")
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

    output$plot = renderPlot({ .debug2("plot")
      x = req(currData$ped)
      famid = currData$famid
      miss = currData$miss
      refs = currData$refs
      labs = if(input$showall) x$ID else c(miss, refs)
      dat = tryCatch(
        plot(x, title = famid, margins = 2, labs = labs,
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

    observeEvent(input$ped_click, { .debug2("click")
      posDf = positionDf()
      idInt = nearPoints(posDf, input$ped_click, xvar = "x", yvar = "y",
                         threshold = 20, maxpoints = 1)$idInt
      if(length(idInt) == 0)
        return()

      id = currData$ped$ID[idInt]

      currSel = sel()
      if(id %in% currSel)
        sel(.mysetdiff(currSel, id))
      else
        sel(c(currSel, id))
    })

    editId = reactiveVal(NULL)

    observeEvent(input$ped_dblclick, { .debug2("dblclick")
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

    observeEvent(input$savelabel, { .debug2("savelabel")
      id = req(editId())
      newlab = input$customlabel |> trimws() |> req()
      ped = currData$ped
      miss = currData$miss

      # Check if label should be avoided
      if(newlab %in% allrefs)
        return(showErr(sprintf("The name '%s' is already in use by a reference individual", newlab)))
      if(newlab %in% avoidLabs$vics)
        return(showErr(sprintf("The name '%s' is already in use by a victim sample", newlab)))
      if(newlab %in% avoidLabs$miss)
        return(showErr(sprintf("The name '%s' is used by a missing person in another family", newlab)))
      if(id %in% miss && newlab %in% avoidLabs$labs)
        return(showErr(sprintf("The name '%s' is already in use in another family", newlab)))

      # If no change, just exit
      if(id == newlab) {
        output$editlabel = renderUI(NULL)
        return()
      }

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

