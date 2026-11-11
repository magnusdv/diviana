# Register alias controls within the existing data module
registerAliases = function(id, input, output, session, mainTable, setData,
                           assignedRefs, missingIDs, idEdits, .debug2) {
  ns = session$ns

  origs = reactiveVal(NULL)
  aliasErrorMsg = reactiveVal("")

  # Preserve alias rules across dialog openings
  aliasOpts = reactiveValues(method = "none",
                             prefix = switch(id, PM = "V", AM = "R", "ID"),
                             cutFrom = "", cutUntil = "", remove = "", regex = "")

  pendingReplace = reactiveVal(NULL)

  observeEvent(input$aliasMethod, {aliasOpts$method = input$aliasMethod})
  observeEvent(input$prefix, {aliasOpts$prefix = input$prefix})
  observeEvent(input$cutFrom, {aliasOpts$cutFrom = input$cutFrom})
  observeEvent(input$cutUntil, {aliasOpts$cutUntil = input$cutUntil})
  observeEvent(input$remove, {aliasOpts$remove = input$remove})
  observeEvent(input$regex, {aliasOpts$regex = input$regex})

  # Function for opening the alias dialog
  showAliasDialog = function() {
    dat = req(mainTable$main)
    or = if(id == "AM") displayFamily(dat, assignedRefs())$Sample else dat$Sample
    origs(or)

    showModal(modalDialog(
      title = "Generate Aliases",
      fluidRow(
        column(6,
          radioButtons(ns("aliasMethod"), "Choose method:",
                       choices = list("Do not use aliases" = "none",
                                      "Sequence with prefix" = "prefix",
                                      "Keep part of text" = "cut",
                                      "Remove text" = "remove",
                                      "Regular expression" = "regex"),
                       selected = isolate(aliasOpts$method))
        ),
        column(6,
          conditionalPanel("input.aliasMethod == 'prefix'", ns = ns,
            textInput(ns("prefix"), "Prefix:", value = isolate(aliasOpts$prefix))
          ),
          conditionalPanel("input.aliasMethod == 'cut'", ns = ns,
            div(class = "d-flex", style = "gap:6px",
              div(style = "flex:1;min-width:0",
                textInput(ns("cutFrom"), "From:", value = isolate(aliasOpts$cutFrom),
                          placeholder = "beginning", width = "100%")),
              div(style = "flex:1;min-width:0",
                textInput(ns("cutUntil"), "Until:", value = isolate(aliasOpts$cutUntil),
                          placeholder = "end", width = "100%"))
            )
          ),
          conditionalPanel("input.aliasMethod == 'remove'", ns = ns,
            textInput(ns("remove"), "Strings (separate w/comma):",
                      value = isolate(aliasOpts$remove))
          ),
          conditionalPanel("input.aliasMethod == 'regex'", ns = ns,
            textInput(ns("regex"), "Regex pattern to match:",
                      value = isolate(aliasOpts$regex))
          ),
          uiOutput(ns("aliasErrorUI"))
        )
      ),
      div(style = "margin-top: 10px;", DT::DTOutput(ns("aliasTable"))),
      footer = tagList(
        tagAppendAttributes(
          actionButton(ns("aliasReplace"), HTML("Replace originals<br>with aliases"), warning = "info"),
          class = "btn-xs",
          style = "float:left; margin-right:auto; color:tomato; font-size:80%; font-weight:550; line-height:1.1; border-radius: 12px; padding: 2px 6px;"
        ),
        modalButton("Cancel"),
        actionButton(ns("aliasSave"), "Save", status = "success"))
    ))
  }

  observeEvent(input$aliasButton, { .debug2("open Alias UI")
    showAliasDialog()
  })

  output$aliasTable = DT::renderDT({
    df = data.frame(Original = origs(), Alias = generateAliases())
    aliasDT(df)
  })

  generateAliases = reactive({ .debug2("generate aliases")
    origs = req(origs())
    aliasErrorMsg("")
    method = input$aliasMethod
    inp = if(identical(method, "cut"))
      paste0(input$cutFrom %||% "", input$cutUntil %||% "")
    else input[[method]]
    if(is.null(inp) || inp == "")
      return(origs)

    res = origs
    if(method == "none")
      return(origs)

    if(method == "prefix")
      return(paste0(inp, seq_along(origs)))

    # Remaining methods
    tryCatch({
      if(method == "cut") {
        from = input$cutFrom %||% ""
        until = input$cutUntil %||% ""
        start = rep.int(1L, length(origs))
        end = nchar(origs)
        ok = rep(TRUE, length(origs))
        if(nzchar(from)) {
          r = regexpr(from, origs, fixed = TRUE)
          ok = ok & r > 0L
          start = r + nchar(from)
        }
        if(nzchar(until)) {
          r = vapply(gregexpr(until, origs, fixed = TRUE), tail, integer(1), n = 1L)
          ok = ok & r > 0L
          end = r - 1L
        }
        ok = ok & start <= end
        res[ok] = substr(origs[ok], start[ok], end[ok])
      }
      else if(method == "remove") {
        pattern = strsplit(inp, ",")[[1]] |> paste(collapse = "|")
        res = gsub(pattern, "", origs)
      }
      else if(method == "regex") {
        r = regexpr(inp, origs)
        al = origs
        al[r > -1] = regmatches(origs, r)
        res = al
      }
    },
      warning = function(w) NULL,
      error = function(e) aliasErrorMsg("Invalid regular expression")
    )
    if(any(res == ""))
      aliasErrorMsg("Empty aliases (not permitted)")
    else if(anyDuplicated.default(res))
      aliasErrorMsg(if(id == "AM")
        "Warning: Duplicated aliases (ok for AM samples)" else
          "Duplicated aliases (not permitted for PM samples)")
    res
  })

  output$aliasErrorUI = renderUI({ .debug2("alias error ui")
    msg = aliasErrorMsg()
    if(msg != "")
      div(class = if(id == "AM" && startsWith(msg, "Duplicated")) "text-warning" else "text-danger", msg)
  })

  observeEvent(input$aliasSave, { .debug2("alias save")
    dat = mainTable$main
    dat$Alias = generateAliases()[match(dat$Sample, origs())]
    tryCatch({
      setData(dat)
      removeModal()
    }, error = showErr)
  })

  observeEvent(input$aliasReplace, { .debug2("alias replace")
    dat = mainTable$main
    al = generateAliases()[match(dat$Sample, origs())]

    if(anyNA(al) || any(!nzchar(al)) || anyDuplicated(al)) {
      showErr("Replacing sample IDs requires unique, nonempty aliases")
      return()
    }
    if(any(al %in% missingIDs())) {
      showErr("Replacement ID already in use by missing person: ",
              toString(intersect(al, missingIDs())))
      return()
    }

    pendingReplace(list(dat = dat, aliases = al))
    showModal(confirmModal(
      sprintf("Replace all %s sample IDs with their aliases?", id),
      ns("aliasReplaceBack"), ns("aliasReplaceConfirm")
    ))
  })

  observeEvent(input$aliasReplaceBack, {
    pendingReplace(NULL)
    showAliasDialog()
  })

  observeEvent(input$aliasReplaceConfirm, { .debug2("alias replace confirmed")
    p = req(pendingReplace())
    dat = p$dat
    al = p$aliases
    changes = list(ids = .setnames(al, dat$Sample))
    dat$Alias = dat$Sample = al

    tryCatch({
      setData(dat)
      idEdits(changes)
      pendingReplace(NULL)

      # Start with default alias rules after replacing IDs
      aliasOpts$method = "none"
      aliasOpts$prefix = switch(id, PM = "V", AM = "R", "ID")
      aliasOpts$cutFrom = ""
      aliasOpts$cutUntil = ""
      aliasOpts$remove = ""
      aliasOpts$regex = ""
      removeModal()
    }, error = showErr)
  })

}

aliasDT = function(dat, scrollY = "250px") {
  res = DT::datatable(
    dat,
    rownames = FALSE,
    selection = "none",
    class = "stripe hover compact",
    plugins = "natural",
    options = list(dom = 't',
                   scrollX = FALSE,
                   scrollY = scrollY,
                   scrollCollapse = TRUE,
                   paging = FALSE,
                   order = list(),
                   columnDefs = list(list(type = "natural", targets = 0:1)))
    ) |>
    DT::formatStyle(names(dat), target = "row", lineHeight = "80%")

  # Highlight empty or duplicated aliases
  bad = c("", unique.default(dat$Alias[duplicated.default(dat$Alias)]))
  cols = c("lightgray", "pink", "lightblue", "yellow", "lightgreen", "orange")[seq_along(bad)]
  DT::formatStyle(res, "Alias", backgroundColor = DT::styleEqual(bad, cols))
}
