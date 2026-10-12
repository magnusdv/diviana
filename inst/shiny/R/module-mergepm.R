# Merge PM module ----------------------------------------------------------

mergePmServer = function(id, trigger, dvi, defaultThreshold = 1e4, defaultDropout = 0,
                         defaultDropin = 0, defaultTypingError = 0, defaultIgnoreSex = FALSE,
                         defaultMethod = "combine", defaultNames = "combine") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns

    analysis = reactiveVal(NULL)
    settings = reactiveVal(list(threshold = defaultThreshold, dropout = defaultDropout,
                                dropin = defaultDropin, typingError = defaultTypingError,
                                ignoreSex = defaultIgnoreSex, defaultMethod = defaultMethod,
                                defaultNames = defaultNames))
    approved = reactiveVal(character())
    mergedNames = reactiveVal(character())
    mergeMethods = reactiveVal(character())
    selected = reactiveVal(NULL)
    clicks = reactiveVal(integer())
    lrClicks = reactiveVal(integer())
    pending = reactiveVal(NULL)
    saved = reactiveVal(NULL)

    currentSettings = function() {
      list(threshold = input$threshold, dropout = input$dropout,
           dropin = input$dropin, typingError = input$typingError,
           ignoreSex = input$ignoreSex,
           defaultMethod = input$defaultMethod, defaultNames = input$defaultNames)
    }

    # Remember settings and require fresh analysis after any change
    observe({
      req(input$threshold, input$dropout, input$dropin, input$typingError,
          !is.null(input$ignoreSex), input$defaultMethod, input$defaultNames)
      s = currentSettings()
      if(!identical(s, settings())) {
        settings(s)
        analysis(NULL)
      }
    }, priority = 100)

    showMain = function() {
      s = settings()

      showModal(modalDialog(
        title = "Merge PM",
        size = "l",
        easyClose = FALSE,

        fluidRow(
          column(3,
            numericInput(ns("threshold"), "LR threshold", value = s$threshold,
                         min = 1, step = 1000)
          ),
          column(3,
            numericInput(ns("dropout"), "Dropout", value = s$dropout,
                         min = 0, max = 0.99, step = 0.01)
          ),
          column(3,
            numericInput(ns("dropin"), "Dropin", value = s$dropin,
                         min = 0, max = 0.99, step = 0.01)
          ),
          column(3, style = "position:relative;",
            numericInput(ns("typingError"), "Typing error", value = s$typingError,
                         min = 0, max = 0.99, step = 0.01),
            div(style = "position:absolute; right:10px;",
                tags$label(
                  style = "display:flex; align-items:center; gap:8px; font-weight:normal;",
                  "Ignore sex",
                  tags$input(id = ns("ignoreSex"), type = "checkbox",
                             class = "shiny-input-checkbox",
                             checked = if(s$ignoreSex) "checked")))
          )
        ),
        br(),
        fluidRow(
          column(5,
            radioButtons(ns("defaultMethod"), "Default merge method", inline = TRUE,
                         choices = c("Combine" = "combine", "Most complete" = "mostcomplete"),
                         selected = s$defaultMethod)
          ),
          column(5,
            radioButtons(ns("defaultNames"), "Default names", inline = TRUE,
                         choices = c("Combine" = "combine", "Most complete" = "mostcomplete"),
                         selected = s$defaultNames)
          ),
          column(2,
            div(style = "padding-top:25px; text-align:right;",
                actionButton(ns("analyse"), "Analyse", status = "primary"))
          )
        ),
        br(),
        uiOutput(ns("results")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save"), "Save and replace", status = "success")
        )
      ))
    }

    observeEvent(trigger(), {
      analysis(NULL)
      approved(character())
      mergedNames(character())
      mergeMethods(character())
      selected(NULL)
      clicks(integer())
      lrClicks(integer())
      pending(NULL)
      saved(NULL)
      showMain()
    })


    # Identify groups once, always computing the combined profile
    observeEvent(input$analyse, {
      d = req(dvi())
      if(length(d$pm) < 2L) {
        showNotification("At least two PM samples are needed", type = "warning")
        return()
      }

      s = currentSettings()
      settings(s)
      res = tryCatch(
        dvir::mergePM(d$pm, threshold = s$threshold, method = "combine",
                      names = "combine", dropout = s$dropout,
                      dropin = s$dropin, typingError = s$typingError,
                      ignoreSex = s$ignoreSex, verbose = FALSE),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if(is.null(res))
        return()

      groups = res$groups[lengths(res$groups) > 1L]
      analysis(res)
      approved(character())
      nms = if(s$defaultNames == "combine") names(groups) else
        vapply(groups, function(g) g[which.max(res$nonmissing[g])], character(1))
      mergedNames(setNames(unname(nms), names(groups)))
      mergeMethods(setNames(rep.int(s$defaultMethod, length(groups)), names(groups)))
      clicks(vapply(seq_along(groups), function(i) input[[paste0("inspect", i)]] %||% 0,
                    numeric(1)))
      lrClicks(vapply(seq_along(groups), function(i) input[[paste0("lr", i)]] %||% 0,
                      numeric(1)))
    })

    output$results = renderUI({
      res = analysis()
      if(is.null(res))
        return(NULL)

      groups = res$groups[lengths(res$groups) > 1L]
      if(!length(groups))
        return(div(class = "text-muted", "No merge groups identified."))

      appr = approved()
      nms = mergedNames()
      methods = mergeMethods()

      # Build one compact row per proposed group
      rows = lapply(seq_along(groups), function(i) {
        gn = names(groups)[i]
        g = groups[[i]]
        probs = res$problems[[gn]]
        ok = gn %in% appr

        lr = if(length(g) == 2L)
          formatC(res$LRmat[g[1], g[2]], format = "g", digits = 4)
        else
          actionButton(ns(paste0("lr", i)), "LR matrix",
                       style = "padding:3px 7px; font-size:85%;")

        status = if(ok) {
          method = if(methods[gn] == "combine") "Combine" else "Most complete"
          span(icon("check"), " ", method, class = "text-success", title = "Approved",
               style = "font-weight:bold; white-space:nowrap;")
        }
        else
          span("Pending", class = "text-muted")

        tags$tr(
          tags$td(paste(g, collapse = ", ")),
          tags$td(nms[gn]),
          tags$td(lr),
          tags$td(if(length(probs))
                    span(paste(probs, collapse = ", "), class = "text-danger") else ""),
          tags$td(status),
          tags$td(actionButton(ns(paste0("inspect", i)), "Inspect",
                               style = "padding:3px 7px; font-size:85%;"))
        )
      })

      tagList(
        div(class = "mb-2",
            sprintf("%d merge group%s identified.", length(groups),
                    if(length(groups) == 1L) "" else "s")),
        tags$table(
          class = "table table-sm table-bordered",
          style = "margin-bottom:10px; width:100%;",
          tags$thead(tags$tr(
            tags$th("Samples"),
            tags$th("Merged name"),
            tags$th("LR"),
            tags$th("Problems"),
            tags$th("Status"),
            tags$th("Review")
          )),
          tags$tbody(style = "font-size:95%;", rows)
        ),
        div(style = "text-align:right;", actionLink(ns("approveAll"), "Approve all"))
      )
    })

    observeEvent(input$approveAll, {
      res = req(analysis())
      groups = res$groups[lengths(res$groups) > 1L]
      approved(names(groups))
    })

    # Detect which dynamically generated Inspect button was clicked
    observe({
      res = analysis()
      req(res)
      groups = res$groups[lengths(res$groups) > 1L]
      n = length(groups)
      if(!n)
        return()

      vals = vapply(seq_len(n), function(i) input[[paste0("inspect", i)]] %||% 0,
                    numeric(1))
      old = clicks()
      if(length(old) != n)
        old = numeric(n)
      hit = which(vals > old)
      clicks(vals)
      if(!length(hit))
        return()

      i = hit[1]
      gn = names(groups)[i]
      g = groups[[i]]
      selected(gn)
      s = settings()
      best = g[which.max(res$nonmissing[g])]

      showModal(modalDialog(
        title = tagList(
          sprintf("Inspect cluster: %s", paste(g, collapse = ", ")),
          div(class = "text-muted",
              style = "position:absolute; top:10px; right:10px; font-size:60%; line-height:1.2;
                       font-family:monospace; white-space:pre; text-align:left;",
              sprintf("%8s %g\n%8s %g\n%8s %g",
                      "Dropout:", s$dropout, "Dropin:", s$dropin, "Error:", s$typingError))
        ),
        size = "l",
        fluidRow(
          column(6,
            textInput(ns("mergedName"), "Merged sample name", value = mergedNames()[gn])
          ),
          column(6,
            radioButtons(ns("mergeMethod"), "Merge method", inline = TRUE,
              choices = setNames(c("combine", "mostcomplete"),
                                 c("Combine profiles", sprintf("Most complete (%s)", best))),
              selected = mergeMethods()[gn])
          )
        ),
        DT::DTOutput(ns("inspectTable")),
        footer = tagList(
          actionButton(ns("reject"), "Don't merge"),
          actionButton(ns("approve"), "Approve merge", status = "success")
        )
      ))
    })

    # Show pairwise LRs for groups with more than two samples
    observe({
      res = analysis()
      req(res)
      groups = res$groups[lengths(res$groups) > 1L]
      n = length(groups)
      if(!n)
        return()

      vals = vapply(seq_len(n), function(i) input[[paste0("lr", i)]] %||% 0,
                    numeric(1))
      old = lrClicks()
      if(length(old) != n)
        old = numeric(n)
      hit = which(vals > old)
      lrClicks(vals)
      if(!length(hit))
        return()

      g = groups[[hit[1]]]
      m = res$LRmat[g, g, drop = FALSE]
      fm = matrix(formatC(m, format = "g", digits = 4), nrow = length(g),
                  dimnames = dimnames(m))
      diag(fm) = "–"

      showModal(modalDialog(
        title = "Pairwise LR matrix",
        tags$table(
          class = "table table-sm table-bordered w-auto mx-auto mb-0",
          tags$thead(tags$tr(tags$th(""), lapply(g, function(x) tags$th(x, class = "text-center")))),
          tags$tbody(lapply(seq_along(g), function(r)
            tags$tr(tags$th(g[r]),
                    lapply(seq_along(g), function(cc) tags$td(fm[r, cc])))))
        ),
        footer = actionButton(ns("lrBack"), "Back")
      ))
    })

    observeEvent(input$lrBack, showMain())

    output$inspectTable = DT::renderDT({
      res = req(analysis())
      gn = req(selected())
      d = req(dvi())
      g = names(d$pm)[names(d$pm) %in% res$groups[[gn]]]

      orig = getGenotypes(d$pm[g])
      method = input$mergeMethod %||% mergeMethods()[gn]
      result = if(method == "combine") {
        getGenotypes(res$pmReduced[[gn]])[1, ]
      }
      else {
        best = g[which.max(res$nonmissing[g])]
        getGenotypes(d$pm[[best]])[1, ]
      }

      probs = res$problems[[gn]]
      problems = colnames(orig) %in% probs
      dropout = settings()$dropout

      # Describe differences among the original profiles
      diffs = vapply(seq_len(ncol(orig)), function(j) {
        z = orig[, j]
        if(problems[j])
          return("Inconsistent")
        if(length(unique.default(z)) == 1L)
          return("")
        if(any(z == "-/-") && length(unique.default(z[z != "-/-"])) == 1L)
          return("Missing")
        if(dropout > 0)
          return("Dropout")
        "Inconsistent"
      }, character(1))

      tab = data.frame(Marker = colnames(orig), t.default(orig), Result = unname(result),
                       Difference = diffs, check.names = FALSE, row.names = NULL)
      diffsCol = ncol(tab) - 1L

      DT::datatable(tab,
        rownames = FALSE,
        selection = "none",
        class = "stripe hover compact nowrap",
        options = list(
          dom = "t", paging = FALSE, scrollX = TRUE,
          scrollY = if(nrow(tab) > 15L) "420px" else NULL,
          scrollCollapse = TRUE, order = list(),
          columnDefs = list(
            list(orderable = FALSE, targets = 0:(diffsCol - 1L)),
            list(orderable = TRUE, targets = diffsCol)
          )
        )
      ) |>
        DT::formatStyle(names(tab), target = "row", lineHeight = "80%") |>
        DT::formatStyle("Result", borderLeft = "2px solid #adb5bd") |>
        DT::formatStyle(names(tab), valueColumns = "Difference", target = "row",
          backgroundColor = DT::styleEqual(
            c("Missing", "Dropout", "Inconsistent"),
            c("#fff3cd", "#fff3cd", "#f8d7da")
          ))
    }, server = FALSE)

    # Store the cluster decision without changing PM data
    observeEvent(input$approve, {
      gn = req(selected())
      nm = trimws(input$mergedName)
      if(!nzchar(nm)) {
        showNotification("Merged sample name cannot be empty", type = "error")
        return()
      }

      res = req(analysis())
      d = req(dvi())
      outside = setdiff(names(d$pm), res$groups[[gn]])
      other = setdiff(approved(), gn)
      if(nm %in% c(outside, mergedNames()[other])) {
        showNotification("Merged sample name is already in use", type = "error")
        return()
      }

      nms = mergedNames()
      nms[gn] = nm
      mergedNames(nms)

      methods = mergeMethods()
      methods[gn] = input$mergeMethod
      mergeMethods(methods)
      approved(unique.default(c(approved(), gn)))
      showMain()
    })

    observeEvent(input$reject, {
      gn = req(selected())
      approved(setdiff(approved(), gn))
      showMain()
    })

    # Replace only approved groups, preserving all other singleton objects
    buildReplacement = function() {
      d = req(dvi())
      res = req(analysis())
      appr = approved()
      ids = names(d$pm)
      groups = res$groups
      methods = mergeMethods()
      nms = mergedNames()

      grp = rep(names(groups), lengths(groups))
      names(grp) = unlist(groups, use.names = FALSE)

      newpm = lapply(ids, function(id) {
        gn = unname(grp[id])
        g = groups[[gn]]
        if(length(g) == 1L || !gn %in% appr)
          return(d$pm[[id]])

        first = ids[match(TRUE, ids %in% g)]
        if(id != first)
          return(NULL)

        prof = if(methods[gn] == "combine") {
          res$pmReduced[[gn]]
        }
        else {
          best = g[which.max(res$nonmissing[g])]
          d$pm[[best]]
        }
        if(settings()$ignoreSex)
          prof$SEX = res$pmReduced[[gn]]$SEX

        relabel(prof, new = unname(nms[gn]))
      })

      dvir::setVictims(d, newpm[lengths(newpm) > 0L])
    }

    observeEvent(input$save, {
      req(analysis())
      if(!length(approved())) {
        showNotification("No merge groups have been approved", type = "warning")
        return()
      }

      x = tryCatch(buildReplacement(), error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      })
      if(is.null(x))
        return()
      pending(x)

      showModal(modalDialog(
        "Are you sure you want to replace the current PM data with the merged data?",
        footer = tagList(
          actionButton(ns("confirmBack"), "Cancel"),
          actionButton(ns("confirmSave"), "Save and replace", status = "danger")
        )
      ))
    })

    observeEvent(input$confirmBack, showMain())

    observeEvent(input$confirmSave, {
      saved(req(pending()))
      removeModal()
    })

    reactive(saved())
  })
}

