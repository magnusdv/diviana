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
                         min = 0, step = 1000)
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
            div(style = "padding-top:15px; text-align:right;",
                actionButton(ns("analyse"), "Analyse", status = "primary"))
          )
        ),
        uiOutput(ns("results")),
        footer = div(style = "display:flex; justify-content:space-between; width:100%;",
          uiOutput(ns("downloadControl")),
          div(style = "display:flex; gap:8px;",
              modalButton("Cancel"),
              actionButton(ns("save"), "Apply merges", status = "success"))
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
                      ignoreSex = s$ignoreSex, markerwise = TRUE, verbose = FALSE),
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

    output$downloadControl = renderUI({
      if(!is.null(analysis()))
        downloadButton(ns("downloadExcel"), "Download Excel")
    })

    output$results = renderUI({
      res = analysis()
      if(is.null(res))
        return(NULL)

      groups = res$groups[lengths(res$groups) > 1L]
      if(!length(groups))
        return(div(class = "mt-3 mb-2 text-muted", "No merge groups identified."))

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
        div(class = "mt-3 mb-2",
            sprintf("%d merge group%s identified.", length(groups),
                    if(length(groups) == 1L) "" else "s")),
        tags$style(".merge-pm-results td {vertical-align:middle;}"),
        tags$table(
          class = "table table-sm table-bordered merge-pm-results",
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
        div(style = "text-align:right; padding-right:5px", actionLink(ns("approveAll"), "Approve all"))
      )
    })

    observeEvent(input$approveAll, {
      res = req(analysis())
      groups = res$groups[lengths(res$groups) > 1L]
      if(anyDuplicated(unname(mergedNames()))) {
        showNotification("Duplicate merged sample names", type = "error")
        return()
      }
      approved(names(groups))
    })

    # Show Inspect with optional unsaved values when returning from an LR matrix
    showInspect = function(nm = NULL, method = NULL) {
      res = req(analysis())
      gn = req(selected())
      g = res$groups[[gn]]
      s = settings()
      best = g[which.max(res$nonmissing[g])]

      showModal(modalDialog(
        title = tagList(
          sprintf("Inspect cluster: %s", paste(g, collapse = ", ")),
          div(class = "text-muted",
              style = "position:absolute; top:10px; right:15px; font-size:60%; line-height:1.1;
                       font-family:monospace; white-space:pre; text-align:left;",
              sprintf("%8s %g\n%8s %g\n%8s %g",
                      "Dropout:", s$dropout, "Dropin:", s$dropin, "Error:", s$typingError))
        ),
        size = "l",
        fluidRow(
          column(6,
            textInput(ns("mergedName"), "Merged sample name", value = nm %||% mergedNames()[gn])
          ),
          column(6,
            radioButtons(ns("mergeMethod"), "Merge method", inline = TRUE,
              choices = setNames(c("combine", "mostcomplete"),
                                 c("Combine profiles", sprintf("Most complete (%s)", best))),
              selected = method %||% mergeMethods()[gn])
          )
        ),
        DT::DTOutput(ns("inspectTable")),
        footer = tagList(
          actionButton(ns("reject"), "Don't merge"),
          actionButton(ns("approve"), "Approve merge", status = "success")
        )
      ))
    }

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

      selected(names(groups)[hit[1]])
      showInspect()
    })

    # Display overall or marker-wise LR matrix
    showLRmatrix = function(m, title, back) {
      g = rownames(m)
      fm = matrix(formatC(m, format = "g", digits = 4), nrow = length(g), dimnames = dimnames(m))
      diag(fm) = "–"

      dlg = modalDialog(
        title = title,
        div(style = "overflow-x:auto",
          tags$table(
            class = "table table-sm table-bordered w-auto mx-auto mb-0 text-center",
            style = "white-space:nowrap",
            tags$thead(tags$tr(tags$th(""), lapply(g, tags$th))),
            tags$tbody(lapply(seq_along(g), \(r)
              tags$tr(tags$th(g[r]), lapply(fm[r, ], tags$td))))
          )
        ),
        footer = actionButton(ns(back), "Back"),
        easyClose = TRUE
      )
      dlg$children[[1]]$attribs$style = "width:max-content;max-width:calc(100vw - 32px)"
      showModal(dlg)
    }

    markerState = reactiveVal(NULL)

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
      showLRmatrix(res$LRmat[g, g, drop = FALSE], "Pairwise LR matrix", "lrBack")
    })

    observeEvent(input$lrBack, showMain())

    # Click the LR column in Inspect to view one marker's pairwise LRs
    observeEvent(input$inspectTable_cell_clicked, {
      z = input$inspectTable_cell_clicked
      res = req(analysis())
      gn = req(selected())
      g = res$groups[[gn]]
      req(isTRUE(z$col == length(g) + 3L), identical(z$value, "Matrix"),
          !is.null(z$row))
      marker = dimnames(res$LRmarker)[[3]][z$row]
      req(!is.na(marker))

      markerState(list(name = input$mergedName, method = input$mergeMethod))
      showLRmatrix(res$LRmarker[g, g, marker], paste("LR matrix:", marker), "markerBack")
    })

    observeEvent(input$markerBack, {
      st = req(markerState())
      showInspect(st$name, st$method)
    })

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

      # Display the common LR for typed pairs, otherwise offer the full matrix
      lr = vapply(seq_len(ncol(orig)), function(j) {
        typed = g[orig[, j] != "-/-"]
        if(length(typed) < 2L)
          return("–")
        m = res$LRmarker[typed, typed, colnames(orig)[j]]
        v = m[lower.tri(m)]
        if(length(unique.default(signif(v, 8))) == 1L)
          formatC(v[1], format = "g", digits = 4)
        else
          "Matrix"
      }, character(1))
      tab = data.frame(Marker = colnames(orig), t.default(orig), Result = unname(result),
                       Difference = diffs, LR = lr, check.names = FALSE, row.names = NULL)
      diffsCol = ncol(tab) - 2L

      dt = DT::datatable(tab,
        rownames = FALSE,
        container = tags$table(tags$thead(tags$tr(
          lapply(names(tab)[-ncol(tab)], tags$th),
          wrap_tooltip(tags$th("LR"), "mergePmLR")
        ))),
        selection = "none",
        class = "stripe hover compact nowrap",
        options = list(
          dom = "t", paging = FALSE, scrollX = TRUE,
          scrollY = if(nrow(tab) > 15L) "420px" else NULL,
          scrollCollapse = TRUE, order = list(),
          columnDefs = list(
            list(orderable = FALSE, targets = 0:(diffsCol - 1L)),
            list(orderable = TRUE, targets = diffsCol),
            list(orderable = FALSE, targets = diffsCol + 1L)
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
      DT::formatStyle(dt, "LR", color = DT::styleEqual("Matrix", "steelblue"),
                      cursor = DT::styleEqual("Matrix", "pointer"),
                      textDecoration = DT::styleEqual("Matrix", "underline"))
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

    # Download groups and pairwise LRs, including marker-wise contributions
    output$downloadExcel = downloadHandler(
      filename = function() sprintf("mergePM-%s.xlsx", Sys.Date()),
      content = function(file) {
        res = req(analysis())
        s = settings()
        groups = res$groups[lengths(res$groups) > 1L]
        gn = names(groups)
        ids = names(req(dvi())$pm)

        # First sheet: parameters
        params = data.frame(
          Parameter = c("LR threshold", "Dropout", "Dropin", "Typing error", "Ignore sex"),
          Value = vapply(s[c("threshold", "dropout", "dropin", "typingError", "ignoreSex")],
                         as.character, character(1), USE.NAMES = FALSE))

        # One row for each unordered pair within a merge group
        pairs = if(length(groups)) do.call(rbind, lapply(gn, function(x) {
          g = ids[ids %in% groups[[x]]]
          p = t.default(combn(g, 2L))
          data.frame(Group = x, `Sample 1` = p[, 1], `Sample 2` = p[, 2],
                     check.names = FALSE)
        })) else data.frame(Group = character(), `Sample 1` = character(),
                           `Sample 2` = character(), check.names = FALSE)
        pairTab = pairs
        pairTab$LR = res$LRmat[as.matrix(pairs[, 2:3])]
        pairTab$Group = unname(mergedNames()[pairTab$Group])

        # Expand each pair over all markers, keeping LRs numeric in Excel
        markers = dimnames(res$LRmarker)[[3]]
        markerTab = pairs[rep(seq_len(nrow(pairs)), each = length(markers)), , drop = FALSE]
        markerTab$Marker = rep(markers, nrow(pairs))
        geno = getGenotypes(req(dvi())$pm)
        markerTab$Geno1 = geno[cbind(markerTab[[2]], markerTab$Marker)]
        markerTab$Geno2 = geno[cbind(markerTab[[3]], markerTab$Marker)]
        markerTab$LR = res$LRmarker[cbind(markerTab[[2]], markerTab[[3]], markerTab$Marker)]
        names(markerTab)[2:3] = c("Sample1", "Sample2")

        # One sheet per group
        tabs = split(markerTab[-1], markerTab$Group)
        sheetNames = make.unique(substr(chartr("\\/:*?[]", "_______", unname(mergedNames()[gn])), 1, 28))

        sheets = c(list(Parameters = params, `LR totals` = pairTab),
                   setNames(tabs[gn], sheetNames))

        openxlsx::write.xlsx(sheets, file, colNames = TRUE, colWidths = "auto",
                             headerStyle = openxlsx::createStyle(textDecoration = "bold"))
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    # Replace only approved groups, preserving all other singleton objects
    buildReplacement = function() {
      d = req(dvi())
      res = req(analysis())
      appr = approved()
      ids = names(d$pm)
      groups = res$groups
      methods = mergeMethods()
      nms = mergedNames()
      ignsex = settings()$ignoreSex

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

        # Merge method
        meth = methods[gn]
        if(meth == "combine")
          prof = res$pmReduced[[gn]]
        else {
          best = g[which.max(res$nonmissing[g])]
          prof = d$pm[[best]]
          if(ignsex)
            prof = setSex(prof, prof$ID, sex = res$pmReduced[[gn]]$SEX)
        }

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

      confirm = modalDialog(
        "Replace the current PM data with the approved merges?",
        footer = div(style = "display:flex; justify-content:center; gap:8px; width:100%;",
          actionButton(ns("confirmBack"), "No, go back"),
          actionButton(ns("confirmSave"), "Yes, replace", status = "danger"))
      )
      # Center the confirmation dialog
      confirm$children[[1]]$attribs$class = paste(
        confirm$children[[1]]$attribs$class, "modal-dialog-centered")
      confirm$children[[1]]$attribs$style = "max-width:420px; top:-15vh;"

      showModal(confirm)
    })

    observeEvent(input$confirmBack, showMain())

    observeEvent(input$confirmSave, {
      saved(req(pending()))
      removeModal()
    })

    reactive(saved())
  })
}

