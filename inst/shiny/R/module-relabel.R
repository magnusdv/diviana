# Relabel families and missing persons --------------------------------------

setupRelabel = function(input, output, session, pedigrees, pedIssues,
                            mainMissing, identifications, currentDviData) {
  defaultRelabelOptions = list(families = "keep", prefix = "F", missing = "keep",
                               format = "[FNAME]-M[MIDX_FAM]")
  pendingRelabel = reactiveVal(NULL)

  observeEvent(input$relabelPeds, {
    req(length(pedigrees()) > 0)
    pendingRelabel(NULL)
    showModal(relabelPedigreesDialog(defaultRelabelOptions))
  })

  relabeled = reactive({
    req(input$relabelFamilies, input$relabelMissing)
    fmt = input$relabelMissing
    if(fmt == "keep") fmt = NULL
    else if(fmt == "custom") fmt = input$relabelFormat
    families = if(input$relabelFamilies == "mp")
      vapply(pedigrees(), function(p) if(length(p$miss)) p$miss[1] else p$famid, "")
    dvir::relabelDVI(req(currentDviData()), families = families,
                     familyPrefix = if(input$relabelFamilies == "prefix") input$relabelPrefix,
                     missingFormat = fmt)
  })

  output$relabelPreview = renderUI({
    tryCatch(relabelPreviewTable(req(currentDviData()), relabeled()),
             error = function(e) span(class = "text-danger", conditionMessage(e)))
  })

  observeEvent(input$applyRelabel, {
    tryCatch({
      pendingRelabel(list(dvi = relabeled(),
                          opt = list(families = input$relabelFamilies, prefix = input$relabelPrefix,
                                     missing = input$relabelMissing, format = input$relabelFormat)))
      showModal(confirmModal("Replace existing family and missing-person labels?",
                             "relabelBack", "confirmRelabel"))
    }, error = showErr)
  })

  observeEvent(input$relabelBack,
               showModal(relabelPedigreesDialog(req(pendingRelabel())$opt)))

  observeEvent(input$confirmRelabel, {
    tryCatch({
      z = req(pendingRelabel())
      dvi = z$dvi
      old = names(pedigrees())
      peds = Map(function(dat, ped, fam) {
        dat$ped = ped
        dat$famid = fam
        dat$miss = intersect(dvi$missing, ped$ID)
        dat
      }, pedigrees(), dvi$am, names(dvi$am))
      names(peds) = names(dvi$am)

      issues = pedIssues()
      names(issues) = names(peds)[match(names(issues), old)]
      pedIssues(issues)
      mainMissing(dvi$missing)
      identifications(dvi$identifications)
      pedigrees(peds)
      pendingRelabel(NULL)
      removeModal()
      isolate(updateSelectInput(session, "example", selected = ""))
    }, error = showErr)
  })

  observeEvent(input$resetall, pendingRelabel(NULL))
}


# Dialog for renaming families and missing persons
relabelPedigreesDialog = function(opt) {
  modalDialog(
    title = "Relabel families and missing persons",
    div(class = "p-2",
      div(class = "d-flex",
        div(style = "flex:1;min-width:0",
          radioButtons("relabelFamilies", "Family names",
                       choices = c("Keep existing" = "keep",
                                   "Name after missing person" = "mp",
                                   "Sequential (1, 2, ...) with prefix" = "prefix"),
                       selected = opt$families),
          conditionalPanel("input.relabelFamilies == 'prefix'",
                           textInput("relabelPrefix", NULL, opt$prefix, width = "120px"))
        ),
        div(style = "flex:0 0 190px;min-width:0",
          radioButtons("relabelMissing", "Missing persons",
                       choiceNames = list(
                         "Keep existing",
                         "M1, M2, ...",
                         "M1-1, M1-2, M2-1, ...",
                         "Name after family",
                         tagList("Custom format ",
                           tags$details(
                             style = "display:inline-block;position:relative;margin-left:2px",
                             tags$summary(icon("question-circle"),
                               style = "list-style:none;cursor:pointer;display:inline"),
                             div(class = "bg-white border rounded shadow-sm p-2",
                                 style = paste0(
                                   "position:absolute;bottom:calc(100% + 4px);left:0;",
                                   "width:230px;z-index:1050;line-height:1.2;white-space:nowrap"),
                                 div("[FIDX]: Family index"),
                                 div("[FNAME]: Family name"),
                                 div("[MIDX]: Global missing index"),
                                 div("[MIDX_FAM]: Within-family index"))
                           )
                         )
                       ),
                       choiceValues = c("keep", "M[MIDX]", "M[FIDX]-[MIDX_FAM]",
                                        "[FNAME]", "custom"),
                       selected = opt$missing),
          conditionalPanel("input.relabelMissing == 'custom'",
                           textInput("relabelFormat", NULL, opt$format, width = "100%"))
        )
      ),
      br(),
      uiOutput("relabelPreview"),
      div(class = "text-muted small", style = "font-style:italic; margin-top:5px",
          "Note: These labels can also be set individually in the pedigree editor.")
    ),
    footer = div(class = "aligned-row",
                 style = "justify-content:flex-end;gap:12px;width:100%",
                 modalButton("Cancel"), actionButton("applyRelabel", "Apply")),
    easyClose = TRUE, size = "m"
  )
}

# Show at most ten labels from the first three families
relabelPreviewTable = function(old, new) {
  sep = "border-left:2px solid #adb5bd"
  rows = lapply(seq_len(min(3L, length(old$am))), function(i) {
    a = intersect(old$missing, old$am[[i]]$ID)
    b = intersect(new$missing, new$am[[i]]$ID)
    if(!length(a)) a = b = ""
    lapply(seq_along(a), function(j)
      tags$tr(tags$td(if(j == 1L) names(old$am)[i]), tags$td(a[j]),
              tags$td(if(j == 1L) names(new$am)[i], style = sep), tags$td(b[j])))
  })
  div(class = "table-responsive",
    tags$table(class = "table table-sm table-striped mb-0",
      tags$thead(
        tags$tr(tags$th(tags$em("Old"), colspan = 2, class = "text-center border-0"),
                tags$th(tags$em("New"), colspan = 2, class = "text-center border-0", style = sep)),
        tags$tr(tags$th("Family"), tags$th("Missing"),
                tags$th("Family", style = sep), tags$th("Missing"))),
      tags$tbody(head(do.call(c, rows), 10L)))
  )
}
