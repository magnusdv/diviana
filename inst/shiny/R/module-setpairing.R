# Fix a provided identification by moving a PM profile into the AM data.
setPairingServer = function(id, trigger, dvi, pedigrees, am, pm, identifications) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    errorMsg = reactiveVal(NULL)

    observeEvent(trigger(), {
      x = req(dvi())
      if(!length(x$pm) || !length(x$missing)) {
        showErr("A PM sample and a missing person are needed")
        return()
      }
      choices = setNames(x$missing, paste(getFamily(x, x$missing), x$missing, sep = ": "))
      errorMsg(NULL)
      showModal(modalDialog(
        title = "Manual identification",
        p("Fix this identification and use the PM profile as reference data."),
        selectInput(ns("victim"), "PM sample:", choices = names(x$pm), width = "100%"),
        selectInput(ns("missing"), "Missing person:", choices = choices, width = "100%"),
        textInput(ns("comment"), "Comment (for result tables)", width = "100%"),
        uiOutput(ns("error")),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("save"), "Identify", status = "success"))
      ))
    }, ignoreInit = TRUE)

    output$error = renderUI({
      msg = req(errorMsg())
      div(class = "text-danger", msg)
    })

    observeEvent(input$save, {
      victim = req(input$victim)
      missing = req(input$missing)
      errorMsg(NULL)
      tryCatch({
        am0 = am$main()
        pm0 = pm$main()
        ref = pm0[victim, , drop = FALSE]
        z = dvir::setPairing(req(dvi()), victim = victim, missing = missing,
                             Comment = trimws(input$comment %||% ""),
                             prune = FALSE, verbose = FALSE)
        fam = z$summary$Family
        peds = pedigrees()
        ped = peds[[fam]]
        ped$ped = z$dviReduced$am[[fam]]
        ped$miss = setdiff(ped$miss, missing)
        ped$refs = union(ped$refs, missing)
        peds[[fam]] = ped

        # Retain sample attributes while changing its ID to the missing person.
        ref$Sample = missing
        rownames(ref) = missing
        ref$Sex = c("?", "M", "F")[getSex(ped$ped, missing) + 1L]
        ref$Fam = NULL
        amNew = standardiseGenoData(rbindSafe(am0, ref))
        sources = unique.default(c(am$sources(), pm$sources()))

        am$set(amNew)
        am$sourceValues$all = sources
        identifications(z$dviReduced$identifications)
        pedigrees(peds)
        removeModal()
      }, error = function(e) errorMsg(conditionMessage(e)))
    })

    # Follow subsequent manual edits to reference IDs.
    observeEvent(am$idEdits(), {
      x = identifications()
      ids = am$idEdits()$ids
      if(!NROW(x) || !length(ids)) return()
      j = match(x$Missing, names(ids))
      x$Missing[!is.na(j)] = unname(ids[j[!is.na(j)]])
      identifications(x)
    })

    # The original PM sample remains editable, so follow its ID changes.
    observeEvent(pm$idEdits(), {
      x = identifications()
      ids = pm$idEdits()$ids
      if(!NROW(x) || !length(ids)) return()
      j = match(x$Sample, names(ids))
      x$Sample[!is.na(j)] = unname(ids[j[!is.na(j)]])
      identifications(x)
    })

    # Follow family-name edits; retain historical labels if a family is deleted.
    observeEvent(pedigrees(), {
      x = identifications()
      if(!NROW(x)) return()
      peds = pedigrees()
      refs = lapply(peds, `[[`, "refs")
      fam = setNames(rep(names(peds), lengths(refs)), unlist(refs, use.names = FALSE))
      j = match(x$Missing, names(fam))
      x$Family[!is.na(j)] = unname(fam[j[!is.na(j)]])
      if(!identical(x, identifications())) identifications(x)
    })
  })
}
