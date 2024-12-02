
cardNavigation = function(id, title, ...) {
  ns = NS(id)
  tagList(
    tags$style("
      .nav-card-title .bttn-jelly.bttn-sm {padding-top: 0;padding-bottom: 0;}
      .nav-card-title .shiny-text-output {font-weight: bold;}
    "),
    div(class = "nav-card-title",
      style = "display: flex; justify-content: space-between; align-items: center",
      title,
      ...,
      div(style = "display: flex; flex-wrap: nowrap;",
        actionBttn(ns("prev"), label = NULL, style = "jelly", size = "s", icon = icon("backward-step")),
        textOutput(ns("counter"), inline = TRUE), # Direct text output for the counter
        actionBttn(ns("nxt"), label = NULL, style = "jelly", size = "s", icon = icon("forward-step"))
      )
    )
  )
}

cardCounter = function(id, mod) {
  moduleServer(id, function(input, output, session) {
    counter = reactiveVal(0) # Initialize the counter

    # Navigation logic
    observeEvent(input$prev, {
      m = mod()
      req(m > 0)
      co = counter()
      counter(if(co == 0) m else (co - 1 - 1) %% m + 1)
    })
    observeEvent(input$nxt, {
      m = mod()
      req(m > 0)
      co = counter()
      counter(if(co == 0) 1 else (co - 1 + 1) %% m + 1)
    })

    # Ensure counter is consistent with mod()
    observeEvent(mod(), {
      if(mod() == 0) counter(0)
      else if(counter() == 0) counter(1)
      else if(counter() > mod()) counter(mod())
    })

    # Render "current/total"
    output$counter = renderText({paste(counter(), mod(), sep = "/")})

    # Return the counter for external use
    counter
  })
}



if(sys.nframe() == 0) {

  library(shiny)
  library(bs4Dash)
  library(shinyWidgets)

  ui = dashboardPage(
    dashboardHeader(),
    bs4DashSidebar(disable = TRUE, minified = FALSE),
    dashboardBody(
      includeCSS("inst/shiny/www/custom.css"),
      fluidRow(
        bs4Card(width = 4, collapsible = FALSE,
          title = cardNavigation("card1", "Card 1 Title", actionButton("mod3", "Mod3")),
          textOutput("foo1"),
          actionButton("reset1", "Reset")
        ),
        bs4Card(width = 4, collapsible = FALSE,
          title = cardNavigation("card2", "Card 2 Title"),
          textOutput("foo2")
        ),
        bs4Card(width = 4, collapsible = FALSE, title = "Empty card"
        )
      )
    )
  )

  server = function(input, output, session) {
    mod1 = reactiveVal(5)
    counter1 = cardCounter("card1", mod = mod1) # Initialize the module
    observeEvent(input$reset1, { counter1(0) })
    output$foo1 = renderText(paste("The number of this card is", counter1()))

    counter2 = cardCounter("card2", mod = counter1) # Initialize the module
    output$foo2 = renderText(paste("The number of this card is", counter2()))

    observeEvent(input$mod3, { mod1(3) })
  }


  shinyApp(ui, server)
}
