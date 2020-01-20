tabItem(
  tabName = "overview",
  fluidPage(
    
    fluidRow(
        box(
          width = 12,
          height = "45px",
          background = "red",
          p("UK Real House Prices",
            style = "font-size:22px;text-align:center;")
        )
      ),
      fluidRow(
        box2(
          plotOutput("plot_growth_UK_price"),
          title = "Growth Rates (annual)",
          popover = TRUE,
          popover_title = "Note:",
          popover_content = note_bands,
          width = 6),
        box2(
          plotOutput("autoplot_datestamp_price"),
          title = "Peak-to-Trough Contraction Periods",
          width = 6)
      ),
    fluidRow(
      box(
        width = 12,
        height = "45px",
        background = "red",
        p("UK Affordability Index",
          style = "font-size:22px;text-align:center;")
      )
    ),
    fluidRow(
      box2(
        plotOutput("plot_growth_UK_afford"),
        popover = TRUE,
        popover_title = "Note:",
        popover_content = note_bands,
        title = "Growth Rates (annual)",
        width = 6),
      box2(
        plotOutput("autoplot_datestamp_afford"),
        title = "Peak-to-Trough Contraction Periods",
        width = 6)
    )
  ),
  includeHTML("www/footer.html")
)
