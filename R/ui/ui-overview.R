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
          # status = "danger",
          plotOutput("plot_growth_UK_price"),
          title = "Growth Rates (annual)",
          width = 6),
        box2(
          # background = "red",
          # img(src = "https://via.placeholder.com/450X450"),
          plotOutput("autoplot_datestamp_price"),
          title = "Peak-to-Trough Contraction Periods",
          width = 6)
        # box2(
        #   img(src = "https://via.placeholder.com/450X450"),
        #   title = "Peak-to-Trough Contraction Periods",
        #   width = 4)
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
        popover = TRUE, popover_title = "hello", popover_content = "What a nice content",
        title = "Growth Rates (annual)",
        width = 6),
      box2(
        plotOutput("autoplot_datestamp_afford"),
        # img(src = "https://via.placeholder.com/450X450"),
        title = "Peak-to-Trough Contraction Periods",
        width = 6)
    )
  ),
  includeHTML("www/footer.html")
)
