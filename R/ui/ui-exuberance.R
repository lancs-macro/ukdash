tabItem(
  tabName = "exuberance",
  fluidRow(
    column_4(
      box2(
        title = "Real House Prices",
        subtitle = "Index Levels",
        width = 12,
        plotOutput("plot_price")
      )
    ),
    column_4(
      box2(
        title = "Real House Prices",
        subtitle = "Exuberance",
        popover = TRUE,
        popover_title = "Note:",
        popover_content = exuber_note,
        width = 12,
        plotOutput("autoplot_price")
      )
    ),
    column_4(
      infoBoxOutput("price_growth_box", width = 12),
      infoBoxOutput("price_exuberance_box", width = 12),
      box(width = 12, title = "Datestamping", dataTableOutput("ds_price"))
    )
  ),
  fluidRow(
    column_4(
      box2(
        title = "Affordability Index",
        subtitle = "Index Levels (1975 Q1 = 1)",
        width = 12,
        plotOutput("plot_afford")
      )
    ),
    column_4(
      box(
        title = "Affordability Index - Exuberance",
        width = 12,
        plotOutput("autoplot_afford")
      )
    ),
    column_4(
      infoBoxOutput("afford_growth_box", width = 12),
      infoBoxOutput("afford_exuberance_box", width = 12),
      box(width = 12, title = "Datestamping", dataTableOutput("ds_afford"))
    )
  ),
  includeHTML("www/footer.html")
)
