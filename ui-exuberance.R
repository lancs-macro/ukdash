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
        popover_content = "There is exuberance when the blue solid line surpasses the red dashed line.",
        width = 12,
        plotOutput("autoplot_price")
      )
    ),
    column_4(
      infoBoxOutput("price_growth_box", width = 12),
      # infoBoxOutput("price_exuber_box"),
      # infoBox(width = 12, title = "House Price Growth", "2.4%", 
              # icon = icon("arrow-up")),
      infoBox(title = "BSADF", width = 12, HTML("2.5 <br> Exuberance"),
              icon = icon("exclamation")),
      box(width = 12, title = "Datestamping", dataTableOutput("table3"))
    )
  ),
    
  fluidRow(
    column_4(
      box2(
        title = "Affordability Index",
        subtitle = "Index Levels (1975 Q1 = 1)",
        width = 12,
        plotOutput("plot_income")
      )
    ),
    column_4(
      box(
        title = "Affordability Index - Exuberance",
        width = 12,
        plotOutput("autoplot_income")
      )
    ),
    column_4(
      infoBox(width = 12, title = "House Price Growth", "2.4%", 
              icon = icon("arrow-up")),
      infoBox(title = "BSADF", width = 12, HTML("2.5 <br> Exuberance"),
              icon = icon("exclamation")),
      box(width = 12, title = "Datestamping", dataTableOutput("table4"))
    )
  ),
)
