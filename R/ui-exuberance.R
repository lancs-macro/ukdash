ui_exuber <- function() {
  tabItem(
  tabName = "exuberance",
  fluidRow(
    column_4(
      box2(
        title = "Real House Prices",
        subtitle = "Index Levels",
        popover = TRUE,
        popover_title = "Note:",
        popover_content = note_shade,
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
        popover_content = note_exuber,
        width = 12,
        plotOutput("autoplot_price")
      )
    ),
    column_4(
      infoBoxOutput("price_growth_box", width = 12),
      infoBoxOutput("price_exuberance_box", width = 12),
      box(width = 12,
          title = "Periods of Exuberance",
          dataTableOutput("ds_price"))
    )
  ),
  fluidRow(
    column_4(
      box2(
        title = "Affordability Index",
        subtitle = "Index Levels",
        popover = TRUE,
        popover_title = "Note:",
        popover_content = note_shade,
        width = 12,
        plotOutput("plot_afford")
      )
    ),
    column_4(
      box2(
        title = "Affordability Index",
        subtitle = "Exuberance",
        popover = TRUE,
        popover_title = "Note:",
        popover_content = note_exuber,
        width = 12,
        plotOutput("autoplot_afford")
      )
    ),
    column_4(
      infoBoxOutput("afford_growth_box", width = 12),
      infoBoxOutput("afford_exuberance_box", width = 12),
      box(width = 12,
          title = "Periods of Exuberance",
          dataTableOutput("ds_afford"))
    )
  ),
  includeHTML("www/footer.html")
  )

}