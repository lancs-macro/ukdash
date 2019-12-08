
tabItem(
  tabName = "uncertainty",
  fluidPage(
    box(width = 12,
        highchartOutput("uncertainty_index", height = 700)
        
    )
  ),
  includeHTML("content/footer.html")
)