ui_uncertainty <- function() {
  tabItem(
  tabName = "uncertainty",
  fluidPage(
    box2(
      width = 12,
      highchartOutput("uncertainty_index", height = 700),
      popover = TRUE,
      popover_title = "Options:",
      popover_content =
        HTML(
          "<ul>
            <li> Click on the legend to enable/disble the visibility of the series </li>
            <li> Select pre specified date ranges on the top-left corner</li>
            <li> Choose custom date range on the top-right corner </li>
            <li> Adjust the bar on the bottom of the graph to select custom date range</li>
          </ul>"
        )

    )
  ),
  includeHTML("www/footer.html")
  )

}