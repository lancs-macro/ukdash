ui_overview <- function() {
  tabItem(
  tabName = "overview",
  fluidPage(

    fluidRow(
        box2(
          width = 12,
          height = "75px",
          HTML("
        <p style = 'text-align:center;padding:0px;'>
          <span style = 'font-size:22px;'> UK Real House Prices </span>  <br>
        </p>")
        )
      ),
      fluidRow(
        box2(
          plotOutput("plot_growth_UK_price"),
          title = "Growth Rates",
          popover = TRUE,
          popover_title = "Note:",
          popover_content = note_bands,
          width = 6),
        box2(
          plotOutput("autoplot_datestamp_price"),
          title = "Periods of Exuberance",
          popover = TRUE,
          popover_title = "Note:",
          popover_content = note_ds,
          width = 6)
      ),
    fluidRow(
      box2(
        width = 12,
        height = "75px",
      HTML("
      <p style = 'text-align:center;padding:0px;'>
        <span style = 'font-size:22px;'> UK Affordability Index </span>  <br>
        <span style='font-size:18px:'> (The index is constructed as the ratio of real house prices over real personal disposable income)</span>
      </p>")
      )
    ),
    fluidRow(
      box2(
        plotOutput("plot_growth_UK_pti"),
        popover = TRUE,
        popover_title = "Note:",
        popover_content = note_bands,
        title = "Growth Rates (annual)",
        width = 6),
      box2(
        plotOutput("autoplot_datestamp_pti"),
        title = "Periods of Exuberance",
        popover = TRUE,
        popover_title = "Note:",
        popover_content = note_ds,
        width = 6)
    )
  ),
  includeHTML("www/footer.html")
  )

}