tabItem(
  tabName = "overview",
  fluidPage(
    fluidRow(
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
          # background = "red",
          img(src = "https://via.placeholder.com/450X450"),
          title = "Index Levels ",
          width = 4),
        box2(
          # status = "danger",
          img(src = "https://via.placeholder.com/450X450"),
          title = "Growth Rates (percent quarter-on-quarter)",
          width = 4),
        box2(
          img(src = "https://via.placeholder.com/450X450"),
          title = "Peak-to-Trough Contraction Periods",
          width = 4),
      )
    ),
    
    fluidRow(
      box2(
        popover = TRUE, popover_title = "hello", popover_content = "What a nice content",
        title = "UK Real House Prices",
        width = 4),
      box2(
        title = "UK Real House Prices",
        width = 4),
      box2(
        title = "UK Real House Prices",
        width = 4),
    )
  )
)
