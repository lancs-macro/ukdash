ui_indices <- function() {
  tabItem(
  tabName = "indices",
  fluidPage(
    fluidRow(
      box2(
        width = 12,
<<<<<<< HEAD
        height = "60px",
        HTML("
        <p style = 'text-align:center;padding:0px;'>
          <span style = 'font-size:22px;'> Housing Observatory Price Index - HOPI </span>  <br>
        </p>")
=======
        height = "80px",
        background = "red",
        HTML("
      <p style = 'text-align:center;padding:0px;'>
        <span style = 'font-size:26px;'> Housing Observatory Price Index - HOPI </span>  <br>
        <span style='font-size:20px:'> (The index is posted with a three-month lag to ensure sufficient observations.)</span>
      </p>")
>>>>>>> aa54f90fb530330940eaa6996d68b8fc1d8098ec
      )
    ),
    #   box(
    #     width = 12,
    #     height = "60px",
    #     background = "red",
    #     p("Housing Observatory Price Index - HOPI",
    #       style = "font-size:26px;text-align:center;")
    #   )
    # ),
    fluidRow(
      column(
        width = 7,
        box(
          width = 12,
          height = "1040px",
          title = "Level of Regional Disaggregation (NUTS)",
          tabsetPanel(
            tabPanel("Level 1", leaflet::leafletOutput("map", height = "950px")),
            tabPanel("Level 2", leaflet::leafletOutput("map2", height = "950px")),
            tabPanel("Level 3", leaflet::leafletOutput("map3", height = "950px"))
          ))
      ),
      column(
        width = 5,
        box2(width = 12, verbatimTextOutput("widget", placeholder = TRUE)),
        box(
          width = 12,
          title = "House Prices",
          plotOutput("map_price")),
        box(
          width = 12,
          title = "House Price Growth",
          plotOutput("map_price_growth"))
      )
    ),
    includeHTML("www/footer.html")
  )
  )
}