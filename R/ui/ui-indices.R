tabItem(
  tabName = "indices",
  fluidPage(
    fluidRow(
      column(
        width = 7, 
        box(
          width = 12, 
          height = "1040px",
          tabsetPanel(
            tabPanel("NUTS 1", leaflet::leafletOutput("map", height = "950px")),
            tabPanel("NUTS 2", leaflet::leafletOutput("map2", height = "950px")),
            tabPanel("NUTS 3", leaflet::leafletOutput("map3", height = "950px"))
          ))
      ),
      column(
        width = 5, 
        box(width = 12, verbatimTextOutput("widget", placeholder = TRUE)),
        box(
          width = 12, 
          title = "House Prices",
          plotOutput("map_price")),
        box(
          width = 12, 
          title = "House Price Growth (Annual)",
          plotOutput("map_price_growth"))
      )
    ),
    includeHTML("www/footer.html")
  )
)