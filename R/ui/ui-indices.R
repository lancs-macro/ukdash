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
            tabPanel("Nuts 1", leaflet::leafletOutput("map", height = "1000px")),
            tabPanel("Nuts 2", leaflet::leafletOutput("map2", height = "1000px")),
            tabPanel("Nuts 3", leaflet::leafletOutput("map3", height = "1000px"))
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
          title = "House Price Growth (QoQ)",
          plotOutput("map_price_growth"))
      )
    )
  )
)

# pre.shiny-text-output {
#   text-align: center;
#   border-style: none;
#   font-size: 16px;
#   font-weight: 700;
# # }