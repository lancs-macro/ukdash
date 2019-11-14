tabItem(
  tabName = "indices",
  verticalLayout(
    wellPanel(verbatimTextOutput("widget")),
    
    fluidRow(
      column(width = 6, 
             box(width = 12, height = "700px",
                 tabsetPanel(
                   tabPanel("Nuts 1", leaflet::leafletOutput("map", height = "700px")),
                   tabPanel("Nuts 2", leaflet::leafletOutput("map2", height = "700px")),
                   tabPanel("Nuts 3", box(width = 7, height="700px", leaflet::leafletOutput("map3", height = "700px")))
                 ))),
      column(width = 4, offset = 1, 
             box(width = 12, plotOutput("map_price")),
             box(width = 12, plotOutput("map_price_growth"))
      ))
  )
)