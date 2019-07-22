home <- 
  div(
    div(class = "hero-image",
        div(class = "container",
            h1(
              span(
                "UK Housing Observatory"
              )
            ),
            span(
              "A project developed by the Economics Department at Lancaster
              University Management School aimed at improving understanding
              of the UK national and regional house price dynamics."
              )
            )
        ),
    br(),
    br(),
    fluidRow(
      # column(4, 
             # img(src = "side.png", width = 400, height = 400)),
        column(12,
               div(class = "about",
                   br(),
                   h1("About us", style = "text-align: center;"),
                   br(),
                   p(
                     span(
                       "The UK Housing Observatory is a project of the Economics Department
                        at Lancaster University Management School (LUMS), led by Ivan Paya,
                        Alisa Yusupova, Efthymios Pavlidis and David Peel, aiming to improve
                        our understanding of the UK national and regional housing markets."
                     )
                   ),

                   p(
                     span(
                       "We provide a comprehensive publicly available set of information and
                        tools to perform real-time monitoring of domestic real estate markets
                        both at the national and regional level. We generate a novel House Price
                        Uncertainty Index;original Indicators of House Price Exuberance,
                        and produce", 
                           strong("Forecasts of house price growth for 1,2,3 and 4 quarters
                                  ahead for the UK national and regional markets, updated quarterly.
                                  Please note our disclaimer in regards to the usage of our data.")
                     )
                   ),
                   p(
                     span(
                       "Our research is not confined to the UK real estate market alone.
                        We work in cooperation with the", strong("International House Price Database"),
                       "of the Globalization and Monetary Policy Institute on providing
                        similar exuberance indicators for international markets, which are
                        available at", a("The Federal Reserve Bank of Dallas website.",
                                           href = "https://www.dallasfed.org/institute/houseprice.aspx",
                                           target = "_blank", rel = "noopener noreferrer" ))
                     )
                   )
               )
      ),
    br()
    )
  
  
  
  # div(
#   
#   div(class = "hero-image", 
#       img(src = "hero-image.png", height = "100%", width = "100%")),
#   
#   div(class = "hero-text",
#       h1("UK Housing Observatory"),
#       p(
#         span(
#           "A project developed by the Economics Department at Lancaster 
#           University Management School aimed at improving understanding 
#           of the UK national and regional house price dynamics."
#         )
#        )
#       )
# )

# ,
# 
# hr(),
# # wellPanel(
# fluidRow(
#   column(4, img(src = "side.png", width = 400, height = 400)),
#   column(8,
#          div(class = "about",
#              h1("About us" ),
#              br(),
#              
#              p("The UK Housing Observatory is a project of the Economics Department
#                              at Lancaster University Management School (LUMS), led by Ivan Paya,
#                              Alisa Yusupova, Efthymios Pavlidis and David Peel, aiming to improve
#                              our understanding of the UK national and regional housing markets."),
#              
#              p("We provide a comprehensive publicly available set of information and
#                              tools to perform real-time monitoring of domestic real estate markets
#                              both at the national and regional level. We generate a novel House Price
#                              Uncertainty Index;original Indicators of House Price Exuberance,
#                              and produce", strong("Forecasts of house price growth for 1,2,3 and 4 quarters
#                                                   ahead for the UK national and regional markets, updated quarterly.
#                                                   Please note our disclaimer in regards to the usage of our data.")),
#              
#              p("Our research is not confined to the UK real estate market alone.
#                              We work in cooperation with the", strong("International House Price Database"),
#                "of the Globalization and Monetary Policy Institute on providing
#                              similar exuberance indicators for international markets, which are
#                              available at", a("The Federal Reserve Bank of Dallas website.",
#                                               href = "https://www.dallasfed.org/institute/houseprice.aspx",
#                                               target = "_blank", rel = "noopener noreferrer" ))
#          )
#          
#   )
#   # )
#   
#   
# )