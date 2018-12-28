## app.R ##

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
# library(shinydashboardPlus)
library(dashboardthemes)

# library(shinyWidgets) # https://dreamrs-vic.shinyapps.io/shinyWidgets/

# library(mapview)
library(leaflet)
library(tidyverse)
library(DT)


library(htmltools)
library(htmlwidgets)


# Set Options -------------------------------------------------------------

src <- TRUE
download_primary <<- FALSE
download_secondary <<- FALSE

if (src) {
  suppressMessages(
    list.files(c("html", "scripts"), full.names = TRUE, pattern = ".R") %>% 
      map(source)
  )
}

# Load everything ---------------------------------------------------------

store <- c("plot_price", "autoplot_price", "plot_income", "autoplot_income",
           "price_bsadf_table", "income_bsadf_table", "stat_table")

path_store <- paste0("data/RDS/", store, ".rds")

for (i in seq_along(path_store)) assign(store[i], readRDS(file = path_store[i]))


# Header ------------------------------------------------------------------

mytitle = titlePanel(
  HTML('<p style="font-size:20px;color:white;">
       <b> UK </b>Housing Observatory<span> &nbsp; </span>
       <span style="background-color: rgb(45, 59, 66);
       border-radius: 3px; "> &nbsp;
       <font color="white" size="2">BETA  </font> &nbsp; </span> <body> </p>'),
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo.png")
    )
  )

header <- dashboardHeader(
  
  title = mytitle,
  titleWidth = 335,
  
  # tags$title(
  #   "Housing Observatory"
  # ),
  
  tags$li(
    a(
      href = "http://www.lancaster.ac.uk/lums/our-departments/economics/research/uk-housing-observatory/",
      icon("power-off"),
      title = "Back to Lancaster Home"),
    class = "dropdown"
    )
  # tags$li(,
  #         class = "dropdown")
)


# Sidebar -----------------------------------------------------------------


sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "tabs",
              
              menuItem("Home", tabName = "home", selected = T,
                       icon = icon("home")),
              menuItem("Overview of UK Market", tabName = "overview", 
                       icon = icon("globe",  lib = "glyphicon")),
              menuItem("Financial Stability",  tabName = "exuberance",
                       icon = icon("chart-area")),
              menuItem("Forecasting", tabName = "forecasting", 
                       icon = icon("line-chart")),
              menuItem("Uncertainty", tabName = "uncertainty", 
                       icon = icon("underline")),
              # menuItem("New House Price Indices", tabName = "indices", 
              #          icon = icon("file-alt")),
              menuItem("Download Data", icon = icon("download"), 
                       menuSubItem("Raw Data", tabName = "download_raw",
                                   icon = icon("angle-right")),
                       menuSubItem("Exuberance", tabName = "download_exuberance",
                                   icon = icon("angle-right")),
                       menuSubItem("Forecasting", tabName = "download_forecasting",
                                   icon = icon("angle-right")),
                       menuSubItem("Uncertainty", tabName = "download_uncertainty",
                                   icon = icon("angle-right"))),
              menuItem("Data Sources & Methodology", tabName = "methodology", 
                       icon = icon("chalkboard-teacher")),
              menuItem("Publications", tabName = "pub", 
                       icon = icon("education",  lib = "glyphicon")),
              menuItem("Media Coverage", tabName = "media",
                       icon = icon("newspaper")),
              menuItem("Members", tabName = "meet", 
                       icon = icon("users")),
              menuItem("Disclaimer", tabName = "disclaimer", 
                       icon = icon("exclamation"))
              
              # tags$li(
              #   
              #   p(
              #     span(
              #       "Disclaimer"), 
              #     style = "color:grey;padding-left:20px;padding-top: 200px;"
              #     )
              # )
  )
  # ,
  # 
  # a( href = 'http://www.company.com',
  #           img(src = 'lu-shield.svg',
  #               title = "Company Home", height = "30px"),
  #           "Lancaster University",
  #           style = "padding-top:10px; padding-bottom:10px;")
)

body <- dashboardBody(
  
  useShinyjs(),
  
  includeCSS("img-style.css"),
  
  
  ######## Customization #################
  includeCSS("style.css"),
  tags$head(tags$script(src = "leaflet.js")),
  
  
  # Make theme "html/theme.R"
  theme_boe_website,
  
  tags$style(
    type = 'text/css', 
    '.well {background-color: rgb(255, 255, 255)!important; }'
  ),
  
  # Change the light-blue to gray
  tags$style(
    type = 'text/css', 
    '.bg-light-blue {background-color: rgb(239,240,241)!important;}'
  ),
  
  # Customize the red to red_lanc
  tags$style(
    type = 'text/css', 
    '.bg-red {background-color: rgb(185, 80, 74)!important; }'
  ),
  
  
  
  
  tags$style(
    type = 'text/css',
    '.leaflet-control-layers {display: none;}'
  ),
  tags$style(
    type = 'text/css',
    '.leaflet-container {background-color: rgb(255, 255, 255);}'
  ),
  tags$style(
    type = "text/css",
    'div.dt-buttons {float: right;}'
  ),
  
  
  # Home --------------------------------------------------------------------
  
  tabItems(
    
    tabItem(tabName = "home",
           home,
           avatars
           
           
           ),
    
    
    
    # Oveview -----------------------------------------------------------------
    
    
    tabItem(tabName = "overview",
            
            fluidRow(
              column(4, 
                     h1("Latest Release: 2018 Q4")
                     ),
              column(1,
                     br(),
                     downloadButton("downloadData", "Download")
                     )
            ),
            
            br(),
            
            fluidRow(
              box(width = 6, title = "Real House Prices and Trend",
                  plotOutput("plot_UK")
              ),
              box(width = 6 ,
                  h2("Summary", style = "color:rgb(185, 80, 74);"),
                  br(),
                  p(
                    span(
                      "The growth rate of UK property  prices has been falling over 
                      the  last  two  quarters. The annual growth rate currently stands at 2.2%. This 
                      is the lowest growth rate of national housing prices since 2013 Q2. Furthermore, 
                      property prices in Greater London have fallen by nearly 2% over the last year. 
                      However, house prices have increased in all other regional markets. Overall, the 
                      regions that have experienced the highest property price inflation rates 
                      in the course of the last year are not in the South of the country: East 
                      Midlands(4.4%), West Midlands(4.2%),Wales(4%), and Scotland(3.1%)."
                    )
                    )
                    )
              
                    ),
            fluidRow(
              
              box(width = 6
              ),
              box(width = 6, 
                  h2("House Price Uncertainty (HPU) Index", style = "color:rgb(185, 80, 74);"),
                  br(),
                  p(
                    span(
                      "The UK Housing Observatory has created this new series that will contribute to 
                      the analysis of the housing market and general economic conditions in the UK. 
                      HPU is an index of search results from five large newspapers in the UK. This 
                      index proxies for movements in house price uncertainty. We have now incorporated 
                      this seriesto the set of variables we employ in our forecast modelsbecause it 
                      helps improve their out-of-sample forecasting power."
                    )),
                  p(
                    span(
                      "Furthermore, we note that HPU increased ahead of the EU Referendum and reached 
                      an all-time high right after the referendum took place(2016 Q3). Although the 
                      index eventually dropped,it has remained at a high level ever since. Its current 
                      level (140) is still highfor historical standards, and signals potential 
                      downside risks in the UK housing market and the overall economy."
                    )
                    )
                    )
                    ),
            fluidRow(
              
              box(width = 6,  title = "Exuberance Indicator",
                  plotOutput("autoplot_UK")
              ),
              box(width = 6,
                  h2("Financial Stability", style = "color:rgb(185, 80, 74);"),
                  br(),
                  p(
                    span(
                      "With regards  to  the  exuberance  indicators, the  reported  statistics  show 
                      no signs of exuberance at the national level. None of the regional market 
                      indicators are close to the explosive  threshold  either.  The  risk  of  any 
                      of  those  markets  to  enter  in  an  exuberance  phase  is therefore very low 
                      at the moment (the estimated probability that any of those markets enter a phase 
                      of exuberance is below 10%)."
                    )
                    ),
                  p(
                    span(
                      "The Price-to-Income Ratio continues to be high for
                      historical standards, close to its all-time high in 2007. Despite the decrease 
                      in London house prices, the ratio has not declined substantially due to  the  
                      fall  in  real  income.  This  indicator  will  therefore  continue  to  be  a 
                      sourceof  concern  as pressure on indebted households does not show signs of
                      easing off."
                    ))
                    )
                    ),
            fluidRow(
              
              box(width = 6
              ),
              box(width = 6, 
                  h2("Forecasts", style = "color:rgb(185, 80, 74);"),
                  br(),
                  p(
                    span(
                      "The prediction of the UK Housing Observatory is that the 
                      growth rate ofhouse prices in the national and the majority of regional markets 
                      will continue to dropin the course of 2018and the first half of 2019.We forecast 
                      a growth rate of about 1.7% inthe secondquarter of 2019. The forecasts  predict  
                      a  similar  pattern  of  house  price  behaviourin  all  regions  witha  much  
                      stronger patter in Greater London. According to the forecasting results, the 
                      property prices in this region will experience negative growth, they will 
                      continue to decline during 2018 and the first quarter of 2019,  however  the  
                      growth  in housingprices  is  predicted  to  build  up  towards  the  end of  
                      the followingyear. "
                    )),
                  p(
                    span(
                      "Although the UK house prices are expected to grow at a lower rate than last year, 
                      the two main factors responsible for theslow, butpositive,forecasted growth in 
                      the UK house prices are:the fall in the real mortgage rate and therestricted 
                      supply of new houses. We note that both the number ofhousing starts and housing 
                      completionshas been continually declining throughout the last year."
                    ))
                    )
                    )
                    ),
    
    
    # tabItem(tabName = "national",
    #         fluidRow(
    #           box(title = "Price",
    #               plotOutput("plot_UK"),
    #               width = 6),
    #           box(title = "Bubble",
    #               plotOutput("autoplot_UK"),
    #               width = 6)
    #         )),
    
    
    # Exuberance --------------------------------------------------------------
    
    tabItem(tabName = "exuberance",
            
            fluidRow(
              column(5, h1("Financial Stability")
                     ),
              br(),
              column(7, 
                     selectInput(inputId = "country", choices = slider_names,
                                 label = "Select Geographical Area:")
              )
            ),
            br(),
            tags$ul(
            fluidRow(
              column(12,
                     tags$li(
                       h2("Exuberance Indicators: Real House Prices")
                       ),
                     p(
                       span(
                         "The figures below display the Real House Prices (left) and the 
                         corresponding Exuberance Indication (right) for the selected
                         geographical location. There is exuberance when the statistic
                         (blue line) exceeds the critical value (red line)."
                         )
                       )
                     )
            ),
            fluidRow(
              box(title = "Real House Prices", width = 6,
                  plotOutput("plot_price")),
              box(title = "Exuberance Indicator",
                  plotOutput("autoplot_price"),
                  width = 6)
              ),
                tags$li(
                  h2("Exuberance Indicators: Affordability (Real House Price to Income)")
                ), 
                p(
                  span(
                    "The figures below display the Affordability Index(left) and the 
                    corresponding Exuberance Indication (right) for the selected
                    geographical location. There is exuberance when the statistic
                    (blue line) exceeds the critical value (red line)."
                    )
                  ),
            fluidRow(
              box(title = "Affordability Index", width = 6,
                  plotOutput("plot_income")
              ),
              box(title = "Exuberance Indicator",
                  plotOutput("autoplot_income"),
                  width = 6)
            )
            )
            ),
    
    
    
    # Forecasting -------------------------------------------------------------
    
    tabItem(tabName = "forecasting",
            
            h1("Forecasting")
    ),
    
    # Uncertainty -------------------------------------------------------------
    
    tabItem(tabName = "uncertainty",
            
            h1("Uncertainty")
    ),
    
    
    # New House Price Indices -------------------------------------------------
    
    # tabItem(tabName = "indices",
    #         
    #         h1("New House Price Indices")
    # ),
    
    # Download Date -------------------------------------------------------
    
    
    tabItem(tabName = "download_raw",
            
            
            fluidRow(
              column(4,
                     h1("Download Raw Data")
                     
                     ),
              column(4,
              # div(class = "download_page",
                  # box(width = 4,
              br(),
                      dateRangeInput('daterange',
                                     label = 'Date range input: yyyy-mm-dd',
                                     start = regional_date[1],
                                     end = regional_date[length(regional_date)],
                                     min = regional_date[1],
                                     max = regional_date[length(regional_date)]
                                     # 
                      # )
                  ))
              
            ),
            fluidRow(
              tabBox(width = 12,
                     side = "left",
                     tabPanel(dataTableOutput("price_table"), 
                              title = "Real House Prices"),
                     tabPanel(dataTableOutput("income_table"), 
                              title = "Real House Price to Income")
              )
            )
            
    ),
    
    
    tabItem(tabName = "download_exuberance",
            h1("Download Exuberance Statistics"),
            br(),
            
            fluidRow(
              tabBox(width = 12, 
                     side = "left",
                     tabPanel(dataTableOutput("price_bsadf_table"), 
                              title = "Real House Price Exuberance Statistics"),
                     tabPanel(dataTableOutput("income_bsadf_table"), 
                              title = "Real House Price to Income Exuberance Statistics"),
                     tabPanel(dataTableOutput("stat_table"), 
                              title = "GSADF statistics")
              )
            )
            
    ),
    
    
    
    
    
    # Data Source & Methodology -----------------------------------------------
    
    
    tabItem(tabName = "methodology",
            div(class = "pub",
                h1("Data Sources & Methodology"),
                br(),
                p(
                  span(
                    "National level data is from Federal Reserve Bank of Dallas' 
                    International House Price Database. Each release of the database 
                    includes the following time-series: the house price index (HPI), 
                    the house price index adjusted for inflation (RHPI), the index 
                    of personal disposable income (PDI) and the index of personal 
                    disposable income adjusted for inflation (RPDI). The data is 
                    reported quarterly and goes back to the first quarter of 1975.
                    We construct a measure of housing affordability as the ratio of 
                    RHPI to RPDI. Visit the Federal Reserve Bank of Dallas webpage 
                    for detailed description of the data construction methodology."
                    )
                  ),
                p(
                  span(
                    "Regional House Price indices for 13 regions of the UK are 
                    published quarterly by the Nationwide. The dataset dates back to
                    the fourth quarter of 1973, but we start our analysis from the 
                    first quarter of 1975. To transform the data into real values 
                    each regional series is deflated with consumer price index (CPI)."
                    )
                  ),
                p(
                  span(
                    "Regional Income data is obtained from the Family Expenditure 
                    Survey (FES)*. For each year, starting from 1975, the dataset is 
                    split by region** and the data on average total household's 
                    weekly expenditure is extracted.  The annual data is then 
                    interpolated to obtain quarterly series."
                    )
                  ),
                
                p(
                  span(
                    "*The FES runs from 1961 to 2001. From 2001 it was replaced by a 
                    new survey, the Expenditure and Food Survey (EFS), which became 
                    the Living Costs and Food Survey (LCF) from 2008."
                  )
                  ),
                p(
                  span(
                    "**Due to the differences in regional classifications, 
                    Outer Metropolitan and Outer South East are assumed to 
                    correspond to the South East region in the regional 
                    classification adopted by the FES."
                    )
                  )
                )
    ),
    
    
    # Publications ------------------------------------------------------------
    
    
    tabItem(tabName = "pub",
            # html/literature.R
            literature 
    ),
    
    
    # Media Coverage ----------------------------------------------------------
    
    
    
    tabItem(tabName = "media",
            # html/media_coverage.R
            articles 
    ),
    
    
    # Members -----------------------------------------------------------------
    
    
    tabItem(tabName = "meet",
            # html/avatars.R
            avatars
    ),
    
    
    # Disclaimer --------------------------------------------------------------
    
    tabItem(tabName = "disclaimer",
            div(class = "pub",
                h1("Disclaimer"),
                br(),
                p(
                  span(
                    "The UK Housing Observatory disclaims all warranties, expressed or 
                    implied, with respect to the use of the information provided herein
                    or any results with respect thereto. In addition, the information 
                    contained herein shall in no way be construed to constitute a 
                    recommendation by the UK Housing Observatory with respect to the 
                    purchase or sale of any investment, security or its derivatives."
                    )
                  )
                )
            )
    )
    
    
)



server <- function(session, input, output) {
  
  
  # Overview
  output$downloadData <- downloadHandler(
    filename = "summary2018Q4.pdf",
    content = function(file) {
      file.copy("scripts/101-summary-reports.pdf", file)
    }
  )
  

  
  # Exuberance
  
  output$plot_UK <- renderPlot({plot_price[["UK"]]})
  output$autoplot_UK <- renderPlot({autoplot_price[["UK"]]})
  
  output$plot_price <- renderPlot({plot_price[[input$country]]})
  output$autoplot_price <- renderPlot({autoplot_price[[input$country]]})
  
  output$plot_income <- renderPlot({plot_income[[input$country]]})
  output$autoplot_income <- renderPlot({autoplot_income[[input$country]]})
  
  
  # Download Data -----------------------------------------------------------
  


  # nc <- NCOL(regional_price) - 1
  
  nationwide <- "The House Prices are provided by Nationwide and their use
                        should be cited accordingly https://www.nationwide.co.uk/about/house-price-index/download-data"
  
  output$price_table <-  DT::renderDataTable({make_DT(rhpi, input ,"rhpi", 
                                                      caption_string = nationwide)})
  output$income_table <-  DT::renderDataTable({make_DT(rhp_pdi, input, "rhp_pdi",
                                                       caption_string = nationwide)})
  
  output$price_bsadf_table <-  DT::renderDataTable({make_DT(price_bsadf_table, input ,"bsadf_rhpi")})
  output$income_bsadf_table <-  DT::renderDataTable({make_DT(income_bsadf_table, input, "bsadf_rhp_pdi")})
  output$stat_table <-  DT::renderDataTable({make_DT_general(stat_table, "stat_table")})
  
  
  
}


shinyApp(ui = dashboardPage(header, sidebar, body), server)