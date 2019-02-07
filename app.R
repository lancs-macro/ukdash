## app.R ##

library(shiny)
library(shinyjs)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)


# library(shinydashboardPlus)
# library(shinyWidgets) # https://dreamrs-vic.shinyapps.io/shinyWidgets/
# library(mapview)
# library(leaflet)

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
  HTML('<a href="#shiny-tab-home" data-toggle="tab">
       <p style="font-size:20px;color:white;">
       <b> UK </b>Housing Observatory <span> &nbsp; </span>
       <span style="background-color: rgb(45, 59, 66); border-radius: 3px;"> 
       &nbsp;<font color="white" size="2">BETA  </font> &nbsp; </span> </p>
       </a>'),
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo.png")
  )
  )

header <- dashboardHeader(
  
  title = mytitle,
  titleWidth = 335,
  
  # Return to original website
  tags$li(
    a(href = "http://www.lancaster.ac.uk/lums/our-departments/economics/research/uk-housing-observatory/",
      icon("power-off"),
      title = "Back to Lancaster's Website"),
    class = "dropdown"
  )
)


# Sidebar -----------------------------------------------------------------


sidebar <- dashboardSidebar(
  collapsed = TRUE,
  
  sidebarMenu(id = "tabs",
              
              menuItem("Home", tabName = "home",  selected = T,
                       icon = icon("home")),
              menuItem("Overview of UK Market", tabName = "overview", #selected = T,
                       icon = icon("globe",  lib = "glyphicon")),
              menuItem("Financial Stability",  tabName = "exuberance", 
                       icon = icon("chart-area")),
              menuItem(HTML('Forecasting &nbsp; <span class="label label-default">TBA</span>'), 
                       tabName = "forecasting", icon = icon("line-chart")),
              menuItem(HTML('Uncertainty &nbsp; <span class="label label-default">TBA</span>'), 
                       tabName = "uncertainty", icon = icon("underline")
              ),
              # menuItem("New House Price Indices", tabName = "indices", 
              #          icon = icon("file-alt")),
              menuItem("Download Data", icon = icon("download"), 
                       menuSubItem("Raw Data", tabName = "download_raw",
                                   icon = icon("angle-right")),
                       menuSubItem("Exuberance", tabName = "download_exuberance",
                                   icon = icon("angle-right"))),
              # menuSubItem("Forecasting", tabName = "download_forecasting",
              #             icon = icon("angle-right")),
              # menuSubItem("Uncertainty", tabName = "download_uncertainty",
              #             icon = icon("angle-right"))),
              menuItem("Data Sources & Methodology", tabName = "methodology",
                       icon = icon("chalkboard-teacher")),
              # menuItem("Publications", tabName = "pub",
              #          icon = icon("education",  lib = "glyphicon")),
              # menuItem("Media Coverage", tabName = "media",
              #          icon = icon("newspaper")),
              # menuItem("Members", tabName = "meet", 
              #          icon = icon("users")),
              # menuItem("Disclaimer", tabName = "disclaimer", 
              #          icon = icon("exclamation"))
              HTML('<li>
                   <div class="line"></div>
                   </li>  
                   ')
              
              
              )
  )

body <- dashboardBody(
  
  # Make theme "html/theme.R"
  theme_boe_website,
  
  ######## Customization #################
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  
  # Customize the red to red_lanc
  tags$style(
    type = 'text/css', 
    '.bg-red {background-color: rgb(185, 80, 74)!important; }'
  ),
  tags$style(
    type = "text/css",
    'div.dt-buttons {float: right;}'
  ),
  
  # Home --------------------------------------------------------------------
  
  tabItems(
    
    tabItem(tabName = "home",
            includeCSS("style.css"),
            includeCSS("style-tabs.css"),
            includeHTML("home.html")
    ),
    
    # Oveview -----------------------------------------------------------------
    
    tabItem(tabName = "overview",
            
            tags$section(
              id = "overview", 
              class = "grid",
              div(class = "content-wrap",
                  
                  # Title and Download Button
                  
                  fluidRow(
                    column(5, h1("Latest Release: 2018 Q4",
                                 class = "content-title")
                           # h1(
                           #   paste0("Latest Release: ",
                           #          lubridate::year(now()),
                           #          " Q",
                           # 
                             
                    )
                    # ,
                    # column(2,
                    #        br(),
                    #        downloadButton("downloadData", "Download")
                    # )
                  ),
                  
                  # Summary
                  
                  fluidRow(
                    box(width = 6, title = "Real House Prices and Trend",
                        plotOutput("plot_UK")
                    ),
                    box(width = 6 ,
                        h2("Summary"),
                        br(),
                        p(
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
                        ),
                  
                  # House Price Uncertainty
                  
                  fluidRow(
                    
                    box(width = 6
                    ),
                    box(width = 6, 
                        h2("House Price Uncertainty (HPU) Index"),
                        p(
                          "The UK Housing Observatory has created this new series that will contribute to 
                          the analysis of the housing market and general economic conditions in the UK. 
                          HPU is an index of search results from five large newspapers in the UK. This 
                          index proxies for movements in house price uncertainty. We have now incorporated 
                          this series to the set of variables we employ in our forecast models because it 
                          helps improve their out-of-sample forecasting power."
                        ),
                        p(
                          "Furthermore, we note that HPU increased ahead of the EU Referendum and reached 
                          an all-time high right after the referendum took place(2016 Q3). Although the 
                          index eventually dropped, it has remained at a high level ever since. Its current 
                          level (140) is still high for historical standards, and signals potential 
                          downside risks in the UK housing market and the overall economy."
                        )
                        )
                        ),
                  
                  # Exuberance Indicators
                  
                  fluidRow(
                    
                    box(width = 6,  title = "Exuberance Indicator",
                        plotOutput("autoplot_UK")
                    ),
                    box(width = 6,
                        h2("Financial Stability"),
                        p(
                          "With regards  to  the  exuberance  indicators, the  reported  statistics  show 
                          no signs of exuberance at the national level. None of the regional market 
                          indicators are close to the explosive  threshold  either.  The  risk  of  any 
                          of  those  markets  to  enter  in  an  exuberance  phase  is therefore very low 
                          at the moment (the estimated probability that any of those markets enter a phase 
                          of exuberance is below 10%)."
                        ),
                        p(
                          "The Price-to-Income Ratio continues to be high for
                          historical standards, close to its all-time high in 2007. Despite the decrease 
                          in London house prices, the ratio has not declined substantially due to  the  
                          fall  in  real  income.  This  indicator  will  therefore  continue  to  be  a 
                          source of  concern  as pressure on indebted households does not show signs of
                          easing off."
                        )
                        )
                        ),
                  
                  # Forecast
                  
                  fluidRow(
                    box(width = 6
                    ),
                    box(width = 6, 
                        h2("Forecasts"),
                        p("The prediction of the UK Housing Observatory is that the 
                          growth rate ofhouse prices in the national and the majority of regional markets 
                          will continue to drop in the course of 2018 and the first half of 2019. We forecast 
                          a growth rate of about 1.7% in the second quarter of 2019. The forecasts predict  
                          a similar  pattern  of  house  price  behaviour in  all  regions with a much  
                          stronger pattern in Greater London. According to the forecasting results, the 
                          property prices in this region will experience negative growth, they will 
                          continue to decline during 2018 and the first quarter of 2019, however,  the  
                          growth  in housing prices is predicted to build up towards the end of  
                          the following year."),
                        p("Although the UK house prices are expected to grow at a lower rate than last year, 
                          the two main factors responsible for the slow, but positive, forecasted growth in 
                          the UK house prices are the fall in the real mortgage rate and the restricted 
                          supply of new houses. We note that both the number of housing starts and housing 
                          completions has been continually declining throughout the last year.")
                        )
                    )
                  )
              ),
            includeHTML("footer.html")
            ),
    
    
    # Exuberance --------------------------------------------------------------
    
    tabItem(tabName = "exuberance",
            
            tags$section(
              id = "exuberance-title", 
              class = "grid",
              div(class = "content-wrap",
                  h1("Financial Stability", 
                     class = "content-title", 
                     style = "text-align:left;"),
                  fluidRow(
                    column(4, 
                           h3("Exuberance Indicators"),
                           p(
                             "The figures below display the real house prices and the
                             affordability index (left) and the
                             corresponding exuberance indicator (right) for the 
                             selected geographical location. There is exuberance when 
                             the statistic (blue line) exceeds the critical value 
                             (red line)."
                           )
                    ),
                    column(3,
                           div(class = "center",
                               selectInput(
                                 inputId = "country",
                                 choices = slider_names,
                                 selected = slider_names[2],
                                 label = "Select Geographical Area:")
                           )
                           
                    ),
                    column(5,
                           div(class = "regional",
                               h3("Regional Compostion"),
                               textOutput("composition")
                           )
                    )
                    
                  )
              )
             
            ),
            
            tags$section(
              id = "exuberance", 
              class = "grid",
              div(class = "content-wrap",
                  h2("Real House Prices"),
                  fluidRow( 
                    box(title = "Real House Prices", width = 6,
                        plotOutput("plot_price")),
                    box(title = "Exuberance Indicator",
                        plotOutput("autoplot_price"),
                        width = 6)
                  ),
                  h2("Affordability (Real House Price to Income)"),
                  fluidRow(  
                    box(title = "Affordability Index", width = 6,
                        plotOutput("plot_income")),
                    box(title = "Exuberance Indicator",
                        plotOutput("autoplot_income"),
                        width = 6)
                  )
              )
            ),
                  
                  
            includeHTML("footer.html")
            ),
    
    
    # Forecasting -------------------------------------------------------------
    
    # tabItem(tabName = "forecasting",
    #         
    #         tags$section(
    #           id = "forecasting", 
    #           class = "grid",
    #           div(class = "content-wrap",
    #               h1("Forecasting", 
    #                  class = "content-title"
    #               )
    #           )
    #         )
    #         
    #        
    # ),
    
    # Uncertainty -------------------------------------------------------------
    
    # tabItem(tabName = "uncertainty",
    #         
    #         tags$section(
    #           id = "uncertainty", 
    #           class = "grid",
    #           div(class = "content-wrap",
    #               h1("Uncertainty", 
    #                  class = "content-title"
    #               )
    #           )
    #         )
    # ),
    
    # Download Date -------------------------------------------------------
    
    
    tabItem(tabName = "download_raw",
            
            fluidPage(
              column(4,
                     h1("Download Raw Data")
              ),
              column(4,
                     br(),
                     dateRangeInput(
                       'daterange',
                       label = 'Date range input: yyyy-mm-dd',
                       start = regional_date[1],
                       end = regional_date[length(regional_date)],
                       min = regional_date[1],
                       max = regional_date[length(regional_date)]
                     )
              )
            ),
            fluidRow(
              tabBox(width = 12,
                     side = "left",
                     tabPanel(dataTableOutput("price_table"), 
                              title = "Real House Prices"),
                     tabPanel(dataTableOutput("income_table"), 
                              title = "Real House Price to Income")
              )
            ),
            includeHTML("footer.html")
    ),
    
    
    tabItem(tabName = "download_exuberance",
            
            fluidPage(
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
            includeHTML("footer.html")
    ),
    
    
    # Data Source & Methodology -----------------------------------------------
    
    tabItem(tabName = "methodology",
            includeHTML("methodology.html")
    )
    
                    )
  
                    )

server <- function(session, input, output) {
  
  
  # Oveview - Download Report -------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = "summary2018Q4.pdf",
    content = function(file) {
      file.copy("scripts/101-summary-reports.pdf", file)
    }
  )
  
  output$plot_UK <- 
    renderPlot({
      plot_price[["UK"]]})
  output$autoplot_UK <- 
    renderPlot({
      autoplot_price[["UK"]]})
  
  # Exuberance --------------------------------------------------------------
  
  output$composition <- 
    renderText({
      reg_comp[[input$country]]
    })
  output$plot_price <- 
    renderPlot({
      plot_price[[input$country]]})
  output$autoplot_price <- 
    renderPlot({
      autoplot_price[[input$country]]})
  output$plot_income <- 
    renderPlot({
      plot_income[[input$country]]})
  output$autoplot_income <- 
    renderPlot({
      autoplot_income[[input$country]]})
  
  # Download Data -----------------------------------------------------------
  
  nationwide_caption <- 
    "The House Prices are provided by Nationwide and their use should be cited 
    accordingly https://www.nationwide.co.uk"
  
  output$price_table <-  
    DT::renderDataTable({
      make_DT(
        rhpi, input ,"rhpi", 
        caption_string = nationwide_caption)})
  
  output$income_table <-  
    DT::renderDataTable({
      make_DT(
        rhp_pdi, input, "rhp_pdi",
        caption_string = nationwide_caption)})
  
  output$price_bsadf_table <-  
    DT::renderDataTable({
      make_DT(
        price_bsadf_table, input ,"bsadf_rhpi")
    })
  
  output$income_bsadf_table <- 
    DT::renderDataTable({
      make_DT(
        income_bsadf_table, input, "bsadf_rhp_pdi")
    })
  
  output$stat_table <- 
    DT::renderDataTable({
      make_DT_general(
        stat_table, "stat_table")
    })
}

shinyApp(ui = dashboardPage(header, sidebar, body), server)