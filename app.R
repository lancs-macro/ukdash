## app.R ##

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
# library(shinythemes)
library(tidyverse)
library(DT)
library(highcharter)

# Set Options -------------------------------------------------------------

opt_src <- "main"
download_primary <<- FALSE
opt_load_rds <- TRUE

# Load everything ---------------------------------------------------------

if (opt_load_rds) {
  path_store_rds <- list.files("data/RDS", full.names = TRUE)
  store_rds <-  stringr::str_remove(list.files("data/RDS"), ".rds")
  for (i in seq_along(path_store_rds)) {
    assign(store_rds[i], readRDS(file = path_store_rds[i]))
  }
}

# source ------------------------------------------------------------------

if (opt_src == "main") {
  # suppressMessages(
  list.files("R", full.names = TRUE, pattern = "-src.R") %>% 
    purrr::map(source)
  # )
}else {
  # suppressMessages(
  list.files(c("R"), full.names = TRUE, pattern = ".R") %>% 
    purrr::map(source)
  # )
}


# Header ------------------------------------------------------------------

header <- dashboardHeaderPlus(
  titleWidth = 380,
  title = shiny::tagList(
    span(class = "logo-lg", 
         span(shiny::img(src = "logo.png",  height = "32", width = "32"),
              "International Housing Observatory")), 
    shiny::img(src = "logo.png",  height = "32", width = "32")
  ),
  tags$li(
    a(href = 'https://github.com/lancs-macro/uk-housing-observatory',
      target = "_`blank",
      HTML('<i title="Browse our github repositoty" class="fab fa-github"></i>'),
      style = "font-size:28px; padding-top:10px; padding-bottom:10px;"),
    class = "dropdown"),
  
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
  
  sidebarMenu(
    id = "tabs", 
    # menuItem("Home", tabName = "home",  selected = T,
    #          icon = icon("home")),
    # menuItem("Overview of UK Market", tabName = "overview", #selected = T,
    #          icon = icon("globe",  lib = "glyphicon")),
    # menuItem("House Prices",  tabName = "hprices", 
    #          icon = icon("chart-area")),
    menuItem("Financial Stability",
             # selectInput(
             #   inputId = "country",
             #   choices = slider_names,
             #   selected = slider_names[2],
             #   label = "Select Geographical Area:"),
             tabName = "exuberance", 
             icon = icon("chart-area")),
    # menuItem(HTML('Forecasting &nbsp; <span class="label label-default">TBA</span>'), 
    #          tabName = "forecasting", icon = icon("line-chart")),
    menuItem(HTML('Uncertainty'), tabName = "uncertainty", icon = icon("underline")),
    menuItem("New House Price Indices", tabName = "indices", icon = icon("tv"), selected = TRUE), #house-damage
    menuItem("Download Data", icon = icon("download"), tabName = "download"),
    # menuSubItem("Forecasting", tabName = "download_forecasting",
    #             icon = icon("angle-right")),
    # menuSubItem("Uncertainty", tabName = "download_uncertainty",
    #             icon = icon("angle-right"))),
    # menuItem("Data Sources & Methodology", tabName = "methodology",
    #          icon = icon("chalkboard-teacher")),
    # menuItem("Publications", tabName = "pub",
    #          icon = icon("education",  lib = "glyphicon")),
    # menuItem("Media Coverage", tabName = "media",
    #          icon = icon("newspaper")),
    # menuItem("Members", tabName = "meet", 
    #          icon = icon("users")),
    # menuItem("Disclaimer", tabName = "disclaimer", 
    #          icon = icon("exclamation"))
    HTML('<li> <div class="line"></div></li> '),
    HTML('<li style = "position:absolute; padding-right:1rem; bottom:0; color:grey; font-size:12px;"> 
         <p> @ UKHO </p> </li>')
    
        
              
              )
  )

body <- dashboardBody(
  
  theme_boe_website,
  
  ######## Customization #################
  
  tags$head(
    
    tags$title("UK Housing Observatory"),
    tags$link(rel = "shortcut icon", href = "logo.png"),
    tags$link(rel = "stylesheet", type = "text/css", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
    # includeHTML("content/google-analytics.html")
    
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
  tags$style(
    type = "text/css",
    '.leaflet-container { background-color:rgba(255,0,0,0.0);}'
  ),
  
  # Home --------------------------------------------------------------------
  
  
    
    # tabItem(tabName = "home",
    #         includeCSS("content/style.css"),
    #         includeCSS("content/style-tabs.css"),
    #         includeHTML("content/home.html")
    # ),
    
    # Oveview -----------------------------------------------------------------
    
    # tabItem(
    #   tabName = "overview",
    #   
    #   tags$section(
    #     id = "overview", 
    #     class = "grid",
    #     div(
    #       class = "content-wrap",
    #       
    #       # Title and Download Button
    #       
    #       fluidRow(
    #         column(
    #           width = 5, 
    #           h1("Latest Release: 2018 Q4",
    #              class = "content-title")
    #           
    #         )
    #         # ,
    #         # column(2,
    #         #        br(),
    #         #        downloadButton("downloadData", "Download")
    #         # )
    #       ),
    #       
    #       # Summary
    #       
    #       fluidRow(
    #         box(
    #           width = 6, 
    #           title = "Real House Prices and Trend",
    #           plotOutput("plot_UK")
    #         ),
    #         box(
    #           width = 6 ,
    #           h2("Summary"),
    #           br(),
    #           p("The growth rate of UK property  prices has been falling over 
    #           the  last  two  quarters. The annual growth rate currently stands at 2.2%. This 
    #           is the lowest growth rate of national housing prices since 2013 Q2. Furthermore, 
    #           property prices in Greater London have fallen by nearly 2% over the last year. 
    #           However, house prices have increased in all other regional markets. Overall, the 
    #           regions that have experienced the highest property price inflation rates 
    #           in the course of the last year are not in the South of the country: East 
    #           Midlands(4.4%), West Midlands(4.2%),Wales(4%), and Scotland(3.1%).")
    #         )
    #       ),
    #       
    #       
    #       # House Price Uncertainty
    #       
    #       fluidRow(
    #         
    #         box(
    #           width = 6,
    #           title = "House Price Uncertainty Index",
    #           plotOutput("plot_index_hpu")
    #         ),
    #         box(
    #           width = 6, 
    #           
    #           h2("House Price Uncertainty (HPU) Index"),
    # 
    #           p("The UK Housing Observatory has created this new series that will 
    #             contribute to the analysis of the housing market and general economic
    #             conditions in the UK. HPU is an index of search results from five 
    #             large newspapers in the UK. This index proxies for movements in house
    #             price uncertainty. We have now incorporated this series to the set of 
    #             variables we employ in our forecast models because it helps improve 
    #             their out-of-sample forecasting power."),
    #           p("Furthermore, we note that HPU increased ahead of the EU Referendum 
    #               and reached an all-time high right after the referendum took place(2016 Q3).
    #               Although the index eventually dropped, it has remained at a high level 
    #               ever since. Its current level (140) is still high for historical standards,
    #               and signals potential downside risks in the UK housing market and the overall 
    #               economy.")
    #         )
    #       ),
    #       
    #       # Exuberance Indicators
    #       
    #       fluidRow(
    #         
    #         box(width = 6,  title = "Exuberance Indicator",
    #             plotOutput("autoplot_UK")
    #         ),
    #         box(width = 6,
    #             h2("Financial Stability"),
    #             p(
    #               "With regards  to  the  exuberance  indicators, the  reported  statistics  show 
    #                       no signs of exuberance at the national level. None of the regional market 
    #                       indicators are close to the explosive  threshold  either.  The  risk  of  any 
    #                       of  those  markets  to  enter  in  an  exuberance  phase  is therefore very low 
    #                       at the moment (the estimated probability that any of those markets enter a phase 
    #                       of exuberance is below 10%)."
    #             ),
    #             p(
    #               "The Price-to-Income Ratio continues to be high for
    #                       historical standards, close to its all-time high in 2007. Despite the decrease 
    #                       in London house prices, the ratio has not declined substantially due to  the  
    #                       fall  in  real  income.  This  indicator  will  therefore  continue  to  be  a 
    #                       source of  concern  as pressure on indebted households does not show signs of
    #                       easing off."
    #             )
    #         )
    #       ),
    #       
    #       # Forecast
    #       
    #       fluidRow(
    #         box(width = 6
    #         ),
    #         box(width = 6, 
    #             h2("Forecasts"),
    #             p("The prediction of the UK Housing Observatory is that the 
    #                       growth rate ofhouse prices in the national and the majority of regional markets 
    #                       will continue to drop in the course of 2018 and the first half of 2019. We forecast 
    #                       a growth rate of about 1.7% in the second quarter of 2019. The forecasts predict  
    #                       a similar  pattern  of  house  price  behaviour in  all  regions with a much  
    #                       stronger pattern in Greater London. According to the forecasting results, the 
    #                       property prices in this region will experience negative growth, they will 
    #                       continue to decline during 2018 and the first quarter of 2019, however,  the  
    #                       growth  in housing prices is predicted to build up towards the end of  
    #                       the following year."),
    #             p("Although the UK house prices are expected to grow at a lower rate than last year, 
    #                       the two main factors responsible for the slow, but positive, forecasted growth in 
    #                       the UK house prices are the fall in the real mortgage rate and the restricted 
    #                       supply of new houses. We note that both the number of housing starts and housing 
    #                       completions has been continually declining throughout the last year.")
    #         )
    #       )
    #     )
    #   ),
    #   includeHTML("content/footer.html")
    # ),
    
    



tabItems(
  source("exuberance-ui.R", local = TRUE)$value,
  source("indices-ui.R", local = TRUE)$value,
    
    # fluidPage(
    #   style = "padding:0 5em;",
    #   
    #   h2("Financial Stability", 
    #      style = "padding: 1em 0 0 1em;"
    #   ),
    #   
    #   fluidRow(
    #     style = "text-align:left;padding:2em;",
    #     
    #     column(
    #       width = 4, 
    #       
    #       h3("Exuberance Indicators"),
    #       
    #       p("The figures below display the real house prices and the affordability 
    #         index (left) and the corresponding exuberance indicator (right) for the 
    #         selected geographical location. There is exuberance when the statistic 
    #         (blue line) exceeds the critical value (red line).")
    #     ),
    #     
    #     column(
    #       width = 3,
    #       
    #       div(
    #         class = "center",
    #         selectInput(
    #           inputId = "country",
    #           choices = slider_names,
    #           selected = slider_names[2],
    #           label = "Select Geographical Area:")
    #       )
    #     ),
    #     column(
    #       width = 5,
    #       div(
    #         class = "regional",
    #         h3("Regional Compostion"),
    #         textOutput("composition")
    #       )
    #     )
    #   ),
    #   
    #   fluidRow(
    #     box(title = "Real House Prices", width = 6,
    #         plotOutput("plot_price")),
    #     box(title = "Affordability Index",
    #         plotOutput("plot_income"),
    #         width = 6)
    #   ),
    #   
    #   fluidRow(
    #     box(
    #       width = 12,
    #       background = "red",
    #       "Exuberance Statistics",
    #       style = "font-size:22px;text-align:center;")
    #   ),
    #   
    #   fluidRow(
    #     box(
    #       title = "Real House Prices",
    #       tableOutput("table1")
    #     ),
    #     box(
    #       title = "House-Price-to-Income Ratio", 
    #       tableOutput("table2")
    #     )
    #   ),
    #   
    #   fluidRow(
    #     box(
    #       width = 12,
    #       background = "red",
    #       "Date-Stamping Periods of Exuberance",
    #       style = "font-size:22px;text-align:center;")
    #   ),
    #   
    #   fluidRow(
    #     
    #     box(title = "Real House Prices", width = 6,
    #         plotOutput("autoplot_price")),
    #     
    #     box(title = "Affordability Index",
    #         plotOutput("autoplot_income"),
    #         width = 6)
    #   ),
    #   
    #   fluidRow(
    #     box(
    #       width = 12,
    #       background = "red",
    #       "Date-Stamping Periods of Exuberance Table",
    #       style = "font-size:22px;text-align:center;"
    #     )
    #   ),
    #   fluidRow(
    #     box(
    #       title = "Real House Prices",
    #       dataTableOutput("table3")
    #     ),
    #     box(
    #       title = "Affordability Index", 
    #       dataTableOutput("table4")
    #     )
    #   )
    # ),
    # includeHTML("content/footer.html")

# House Prices ------------------------------------------------------------

  # tabItem(
  #   tabName = "hprices",
  #   
  #   fluidRow(
  #     box(
  #       title = "Regional House Prices",
  #       width = 8),
  #     box(
  #       title = "Mpla Mpla",
  #       width = 4
  #     )
  #   )
  #   
  # ),
  
# Exuberance --------------------------------------------------------------



    # Forecasting -------------------------------------------------------------
    
    # tabItem(
    #   tabName = "forecasting",
    #   
    #   fluidPage(
    #     style = "padding:0 5em;",
    #     
    #     h2(
    #       "Forecasting", 
    #       style = "padding: 1em 0 0 1em;"
    #     ),
    #     
    #     fluidRow(
    #       style = "text-align:left;padding:2em;",
    #       
    #       h3("Methodology"),
    #       
    #       column(
    #         width = 8,
    #         p(
    #           "The table reports actual and predicted annual log house price 
    #           growth rates. Please note that actual growth  rates are revised each  
    #           quarter  and,  therefore,  may  differ  from  one  release  to  the  
    #           other. The forecasts  are shown in blue. Predictionsare for 1,2,3 and 4  
    #           quarters  ahead. The reported predictionsare computed  as  the  average 
    #           of  forecasts  generated  bya  batteryof  forecastingmodels, 
    #           including  Dynamic Model  Averaging  (DMA)  and  Dynamic  Model  
    #           Selection  (DMS),  Time-Varying  Parameters  (TVP) model, Bayesian VAR 
    #           (BVAR) and the mean combination of individual Autoregressive Distributed L
    #           ag Model  (ARDL)  forecasts.  For  further  details  about  the  methodology  
    #           see Yusupova A.,2016. 'An Econometric Analysis of U.K. Regional Real Estate Markets'."
    #         )
    #         
    #       ),
    #       
    #       column(
    #         width = 3,
    #         offset = 1,
    #         
    #         div(
    #           class = "center",
    #           selectInput(
    #             inputId = "country",
    #             choices = slider_names,
    #             selected = slider_names[2],
    #             label = "Select Geographical Area:")
    #         )
    #       )
    #     ),
    #     
    #     fluidRow(
    #       column(
    #         width = 6, 
    #         box(width = 12,
    #             title = "Average",
    #             DT::dataTableOutput("forecasts")
    #         )
    #       ),
    #       
    #       column(
    #         width = 6, 
    #         tabBox(width = 12,
    #             title = "Model Forecast Overview",
    #             tabPanel("BVAR", 
    #                      DT::dataTableOutput("forecasts_models")),
    #             tabPanel("DMS(0.95)"),
    #             tabPanel("DMS(0.99)"),
    #             tabPanel("DMA(0.95)")
    #         )
    #       )
    #     ),
    #     
    #     
    #     h2(
    #       "Predictors", 
    #       style = "padding: 1em 0 1.5em 1em;"
    #     ),
    #     
    #     fluidRow(
    #       box(
    #         width  = 12,
    #         # title = "Predictors",
    #         plotOutput("plot_predictors", height = 900)
    #       )
    #     )
    #   ),
    #   includeHTML("content/footer.html")
    # ),
    
    # Uncertainty -------------------------------------------------------------
    
    tabItem(
      tabName = "uncertainty",
      fluidRow(
        style = "text-align:left;padding:2em;",
        
        h3("Uncertainty Title"),
        
        column(
          width = 12,
          p(
            "TheHouse  Price  Uncertainty (HPU) Index is constructedusing  the
              methodology  suggested  byBaker, Bloom and Davis (2016) to proxy
              for economic policy uncertainty. The HPUis an index of search results
              from five large newspapers in the UK: The Guardian, The Independent,
              The Times, Financial Times and Daily  Mail.  In  particular,  we  use
              LexisNexis  digital  archives  of  these  newspapers  to  obtain  a
              quarterly count  of articles  that contain the following three terms:
              ‘uncertainty’ or ‘uncertain’; ‘housing’ or ‘house prices’ or ‘real estate’;
              and one of the following: ‘policy’, ‘regulation’, ‘Bank of England, ‘mortgage’,
              ‘interest  rate’,  ‘stamp-duty’,  ‘tax’,  ‘bubble’  or  ‘buy-to-let’ (including
              variants  like  ‘uncertainties’, ‘housing market’ or ‘regulatory’). To meet the
              search criteria an article must contain terms in all three categories.")
        )
      ),
      box(width = 12,
          title = "Quartely Housing Policy Uncertainty Index",
          highchartOutput("uncertainty_index", height = 700)
          
      )
    )
    
    # Download Data -------------------------------------------------------
    
    
    # tabItem(
    #   tabName = "download",
    #   
    #   fluidPage(
    #     style = "padding: 0 5em;",
    #     
    #     h2("Download", 
    #        style = "padding:1em 0 0 20px;"),
    #     h3("1) Raw Data", 
    #        style = "padding:0 0 0 20px;"),
    #     br(),
    #     fluidRow(
    #       tabBox(
    #         width = 12,
    #         side = "left",
    #         tabPanel(dataTableOutput("price_table"), 
    #                  title = "Real House Prices"),
    #         tabPanel(dataTableOutput("income_table"), 
    #                  title = "Real House Price to Income")
    #       )
    #     ),
    #     h3("2) Exuberance Statistics", 
    #        style = "padding:0 0 0 20px;"),
    #     br(),
    #     fluidRow(
    #       tabBox(
    #         width = 12, 
    #         side = "left",
    #         tabPanel(dataTableOutput("price_bsadf_table"), 
    #                  title = "Real House Price Exuberance Statistics"),
    #         tabPanel(dataTableOutput("income_bsadf_table"), 
    #                  title = "Real House Price to Income Exuberance Statistics"),
    #         tabPanel(dataTableOutput("stat_table"), 
    #                  title = "GSADF statistics")
    #       )
    #     )
    #     
    #     # h3("3) Forecasts", 
    #     #    style = "padding:0 0 0 20px;"),
    #     # br(),
    #     # fluidRow(
    #     #   tabBox(
    #     #     width = 12, 
    #     #     side = "left"
    #     #   )
    #     # )
    #     
    #   ),
    #   includeHTML("content/footer.html")
    # )
    # 
    # Data Source & Methodology -----------------------------------------------
    
    # tabItem(
    #   tabName = "methodology",
    #   includeHTML("content/methodology.html")
    # )
    
    
  )
)
    
                  

server <- function(session, input, output) {
  
  # Oveview - Download Report -------------------------------------------------
  
  # output$downloadData <- downloadHandler(
  #   filename = "summary2018Q4.pdf",
  #   content = function(file) {
  #     file.copy("scripts/101-summary-reports.pdf", file)
  #   }
  # )
  
  # output$plot_UK <- 
  #   renderPlot({
  #     plot_price[["UK"]]})
  # output$plot_index_hpu <- 
  #   renderPlot({
  #     ggplot(hpu_index, aes(Date, HPU)) +
  #       geom_line() + 
  #       theme_bw() + 
  #       theme(
  #         axis.title = element_blank()
  #       )
  #   })
  # output$autoplot_UK <- 
  #   renderPlot({
  #     autoplot_price[["UK"]]})
  
  # Exuberance --------------------------------------------------------------
  
  output$composition <- 
    renderText({
      reg_comp[[input$country]]
    })

  output$plot_price <- 
    renderPlot({
      plot_price[[input$country]]})
  output$plot_income <- 
    renderPlot({
      plot_income[[input$country]]})
  
  output$table1 <-
    renderTable({
      summary(radf_price, cv_price) %>% 
        purrr::pluck(input$country)},
      striped = TRUE, bordered = TRUE,  
      width = '100%', rownames = TRUE,
      align = 'ccccc')
  
  output$table2 <- renderTable({
    summary(radf_income, cv_income) %>% 
      purrr::pluck(input$country)},
    striped = TRUE, bordered = TRUE,  
    width = '100%', rownames = TRUE,
    align = 'ccccc')
  
  output$autoplot_price <- 
    renderPlot({
      autoplot_price[[input$country]]})
  output$autoplot_income <- 
    renderPlot({
      autoplot_income[[input$country]]})
  
  output$table3 <- 
    DT::renderDataTable({
      exuber::datestamp(radf_price, cv_price) %>%
        .[[input$country]] %>% 
        to_yq(radf_price, cv_var = cv_price)
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      dom = "t"))
  
  output$table4 <- 
    DT::renderDataTable({
      exuber::datestamp(radf_income, cv_income) %>%
        .[[input$country]] %>% 
        to_yq(radf_income, cv_var = cv_income)
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      dom = "t"))
  

# indices -----------------------------------------------------------------

  output$map <- 
    leaflet::renderLeaflet({map_nuts1})
  
  # observeEvent(input$map_marker_click, {
  #   click  <-  input$map_marker_click
  #   print(click)
  #   text <- paste("Lattitude ", click$lat, "Longtitude ", click$lng)
  #   proxy <- leafletProxy('map')
  #   
  #   proxy %>% clearPopups() %>%
  #     addPopups(click$lng, click$lat, text)
  # })
  
  observeEvent(input$map_shape_click , {
    
    event <- input$map_shape_click 
    output$widget <- renderText(event$id)
    
    output$map_price <- 
      renderPlot({plot_price[[event$id]]})
    output$map_price_growth <- 
      renderPlot({plot_income[[event$id]]})
  })
  
  


  # Forecasting -------------------------------------------------------------

  # output$plot_predictors <-
  #   renderPlot({
  #     plot_predictors
  #   }, height = 700)

  
  # options = list(rowCallback = JS(
  #   'function(row, data, index, rowId) {',
  #   'if(rowId >= 1 && rowId < 4) {',
  #   'row.style.backgroundColor = "pink";','}','}'))
  
  # df <- econdata::bq1989[1:8,]
  # midday <- df$date[4]
  # output$forecasts <- 
  #   renderDataTable({
  #     datatable(
  #       df,
  #       rownames = FALSE,
  #       options = list( 
  #         dom = 't')) %>%
  #       formatStyle(
  #         columns = colnames(df),
  #         valueColumns = c("date"),
  #         backgroundColor = styleInterval(
  #           as.Date(midday, format = "%Y-%m-%d"), 
  #           c("#FFFFFF", "#EBEBEB"))
  #       )
  #   })
  # output$forecasts_models <- 
  #   DT::renderDataTable({
  #     datatable(
  #       df,
  #       rownames = FALSE,
  #       options = list( 
  #         dom = 't')) %>%
  #       formatStyle(
  #         columns = c("date"),
  #         backgroundColor = styleInterval(
  #           as.Date(midday, format = "%Y-%m-%d"), 
  #           c("#FB717E", "#89EC6A"))
  #     )
  #   })
  

# Uncertainty -------------------------------------------------------------


  output$uncertainty_index <-
    renderHighchart({
      highchart(type = "stock") %>%
        hc_add_series(hpu_index, hcaes(x = Date, y = HPU), zoomType = "x",
                      type = "line", color = "#B9274A", name = "HPU",
                      alpha = 0.2) %>%
        # hc_add_series(epu_index, hcaes(x = Date, y = EPU),
        #               type = "line", name = "EPU") %>%
        hc_xAxis(type = 'date',
                 minRange = 10,
                 # min = hpu_index$Date[1],
                 # max = hpu_index$Date[nrow(hpu_index)],
                 # events = list(setExtremes = list(hpu_index$Date[1], hpu_index$Date[nrow(hpu_index)])),
                 breaks = list(breakSize = 10),
                 labels = list(format = '{value:%Y}')) %>%
        hc_tooltip(valueDecimals = 0) %>%
        # hc_rangeSelector(enabled = FALSE)
      hc_rangeSelector(selected = 5, inputEnabled = FALSE)

  })
  
  # Download Data -----------------------------------------------------------
  
  # nationwide_caption <- 
  #   glue::glue(
  #   "The House Prices are provided by Nationwide and their use should be cited 
  #   accordingly https://www.nationwide.co.uk"
  #   )
  # 
  # output$price_table <-  
  #   DT::renderDataTable({
  #     make_DT(price, "rhpi", nationwide_caption)
  #     })
  # 
  # output$income_table <-  
  #   DT::renderDataTable({
  #     make_DT(
  #       price_income, "rhp_pdi", nationwide_caption)
  #     })
  # 
  # output$price_bsadf_table <-  
  #   DT::renderDataTable({
  #     make_DT(
  #       price_bsadf_table,"bsadf_rhpi")
  #   })
  # 
  # output$income_bsadf_table <- 
  #   DT::renderDataTable({
  #     make_DT(
  #       income_bsadf_table, "bsadf_rhp_pdi")
  #   })
  # 
  # output$stat_table <- 
  #   DT::renderDataTable({
  #     make_DT_general(
  #       stat_table, "stat_table")
  #   })
}

shinyApp(ui = dashboardPagePlus(title = "UK Housing Observatory", header, sidebar, body), server)