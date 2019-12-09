## app.R ##

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
# library(shinythemes)
library(tidyverse)
library(DT)
library(highcharter)
library(shinyWidgets)
library(leaflet.extras)

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
  list.files("R", full.names = TRUE, pattern = "-src.R") %>% 
    purrr::map(source)
}else {
  list.files(c("R"), full.names = TRUE, pattern = ".R") %>% 
    purrr::map(source)
}


# Header ------------------------------------------------------------------

header <- dashboardHeaderPlus(
  titleWidth = 440,
  title = shiny::tagList(

    span(class = "logo-lg",
         span(shiny::img(src = "minimal.png",  height = "32", width = "32"),
              HTML('<span class="name"> United Kingdom </span> 
                   <span class= "bottom-name"> Housing Observatory </span>')
              )),
    shiny::img(src = "minimal.png",  height = "32", width = "32")
  )
)


# Sidebar -----------------------------------------------------------------


sidebar <- dashboardSidebar(
  collapsed = TRUE,
  
  sidebarMenu(
    id = "tabs", 
    menuItem('Overview', tabName = "overview", icon = icon("globe", lib = "glyphicon")),
    
    menuItem("Financial Stability", tabName = "exuberance", icon = icon("chart-area")),
    conditionalPanel("input.tabs === 'exuberance'",
                     selectInput(
                       inputId = "country",
                       choices = slider_names,
                       selected = slider_names[2],
                       label = "Select Geographical Area:")
    ),
    menuItem('Uncertainty', tabName = "uncertainty", icon = icon("underline")),
    menuItem("New House Price Indices", icon = icon("house-damage"),
             tabName = "indices"), #house-damage
    menuItem("Download Data", icon = icon("download"), tabName = "download")
    )
  )

body <- dashboardBody(

  ######## Customization #################
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", 
              href = 'https://fonts.googleapis.com/css?family=Gloria Hallelujah'),
    tags$style(".name {font-family: 'Gloria Hallelujah';color:#B22222;white-space: nowrap; padding-left:10px;}"),
    tags$style(".bottom-name { color: black; font-family: 'Gloria Hallelujah';}"),
    
    tags$title("UK Housing Observatory"),
    tags$link(rel = "shortcut icon", href = "minimal.png"),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://use.fontawesome.com/releases/v5.5.0/css/all.css")
    ),
  includeCSS("content/style-ui.css"),
  includeScript("content/popover.js"),

tabItems(
  source("ui-overview.R", local = TRUE)$value,
  source("ui-exuberance.R", local = TRUE)$value,
  source("R/ui-uncertainty.R", local = TRUE)$value,
  source("ui-indices.R", local = TRUE)$value,
  source("ui-download.R", local = TRUE)$value
  )
)




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
    
    
      # fluidRow(
      #   style = "text-align:left;padding:2em;",
      #   
      #   h3("Uncertainty Title"),
      #   
      #   column(
      #     width = 12,
      #     p(
      #       "TheHouse  Price  Uncertainty (HPU) Index is constructedusing  the
      #         methodology  suggested  byBaker, Bloom and Davis (2016) to proxy
      #         for economic policy uncertainty. The HPUis an index of search results
      #         from five large newspapers in the UK: The Guardian, The Independent,
      #         The Times, Financial Times and Daily  Mail.  In  particular,  we  use
      #         LexisNexis  digital  archives  of  these  newspapers  to  obtain  a
      #         quarterly count  of articles  that contain the following three terms:
      #         ‘uncertainty’ or ‘uncertain’; ‘housing’ or ‘house prices’ or ‘real estate’;
      #         and one of the following: ‘policy’, ‘regulation’, ‘Bank of England, ‘mortgage’,
      #         ‘interest  rate’,  ‘stamp-duty’,  ‘tax’,  ‘bubble’  or  ‘buy-to-let’ (including
      #         variants  like  ‘uncertainties’, ‘housing market’ or ‘regulatory’). To meet the
      #         search criteria an article must contain terms in all three categories.")
      #   )
      # ),
     
    
                  

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
  
  icon_growth_box <- function(x) {
    if (x > 0) {
      return(icon("arrow-up"))
    }else{
      return(icon("arrow-down"))
    }
  }
  
  icon_exuberanec_box <- function(x, crit) {
    if (x > crit) {
      return(icon("exclamations"))
    }else{
      return(icon("flag"))
    }
  }
  
  text_exuberance_box <- function(x, crit) {
    if (x > crit) {
      return("Exuberance")
    }else{
      return("No Exuberance")
    }
  }
  
  output$price_growth_box <- renderInfoBox({
    infoBox(
      title = "House Price Growth",
      paste(round(tail(price[[input$country]], 1), 2), "%"),
      icon = icon_growth_box(tail(price[[input$country]], 1))
    )
  })
  
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

  output$map_price <- 
    renderPlot({plot_price[["Wales"]]})
  output$map_price_growth <- 
    renderPlot({plot_income[["Wales"]]})
  output$widget <- renderText("Wales")
  
  
  observeEvent(input$map_shape_click, ignoreInit = TRUE, {
    
    event <- input$map_shape_click 
    output$widget <- renderText(event$id)
    
    output$map_price <- 
      renderPlot({plot_price[[event$id]]})
    output$map_price_growth <- 
      renderPlot({plot_income[[event$id]]})
    
  })
  
  output$map2 <- 
    leaflet::renderLeaflet({map_nuts2})
  
  observeEvent(input$map2_shape_click, ignoreInit = TRUE, {
    
    event <- input$map2_shape_click 
    output$widget <- renderText(event$id)
    
    output$map2_price <- 
      renderPlot({plot_price[[event$id]]})
    output$map2_price_growth <- 
      renderPlot({plot_income[[event$id]]})
    
  })
  
  output$map3 <- 
    leaflet::renderLeaflet({map_nuts3})

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
      highchart
  })
  
  # Download Data -----------------------------------------------------------
  
  output$dataOutput <- 
    DT::renderDataTable(
      make_DT(price, "rhpi", nationwide_caption)
    )
  
  
  nationwide_caption <-
    glue::glue(
    "The House Prices are provided by Nationwide and their use should be cited
    accordingly https://www.nationwide.co.uk"
    )

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

shinyApp(ui = dashboardPagePlus(skin = "black", title = "UK Housing Observatory", header, sidebar, body), server)
