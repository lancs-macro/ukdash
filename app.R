## app.R ##

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(highcharter)
library(shinyWidgets)
library(leaflet.extras)

# Load everything ---------------------------------------------------------

path_store_rds <- list.files("data/RDS", full.names = TRUE)
store_rds <-  stringr::str_remove(list.files("data/RDS"), ".rds")
for (i in seq_along(path_store_rds)) {
  assign(store_rds[i], readRDS(file = path_store_rds[i]))
}

# source ------------------------------------------------------------------

source("R/src-functions.R")
source("R/src-read.R")
source("R/src-hpu-index.R")
source("R/src-map.R")

# Header ------------------------------------------------------------------

header <- dashboardHeaderPlus(
  titleWidth = 440,
  title = shiny::tagList(
    span(
      class = "logo-lg",
      span(
        shiny::img(src = "minimal.png",  height = "32", width = "32"),
        HTML('<span class="name"> United Kingdom </span>
             <span class= "bottom-name"> Housing Observatory </span>')
        )
      ),
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
                       inputId = "country", choices = nms$names,
                       selected = nms$names[11], label = "Select Geographical Area:")),
    menuItem('Uncertainty', tabName = "uncertainty", icon = icon("underline")),
    menuItem("New House Price Indices", icon = icon("house-damage"), tabName = "indices"),
    menuItem("Download Data", icon = icon("download"), tabName = "download")
    )
  )

body <- dashboardBody(

  ######## Customization #################
  tags$head(
    tags$title("UK Housing Observatory"),
    tags$link(rel = "shortcut icon", href = "minimal.png"),
    tags$link(rel = "stylesheet", type = "text/css", 
              href = 'https://fonts.googleapis.com/css?family=Gloria Hallelujah'),
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "https://use.fontawesome.com/releases/v5.5.0/css/all.css"),
    includeCSS("www/style-ui.css"),
    includeScript("www/popover.js")
  ),
  tabItems(
    source("R/ui/ui-overview.R", local = TRUE)$value,
    source("R/ui/ui-exuberance.R", local = TRUE)$value,
    source("R/ui/ui-uncertainty.R", local = TRUE)$value,
    source("R/ui/ui-indices.R", local = TRUE)$value,
    source("R/ui/ui-download.R", local = TRUE)$value
  )
)
                  

server <- function(session, input, output) {
  
  # Oveview - Download Report -------------------------------------------------

  output$plot_growth_UK_price <-
    renderPlot({
      plot_growth_UK_price})
  
  output$plot_growth_UK_afford <-
    renderPlot({
      plot_growth_UK_afford})
  
  output$autoplot_datestamp_price <-
    renderPlot({
      autoplot_datestamp_price
      })
  output$autoplot_datestamp_afford <-
    renderPlot({
      autoplot_datestamp_afford
    })
  
  # Exuberance --------------------------------------------------------------

  # Index Plots
  output$plot_price <- 
    renderPlot({
      plot_price[[input$country]]})
  output$plot_afford <- 
    renderPlot({
      plot_afford[[input$country]]})
  
  # Exuberance Plots
  output$autoplot_price <- 
    renderPlot({
      autoplot_price[[input$country]]})
  output$autoplot_afford <- 
    renderPlot({
      autoplot_afford[[input$country]]})
  
  # Growth Info boxes
  output$price_growth_box <- renderInfoBox({
    value <- calc_growth(price[[input$country]])
    infoBox(
      title = "House Price Growth",
      paste(value, "%"),
      icon = icon_growth_box(value)
    )
  })
  output$afford_growth_box <- renderInfoBox({
    value <- calc_growth(afford[[input$country]])
    infoBox(
      title = "Affordability Index Growth",
      paste(value, "%"),
      icon = icon_growth_box(value)
    )
  })
  
  # Exuberance Info Boxes
  crit <-  tail(bsadf_table_price$`Critical Values`, 1)
  output$price_exuberance_box <- renderInfoBox({
    value <- round(tail(bsadf_table_price[[input$country]], 1),3)
    infoBox(
      title = "BSADF",
      value,
      text_exuberance_box(value, crit),
      icon = icon_exuberanec_box(value, crit),
      
    )
  })
  output$afford_exuberance_box <- renderInfoBox({
    value <- round(tail(bsadf_table_afford[[input$country]], 1),3)
    infoBox(
      title = "BSADF",
      value,
      text_exuberance_box(value, crit),
      icon = icon_exuberanec_box(value, crit),
      
    )
  })
  
  # Datestamping Tables
  output$ds_price <- 
    DT::renderDataTable({
      exuber::datestamp(radf_price, cv_price) %>%
        .[[input$country]] %>% 
        to_yq(radf_price, cv_var = cv_price)
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      dom = "t"))
  output$ds_afford <- 
    DT::renderDataTable({
      exuber::datestamp(radf_afford, cv_afford) %>%
        .[[input$country]] %>% 
        to_yq(radf_afford, cv_var = cv_afford)
    }, options = list(searching = FALSE,
                      ordering = FALSE,
                      dom = "t"))
  

# indices -----------------------------------------------------------------
  
  # Map 1
  output$map <- 
    leaflet::renderLeaflet({map_nuts1})
  # Default Value
  output$map_price <- 
    renderPlot({plot_ukhp_index(nuts1_data, "Wales")})
  output$map_price_growth <- 
    renderPlot({plot_ukhp_growth(nuts1_data, "Wales")})
  output$widget <- renderText("Wales")
  
  observeEvent(input$map_shape_click, ignoreInit = TRUE, {
    event <- input$map_shape_click 
    output$widget <- renderText(event$id)
    
    output$map_price <- 
      renderPlot({plot_ukhp_index(nuts1_data, event$id)})
    output$map_price_growth <- 
      renderPlot({plot_ukhp_growth(nuts1_data, event$id)})
  })
  
  # Map 2
  output$map2 <- 
    leaflet::renderLeaflet({map_nuts2})
  observeEvent(input$map2_shape_click, ignoreInit = TRUE, {
    event <- input$map2_shape_click 
    output$widget <- renderText(event$id)
    
    output$map2_price <- 
      renderPlot({plot_ukhp_index(nuts2_data, event$id)})
    output$map2_price_growth <- 
      renderPlot({plot_ukhp_growth(nuts2_data, event$id)})
  })
  
  # Map 3
  output$map3 <- 
    leaflet::renderLeaflet({map_nuts3})
  observeEvent(input$map2_shape_click, ignoreInit = TRUE, {
    event <- input$map2_shape_click 
    output$widget <- renderText(event$id)
    
    output$map2_price <- 
      renderPlot({plot_ukhp_index(nuts3_data, event$id)})
    output$map2_price_growth <- 
      renderPlot({plot_ukhp_growth(nuts3_data, event$id)})
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
      highchart
  })
  
  # Download Data -----------------------------------------------------------
  
  nationwide_caption <-
    glue::glue(
    "The House Prices are provided by Nationwide and their use should be cited
    accordingly https://www.nationwide.co.uk"
    )

  output$DT_price <-
    DT::renderDataTable({
      make_DT(price, "prices", nationwide_caption)
      })
  output$DT_afford <-
    DT::renderDataTable({
      make_DT(afford, "afford", nationwide_caption)
    })
  
  output$DT_bsadf_price <-
    DT::renderDataTable({
      make_DT(
        bsadf_table_price,"bsadf_prices")
    })
  output$DT_bsadf_afford <-
    DT::renderDataTable({
      make_DT(
        bsadf_table_afford,"bsadf_afford")
    })
  
  output$DT_stat_table <-
    DT::renderDataTable({
      make_DT_general(
        stat_table, "stat_table")
    })
  
  output$DT_hpu <- DT::renderDataTable({
    make_DT_general(
      hpu_index, "hpu_index")
  })
  
  output$DT_epu <- DT::renderDataTable({
    make_DT_general(
      epu_index, "epu_index")
  })
  
  output$DT_nuts1 <- DT::renderDataTable({
    make_DT(
      nuts1_data, "hp_nuts1")
  })
  
  output$DT_nuts2 <- DT::renderDataTable({
    make_DT(
      nuts2_data, "hp_nuts2")
  })
  
  output$DT_nuts3 <- DT::renderDataTable({
    make_DT(
      nuts3_data, "hp_nuts3")
  })
  
}

shinyApp(ui = dashboardPagePlus(skin = "black", title = "UK Housing Observatory", header, sidebar, body), server)
