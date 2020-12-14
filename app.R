## app.R ##

suppressMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(rlang)
  library(tidyverse)
  library(DT)
  library(highcharter)
  library(shinyWidgets)
  library(leaflet.extras)
  library(exuber)
})

# Load everything ---------------------------------------------------------

path_store_rds <- list.files("data/", full.names = TRUE)
store_rds <-  stringr::str_remove(list.files("data/"), ".rds")
for (i in seq_along(path_store_rds)) {
  assign(store_rds[i], readRDS(file = path_store_rds[i]))
}

# source ------------------------------------------------------------------

idx <- tibble(Date = index(radf_price, trunc = FALSE))

suppressMessages({
  source("analysis/src-read-ntwd.R", local = TRUE)$value
  source("analysis/src-read-shapefiles.R", local = TRUE)$value
  source("analysis/src-hpu-index.R", local = TRUE)$value
  source("analysis/src-map.R", local = TRUE)$value
})

# Header ------------------------------------------------------------------

header <- dashboardHeaderPlus(
  titleWidth = 450,
  title = shiny::tagList(
    span(
      class = "logo-lg",
      a(
        href = "https://uk.housing-observatory.com",
        span(
          shiny::img(src = "ukho-logo.png",  height = "28", width = "28"),
          HTML('<span class="name"> United Kingdom </span>
             <span class= "bottom-name"> Housing Observatory </span>')
        )
      )
    ),
    shiny::img(src = "ukho-logo.png",  height = "28", width = "28")
  ),
  tags$li(
    class = "dropdown",
    h3(
      class = "release", 
      glue::glue("Release: {release_date}")
    )
  )
)

# Sidebar -----------------------------------------------------------------


sidebar <- dashboardSidebar(
  collapsed = TRUE,
  
  sidebarMenu(
    id = "tabs", 
    menuItem('Overview', tabName = "overview", icon = icon("globe", lib = "glyphicon")),
    menuItem("Financial Stability", tabName = "exuberance", icon = icon("chart-area")),
    conditionalPanel("input.tabs === 'exuberance' && input.sidebarCollapsed == true",
                     selectInput(
                         inputId = "region", choices = nms$names,
                       selected = nms$names[11], label = "Select Geographical Area:")),
    menuItem('Uncertainty', tabName = "uncertainty", icon = icon("underline")),
    menuItem("Price Indices - HOPI", icon = icon("house-damage"), 
             badgeLabel = "New", badgeColor = "red",
             tabName = "indices"),
    menuItem("Download Data", icon = icon("download"), tabName = "download")#,
    # menuItem("Report", icon = icon("file-alt"), tabName = "report", selected = T)
    )
  )

body <- dashboardBody(

  ######## Customization #################
  tags$head(
    # tags$title("UK Housing Observatory • Dashboard"),
    tags$link(rel = "shortcut icon", href = "ukho-logo.png"),
    tags$link(rel = "stylesheet", type = "text/css", 
              href = 'https://fonts.googleapis.com/css?family=Gloria Hallelujah'),
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "https://use.fontawesome.com/releases/v5.5.0/css/all.css"),
    includeHTML("www/intro-modal.html"),
    includeCSS("www/style-ui.css"),
    includeScript("www/popover.js")
  ),
  tabItems(
    ui_overview(),
    ui_exuber(),
    ui_uncertainty(),
    ui_indices(),
    ui_download()
    # source("R/ui/ui-report.R", local = TRUE)$value
  )
)

server <- function(session, input, output) {
  
  # Oveview  -------------------------------------------------

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
      plot_price[[input$region]]})
  output$plot_afford <- 
    renderPlot({
      plot_afford[[input$region]]})
  
  # Exuberance Plots
  autoplot_price_reactive <- 
    reactive({
        autoplot(radf_price, cv_price, select_series = input$region) + 
          ggtitle("") + scale_custom(idx) + 
    scale_exuber_manual(color_values = c("#B22222", "black"), size_values = c(0.8, 0.6))
    })
  output$autoplot_price <- 
    renderPlot({
      autoplot_price_reactive()
    })
  autoplot_afford_reactive <- 
    reactive({
      autoplot(radf_afford, cv_afford, select_series = input$region) + 
        ggtitle("") + scale_custom(idx) + 
        scale_exuber_manual(color_values = c("#B22222", "black"), size_values = c(0.8, 0.6))
    })
  output$autoplot_afford <- 
    renderPlot({
      autoplot_afford_reactive()
    })
  
  # Growth Info boxes
  output$price_growth_box <- renderInfoBox({
    value <- calc_growth(price[[input$region]])
    infoBox(
      title = "Latest House Price Change",
      paste(value, "%"),
      icon = icon_growth_box(value)
    )
  })
  output$afford_growth_box <- renderInfoBox({
    value <- calc_growth(afford[[input$region]])
    infoBox(
      title = "Latest Affordability Index Change",
      paste(value, "%"),
      icon = icon_growth_box(value)
    )
  })
  
  # Exuberance Info Boxes
  crit <-  tail(bsadf_table_price$`Critical Values`, 1)
  output$price_exuberance_box <- renderInfoBox({
    value <- round(tail(bsadf_table_price[[input$region]], 1),3)
    infoBox(
      title = "Latest Exuberance Statistic (BSADF)",
      value,
      text_exuberance_box(value, crit),
      icon = icon_exuberanec_box(value, crit),
      
    )
  })
  output$afford_exuberance_box <- renderInfoBox({
    value <- round(tail(bsadf_table_afford[[input$region]], 1),3)
    infoBox(
      title = "Latest Exuberance Statistic (BSADF)",
      value,
      text_exuberance_box(value, crit),
      icon = icon_exuberanec_box(value, crit),
      
    )
  })
  
  # Datestamping Tables
  output$ds_price <- 
    DT::renderDataTable({
      exuber::datestamp(radf_price, cv_price) %>%
        purrr::pluck(input$region) %>%
        to_yq(radf_price, cv_price)
    }, options = list(searching = FALSE, ordering = FALSE, dom = "t"))
  
   output$ds_afford <- 
    DT::renderDataTable({
      exuber::datestamp(radf_afford, cv_afford) %>%
        purrr::pluck(input$region) %>%
        to_yq(radf_afford, cv_afford)
    }, options = list(searching = FALSE, ordering = FALSE, dom = "t"))
  
 # Uncertainty -------------------------------------------------------------
  
  output$uncertainty_index <-
    renderHighchart({
      highchart
    })

# Indices -----------------------------------------------------------------
  
  # Map 1
  output$map <- 
    leaflet::renderLeaflet({map_nuts1})
  
  # Map 2
  output$map2 <- leaflet::renderLeaflet({map_nuts2})
  observeEvent(input$map2_shape_click, ignoreInit = TRUE, {
    event <- input$map2_shape_click 
    output$widget <- renderText(event$id)
    
    output$map_price <- 
      renderPlot({plot_ukhp_index(nuts2_data, aggregate_data, event$id)})
    output$map_price_growth <- 
      renderPlot({plot_ukhp_growth(nuts2_data, aggregate_data, event$id)})
  })
  
  # Map 3
  output$map3 <- leaflet::renderLeaflet({map_nuts3})
  observeEvent(input$map3_shape_click, ignoreInit = TRUE, ignoreNULL = FALSE, {
    event <- input$map3_shape_click 
    output$widget <- renderText(event$id)
    
    output$map_price <- 
      renderPlot({plot_ukhp_index(nuts3_data, aggregate_data, event$id)})
    output$map_price_growth <- 
      renderPlot({plot_ukhp_growth(nuts3_data, aggregate_data, event$id)})
  })
  
  # Default Value
  output$map_price <- 
    renderPlot({plot_ukhp_index(nuts1_data, aggregate_data, "Wales")})
  output$map_price_growth <- 
    renderPlot({plot_ukhp_growth(nuts1_data, aggregate_data, "Wales")})
  output$widget <- renderText("Wales")
  
  observeEvent(input$map_shape_click, ignoreInit = TRUE, {
    event <- input$map_shape_click 
    output$widget <- renderText(event$id)
    
    output$map_price <- 
      renderPlot({plot_ukhp_index(nuts1_data, aggregate_data, event$id)})
    output$map_price_growth <- 
      renderPlot({plot_ukhp_growth(nuts1_data, aggregate_data, event$id)})
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
  


  
  # Download Data -----------------------------------------------------------
  
  nationwide_caption <-
    glue::glue(
    "The House Prices are provided by Nationwide and their use should be cited
    accordingly https://www.nationwide.co.uk"
    )

  output$DT_price <- DT::renderDataTable(server = FALSE, {
      make_DT(price, "data-prices", nationwide_caption)
      })
  output$DT_afford <- DT::renderDataTable(server = FALSE, {
      make_DT(afford, "data-afford", nationwide_caption)
    })
  
  output$DT_stat_table <- DT::renderDataTable(server = FALSE, {
    make_DT_general(stat_table, "fs-gsadf")
  })
  
  output$DT_bsadf_price <- DT::renderDataTable(server = FALSE, {
      make_DT(bsadf_table_price, "fs-bsadf-prices")
    })
  output$DT_bsadf_afford <- DT::renderDataTable(server = FALSE, {
      make_DT(bsadf_table_afford,"fs-bsadf-afford")
    })
  
  output$DT_hpu <- DT::renderDataTable(server = FALSE, {
    make_DT_general(hpu_index, "unc-hpu")
  })
  
  output$DT_nuts1 <- DT::renderDataTable(server = FALSE, {
    make_DT(nuts1_data, "hopi-nuts1")
  })
  
  output$DT_nuts2 <- DT::renderDataTable(server = FALSE, {
    make_DT(nuts2_data, "hopi-nuts2")
  })
  
  output$DT_nuts3 <- DT::renderDataTable(server = FALSE, {
    make_DT(nuts3_data, "hopi-nuts3")
  })
  
  
  # Report ------------------------------------------------------------------
  
  pdf_path <- reactive({
    if (input$report_choice == "regional") {
      out <- input$region_choice
    }else {
      out <- input$report_choice
    }
    vers <- gsub(" ", "", release_date)
    out_id <- gsub(" |&", "", out)
    paste0("reports/UKHO-", out_id, "-", vers,".pdf")
  })
  pdf_name <- reactive({
    if (input$report_choice == "regional") {
      out <- filter(nms, names == input$region_choice)$names
    }else {
      out <- capitalize(input$report_choice)
    }
    vers <- gsub(" ", "", release_date)
    out_id <- gsub(" |&", "", out)
    paste0("UKHO-", out_id, "-", vers,".pdf")
  })
  
  # output$pdf <- renderText({pdf_path()})
  output$pdf2 <- renderText({pdf_name()})
  
  output$pdfview <- renderUI({
    tags$object(
      tags$p(tags$a(href = pdf_path())),
      data = paste0(pdf_path(), "#zoom=FitV"),
      type = "application/pdf", width = "100%", height = "700px") #height = "100%")
    # pdf_path()
    # tags$iframe(
    #   sandbox = "allow-same-origin",
    #   onload = "this.style.height=(this.contentWindow.document.body.scrollHeight+20)+'px';",
    #   style = "width:100%;", 
    #   src = paste0(pdf_path()))# "#zoom=FitV"
  })
  # https://stackoverflow.com/questions/20562543/zoom-to-fit-pdf-embedded-in-html
  
  output$report <- downloadHandler(
    filename = pdf_name, # take out brackets since filename needs function
    content = function(file) {
      file.copy(paste0("www/", pdf_path()), file)
    }
  )
  # output$report2 <- downloadHandler(
  #   filename = function() paste0("UKHO-", gsub(" ", "", release_date), ".zip"),
  #   content = function(file) {
  #     pdf_files <- list.files("www/reports/", pattern = ".pdf", full.names = TRUE)
  #     vers <- gsub(" ", "", release_date)
  #     temp <- zip::zipr(paste0("www/reports/UKHO-", vers, ".zip"), pdf_files)
  #     file.copy(temp, file)
  #   }
  # )
}

shinyApp(ui = dashboardPagePlus(
  skin = "black", title = "UK Housing Observatory • Dashboard", header, sidebar, body), server)
