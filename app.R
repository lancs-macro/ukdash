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
  library(sf)
  library(here)
})
options(shiny.autoreload = TRUE)

# API Approach ------------------------------------------------------------

BASE_URL <- "https://api.housing-observatory.com/datasets/uk"


rel <- glue::glue("{BASE_URL}/index.json") %>% 
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  .$releases %>% 
  as.vector() %>% 
  .[1]

price <- glue::glue("{BASE_URL}/{rel}/rhpi.json") %>% 
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))

radf_price <- glue::glue("{BASE_URL}/{rel}/rhpi-radf.json") %>%
  read_lines() %>% 
  jsonlite::unserializeJSON(.)

price_income <- glue::glue("{BASE_URL}/{rel}/pti.json") %>% 
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))


radf_income <- glue::glue("{BASE_URL}/{rel}/pti-radf.json") %>%
  read_lines() %>% 
  jsonlite::unserializeJSON(.)


mc_con <- glue::glue("{BASE_URL}/{rel}/crit-mc.json") %>%
  read_lines() %>% 
  jsonlite::unserializeJSON(.)

epu_index <- glue::glue("{BASE_URL}/{rel}/epu.json") %>%
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))

hpu_index <- glue::glue("{BASE_URL}/{rel}/hpu.json") %>%
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))

aggregate_data <- glue::glue("{BASE_URL}/{rel}/hopi-aggregate.json") %>%
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))
  
nuts1_data <- glue::glue("{BASE_URL}/{rel}/hopi-nuts1.json") %>%
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))
  
nuts2_data <- glue::glue("{BASE_URL}/{rel}/hopi-nuts2.json") %>%
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))
  
nuts3_data <- glue::glue("{BASE_URL}/{rel}/hopi-nuts3.json") %>%
  jsonlite::read_json(., simplifyVector = TRUE) %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))

# * Wrangle ------------------------------------------------------------------

nms <- tibble::tribble(
  ~num,   ~abbr,             ~ntwd,                   ~names,
  1,   "EA",      "East Anglia",               "East Anglia",
  2,   "EM",        "East Mids",              "East Midlands",
  3,   "GL",           "London",           "Greater London",
  4,   "NT",            "North",                    "North",
  5,   "NW",       "North West",               "North West",
  4,   "NI",        "N Ireland",         "Northern Ireland",
  7,   "OM",        "Outer Met",       "Outer Metropolitan",
  8,  "OSE",     "Outer S East",         "Outer South East",
  9,   "SC",         "Scotland",                 "Scotland",
  10,   "SW",       "South West",               "South West",
  11,   "UK",               "UK",           "United Kingdom",
  12,   "WW",            "Wales",                    "Wales",
  13,   "WM",        "West Mids",            "West Midlands",
  14,   "YH",    "Yorks & Hside",   "Yorkshire & Humberside",
)

abbr_to_names <- c(
  "EA" = "East Anglia", 
  "EM" = "East Midlands", 
  "GL" = "Greater London",
  "NI" = "Northern Ireland", 
  "NT" = "North",
  "NW" = "North West",
  "OM" = "Outer Metropolitan",
  "OSE" = "Outer South East",
  "SC" = "Scotland",
  "SW" = "South West",
  "UK" = "United Kingdom",
  "WM" = "West Midlands",
  "WW" =  "Wales",                 
  "YH" =  "Yorkshire & Humberside"
)

ntwd_to_names <- c(
  "East Anglia" = "East Anglia", 
  "East Mids" = "East Midlands", 
  "London" = "Greater London",
  "N Ireland" = "Northern Ireland", 
  "North" = "North",
  "North West" = "North West",
  "Outer Met" = "Outer Metropolitan",
  "Outer S East" = "Outer South East",
  "Scotland" = "Scotland",
  "South West" = "South West",
  "UK" = "United Kingdom",
  "West Mids" = "West Midlands",
  "Wales" =  "Wales",                 
  "Yorks & Hside" =  "Yorkshire & Humberside"
)

ind <- exuber::index(radf_price, trunc = TRUE)


# * Version -----------------------------------------------------------------

idx <- tibble(Date = index(radf_price, trunc = FALSE))

cnames <- series_names(radf_price)

vers <- price[nrow(price), 1][[1]] %>%
  zoo::as.yearqtr()

# source ------------------------------------------------------------------

suppressMessages({
  source("analysis/src-map.R", local = TRUE)$value
})

# Header ------------------------------------------------------------------

header <- dashboardHeader(
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
      glue::glue("Release: {vers}")
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
    conditionalPanel("input.tabs === 'exuberance' && input.sidebarCollapsed == false",
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
  
  growth_rates_price <- 
    price %>% 
    modify_at(vars(-Date), ~ ldiff(.x, n = 4) *100) %>% 
    drop_na() 
  
  quantiles_price <- growth_rates_price %>% 
    gather(region, value, -Date) %>% 
    group_by(Date) %>% 
    summarise(
      q10 = quantile(value, probs = c(0.10)),
      q90 = quantile(value, probs = c(0.90))
    )
  
  plot_growth_UK_price <- 
    ggplot() +
    geom_line(data = growth_rates_price, aes(Date, `United Kingdom`)) +
    geom_ribbon(data = quantiles_price,
                aes(x = Date, ymin = q10, ymax = q90), fill = "grey75", alpha = 0.5) +
    theme_bw() +
    ylab("Year on Year (%)") +
    scale_custom(growth_rates_price) +
    theme(
      axis.title.x = element_blank(),
      panel.grid = element_line(linetype = 2),
      panel.grid.minor = element_blank()) 
  
  
  output$plot_growth_UK_price <-
    renderPlot({
      plot_growth_UK_price})
  
  
  growth_rates_pti <- 
    price_income %>% 
    modify_at(vars(-Date), ~ ldiff(.x, n = 4) *100) %>% 
    drop_na() 
  
  quantiles_pti <- 
    growth_rates_pti %>% 
    select(-`United Kingdom`) %>% 
    gather(region, value, -Date) %>% 
    group_by(Date) %>% 
    summarise(
      q10 = quantile(value, probs = c(0.10)),
      q90 = quantile(value, probs = c(0.90))
    )
  
  plot_growth_UK_pti <- ggplot() +
    geom_line(data = growth_rates_pti, aes(Date, `United Kingdom`)) +
    geom_ribbon(data = quantiles_pti,
                aes(x = Date, ymin = q10, ymax = q90), fill = "grey75", alpha = 0.5) + #"#174b97"
    theme_bw() +
    scale_custom(growth_rates_price) +
    ylab("Year on Year (%)") +
    theme(
      axis.title.x = element_blank(),
      panel.grid = element_line(linetype = 2),
      panel.grid.minor = element_blank()) 
  
  
  output$plot_growth_UK_pti <-
    renderPlot({
      plot_growth_UK_pti})
  

  
  output$autoplot_datestamp_price <-
    renderPlot({
      radf_price %>% 
        datestamp(cv = mc_con) %>% 
        autoplot() + 
        scale_custom(idx)
      })
  
  output$autoplot_datestamp_pti <-
    renderPlot({
      radf_income %>% 
        datestamp(cv = mc_con) %>% 
        autoplot() + 
        scale_custom(idx)
    })
  # TODO fix scales to point to the last observation
  
  # Exuberance --------------------------------------------------------------

  # Index Plots
  output$plot_price <-
    renderPlot({
      radf_price %>% 
      autoplot2(nonrejected = TRUE, cv = mc_con, select_series = input$region) +
        scale_custom(idx) 
      })
  output$autoplot_price <- 
    renderPlot({
      radf_price %>% 
        autoplot(nonrejected = TRUE, cv = mc_con, select_series = input$region) +
        scale_custom(idx) +
        scale_exuber_manual(color_values = c("#B22222", "black"), size_values = c(0.7, 0.6))
    })
  
  
  crit <- tail(mc_con$bsadf_cv[-1, 2], 1)
  
  output$price_exuberance_box <- renderInfoBox({
    value <- augment(radf_price) %>% 
      filter(id == input$region) %>% 
      tail(1) %>%  
      pull(bsadf) %>% 
      round(3)
    infoBox(
      title = "Latest Exuberance Statistic (BSADF)",
      value,
      text_exuberance_box(value, crit),
      icon = icon_exuberanec_box(value, crit),
      
    )
  })
  
  output$price_growth_box <- renderInfoBox({
    value <- calc_growth(price[[input$region]])
    infoBox(
      title = "Latest House Price Change",
      paste(value, "%"),
      icon = icon_growth_box(value)
    )
  })
  
  output$ds_price <-
    DT::renderDataTable({
      exuber::datestamp(radf_price, mc_con) %>%
        purrr::pluck(input$region) %>%
        to_yq(radf_price, cv_price)
    }, options = list(searching = FALSE, ordering = FALSE, dom = "t"))
  
  
  
  output$plot_pti <-
    renderPlot({
      radf_income %>% 
        autoplot2(nonrejected = TRUE, cv = mc_con, select_series = input$region) +
        scale_custom(idx)
      })
  output$autoplot_pti <-
    renderPlot({
      radf_income %>% 
        autoplot(nonrejected = TRUE, cv = mc_con, select_series = input$region) +
        scale_custom(idx) +
        scale_exuber_manual(color_values = c("#B22222", "black"), size_values = c(0.7, 0.6))
    })
  
  output$afford_growth_box <- renderInfoBox({
    value <- calc_growth(price_income[[input$region]])
    infoBox(
      title = "Latest Affordability Index Change",
      paste(value, "%"),
      icon = icon_growth_box(value)
    )
  })
  output$afford_exuberance_box <- renderInfoBox({
    value <- augment(radf_income) %>% 
      filter(id == input$region) %>% 
      tail(1) %>%  
      pull(bsadf) %>% 
      round(3)
    infoBox(
      title = "Latest Exuberance Statistic (BSADF)",
      value,
      text_exuberance_box(value, crit),
      icon = icon_exuberanec_box(value, crit),

    )
  })
   output$ds_pti <-
    DT::renderDataTable({
      exuber::datestamp(radf_income, mc_con) %>%
        purrr::pluck(input$region) %>%
        to_yq(radf_income, mc_con)
    }, options = list(searching = FALSE, ordering = FALSE, dom = "t"))

 # Uncertainty -------------------------------------------------------------
  
  output$uncertainty_index <-
    renderHighchart({
        highchart(type = "stock") %>%
        hc_add_series(epu_index, hcaes(x = Date, y = EPU), zoomType = "x", visible = FALSE,
                      type = "line", name = "Economic Policy Uncertainty", color = "#222d32")  %>%
        hc_add_series(hpu_index, hcaes(x = Date, y = HPU), zoomType = "x",
                      type = "line", name = "House Price Uncertainty", color = "#B22222",
                      alpha = 0.2) %>%
        hc_legend(
          align = 'top',
          verticalAlign = 'middle',
          enabled = TRUE, 
          layout = 'vertical',
          y = -200,
          x = 100,
          itemStyle = list(fontSize = "16px")
        ) %>% 
        hc_title(
          text = " House Price Uncertainty Index",
          style = list(`font-weight` = "600", `font-size` = "22px", useHTML = TRUE)) %>% 
        # hc_subtitle(text = "Deaths from bronchitis, emphysema and asthma") %>% 
        hc_xAxis(type = 'date',
                 minRange = 10,
                 breaks = list(breakSize = 10),
                 labels = list(format = '{value:%Y}')) %>%
        hc_tooltip(valueDecimals = 0) %>%
        hc_rangeSelector(
          buttons = list(
            # list(type = 'month', count = 3, text = '3m'),
            # list(type = 'month', count = 6, text = '6m'),
            list(type = 'year', count = 1, text = '1y'),
            list(type = 'year', count = 5, text = '5y'),
            list(type = 'year', count = 10, text = '10y'),
            list(type = 'year', count = 20, text = '20y'),
            list(type = 'all', text = 'All')
          ),
          selected = 5) %>%  #, inputEnabled = FALSE
        hc_annotations(
          list(
            labels =
              list(
                list(
                  point = list(x = datetime_to_timestamp(as.Date("1990/07/01")), 
                               y = 180, xAxis = 0, yAxis = 0),
                  text = "'Boom & Bust' Economic Cycle"
                ),
                list(
                  point = list(x = datetime_to_timestamp(as.Date("2008/07/01")), 
                               y = 280, xAxis = 0, yAxis = 0),
                  text = "Financial Crisis"
                ),
                list(
                  point = list(x = datetime_to_timestamp(as.Date("2016/06/01")), 
                               y = 270, xAxis = 0, yAxis = 0),
                  text = "EU Referendum"
                ),
                list(
                  point = list(x = datetime_to_timestamp(as.Date("2018/02/01")), 
                               y = 170, xAxis = 0, yAxis = 0),
                  text = "Brexit Uncertainty"
                )
              )
          )
        )
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
      make_DT(price_income, "data-afford", nationwide_caption)
    })
  output$DT_stat_table <- DT::renderDataTable(server = FALSE, {
    make_DT_general(
      tibble(
        Regions = nms$names,
        `Real House Prices` = radf_price$gsadf,
        `Affordability Index` = radf_income$gsadf,
        cv90 = mc_con$gsadf_cv[1],
        cv95 = mc_con$gsadf_cv[2],
        cv99 = mc_con$gsadf_cv[3]
      ), 
      "fs-gsadf")
  })
  output$DT_bsadf_price <- DT::renderDataTable(server = FALSE, {
      make_DT(
        radf_price %>%
          "[["("bsadf") %>% 
          as_tibble() %>%
          bind_cols(`Critical Values` = mc_con$bsadf_cv[-1, 2]) %>% 
          bind_cols(Date = ind) %>% 
          select(Date, `Critical Values`, `United Kingdom`, everything()) , 
        "fs-bsadf-prices")
    })
  output$DT_bsadf_afford <- DT::renderDataTable(server = FALSE, {
    make_DT(
      radf_income %>%
        "[["("bsadf") %>% 
        as_tibble() %>%
        bind_cols(`Critical Values` = mc_con$bsadf_cv[-1, 2]) %>% 
        bind_cols(Date = ind) %>% 
        select(Date, `Critical Values`, `United Kingdom`, everything()) , 
      "fs-bsadf-prices")
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
  
  # pdf_path <- reactive({
  #   if (input$report_choice == "regional") {
  #     out <- input$region_choice
  #   }else {
  #     out <- input$report_choice
  #   }
  #   vers <- gsub(" ", "", release_date)
  #   out_id <- gsub(" |&", "", out)
  #   paste0("reports/UKHO-", out_id, "-", vers,".pdf")
  # })
  # pdf_name <- reactive({
  #   if (input$report_choice == "regional") {
  #     out <- filter(nms, names == input$region_choice)$names
  #   }else {
  #     out <- capitalize(input$report_choice)
  #   }
  #   vers <- gsub(" ", "", release_date)
  #   out_id <- gsub(" |&", "", out)
  #   paste0("UKHO-", out_id, "-", vers,".pdf")
  # })
  
  # output$pdf <- renderText({pdf_path()})
  # output$pdf2 <- renderText({pdf_name()})
  
  # output$pdfview <- renderUI({
  #   tags$object(
  #     tags$p(tags$a(href = pdf_path())),
  #     data = paste0(pdf_path(), "#zoom=FitV"),
  #     type = "application/pdf", width = "100%", height = "700px") #height = "100%")
    # pdf_path()
    # tags$iframe(
    #   sandbox = "allow-same-origin",
    #   onload = "this.style.height=(this.contentWindow.document.body.scrollHeight+20)+'px';",
    #   style = "width:100%;", 
    #   src = paste0(pdf_path()))# "#zoom=FitV"
  # })
  # https://stackoverflow.com/questions/20562543/zoom-to-fit-pdf-embedded-in-html
  
  # output$report <- downloadHandler(
  #   filename = pdf_name, # take out brackets since filename needs function
  #   content = function(file) {
  #     file.copy(paste0("www/", pdf_path()), file)
  #   }
  # )
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

shinyApp(ui = dashboardPage(
  skin = "black", title = "UK Housing Observatory • Dashboard", header, sidebar, body), server)
