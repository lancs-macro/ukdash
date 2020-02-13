library(rgdal)
library(htmltools)
library(leaflet)
library(leaflet.extras)

create_leaflet_nuts <- function(x = nuts3_regions, map_data = nuts3_data, code = "318") {
  
  growth <- map_data %>% 
    select(-Date) %>% 
    map_df(~ldiff(.x, n = 1)*100) %>% 
    slice(n()) %>% 
    map_df(round, 2) %>%
    pivot_longer(
      cols = everything(), 
      names_to = "layerId", 
      values_to = "growth")
  
  price <- map_data %>% 
    select(-Date) %>% 
    slice(n()) %>% 
    map_df(round, 2) %>% 
    pivot_longer(
      cols = everything(), 
      names_to = "layerId", 
      values_to = "price")
  
  
  nuts_cd <- paste0("nuts", code, "cd")
  nuts_nm <- paste0("nuts", code, "nm")
  ss <- !grepl("UKM|UKN", as.character(x@data[[nuts_cd]]))
  
  regional_names <- x@data[[nuts_nm]][ss]
  regional_codes <- x@data[[nuts_cd]][ss]
  
  
  names(x@data)[3] <- "layerId"
  suppressWarnings({
    x@data <- left_join(x@data, price, by = "layerId")
    x@data <- left_join(x@data, growth, by = "layerId")
  })

  
  # bins <- seq(-2, 2, 0.5)
  # bin_colors <- viridis::magma(length(bins))
  # bin_colors <- rev(RColorBrewer::brewer.pal(length(bins), "YlGnBu"))
  
  # pal <- colorBin("YlOrRd", domain = x@data$growth[ss], bins = 2)
  pal <- colorBin("YlOrRd", domain = NULL, bins = 3)
  # previewColors(colorBin("YlOrRd", domain = x@data$growth, 
  #                        bins = 2), x@data$growth)

  lbls <- sprintf(
    paste(
      "<span style='font-size: 18px; font-weight:700;'> %s </span> <br>", 
      "<span style ='font-style: italic; color:grey;'> NUTS Code: %s </span><br>", 
      "Latest Index Level: <strong> %s </strong> <br>", 
      "Latest Growth Rate (Annual %%): <strong> %s </strong>"
    ),
  x@data[,3], x@data[,2], x@data$growth, x@data$price) %>%  #),htmlEscape(price$price)) %>%  #, price$price, growth$growth
    lapply(htmltools::HTML)
  
  highlights <-  highlightOptions(
      weight = 5,
      color = "#B22222",#666", #"#B9504A",#"#444", #666
      dashArray = "",
      fillOpacity = 0.5,
      bringToFront = TRUE)
  
  leaflet(
    x,
    options = 
      leafletOptions(
        zoomControl = TRUE,
        # background = "white",
        minZoom = 7,
        doubleClickZoom = TRUE,
        dragging = TRUE)
  ) %>% 
    addProviderTiles(
      providers$CartoDB.Positron,
      options = providerTileOptions(opacity = 0.5)) %>% 
    addPolygons(
      data = x[ss, ],
      fillColor =  "Reds",#~ pal(growth),   #"Reds",
      fillOpacity = 0.4,
      # color = "#BDBDC3",
      color = "white",
      group = ~ nuts_nm,
      weight = 2,
      opacity = 1,
      # color = "white",
      dashArray = "3",
      layerId = ~ layerId,
      highlightOptions = highlights,
      label = lbls,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>% 
    addResetMapButton() %>% 
    addControl("<p style='color:black;font-size:16px; font-weight:600px;'>Search for a location by name </p>",
               position = 'topleft') %>% 
    addSearchFeatures(
      targetGroups = nuts_nm,
      options = searchFeaturesOptions(
        zoom = 7, 
        autoType = TRUE,
        autoCollapse = TRUE
      ) 
    ) %>% 
    addControl("<p style='color:black;font-size:16px; font-weight:600px;'> Click on the map </p>",
               position = 'topright')
  # %>% 
    # addLegend(pal = pal(growth),
    #           values = ~ growth,
    #           opacity = 0.7,
    #           title = "Annual Growth (%)",
    #           position = "topright")
}

map_nuts1 <- create_leaflet_nuts(nuts1_regions, nuts1_data, "118")
map_nuts2 <- create_leaflet_nuts(nuts2_regions, nuts2_data, "218")
map_nuts3 <- create_leaflet_nuts(nuts3_regions, nuts3_data, "318")
