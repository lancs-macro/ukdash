library(rgdal)
library(htmltools)
library(leaflet)
library(leaflet.extras)

create_leaflet_nuts1 <- function(x = nuts1_regions, code = "118") {
  
  nuts_cd <- paste0("nuts", code, "cd")
  nuts_nm <- paste0("nuts", code, "nm")
  
  ss <- !grepl("UKM|UKN", as.character(x@data[[nuts_cd]]))
  
  regional_names <- x@data[[paste0("nuts", code, "nm")]][ss]
  regional_codes <- x@data[[nuts_cd]][ss]
  
  growth <- nuts1_data %>% 
    select(-Date) %>% 
    map_df(~ldiff(.x, n = 1)*100) %>% 
    slice(n()) %>% 
    map_df(round, 2) %>%
    pivot_longer(
      cols = everything(), 
      names_to = "nuts118nm", 
      values_to = "growth") %>% 
    mutate_at(vars(nuts118nm), as.factor)
  
  price <- nuts1_data %>% 
    select(-Date) %>% 
    slice(n()) %>% 
    map_df(round, 2) %>% 
    pivot_longer(
      cols = everything(), 
      names_to = "nuts118nm", 
      values_to = "price")
  
  suppressWarnings({
    x@data <- full_join(x@data, growth, by = "nuts118nm")
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
   regional_names, regional_codes, price$price, growth$growth) %>%  #),htmlEscape(price$price)) %>%  #, price$price, growth$growth
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
      group = ~ "nuts118nm",
      weight = 2,
      opacity = 1,
      # color = "white",
      dashArray = "3",
      layerId = ~ nuts118nm,
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
      targetGroups = 'nuts118nm',
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

map_nuts1 <- create_leaflet_nuts1(nuts1_regions, "118")
map_nuts1

create_leaflet_nuts2 <- function(x, code = "218") {
  
  nuts_cd <- paste0("nuts", code, "cd")
  nuts_nm <- paste0("nuts", code, "nm")
  
  ss <- !grepl("UKM|UKN", as.character(x@data[[nuts_cd]]))
  
  regional_names <- x@data[[paste0("nuts", code, "nm")]][ss]
  regional_codes <- x@data[[nuts_cd]][ss]
  
  # pal <-  colorNumeric("Reds", domain = order(regional_names))
  # pal_var <- pal(unclass(regional_names))
  
  bins <- c(-2, -1, 0, 1, 2)
  # pal <- colorBin("Reds", domain = x$growth, bins = bins)
  
  lbls <- sprintf( "<strong> %s </strong> <br> NUTS Code: %s", 
                   regional_names, regional_codes) %>% 
    lapply(htmltools::HTML)
  highlights <-  highlightOptions(
    weight = 5,
    color = "#B22222",#666", #"#B9504A",#"#444", #666
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE)
  
  leaflet(
    options = 
      leafletOptions(
        zoomControl = TRUE,
        # background = "white",
        minZoom = 7,
        doubleClickZoom = TRUE,
        dragging = TRUE)
  ) %>% 
    addPolygons(
      data = x[ss, ],
      fillColor = "Reds", #~ pal(growth),
      group = ~ "nuts218nm",
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      layerId = ~ nuts218nm,
      highlightOptions = highlights,
      label = lbls,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal", 
          padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>% 
    # extras
    addResetMapButton() %>% 
    addSearchFeatures(
      targetGroups = 'nuts218nm',
      options = searchFeaturesOptions(
        zoom = 8, autoType = TRUE,
        autoCollapse = TRUE
      )
    ) %>% 
    addControl("<P>Click on the map or search for a location by name</P>",
               position = 'topright')
}


map_nuts2 <- create_leaflet_nuts2(nuts2_regions, "218")
# map_nuts2

create_leaflet_nuts3 <- function(x, code = "318") {
  
  nuts_cd <- paste0("nuts", code, "cd")
  nuts_nm <- paste0("nuts", code, "nm")
  
  ss <- !grepl("UKM|UKN", as.character(x@data[[nuts_cd]]))
  
  regional_names <- x@data[[paste0("nuts", code, "nm")]][ss]
  regional_codes <- x@data[[nuts_cd]][ss]
  
  # pal <-  colorNumeric("Reds", domain = order(regional_names))
  # pal_var <- pal(unclass(regional_names))
  
  bins <- c(-2, -1, 0, 1, 2)
  # pal <- colorBin("Reds", domain = x$growth, bins = bins)
  
  lbls <- sprintf( "<strong> %s </strong> <br> NUTS Code: %s", 
                   regional_names, regional_codes) %>% 
    lapply(htmltools::HTML)
  highlights <-  highlightOptions(
    weight = 5,
    color = "#B22222",#666", #"#B9504A",#"#444", #666
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE)
  
  leaflet(
    options = 
      leafletOptions(
        zoomControl = TRUE,
        # background = "white",
        minZoom = 7,
        doubleClickZoom = TRUE,
        dragging = TRUE)
  ) %>% 
    addPolygons(
      data = x[ss, ],
      fillColor = "Reds", #~ pal(growth),
      group = ~ "nuts318nm",
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      layerId = ~ nuts318nm,
      highlightOptions = highlights,
      label = lbls,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal", 
          padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>% 
    # extras
    addResetMapButton() %>% 
    addSearchFeatures(
      targetGroups = 'nuts318nm',
      options = searchFeaturesOptions(
        zoom = 9, autoType = TRUE,
        autoCollapse = TRUE
      )
    ) %>% 
    addControl("<P>Click on the map or search for a location by name</P>",
               position = 'topright')
}


map_nuts3 <- create_leaflet_nuts3(nuts3_regions)
# map_nuts3
