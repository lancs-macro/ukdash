

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
  ss <- !grepl("UKM|UKN", as.character(x[[nuts_cd]]))
  
  regional_names <- x[[nuts_nm]][ss]
  regional_codes <- x[[nuts_cd]][ss]
  latest_date <- map_data[nrow(map_data), 1, drop = TRUE]
  
  names(x)[3] <- "layerId"
  suppressWarnings({
    x <- left_join(x, price, by = "layerId")
    x <- left_join(x, growth, by = "layerId")
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
      "<span> Date: <strong> %s </strong> </span><br>",
      "Index Level: <strong> %s </strong> <br>", 
      "Growth Rate (Annual %%): <strong> %s </strong>"
    ),
    x$layerId, x[[nuts_cd]], latest_date,
    x$price, x$growth) %>%  #),htmlEscape(price$price)) %>%  #, price$price, growth$growth
    lapply(htmltools::HTML)
  
  lbls <- lbls[ss]
  
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
      group = ~ "layerId",
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
      targetGroups = "layerId",
      options = searchFeaturesOptions(
        zoom = 9, 
        autoType = TRUE,
        autoCollapse = TRUE
      ) 
    ) %>% 
    addControl("<p style='color:black;font-size:16px; font-weight:600px;'> Click on the map </p>",
               position = 'topright') #%>%
  # addLegend(pal = pal(growth),
  #           values = ~ growth,
  #           opacity = 0.7,
  #           title = "Annual Growth (%)",
  #           position = "topright")
}



nuts1_regions <- read_sf(
  here(
    "data-raw/shapefiles/nuts1/",
    "NUTS_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom.shp"
  )
)
nuts2_regions <- read_sf(
  here(
    "data-raw/shapefiles/nuts2/",
    "NUTS_Level_2_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom.shp"
  )
)
nuts3_regions <- read_sf(
  here(
    "data-raw/shapefiles/nuts3/",
    "NUTS_Level_3_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom.shp"
  )
)


map_nuts1 <- create_leaflet_nuts(nuts1_regions, nuts1_data, "118")
map_nuts2 <- create_leaflet_nuts(nuts2_regions, nuts2_data, "218")
map_nuts3 <- create_leaflet_nuts(nuts3_regions, nuts3_data, "318")