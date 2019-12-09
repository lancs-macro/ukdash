library(rgdal)

# Download Boundaries -----------------------------------------------------


# download.file("http://geoportal1-ons.opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_4.zip",
#         destfile = "ignore/data/nuts1/Shapefile.zip")
# zip::unzip("ignore/data/nuts1/Shapefile.zip", exdir = "ignore/data/nuts1")
# 
# 
# download.file("http://geoportal1-ons.opendata.arcgis.com/datasets/48b6b85bb7ea43699ee85f4ecd12fd36_4.zip",
#               destfile = "ignore/data/nuts2/Shapefile.zip")
# zip::unzip("ignore/data/nuts2/Shapefile.zip", exdir = "ignore/data/nuts2")
# 
# 
# download.file("http://geoportal1-ons.opendata.arcgis.com/datasets/473aefdcee19418da7e5dbfdeacf7b90_4.zip",
#               destfile = "ignore/data/nuts3/Shapefile.zip")
# zip::unzip("ignore/data/nuts3/Shapefile.zip", exdir = "ignore/data/nuts3")


# Read boundaries ---------------------------------------------------------

nuts1_boundaries <- 
  readOGR(
    "ignore/data/nuts1", 
    "NUTS_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom")

nuts2_boundaries <-
  readOGR(
    "ignore/data/nuts2",
    "NUTS_Level_2_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom")

nuts3_boundaries <-
  readOGR(
    "ignore/data/nuts3",
    "NUTS_Level_3_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom")

library(leaflet)
library(leaflet.extras)

nuts1_regions <- spTransform(nuts1_boundaries, CRS("+init=epsg:4326")) 
nuts2_regions <- spTransform(nuts2_boundaries, CRS("+init=epsg:4326"))
nuts3_regions <- spTransform(nuts3_boundaries, CRS("+init=epsg:4326"))

create_leaflet_nuts1 <- function(x, code = "118") {
  
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
      group = ~ "nuts118nm",
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
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
      # extras
      addResetMapButton() %>% 
      addSearchFeatures(
        targetGroups = 'nuts118nm',
        options = searchFeaturesOptions(
          zoom = 7, autoType = TRUE,
          autoCollapse = TRUE
        )
      ) %>% 
      addControl("<P>Click on the map or search for a location by name</P>",
                 position = 'topright')
}

map_nuts1 <- create_leaflet_nuts1(nuts1_regions, "118")
# map_nuts1

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
