library(rgdal)

ggg <- readOGR("ignore/data", "NUTS_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom")

library(geojsonio)
ggg_json <- geojson_json(ggg)

lm_linear_trend <- function(x) {
  trend <- 1:NROW(x)
  as_tibble(lm(x ~ trend)$residuals)
}

color_price <-
  price %>% 
  select(-Date, -UK) %>% 
  map(lm_linear_trend) %>% 
  reduce(bind_cols) %>% 
  set_names(names(price[-c(1,2)])) %>% 
  tail(1) %>% 
  names()

library(leaflet)
regions <- spTransform(ggg, CRS("+init=epsg:4326")) 
regions@data <- regions@data

# bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <-
regions@data %>%
  pull(nuts118nm) %>%
  order() %>% 
  # unclass()
  # arrange(nuts118nm) %>% 
  colorNumeric("viridis", domain = .)
qpal <- colorQuantile("viridis", regions$lat, n = 12)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  regions$nuts118nm, regions$st_areasha
) %>% lapply(htmltools::HTML)

highlights <-  highlightOptions(
  weight = 5,
  color = "#444", #666
  dashArray = "",
  fillOpacity = 0.7,
  bringToFront = TRUE)

overview_map <- leaflet() %>%
  leaflet(
    options = 
      leafletOptions(
        zoomControl = FALSE,
        minZoom = 6,
        maxZoom = 6,
        doubleClickZoom = FALSE,
        dragging = FALSE)
      ) %>%
  addPolygons(data = regions,
              fillColor = ~ pal(unclass(nuts118nm)),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlights,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", 
                             padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) 
# %>%
#   addProviderTiles("MapBox", options = providerTileOptions( id = "mapbox.light", accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

saveRDS(overview_map, "data/RDS/overview_map.rds")


# mapview -----------------------------------------------------------------
library(mapview)
m <- mapview(Home,
             alpha.regions = 1,
             native.crs = TRUE,
             color = "white",
             col.regions = "#B9504A",
             legend = FALSE,
             zcol = "nuts118nm",
             popup = NULL) %>%
  removeMouseCoordinates() %>%
  onRender("function(el,x){
  this.touchZoom.disable();
  this.doubleClickZoom.disable();
  this.scrollWheelZoom.disable();
  this.boxZoom.disable();
  this.keyboard.disable();
}
  ")

# Junk --------------------------------------------------------------------


# library(highcharter)
# 
# hcmap(ggg)
# 
# # library(dplyr)
# # 
# library(maptools)
# library(rgeos)
# 
# library(rgdal)
# # 
# # 
# # 
# # library(raster)
# # gbr <- getData("GADM", country = "GBR", level = 1)
# # gbr$Patients <- runif(1:nrow(gbr))
# # 
# ggg <- readOGR("scripts/data", "NUTS_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom")
# # 
# # library(mapview)
# # mapview(ggg)
# # 
# # library(mapview)
# # library(leaflet)
# # library(dplyr)
# # library(sf)
# # library(albersusa)
# # 
# # albersusa::usa_composite("laea") 
# 
library(leaflet)
library(mapview)

# leaflet(options = leafletOptions(zoomControl = FALSE,
#                                  minZoom = 3, maxZoom = 3,
#                                  dragging = FALSE)) %>%
mapview(ggg, native.crs = TRUE,  color = "white", col.regions = "#B9504A",
        legend = FALSE,
        zcol = "nuts118nm") %>%
  removeHomeButton() %>%
  removeMouseCoordinates()
#
# m$x$options = append(m$x$options, list("zoomControl" = FALSE))
# mapshot(m)
# 
# setView(lng = nc$long, lat = nc$lat, zoom = 12)
# 
# 
# 
# 
# 
# # # ggg <- readOGR(dt, GDAL1_integer64_policy = TRUE)
# # # ggg$Patients <- runif(1:nrow(ggg))
# # 
# # leaflet(ggg) %>%
# #   addTiles() %>%
# #   addPolygons()
# # 
# # leaflet() %>% 
# #   addTiles() %>% 
# #   addPolygons(data = ggg, 
# #               fillColor = ~nuts118nm,
# #               fillOpacity = 1, 
# #               color = "white", 
# #               stroke = T, 
# #               weight = 1)
# # 
# # 
# # library(leaflet)
# # pal <- scales::seq_gradient_pal(low = "#132B43", high = "#56B1F7", space = "Lab")(seq(0, 1, length.out = 255))
# # 
# # 
# # 
# # leaflet() %>% 
# #   addPolygons(data = ggg,
# #               color = "#000", weight = 1, opacity = 0.5)
# # 
# # leaflet() %>%
# #   addPolygons(
# #     data = ggg,
# #     color = "#000", weight = 1, opacity = 0.5,
# #     fillColor = ~colorNumeric(pal, ggg$Patients)(Patients), fillOpacity = 1,
# #     popup = with(gbr@data, htmltools::htmlEscape(sprintf("%s: %s", NAME_1, Patients)))
# #   )
# # 
# # 
# # install.packages(rgdal)
# # install.packages("spdplyr")
# # install.packages(geojsonio)
# # install.packages("rmapshaper")
# # 
# library(spdplyr)
# nc
# 
# hcmap(nc)
# 
# hcmap(data = ggg)
# 
# # 



# 
# 
# 
# hcmap(showInLegend = FALSE,
#       dataLabels = list(enabled = TRUE, format = "{point.name}")) %>%
#   hc_add_series(type = "map", data = ggg_json, showInLegend = FALSE, 
#                 # geojson = TRUE,
#                 name = "Region",
#                 color = hex_to_rgba("#B9504A", 1),
#                                 # marker = list(lineWidth = 0, radius = 2),
#                 borderColor = "#FAFAFA",
#                 borderWidth = 1) 
# 
# 
# 
# 
# # library(rmapshaper)
# # county_json_simplified <- ms_simplify(ggg_json)
# # 
# # 
# # leaflet() %>% 
# #   addTiles() %>%
# #   addGeoJSON(ggg_json)
