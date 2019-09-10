library(rgdal)    

if (download_secondary) {
  
  # URL http://geoportal.statistics.gov.uk/datasets/01fd6b2d7600446d8af768005992f76a_0?geometry=-77.43%2C45.365%2C70.93%2C63.013
  download.file(
    "https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_0.zip?outSR=%7B%22wkid%22%3A27700%2C%22latestWkid%22%3A27700%7D&session=283804630.1540772026",
    "scripts/data/postal_shapefile"
  )
  
  unzip("postal_shapefile")
  
}

dt <- list.files("scripts/data", pattern = ".shp", full.names = TRUE)

# plotly ------------------------------------------------------------------

# nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc <- sf::st_read(dt, quiet = TRUE)# %>% 
  # mutate(names = nuts118nm,
  #        acronyms = nuts118cd) %>%
  # dplyr::select(objectid, long, lat, names)

# blank_layer <- list(
#   title = "",
#   showgrid = F,
#   showticklabels = F,
#   zeroline = F)
# 
# plot_ly(nc, 
#         x = ~ long, 
#         y = ~lat,
#         fill = ~names,
#         name = ~ names,
#         color = "Blue",
#         colors = "Blue",
#         showlegend = FALSE,
#         text = ~ names, 
#         hoverinfo = 'text'
#         )
# cbind(
#   regional_names,
#   nc %>% 
#     pull(names) %>% 
#     unique() 
# )
# case_when(
#   "East Midlands (England)"  ~ "East Midlands",
#   "East of England"          ~ "East Anglia",
#   "London"                   ~ "Greater London",
#   "North East (England)"     ~ "North",
#   "North West (England)"     ~ "NorthWest",
#   "Northern Ireland"         ~ "Northern Ireland",
#   "Scotland"                 ~ "Scotland",
#   "South East (England)"     ~ "Outer Metropolitan",
#   "South West (England)"     ~ "South West",
#   "Wales"                    ~ "Wales",
#   "West Midlands (England)"  ~ "West Midlands",
#   "Yorkshire and The Humber" ~ "Yorkshire and The Humber"
# )
# 
# 
# plot_geo(nc) 
# 
# library(mapview)
# mapview(nc)
# 
# 
# library(leaflet)
# leaflet(nc) %>%
#   addTiles() %>%
#   addPolygons()
