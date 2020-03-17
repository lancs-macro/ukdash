library(rgdal)

# Reading House Price Regions ---------------------------------------------

nuts1_regions <- 
  readOGR(
    verbose = FALSE,
    "data/shapefiles/nuts1", 
    "NUTS_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom") %>% 
  spTransform(., CRS("+init=epsg:4326")) 

nuts2_regions <-
  readOGR(
    verbose = FALSE,
    "data/shapefiles/nuts2",
    "NUTS_Level_2_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom") %>% 
  spTransform(., CRS("+init=epsg:4326")) 

nuts3_regions <-
  readOGR(
    verbose = FALSE,
    "data/shapefiles/nuts3",
    "NUTS_Level_3_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom") %>% 
  spTransform(., CRS("+init=epsg:4326")) 



