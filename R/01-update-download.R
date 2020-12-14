
update_download <- function(hopi = FALSE, cpi_epu = FALSE, boundaries = FALSE) {
  
  # Reading House Price Indices --------------------------------
  # Download Data until you fix the uk-house-price API
  
  if (isTRUE(hopi)) {
    
    ukhp_get(frequency = "quarterly", classification = "aggregate") %>% 
      select(Date, `England and Wales`) %>% 
      saveRDS("data/aggregate_data.rds")
    
    ukhp_get(frequency = "quarterly", classification = "nuts1") %>% 
      saveRDS("data/nuts1_data.rds")
    
    ukhp_get(frequency = "quarterly", classification = "nuts2") %>% 
      saveRDS("data/nuts2_data.rds")
    
    ukhp_get(frequency = "quarterly", classification = "nuts3") %>% 
      saveRDS("data/nuts3_data.rds")
  }
  
# Download EPU and CPU index ----------------------------------------------
  
  if (isTRUE(cpi_epu)) {
    
    # Download EPU Index ------------------------------------------------------
    
    download.file("https://www.policyuncertainty.com/media/UK_Policy_Uncertainty_Data.xlsx",
                  "data-raw/epu.xlsx",  mode = 'wb')
    
    # Download CPI index ------------------------------------------------------
    
    download.file("https://stats.oecd.org/sdmx-json/data/DP_LIVE/.CPI.TOT.IDX2015.Q/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en&startPeriod=1973-Q1",
                  "data-raw/cpi.csv")
  }
  
  # Download Boundaries -----------------------------------------------------
  
  if (isTRUE(boundaries)) {
    
    download.file("http://geoportal1-ons.opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_4.zip",
                  destfile = "data/shapefiles/nuts1/Shapefile.zip")
    zip::unzip("data/shapefiles/nuts1/Shapefile.zip", exdir = "data/shapefiles/nuts1")
    unlink("data/shapefiles/nuts1/Shapefile.zip")
    
    download.file("http://geoportal1-ons.opendata.arcgis.com/datasets/48b6b85bb7ea43699ee85f4ecd12fd36_4.zip",
                  destfile = "data/shapefiles/nuts2/Shapefile.zip")
    zip::unzip("data/shapefiles/nuts2/Shapefile.zip", exdir = "data/shapefiles/nuts2")
    unlink("data/shapefiles/nuts2/Shapefile.zip")
    
    download.file("http://geoportal1-ons.opendata.arcgis.com/datasets/473aefdcee19418da7e5dbfdeacf7b90_4.zip",
                  destfile = "data/shapefiles/nuts3/Shapefile.zip")
    zip::unzip("data/shapefiles/nuts3/Shapefile.zip", exdir = "data/shapefiles/nuts3")
    unlink("data/shapefiles/nuts3/Shapefile.zip")
    
  }
  
}



