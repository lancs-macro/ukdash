library(lubridate)

# recent <- "1801"

temp_reg <- "data/regional.xls"
temp_cpi <- "data/cpi.csv"  
temp_fed <- "data/hp1802.xlsx"

library(tidyverse)

if (download_primary) {

  # scrape for latest data --------------------------------------------------
  
  nationwide_url <-
    xml2::read_html("https://www.nationwide.co.uk/about/house-price-index/download-data#tab:Downloaddata") %>% 
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href") %>% 
    grep("regional", ., value = TRUE)
  
  download.file(paste0("https://www.nationwide.co.uk/", nationwide_url), 
                destfile = temp_reg, mode = 'wb')
  
# Regional Data ----------------------------------------------------------

dataURL_reg <- "https://www.nationwide.co.uk/-/media/MainSite/documents/about/house-price-index/downloads/seasonal_regional.xls?date=june"

download.file(dataURL_reg, destfile = temp_reg, mode = 'wb')

# CPI ---------------------------------------------------------------------

dataURL_cpi <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.CPI.TOT.IDX2015.Q/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en&startPeriod=1973-Q1&endPeriod=2018-Q3"

download.file(dataURL_cpi, destfile = temp_cpi, mode = 'wb')

# Quandl ------------------------------------------------------------------

library(Quandl)
Quandl.api_key("sMcvTx8cd-zzewgSdERq") # Use my personal api key

# Quandl("BOE/LPMAILB")


# Dallas Fed --------------------------------------------------------------

dataURL_fed <- "https://www.dallasfed.org/~/media/documents/institute/houseprice/hp1802.xlsx"

download.file(dataURL_fed, destfile = temp_fed, mode = 'wb')

}

