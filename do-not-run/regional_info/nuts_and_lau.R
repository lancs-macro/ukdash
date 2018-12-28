

# Download ----------------------------------------------------------------


nationwide <- "https://www.nationwide.co.uk/-/media/MainSite/documents/about/house-price-index/downloads/NationwideRegionLocalAuthorityList.xls"

# ons_ne <- "https://www.ons.gov.uk/file?uri=/methodology/geography/ukgeographies/eurostat/118ce3f9.xls"

# NUTS 1
URL_nuts1 <- "https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_0.csv?session=283804630.1540772026&outSR=%7B%22wkid%22%3A27700%2C%22latestWkid%22%3A27700%7D"
temp_nuts1 <- "data/regional_info/nuts1.csv"
download.file(URL_nuts1, destfile = temp_nuts1, mode = 'wb')

# NUTS 2
URL_nuts2 <- "https://opendata.arcgis.com/datasets/f021bab88bab4d14b72fa8f17363f4a3_0.csv?session=283804630.1540772026&outSR=%7B%22wkid%22%3A4326%2C%22latestWkid%22%3A4326%7D"
temp_nuts2 <- "data/regional_info/nuts2.csv"
download.file(URL_nuts2, destfile = temp_nuts2, mode = 'wb')

# NUTS 3
URL_nuts3 <- "https://opendata.arcgis.com/datasets/6da7ad08e66e4e68a616b95f50d92d5a_0.csv?session=283804630.1540772026"
temp_nuts3 <- "data/regional_info/nuts3.csv"
download.file(URL_nuts3, destfile = temp_nuts3, mode = 'wb')

# POSTAL CODE TO NUTS3
download.file("http://ec.europa.eu/eurostat/tercet/download.do?file=pc2018_uk_NUTS-2016_v2.0.zip",
              "data/regional_info/zip_pc.zip")
unzip("data/regional_info/zip_pc.zip", exdir = "data/regional_info")


# Read --------------------------------------------------------------------


library(stringr)
library(tidyverse)



nuts1 <- read_csv("do-not-run/regional_info/nuts1.csv") %>% 
  mutate(cd = nuts118cd,
         nm = nuts118nm) %>% 
  select(cd, nm)

nuts2 <- read_csv("do-not-run/regional_info/nuts2.csv") %>% 
  mutate(cd2 = NUTS215CD,
         nm2 = NUTS215NM) %>% 
  select(cd2, nm2) %>% 
  mutate(cd = str_sub(cd2, 1, -2)) %>% 
  select(cd, cd2, nm2)

nuts3 <- read_csv("do-not-run/regional_info/nuts3.csv") %>% 
  mutate(cd3 = NUTS315CD,
         nm3 = NUTS315NM) %>% 
  select(cd3, nm3) %>% 
  mutate(cd = str_sub(cd3, 1, -3),
         cd2 = str_sub(cd3, 1, -2)) %>% 
  select(cd, cd2, cd3, nm3)

# LENGTH 1,759,911 .. exlcuding NORTHERN IRELAND 
pc1_nuts3 <- read_delim("do-not-run//regional_info/pc2018_uk_NUTS-2016_v2.0.csv", 
                                       ";", quote = "'", escape_double = FALSE, 
                                       trim_ws = TRUE) %>% 
  set_names("cd3", "pc") %>% 
  mutate(pc_trim =  gsub("[0-9].*","", pc))


# This is what you need

all_nuts <- full_join(nuts1, nuts2, by = "cd") %>% 
  full_join(nuts3, by = c("cd", "cd2")) %>% 
  full_join(pc1_nuts3, by = "cd3") %>% 
  select(cd, cd2, cd3, pc, pc_trim, nm, nm2, nm3) %>% 
  drop_na() # HAD TO DROP NA TO SEE IF THIS IS CORRECT

all_nuts

# saveRDS(all_nuts, "data/regional_info/nuts123pc.Rds")


# LAU ---------------------------------------------------------------------

URL_lau1 <- "https://opendata.arcgis.com/datasets/3dc07a60f46b4e01ab0ec8ba71c7a879_0.csv?session=283804630.1540772026&outSR=%7B%22wkid%22%3A27700%2C%22latestWkid%22%3A27700%7D"
temp_lau1 <- "data/regional_info/lau1.csv"
download.file(URL_lau1, destfile = temp_lau1, mode = 'wb')


lau1 <- read_csv("data/regional_info/lau1.csv")


URL_lau2 <- "https://opendata.arcgis.com/datasets/c893dfece45f465f857ac34641041863_0.csv?session=283804630.1540772026"
temp_lau2 <- "data/regional_info/lau2.csv"
download.file(URL_lau2, destfile = temp_lau2, mode = 'wb')
  

lau2 <- read_csv("data/regional_info/lau2.csv")



# Everything --------------------------------------------------------------

URL_everything <- "https://opendata.arcgis.com/datasets/c893dfece45f465f857ac34641041863_0.csv?session=283804630.1540772026"
temp_everything <- "data/regional_info/every.csv"
download.file(URL_everything, destfile = temp_everything, mode = 'wb')

every <- read_csv("data/regional_info/every.csv")



# ONS - POSTCODE ----------------------------------------------------------

URL_ONS <- "https://ago-item-storage.s3-external-1.amazonaws.com/14adbf2d1dc34244be01624b1c3c35c7/ONSPD_NOV_2018_UK.zip?X-Amz-Security-Token=FQoGZXIvYXdzEPb%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaDGrmolKGcEUUHcLznyK3A%2BM8wU0v4bZAdOUxfOKUi5XmH5y4DNz0aC0W0JwxAHbgpliavDHlsMDYNOOXfg1PGeANFzqUjeLhk57IWPGI%2FaxX%2FyP3svMMQlA8KMwOBipv5l2Fe5aiCdViMRGNaIBUJJusEKS2bWlZNcbBVQJHxayujLSSFHOCbaq6m4gAFUckYj%2FnIA1JSwWkm0lxQb6gxzUb1OLAiNDdS4DPouNuTNZ73t6YRWgwQ%2FwXQtOyBekvH3ciSH1ztNUo03GZaaQTiof2qq9iWFX7Nm4MgyQUaonTKpUWpSLg9J9RWAZbnK2yISXK%2BKiN16PWhp9K8RGJ%2Fjcnh%2BG6tOLMQCZzMyI4B2P6MIsABYSC8VSYYkgM01yE2qBMxzoC%2BHy5k79zFCOwU%2BQ8aaed7p0mRJ%2Fme3EsMgngXzP0ign2GF1oKUPnTvVaI7F%2BieAFPMU28oLRrQNSvocymnS1E9Y%2F8QuUsZtDubT%2BZQh2Iml2QnYaex0Sqg31AIFDbzDfSliE4vCkQwGqDSyE5vac2A2mN9jwj63UbECJjGR7JQdZqTtnwUgMaN36A1pOuP78B51OS1YN8u2NqMrxAD4SSnIo76uw3wU%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20181114T124634Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAYZTTEKKEVYAYSK5F%2F20181114%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=b34875abb4fc95589a10e8c36a373014da1d3a75b5afcdf8693c582c2f2e178d"
