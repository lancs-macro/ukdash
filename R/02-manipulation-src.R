library(readxl)
library(magrittr)
library(tidyverse)

# UK from Dallas Fed ------------------------------------------------------

regional_names <- 
  reg_comp %>%
  names()

hpi <- nationwider::ntwd_get("seasonal_regional") %>% 
  filter(type == "Index") %>% 
  select(-type) %>% 
  spread(region, value) %>% 
  dplyr::rename(
    "Greater London" = London, 
    "Northern Ireland" = `N Ireland`,
    "UK" = Uk)
  
cpi_url <- "https://stats.oecd.org/sdmx-json/data/DP_LIVE/.CPI.TOT.IDX2015.Q/OECD?contentType=csv&detail=code&separator=comma&csv-lang=en"

cpi <- readr::read_csv(
  cpi_url, col_types = 
    cols_only(LOCATION = col_guess(), 
              TIME = col_guess(), 
              Value = col_guess())) %>% 
  dplyr::filter(LOCATION == "GBR") %>% 
  dplyr::select(TIME, Value) %>% 
  dplyr::rename(Date = TIME, CPI = Value) %>% 
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "%Y-Q%q") %>%
           zoo::as.Date()
  ) 

price <- right_join(hpi, cpi, by = "Date") %>% 
  drop_na() %>% 
  mutate_at(vars(-Date), funs(. / CPI)) %>% 
  select(-CPI) %>% 
  select(Date, UK, everything())
  
slider_names <- 
  price %>% 
  select(-Date) %>% 
  names()


# GVA nowcasting ----------------------------------------------------------


original_data_1970 <- tibble::tribble(
  ~"region", ~"Income from employment", ~"Income from self-employment", ~"Gross trading profits and surpluses", ~"Less Stock appreciation", ~"Rent", ~"Gross domestic product",
            "United Kingdom", "30,425", "3,774",  "7,267",  "1,162",  "3,276",     "43,580",
                     "North",  "1,542",   "190",   "398",   "63",   "153",  "2,220",
  "Yorkshire and Humberside",  "2,550",   "273",   "631",  "100",   "222",  "3,576",
             "East Midlands",  "1,821",   "194",   "430",   "74",   "172",  "2,543",
               "East Anglia",    "833",   "171",   "191",   "38",   "100",  "1,257",
                "South East", "10,804", "1,301", "2,540",  "358", "1,339", "15,626",
                "South West",  "1,850",   "307",   "388",   "71",   "224",  "2,698",
             "West Midlands",  "3,076",   "299",   "691",  "139",   "286",  "4,213",
                "North West",  "3,534",   "381",   "943",  "145",   "343",  "5,056",
                   "England", "26,010", "3,117", "6,211",  "989", "2,840", "37,189",
                     "Wales",  "1,266",   "194",   "281",   "49",   "125",  "1,817",
                  "Scotland",  "2,577",   "350",   "629",  "103",   "255",  "3,708",
          "Northern Ireland",    "572",   "113",   "147",   "22",    "56",    "866",
         "Continental Shelf",     "--",    "--",    "--",   "--",    "--",     "--"
  )

gdp_origin <- original_data_1970 %>% 
  select(region, `Gross domestic product`) %>% 
  filter(region != "Continental Shelf") %>% 
  mutate(region = recode(region, North = "North East", ))

download.file("https://www.escoe.ac.uk/wp-content/uploads/2019/08/August_Nowcast_Regional_Estimates.xlsx",
              "data/gva_growth.xlsx",  mode = 'wb')

readxl::read_xlsx("data/gva_growth.xlsx", sheet = 3) %>% 
  rename(Date = ...1) %>% 
  gather(region, growth) %>% 
  pull(region) %>% unique()

gdp_origin %>% 
  pull(region)


# Real House Price to Income ----------------------------------------------

rpdi <- 
  read_excel("data/rpdi.xlsx") %>% 
  rename(Date = 1) %>%
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "Q%q %Y") %>%
           zoo::as.Date()
  ) %>% 
  select(Date, UK, EA, EM, GL, NT, NW, NI, OM, OSE, SC, SW, WW, WM, YH) %>% 
  set_names(c("Date", slider_names))

long <-
  price %>%
  gather(Region, rhpi, -Date) %>%
  full_join(
    rpdi %>%
    gather(Region, rpdi, -Date),
    by = c("Date", "Region")
  ) %>%
  mutate(rhp_pdi = rhpi/rpdi)


price_income <- long %>%
  select(Date, Region, rhp_pdi) %>%
  spread(Region, rhp_pdi) %>%
  select(Date, UK, sort(current_vars())) %>%
  drop_na()
