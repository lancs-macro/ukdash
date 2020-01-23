library(tidyverse)
library(readxl)

# Naming  -----------------------------------------------------------------

nms <- tibble::tribble(
  ~num,   ~abbr,             ~ntwd,                   ~names,
  1,   "EA",      "East Anglia",               "East Anglia",
  2,   "EM",        "East Mids",              "East Midlands",
  3,   "GL",           "London",           "Greater London",
  4,   "NT",            "North",                    "North",
  5,   "NW",       "North West",               "North West",
  4,   "NI",        "N Ireland",         "Northern Ireland",
  7,   "OM",        "Outer Met",       "Outer Metropolitan",
  8,  "OSE",     "Outer S East",         "Outer South East",
  9,   "SC",         "Scotland",                 "Scotland",
  10,   "SW",       "South West",               "South West",
  11,   "UK",               "UK",           "United Kingdom",
  12,   "WW",            "Wales",                    "Wales",
  13,   "WM",        "West Mids",            "West Midlands",
  14,   "YH",    "Yorks & Hside",   "Yorkshire & Humberside",
)

abbr_to_names <- c(
  "EA" = "East Anglia", 
  "EM" = "East Midlands", 
  "GL" = "Greater London",
  "NI" = "Northern Ireland", 
  "NT" = "North",
  "NW" = "North West",
  "OM" = "Outer Metropolitan",
  "OSE" = "Outer South East",
  "SC" = "Scotland",
  "SW" = "South West",
  "UK" = "United Kingdom",
  "WM" = "West Midlands",
  "WW" =  "Wales",                 
  "YH" =  "Yorkshire & Humberside"
)

ntwd_to_names <- c(
  "East Anglia" = "East Anglia", 
  "East Mids" = "East Midlands", 
  "London" = "Greater London",
  "N Ireland" = "Northern Ireland", 
  "North" = "North",
  "North West" = "North West",
  "Outer Met" = "Outer Metropolitan",
  "Outer S East" = "Outer South East",
  "Scotland" = "Scotland",
  "South West" = "South West",
  "UK" = "United Kingdom",
  "West Mids" = "West Midlands",
  "Wales" =  "Wales",                 
  "Yorks & Hside" =  "Yorkshire & Humberside"
)



# Reading ntwd ------------------------------------------------------------

hpi <- 
  nationwider::ntwd_get("seasonal_regional") %>% 
  dplyr::filter(type == "Index", Date >= "1975-01-01") %>% 
  select(-type, hpi = value) %>% 
  mutate(region = recode(region, "Uk" = "United Kingdom")) %>% 
  mutate(region = recode(region, !!!ntwd_to_names))

last_obs <- select(hpi, Date, region)

cpi <- 
  readr::read_csv(
    "data/cpi.csv", 
    col_types = 
      cols_only(LOCATION = col_guess(), 
                TIME = col_guess(), 
                Value = col_guess())) %>% 
  dplyr::filter(LOCATION == "GBR") %>% 
  dplyr::select(TIME, Value) %>% 
  dplyr::rename(Date = TIME, cpi = Value) %>% 
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "%Y-Q%q") %>%
           zoo::as.Date()
  ) 

rpdi <-  read_excel("data/rpdi.xlsx") %>%
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "Q%q %Y") %>%
           zoo::as.Date()
  ) %>% 
  gather(region, rpdi, -Date) %>% 
  mutate(region = recode(region, !!!abbr_to_names)) %>% 
  right_join(last_obs, by = c("Date", "region")) %>% 
  group_by(region) %>% 
  mutate(rpdi = zoo::na.locf(rpdi)) %>% 
  ungroup() 


ntwd_data <- right_join(hpi, rpdi, by = c("region" ,"Date")) %>%
  right_join(cpi, by = "Date") %>% 
  drop_na() %>% 
  mutate(rhpi = hpi/cpi, afford = rhpi/rpdi) 


price <- ntwd_data %>% 
  select(Date, region, rhpi) %>% 
  spread(region, rhpi)

afford <- ntwd_data %>% 
  select(Date, region, afford) %>% 
  spread(region, afford)

