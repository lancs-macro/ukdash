library(tidyverse)
library(readxl)


# Naming  -----------------------------------------------------------------

nms <- tibble::tribble(
  ~num,   ~abbr,             ~ntwd,                   ~names,
  1,   "EA",      "East Anglia",               "East Anglia",
  2,   "EM",        "East Mids",              "East Midlands",
  3,   "GL",           "London",           "Greater London",
  4,   "NI",        "N Ireland",         "Northern Ireland",
  5,   "NT",            "North",                    "North",
  6,   "NW",       "North West",               "North West",
  7,   "OM",        "Outer Met",       "Outer Metropolitan",
  8,  "OSE",     "Outer S East",         "Outer South East",
  9,   "SC",         "Scotland",                 "Scotland",
  10,   "SW",       "South West",               "South West",
  11,   "UK",               "UK",           "United Kingdom",
  12,   "WM",        "West Mids",            "West Midlands",
  13,   "WW",            "Wales",                    "Wales",
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
  dplyr::filter(type == "Index") %>% 
  select(-type, hpi = value) %>% 
  mutate(region = recode(region, "Uk" = "United Kingdom")) %>% 
  mutate(region = recode(region, !!!ntwd_to_names))

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

rpdi <- 
  read_excel("data/rpdi.xlsx") %>% 
  rename(Date = 1) %>%
  mutate(Date = Date %>% 
           zoo::as.yearqtr(format = "Q%q %Y") %>%
           zoo::as.Date()
  ) %>% 
  gather(region, rpdi, -Date) %>% 
  mutate(region = recode(region, !!!abbr_to_names))


ntwd_data <- right_join(hpi, rpdi, by = c("region" ,"Date")) %>%
  right_join(cpi, by = "Date") %>% 
  drop_na() %>% 
  mutate(rhpi = hpi/cpi, afford = rhpi/rpdi) 

rhpi <- ntwd_data %>% 
  select(Date, region, rhpi) %>% 
  spread(region, rhpi)

afford <- ntwd_data %>% 
  select(Date, region, afford) %>% 
  spread(region, afford)


# Reading House Price Indices --------------------------------

library(jsonlite)
library(httr)

ukhp_get <- function(frequency = "monthly", classification = "nuts1", release = "latest") {
  endpoint <- "https://lancs-macro.github.io/uk-house-prices"
  query <- paste(endpoint, release, frequency, paste0(classification, ".json"), sep = "/")
  request <- GET(query)
  stop_for_status(request)
  parse_json(request, simplifyVector = TRUE)
}


nuts1_data <- ukhp_get(frequency = "quarterly", classification = "nuts1") %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))

nuts2_data <- ukhp_get(frequency = "quarterly", classification = "nuts2") %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))

nuts3_data <- ukhp_get(frequency = "quarterly", classification = "nuts3") %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))
