library(readxl)
library(magrittr)
library(tidyverse)

# UK from Dallas Fed ------------------------------------------------------

regional_names <- 
  reg_comp %>%
  names()

hpi <-
  readxl::read_excel(temp_reg, sheet = 1, skip = 2, .name_repair = make.unique) %>% 
  dplyr::rename(Date = `Q1 1993 = 100`) %>% 
  dplyr::mutate(Date = Date %>% zoo::as.yearqtr(format = "Q%q %Y") %>%
           zoo::as.Date()) %>% 
  tidyr::drop_na() %>% 
  dplyr::rename("GREATER LONDON" = LONDON, "NORTHERN IRELAND" = `N IRELAND`) %>% 
  dplyr::select(Date:UK) %>%
  dplyr::select(Date, UK, sort(current_vars())) %>%
  purrr::set_names("Date", "UK", regional_names)
  
regional_date <-
  hpi %>%
  pull(Date)

cpi <- 
  readr::read_csv(temp_cpi, col_types = 
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

price <-
  hpi %>% 
  right_join(cpi, by = "Date") %>% 
  drop_na() %>% 
  mutate_at(vars(-Date), funs(. / CPI)) %>% 
  select(-CPI) %>% 
  # right_join(uk_rhpi, by = "Date") %>% 
  select(Date, UK, everything())
  
slider_names <- 
  price %>% 
  select(-Date) %>% 
  names()

library(stringr)

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

# 
# Monthly_Prices <- readr::read_delim("data/Monthly_Prices.csv", 
#                              "\t", escape_double = FALSE, trim_ws = TRUE)
