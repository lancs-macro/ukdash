hpu_index_dta <- haven::read_dta("data/HPU_QUARTERLY.dta")

library(dplyr)

hpu_index <- hpu_index_dta %>% 
  mutate(year_quarter = paste(Year, Quarter)) %>% 
  mutate(year_quarter = lubridate::yq(year_quarter)) %>% 
  select(Date = year_quarter, HPU)


# EPU ---------------------------------------------------------------------

# download.file("https://www.policyuncertainty.com/media/UK_Policy_Uncertainty_Data.xlsx", 
#               "data/epu.xlsx",  mode = 'wb')
# 
# 
# epu_index <- readxl::read_xlsx("data/epu.xlsx") %>% 
#   mutate(year_month = paste(year, month)) %>% 
#   mutate(year_month = lubridate::ymd(year_month, truncated = 1)) %>% 
#   select(Date = year_month, EPU = UK_EPU_Index) %>% 
#   group_by(Date = zoo::as.Date(zoo::as.yearqtr(Date, "%b-%Y"))) %>%
#   summarise_all(mean)
