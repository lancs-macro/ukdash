library(tidyverse)
source("R/src-functions.R")
source("R/src-read-ntwd.R")

lr <- uklr::ukhp_get("england") %>% 
  mutate(region = str_to_title(region)) %>% 
  select(Date = date, region, "Land Registry" = housePriceIndex)

ho <- ukhp_get(classification = "countries") %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(-Wales) %>% 
  pivot_longer(-Date, names_to = "region", values_to = "Housing Observatory")

tbl <- full_join(lr, ho, by = c("Date", "region")) %>% 
  filter(Date >= "1995-02-01") %>% 
  pivot_longer(cols = c("Land Registry (HPI)", "Housing Observatory (HOPI)")) %>% 
  group_by(name) %>% 
  # mutate(value = value/value[1]) %>% 
  mutate_at(vars(value), ldiff)

ggplot(tbl, aes(Date, value, col = name), size = 1.1) +
  geom_line() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_bw() +
  ggtitle("House Price Growth", subtitle = "England and Wales") +
  scale_color_manual(values = c("red", "#a6d71c")) +
  # scale_linetype_manual(values = c("solid","twodash")) +
  theme(
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.25, 0.25),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 9)
  )

library(here)

ggsave(here("output", "comparison.png"), width = 5, height = 3)

# stats -------------------------------------------------------------------

library(nationwider)
library(glue)

release_date <- price %>%
  tail(1) %>% 
  mutate(release_date = paste0(lubridate::year(Date), " Q",lubridate::quarter(Date))) %>% 
  pull(release_date)

price %>% 
  tail(5) %>% 
  select(`United Kingdom`, `Greater London`) %>% 
  map_df(ldiff)

afford %>% 
  tail(5) %>% 
  select(`United Kingdom`, `Greater London`) %>% 
  map_df(ldiff)

uk_price <-  price %>%
  select(Date, `United Kingdom`) %>% 
  mutate(stat1 = ldiff(`United Kingdom`)) %>% tail(1) %>% 
  pull(stat1) %>% `*`(100) %>% round(2)

london_price <- price %>%
  select(Date, `Greater London`) %>% 
  mutate(stat1 = ldiff(`Greater London`)) %>% 
  tail(1) %>% pull(stat1) %>% `*`(100) %>% round(2)

uk_afford <-  afford %>%
  select(Date, `United Kingdom`) %>% 
  mutate(stat1 = ldiff(`United Kingdom`)) %>% 
  tail(1) %>% pull(stat1) %>% `*`(100) %>% round(2)

london_afford <- afford %>%
  select(Date, `Greater London`) %>% 
  mutate(stat1 = ldiff(`Greater London`)) %>% 
  tail(1) %>% pull(stat1) %>% `*`(100) %>% round(2)


# write json

jspath <- here("output","stat-template.js")
jscode <- readLines(jspath, warn = FALSE)
json_df <- jsonlite::toJSON(
  data.frame(release = release_date, price_uk = uk_price, price_london = london_price,
             afford_uk = uk_afford, afford_london = london_afford), dataframe = "columns")
jscode[1] <- glue("var txt = '{json_df}';")
cat(jscode, sep = "\n", file = "output/stat.js")
