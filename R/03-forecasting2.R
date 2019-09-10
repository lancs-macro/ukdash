library(tidyverse)

path_xlsx <- list.files("C:/Users/T460p/Desktop/alisa/Data", pattern = "xlsx",
                        full.names = TRUE)

nms <- list.files("C:/Users/T460p/Desktop/alisa/Data", pattern = "xlsx") %>% 
    map_chr( ~ gsub(".xlsx", "", .x)) %>% 
    `[`(-1)

list_xlsx <- map(path_xlsx[-1], readxl::read_excel) %>% 
  set_names(nms)

sub_date <- seq(from = as.Date("1982-01-01"), to = as.Date("2019-04-01"), by = "quarter")

fixed_list <- map(list_xlsx, ~ mutate(.x, Date = sub_date))


# Predicors ---------------------------------------------------------------

predictors <- c("drhp", "ratio", "growth", "ur", "lf", "hs", "cons", 
                   "indus", "rabmr", "spread", "cci", "hpu")

neighbours <- map(fixed_list, ~ rename_all(.x, list(tolower))) %>% 
  map(~ select(.x , -c("date", predictors)))


# Make dataset ------------------------------------------------------------

total_list <- map(fixed_list, ~ rename_all(.x, list(tolower))) %>% 
  map(~ select(.x , date, drhp, ratio, growth, ur, lf, hs, cons, 
               indus, rabmr, spread, cci, hpu)) %>% 
  map2(nms, ~ mutate(.x, region = .y))

total <- total_list %>% 
  reduce(bind_rows) %>% 
  select(date, region, everything()) %>% 
  group_by(drhp == 0) %>% 
  mutate(drhp = drhp + 0.001) %>%
  ungroup() %>% 
  select(-`drhp == 0`)

total_full <- total %>% 
  group_by(region) %>% 
  nest(.key = "predictors") %>% 
  bind_cols(
    map(neighbours, nest, .key = "neighbours") %>% 
      reduce(bind_rows)
  ) %>% 
  unnest(predictors, .preserve = neighbours)

region_drhp <- total %>% 
  select(date, region, drhp) %>% 
  spread(region, drhp)
  
  
# AR4 ---------------------------------------------------------------------

library(eDMA)

ar4 <- list()
for (i in 1:length(nms)) {
  print(i)
  ar4[[i]] <- total %>%
    filter(region == nms[i]) %>%
    DMA(drhp ~ Lag(drhp, 1) + Lag(drhp, 2) + Lag(drhp, 3)+ Lag(drhp, 4),
        data = ., vKeep = c(1, 2 ,3, 4, 5), dAlpha = 1, vDelta = 1, dBeta = 1)
}

# AR4 ---------------------------------------------------------------------

tvp_ar4 <- list()
for (i in 1:length(nms)) {
  print(i)
  tvp_ar4[[i]] <- total %>%
    filter(region == nms[i]) %>%
    DMA(drhp ~ Lag(drhp, 1) + Lag(drhp, 2) + Lag(drhp, 3)+ Lag(drhp, 4),
        data = ., vKeep = c(1, 2 ,3, 4, 5), dAlpha = 0.99, dBeta = 0.96)
}

library(Hmisc)

?approxExtrap



fcst_nahed <- function(.data, model = NULL, nahead = 1) {
  nr <- nrow(.data)
  nc <- ncol(.data)
  new_lines <- matrix(c(rep(NA, nahead), rep(0.1, nahead*(nc - 1))), nrow = nahead)
  colnames(new_lines) <- colnames(.data)
  rbind(.data, new_lines)
}

x <- total %>% 
  filter(region == "EA") %>% 
  select(-date, -region) %>% 
  fcst_nahed(nahead = 1) %>% 
  DMA(drhp ~ Lag(drhp, 1) + Lag(ratio, 1) + Lag(growth, 1) + Lag(ur, 1) +
                 Lag(lf, 1) + Lag(hs, 1) + Lag(cons, 1)+ Lag(indus, 1) + Lag(rabmr, 1) +
                 Lag(spread, 1) + Lag(cci, 1) + Lag(hpu, 1), data = .,
                dAlpha = 0.99, dBeta = 0.96, vKeep = c(1, 2))

x@Est$LastForecast

# DMA ---------------------------------------------------------------------

fcst_dma <- list()
for (i in 1:length(nms)) {
  print(i)
  fcst_dma[[i]] <- total %>%
    filter(region == nms[i]) %>%
    DMA(drhp ~ Lag(drhp, 1) + Lag(ratio, 1) + Lag(growth, 1) + Lag(ur, 1) +
          Lag(lf, 1) + Lag(hs, 1) + Lag(cons, 1)+ Lag(indus, 1) + Lag(rabmr, 1) +
          Lag(spread, 1) + Lag(cci, 1) + Lag(hpu, 1), data = .,
         dAlpha = 0.99, dBeta = 0.96, vKeep = c(1, 2))
}



# MSE ---------------------------------------------------------------------

fcst_dma[[1]]@Est$

mean((fcst_dma[[1]]@data$vY - fcst_dma[[1]]@Est$vyhat)^2)
mean(residuals(fcst_dma[[1]])^2)

# compare -----------------------------------------------------------------

# summary(fcst_ar1[[1]])
# summary(fcst_ar4[[1]])
# summary(fcst_tvp_ar4[[1]])
# summary(fcst_dma[[1]])


# plot --------------------------------------------------------------------



plot_predictors <- total %>% 
  select(-drhp) %>% 
  filter(region == nms[13]) %>% 
  gather(var, value, -date, -region) %>% 
  ggplot(aes(date, value)) + 
  geom_line() +
  facet_wrap(~var, scales = "free_y") + 
  theme_bw() +
  labs(x = "", y = "") +
  theme(strip.background = element_blank())
