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
  


# Helper fcst -------------------------------------------------------------

create_nahed <- function(.data, model = NULL, nahead = 1) {
  .data <- .data[, -c(1,2)]
  nr <- nrow(.data)
  nc <- ncol(.data)
  new_lines <- matrix(c(rep(NA, nahead), rep(0.1, nahead*(nc - 1))), nrow = nahead)
  colnames(new_lines) <- colnames(.data)
  rbind(.data, new_lines)
}

# libraries ---------------------------------------------------------------

library(eDMA)
library(Hmisc)


# ar ----------------------------------------------------------------------


ar1 <- list()
for (i in 1:length(nms)) {
  print(i)
  ar1[[i]] <- total %>%
    filter(region == nms[i]) %>%
    create_nahed(nahead = 1) %>% 
    DMA(drhp ~ Lag(drhp, 1),
        data = ., vKeep = c(1, 2), dAlpha = 1, vDelta = 1, dBeta = 1)
}
# 149
fcst_ar1 <- sapply(ar1, function(x) getLastForecast(x)$PointForecast)
names(fcst_ar1) <- nms


ar4 <- list()
for (i in 1:length(nms)) {
  print(i)
  ar4[[i]] <- total %>%
    filter(region == nms[i]) %>%
    create_nahed() %>% 
    DMA(drhp ~ Lag(drhp, 1) + Lag(drhp, 2) + Lag(drhp, 3)+ Lag(drhp, 4),
        data = ., vKeep = c(1, 2 ,3, 4, 5), dAlpha = 1, vDelta = 1, dBeta = 1)
}

fcst_ar4 <- sapply(ar4, function(x) getLastForecast(x)$PointForecast)
names(fcst_ar4) <- nms

# AR4 ---------------------------------------------------------------------

tvp_ar4 <- list()
for (i in 1:length(nms)) {
  print(i)
  tvp_ar4[[i]] <- total %>%
    filter(region == nms[i]) %>%
    create_nahed() %>% 
    DMA(drhp ~ Lag(drhp, 1) + Lag(drhp, 2) + Lag(drhp, 3)+ Lag(drhp, 4),
        data = ., vKeep = c(1, 2 ,3, 4, 5), dAlpha = 0.99, dBeta = 0.96)
}

fcst_tvp_ar4 <- sapply(tvp_ar4, function(x) getLastForecast(x)$PointForecast)
names(fcst_tvp_ar4) <- nms




x <- total %>% 
  filter(region == "EA") %>% 
  select(-date, -region) %>% 
  fcst_nahed(nahead = 1) %>% 
  DMA(drhp ~ Lag(drhp, 1) + Lag(ratio, 1) + Lag(growth, 1) + Lag(ur, 1) +
                 Lag(lf, 1) + Lag(hs, 1) + Lag(cons, 1)+ Lag(indus, 1) + Lag(rabmr, 1) +
                 Lag(spread, 1) + Lag(cci, 1) + Lag(hpu, 1), data = .,
                dAlpha = 0.99, dBeta = 0.96, vKeep = c(1, 2))


# DMS ---------------------------------------------------------------------



# DMA ---------------------------------------------------------------------

dma95 <- list()
for (i in 1:length(nms)) {
  print(i)
  dma95[[i]] <- total %>%
    filter(region == nms[i]) %>%
    create_nahed() %>% 
    DMA(drhp ~ Lag(drhp, 1) + Lag(ratio, 1) + Lag(growth, 1) + Lag(ur, 1) +
          Lag(lf, 1) + Lag(hs, 1) + Lag(cons, 1)+ Lag(indus, 1) + Lag(rabmr, 1) +
          Lag(spread, 1) + Lag(cci, 1) + Lag(hpu, 1), data = ., vDelta = 0.95,
        dAlpha = 0.99, dBeta = 0.96, vKeep = c(1, 2))
}

fcst_dma95 <- sapply(dma95, function(x) getLastForecast(x)$PointForecast)
names(fcst_dma95) <- nms


dma99 <- list()
for (i in 1:length(nms)) {
  print(i)
  dma99[[i]] <- total %>%
    filter(region == nms[i]) %>%
    create_nahed() %>% 
    DMA(drhp ~ Lag(drhp, 1) + Lag(ratio, 1) + Lag(growth, 1) + Lag(ur, 1) +
          Lag(lf, 1) + Lag(hs, 1) + Lag(cons, 1)+ Lag(indus, 1) + Lag(rabmr, 1) +
          Lag(spread, 1) + Lag(cci, 1) + Lag(hpu, 1), data = ., vDelta = 0.99,
        dAlpha = 0.99, dBeta = 0.96, vKeep = c(1, 2))
}

fcst_dma99 <- sapply(dma99, function(x) getLastForecast(x)$PointForecast)
names(fcst_dma95) <- nms

dma <- list()
for (i in 1:length(nms)) {
  print(i)
  dma[[i]] <- total %>%
    filter(region == nms[i]) %>%
    create_nahed() %>% 
    DMA(drhp ~ Lag(drhp, 1) + Lag(ratio, 1) + Lag(growth, 1) + Lag(ur, 1) +
          Lag(lf, 1) + Lag(hs, 1) + Lag(cons, 1)+ Lag(indus, 1) + Lag(rabmr, 1) +
          Lag(spread, 1) + Lag(cci, 1) + Lag(hpu, 1), data = ., 
        dAlpha = 0.99, dBeta = 0.96, vKeep = c(1, 2))
}

fcst_dma <- sapply(dma, function(x) getLastForecast(x)$PointForecast)
names(fcst_dma95) <- nms




# compare -----------------------------------------------------------------

tibble(
  regions = nms,
  ar1 = fcst_ar1,
  ar4 = fcst_ar4,
  tvp_ar4 = fcst_tvp_ar4,
  dma95 = fcst_dma95,
  dma99 = fcst_dma99,
  dma = fcst_dma
) %>% 
  select(3,4,5,6) %>% 
  rowMeans()


# MSE ---------------------------------------------------------------------

fcst_dma[[1]]@Est$
  
  mean((fcst_dma[[1]]@data$vY - fcst_dma[[1]]@Est$vyhat)^2)
mean(residuals(fcst_dma[[1]])^2)

# plot --------------------------------------------------------------------



plot_predictors <- total %>% 
  select(-drhp) %>% 
  filter(region == nms[13]) %>% 
  gather(var, value, -date, -region) %>% 
  ggplot(aes(date, value)) + 
  geom_line() +
  facet_wrap(~var, scales = "free_y", ncol = 4) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    strip.text.x = element_text(
      hjust = 0.5, size = rel(1.2), face = "bold",
      margin = margin(0, 0, .5, 0, "cm")),
    strip.background = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = "grey20"),
    panel.grid = element_line(colour = "grey92"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", size = 0.7)
    )

saveRDS(plot_predictors, "data/RDS/predictors.Rds")
