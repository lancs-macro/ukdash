
# Exuber ------------------------------------------------------------------

library(exuber)
source("R/src-functions.R")
source("R/src-read.R")

# Create variables in appropriate format ----------------------------------

price <- ntwd_data %>% 
  select(Date, region, rhpi) %>% 
  spread(region, rhpi)

afford <- ntwd_data %>% 
  select(Date, region, afford) %>% 
  spread(region, afford)

# Estimation & Critical Values --------------------------------------------

library(exuber)

radf_price <- price %>%
  radf(lag = 1, minw = 37)

radf_afford <- afford %>%
  radf(lag = 1, minw = 37)

cv_price <- mc_cv(NROW(price), opt_bsadf = "conservative", minw = 37)

cv_afford <- mc_cv(NROW(afford), opt_bsadf = "conservative", minw = 37)

# Summary -----------------------------------------------------------------

summary_price <- 
  radf_price %>% 
  summary(cv = cv_price)

summary_afford <- 
  radf_afford %>% 
  summary(cv = cv_afford)

# diagnostics -------------------------------------------------------------

rejected_price <- 
  radf_price %>% 
  diagnostics(cv = cv_price) %>% 
  .$rejected

rejected_afford <- 
  radf_afford %>% 
  diagnostics(cv = cv_afford) %>% 
  .$rejected

# datestamp ---------------------------------------------------------------

datestamp_price <- 
  radf_price %>%
  datestamp(cv = cv_price)

datestamp_afford <- 
  radf_afford %>%
  datestamp(cv = cv_afford)

# afford ------------------------------------------------------------------

autoplot_price <- 
  radf_price %>%
  autoplot(include = TRUE, cv = cv_price, arrange = FALSE) %>%
  map( ~.x + 
         scale_custom(object = fortify(radf_price, cv = cv_price)) +
         theme(
           panel.grid = element_line(linetype = 2),
           panel.grid.minor = element_blank(),
           title = element_blank()))

autoplot_afford <- 
  radf_afford %>%
  autoplot(include = TRUE, cv = cv_afford, arrange = FALSE) %>%
  map( ~.x + scale_custom(object = fortify(radf_price, cv = cv_price)) +
         theme(
           panel.grid = element_line(linetype = 2),
           panel.grid.minor = element_blank(),
           title = element_blank()))

# autoplot datestamp ------------------------------------------------------

autoplot_datestamp_price <-
  datestamp_price %>%  
  autoplot(cv = cv_price) +
  scale_custom(fortify(radf_price, cv = cv_price)) + 
  scale_color_viridis_d()

autoplot_datestamp_afford <- 
  datestamp_afford %>% 
  autoplot(cv = cv_afford) + 
  scale_custom(fortify(radf_price, cv = cv_price)) + 
  scale_color_viridis_d()

# Overwrite datestamp --------------------------------------------------------

index_yq <- extract_yq(fortify(radf_price, cv = cv_price)) # Remake into yq

ds_yq <- function(ds) {
  start <- ds[, 1]
  start_ind <- which(index_yq$breaks %in% start)
  start_label <- index_yq[start_ind ,2]
  
  end <- ds[, 2]
  end_ind <- which(index_yq$breaks %in% end)
  if (anyNA(end)) end_ind <- c(end_ind, NA)
  end_label <- index_yq[end_ind ,2]
  
  ds[, 1] <- start_label 
  ds[, 2] <- end_label
  ds
}

datestamp_price_mod <-
  datestamp_price %>% 
  map(ds_yq)

datestamp_afford_mod <- 
  datestamp_afford %>% 
  map(ds_yq)

# Plotting ----------------------------------------------------------------

ind <- exuber::index(radf_price, trunc = TRUE)
ind2 <- exuber::index(radf_afford, trunc = TRUE)

# Price
plot_price <- list()
for (i in seq_along(nms$names)) {
  
  shade <- datestamp_price %>% "[["(nms$names[i])
  
  plot_price[[i]] <- 
    filter(price, Date >= ind[1]) %>% 
    ggplot() +
    geom_line(aes_string(x = "Date", y = as.name(nms$names[i])),
              size = 0.7, colour = "black") +
    scale_custom(object = fortify(radf_price, cv = cv_price)) +
    # geom_smooth(method = "lm", se = FALSE,
    #             aes_string("Date", as.name(slide_names[i]))) +
    theme_light() +
    theme(
      axis.title = element_blank(),
      panel.grid = element_line(linetype = 2),
      panel.grid.minor = element_blank(),
      title = element_blank()) +
    geom_rect(data = shade[, -3], fill = "grey", alpha = 0.35, #0.25
              aes_string(xmin = "Start", xmax = "End",
                         ymin = -Inf, ymax = +Inf))
}
names(plot_price) <- col_names(radf_price)

#afford
plot_afford <- list()
for (i in seq_along(nms$names)) {
  
  shade <- datestamp_afford %>% "[["(nms$names[i])
  
  plot_afford[[i]] <- ggplot(afford) +
    geom_line(aes_string(x = "Date", y = as.name(nms$names[i])),
              size = 0.7, colour = "black") +
    scale_custom(object = fortify(radf_afford, cv = cv_afford)) +
    # geom_smooth(method = "lm", se = FALSE,
    #             aes_string("Date", as.name(slide_names[i]))) +
    theme_light() +
    theme_light() +
    theme(
      axis.title = element_blank(),
      panel.grid = element_line(linetype = 2),
      panel.grid.minor = element_blank(),
      title = element_blank()) 
  
  if (!is.null(shade))
    plot_afford[[i]] <- plot_afford[[i]] + 
    geom_rect(data = shade[, -3], fill = "grey", alpha = 0.35, #0.25
              aes_string(xmin = "Start", xmax = "End",
                         ymin = -Inf, ymax = +Inf))
}
names(plot_afford) <- col_names(radf_afford)


# Make bsadf dataframe ----------------------------------------------------

bsadf_table_price <-
  radf_price %>%
  "[["("bsadf") %>% 
  as.tibble() %>%
  bind_cols(`Critical Values` = cv_price$bsadf_cv[-1, 2]) %>% 
  bind_cols(Date = ind) %>% 
  select(Date, `Critical Values`, `United Kingdom`, everything())

bsadf_table_afford <- 
  radf_afford %>%
  "[["("bsadf") %>% 
  as.tibble() %>%
  bind_cols(`Critical Values` = cv_afford$bsadf_cv[-1, 2]) %>% 
  bind_cols(Date = ind2) %>% 
  select(Date, `Critical Values`, `United Kingdom`, everything())


cv <- crit[[NROW(price)]]

stat_table <- 
  tibble(
    Regions = nms$names,
    gsadf_rhpi = radf_price$gsadf,
    gsadf_hpi_dpi = radf_afford$gsadf,
    gsadf_cv90 = cv$gsadf_cv[1],
    gsadf_cv95 = cv$gsadf_cv[2],
    gsadf_cv99 = cv$gsadf_cv[3]
  )


# Overview Graphs ---------------------------------------------------------


growth_rates_price <- 
  price %>% 
  modify_at(vars(-Date), ldiff, n = 4) %>% 
  drop_na() 

quantiles_price <- growth_rates_price %>% 
  gather(region, value, -Date) %>% 
  group_by(Date) %>% 
  summarise(
    q10 = quantile(value, probs = c(0.10)),
    q90 = quantile(value, probs = c(0.90))
  )

plot_growth_UK_price <- ggplot() +
  geom_line(data = growth_rates_price, aes(Date, `United Kingdom`)) +
  geom_ribbon(data = quantiles_price,
              aes(x = Date, ymin = q10, ymax = q90), fill = "#174b97", alpha = 0.5) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_line(linetype = 2),
    panel.grid.minor = element_blank(),
    title = element_blank()) 


growth_rates_afford <- 
  afford %>% 
  modify_at(vars(-Date), ldiff, n = 4) %>% 
  drop_na() 

quantiles_afford <- growth_rates_afford %>% 
  gather(region, value, -Date) %>% 
  group_by(Date) %>% 
  summarise(
    q10 = quantile(value, probs = c(0.10)),
    q90 = quantile(value, probs = c(0.90))
  )

plot_growth_UK_afford <- ggplot() +
  geom_line(data = growth_rates_afford, aes(Date, `United Kingdom`)) +
  geom_ribbon(data = quantiles_afford,
              aes(x = Date, ymin = q10, ymax = q90), fill = "#174b97", alpha = 0.5) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_line(linetype = 2),
    panel.grid.minor = element_blank(),
    title = element_blank()) 

# store -------------------------------------------------------------------

library(glue)

items <- c("price", "afford")
store <- c(
  items, 
  glue("cv_{items}"),
  glue("plot_growth_UK_{items}"),
  glue("plot_{items}"),
  glue("autoplot_{items}"),
  glue("bsadf_table_{items}"),
  glue("autoplot_datestamp_{items}"), 
  glue("radf_{items}"))
path_store <- paste0("data/RDS/", store, ".rds")

for (i in seq_along(store)) saveRDS(get(store[i]), file = path_store[i], compress = "xz")










