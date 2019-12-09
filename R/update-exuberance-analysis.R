library(here)

source(here("R", "00-regional-composition-src.R"))
source(here("R", "00-functions-src.R"))
source(here("R", "01-download-src.R"))
source(here("R", "02-manipulation-src.R"))

library(exuber)

# Estimation & Critical Values --------------------------------------------

radf_price <- price %>%
  radf(lag = 1, minw = 37)

radf_income <- 
  price_income %>%
  radf(lag = 1, minw = 37)

cv_price <- mc_cv(NROW(rhpi), opt_bsadf = "conservative", minw = 37)

cv_income <- mc_cv(NROW(rhp_pdi), opt_bsadf = "conservative", minw = 37)

# Summary -----------------------------------------------------------------

summary_price <- 
  radf_price %>% 
  summary(cv = cv_price)

summary_income <- 
  radf_income %>% 
  summary(cv = cv_income)

# diagnostics -------------------------------------------------------------

rejected_price <- 
  radf_price %>% 
  diagnostics(cv = cv_price) %>% 
  .$rejected

rejected_income <- 
  radf_income %>% 
  diagnostics(cv = cv_income) %>% 
  .$rejected

# datestamp ---------------------------------------------------------------

datestamp_price <- 
  radf_price %>%
  datestamp(cv = cv_price)

datestamp_income <- 
  radf_income %>%
  datestamp(cv = cv_income)

# Income ------------------------------------------------------------------

autoplot_price <- 
  radf_price %>%
  autoplot(include = TRUE, cv = cv_price, arrange = FALSE) %>%
  map( ~.x + scale_custom(object = fortify(radf_price, cv = cv_price)) +
         theme(title = element_blank()))

autoplot_income <- 
  radf_income %>%
  autoplot(include = TRUE, cv = cv_income, arrange = FALSE) %>%
  map( ~.x + scale_custom(object = fortify(radf_price, cv = cv_price)) +
         theme(title = element_blank()))

# autoplot datestamp ------------------------------------------------------

autoplot_datestamp_price <-
  datestamp_price %>%  
  autoplot(cv = cv_price) +
  scale_custom(fortify(radf_price, cv = cv_price)) + 
  scale_color_viridis_d()

autoplot_datestamp_income <- 
  datestamp_income %>% 
  autoplot(cv = cv_income) + 
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

datestamp_price <-
  radf_price %>% 
  datestamp(cv = cv_price) %>% 
  map(ds_yq)

datestamp_income <- 
  radf_income %>% 
  datestamp(cv = cv_income) %>% 
  map(ds_yq)

# Plotting ----------------------------------------------------------------

# Price
plot_price <- list()
for (i in seq_along(slider_names)) {

  shade <- datestamp_price %>% "[["(slider_names[i])

  plot_price[[i]] <- ggplot(rhpi) +
    geom_line(aes_string(x = "Date", y = as.name(slider_names[i])),
              size = 0.7, colour = "black") +
    scale_custom(object = fortify(radf_price, cv = cv_price)) +
    # geom_smooth(method = "lm", se = FALSE,
    #             aes_string("Date", as.name(slide_names[i]))) +
    theme_light() +
    theme(axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
    geom_rect(data = shade[, -3], fill = "grey", alpha = 0.35, #0.25
              aes_string(xmin = "Start", xmax = "End",
                         ymin = -Inf, ymax = +Inf))
}
names(plot_price) <- col_names(radf_price)

#Income
plot_income <- list()
for (i in seq_along(slider_names)) {
  
  shade <- datestamp_income %>% "[["(slider_names[i])
  
  plot_income[[i]] <- ggplot(rhp_pdi) +
    geom_line(aes_string(x = "Date", y = as.name(slider_names[i])),
              size = 0.7, colour = "black") +
    scale_custom(object = fortify(radf_income, cv = cv_income)) +
    # geom_smooth(method = "lm", se = FALSE,
    #             aes_string("Date", as.name(slide_names[i]))) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  if (!is.null(shade))
    plot_income[[i]] <- plot_income[[i]] + 
    geom_rect(data = shade[, -3], fill = "grey", alpha = 0.35, #0.25
              aes_string(xmin = "Start", xmax = "End",
                         ymin = -Inf, ymax = +Inf))
}
names(plot_income) <- col_names(radf_income)


# Make bsadf dataframe ----------------------------------------------------

ind <- exuber::index(radf_price, trunc = TRUE)
ind2 <- exuber::index(radf_income, trunc = TRUE)

price_bsadf_table <-
  radf_price %>%
  "[["("bsadf") %>% 
  as.tibble() %>%
  bind_cols(`Critical Values` = cv_price$bsadf_cv[-1, 2]) %>% 
  bind_cols(Date = ind) %>% 
  select(Date, `Critical Values`, UK, everything())
  
income_bsadf_table <- 
  radf_income %>%
  "[["("bsadf") %>% 
  as.tibble() %>%
  bind_cols(`Critical Values` = cv_income$bsadf_cv[-1, 2]) %>% 
  bind_cols(Date = ind2) %>% 
  select(Date, `Critical Values`, UK, everything())


cv <- crit[[NROW(rhpi)]]

stat_table <- 
  tibble(
    Regions = slider_names,
    gsadf_rhpi = radf_price$gsadf,
    gsadf_hpi_dpi = radf_income$gsadf,
    gsadf_cv90 = cv$gsadf_cv[1],
    gsadf_cv95 = cv$gsadf_cv[2],
    gsadf_cv99 = cv$gsadf_cv[3]
  )


# store -------------------------------------------------------------------
items <- c("price", "income")
store <- c("price", "price_income",
           # glue::glue("estimation_{items}"),
           glue::glue("cv_{items}"),
           glue::glue("autoplot_datestamp_{items}"),
           glue::glue("radf_{items}"))

# store <- c("plot_price", "autoplot_price", "plot_income", "autoplot_income",
#            "price_bsadf_table", "income_bsadf_table", "stat_table")

path_store <- paste0("data/RDS/", store, ".rds")

for (i in seq_along(store)) saveRDS(get(store[i]), file = path_store[i])



# Not Used ----------------------------------------------------------------


# 
# 
# #### Value Box 1
# gsadf_stat <- radf_reg %>%
#   pluck("gsadf")
# gsadf_cv <- crit[[NROW(regional_price)]] %>%
#   pluck("gsadf_cv") %>%
#   "["(2)
# gsadf_colour <- ifelse(gsadf_stat > gsadf_cv, "red", "green")
# 
# gsadf <- data.frame(tstat = gsadf_stat, crit = gsadf_cv, colour = gsadf_colour,
#                     stringsAsFactors = FALSE) %>%
#   rownames_to_column("region") %>% as.tibble()
# ####
# 
# 
# ### Value Box2
# bsadf_stat <- radf_reg %>%
#   pluck("bsadf") %>%
#   as.tibble() %>%
#   slice(nrow(.)) %>%
#   t()
# 
# bsadf_cv <- crit[[NROW(regional_price)]] %>%
#   pluck("bsadf_cv") %>%
#   as.tibble() %>%
#   slice(nrow(.)) %>%
#   "["(2) %>%
#   "[["(1)
# 
# bsadf_colour <- ifelse(bsadf_stat > bsadf_cv, "red", "green")
# exub <- ifelse(bsadf_stat > bsadf_cv, "Exuberance", "No Exuberance")
# 
# bsadf <- data.frame(tstat = bsadf_stat, crit = bsadf_cv, colour = bsadf_colour,
#                     exub = exub, stringsAsFactors = FALSE) %>%
#   rownames_to_column("region") %>% as.tibble()
# ###
# 
# output$value1 <- renderValueBox({
#   valueBox(
#     gsadf %>% dplyr::filter(region == input$country) %>% 
#       dplyr::select(tstat) %>% "[["(1) %>% formatC(),
#     "GSADF",
#     icon = icon("stats",lib = 'glyphicon'),
#     color = gsadf %>% dplyr::filter(region == input$country) %>% 
#       dplyr::select(colour))  
# })
# 
# output$value2 <- renderValueBox({
#   valueBox(
#     bsadf %>% dplyr::filter(region == input$country) %>% 
#       dplyr::select(tstat) %>% "[["(1) %>% formatC(),
#     bsadf %>% dplyr::filter(region == input$country) %>% 
#       dplyr::select(exub) %>% "[["(1),
#     icon = icon("stats",lib = 'glyphicon'),
#     color = bsadf %>% dplyr::filter(region == input$country) %>% 
#       dplyr::select(colour))  
# })


# 
# # Aggregate & Panel -------------------------------------------------------
# 
# 
# sb <- regional_price %>% 
#   select(-UK) %>% 
#   sb_cv()
# 
# autoplot_panel <- regional_price %>% 
#   select(-UK) %>% 
#   radf() %>% 
#   autoplot(cv = sb)
# 
# autoplot_datestamp <-  regional_price %>% 
#   select(-UK) %>% 
#   radf() %>% 
#   datestamp() %>% 
#   fortify() %>% 
#   ggplot(aes_string(colour = "key")) + # 
#   geom_segment(aes_string(x = "Start", xend = "End",
#                           y = "key", yend = "key"), size = 7) +
#   ylab("") + xlab("") + theme_bw() +
#   theme(panel.grid.major.y = element_blank(),
#         legend.position = "none",
#         plot.margin = margin(1, 1, 0, 0, "cm"),
#         axis.text.y = element_text(face = "bold", size = 8, hjust = 0)) +
#   scale_color_viridis_d()