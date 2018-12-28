library(exuber)

source("scripts/00-regional-composition.R")
download_primary <- FALSE
source("scripts/01-download.R")
source("scripts/02-manipulation.R")
# source("do-not-run/exuber2.R")

# cv_custom <- readRDS(file = "do-not-run/cv_custom.Rds")
cv_price <- mc_cv(NROW(rhpi), opt_bsadf = "conservative", minw = 37)
cv_income <- mc_cv(NROW(rhp_pdi), opt_bsadf = "conservative", minw = 37)
# Price -------------------------------------------------------------------

radf_price <- rhpi %>%
  radf(lag = 1, minw = 37)

datestamp_price <- 
  radf_price %>%
  datestamp(cv = cv_price)

autoplot_price <- 
  radf_price %>%
  autoplot(include = TRUE, cv = cv_price) %>%
  map( ~.x + theme(title = element_blank()))


# Income ------------------------------------------------------------------

radf_income <- 
  rhp_pdi %>%
  radf(lag = 1, minw = 37)

datestamp_income <- 
  radf_income %>%
  datestamp(cv = cv_income)

autoplot_income <- 
  radf_income %>%
  autoplot(include = TRUE, cv = cv_income) %>%
  map( ~.x + theme(title = element_blank()))


# Plotting ----------------------------------------------------------------

# Price
plot_price <- list()
for (i in seq_along(slider_names)) {

  shade <- datestamp_price %>% "[["(slider_names[i])

  plot_price[[i]] <- ggplot(rhpi) +
    geom_line(aes_string(x = "Date", y = as.name(slider_names[i])),
              size = 0.7, colour = "black") +
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

store <- c("plot_price", "autoplot_price", "plot_income", "autoplot_income",
           "price_bsadf_table", "income_bsadf_table", "stat_table")

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