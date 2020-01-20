tabItem(
  tabName = "download",
  fluidPage(
    navlistPanel(
      well = TRUE,
      widths = c(3, 9),
      "Available Datasets",
      "Raw Data",
      tab_panel("DT_price", "Real House Prices"),
      tab_panel("DT_afford", "Affordability Index"),
      "Financial Stability",
      tab_panel("DT_stat_table", "GSADF Statistics and Critical Values", prefix = "Financial Stability: "),
      tab_panel("DT_bsadf_price", "Real House Prices Exuberance Statistics", prefix = "Financial Stability: "),
      tab_panel("DT_bsadf_afford", "Affordability Index Exuberance Statistics", prefix = "Financial Stability: "),
      "Uncertainty",
      tab_panel("DT_hpu", "House Price Uncertainty"),
      tab_panel("DT_epu", "Economic Policy Uncertainty"),
      "New House Price Index",
      tab_panel("DT_nuts1", "NUTS 1", "New House Price Index: "),
      tab_panel("DT_nuts2", "NUTS 2", "New House Price Index: "),
      tab_panel("DT_nuts3", "NUTS 3", "New House Price Index: "),
      "---------",
      tabPanel("Archive", icon = icon("angle-double-right"), 
               box2(width = 12, includeHTML("www/archive-table.html")))
    )
  ),
  includeHTML("www/footer.html")
)



# .navbar-brand {
#   font-size: 16px;
#   font-weight: 800;
# }

# ul.nav.nav-pills.nav-stacked {
#   line-height: 0;
# }

# .nav-stacked>li.active>a, .nav-stacked>li.active>a:hover {
#   border-left-color: red;
# }

# .nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {
#   background-color: red;
# }