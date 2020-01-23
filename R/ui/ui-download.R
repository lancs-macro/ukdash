tabItem(
  tabName = "download",
  fluidPage(
    navlistPanel(
      well = TRUE,
      widths = c(3, 9),
      "Data",
      # "---------",
      tab_panel("DT_price", "Real House Prices"),
      tab_panel("DT_afford", "Affordability Index"),
      "Financial Stability",
      tab_panel("DT_stat_table", "Exuberance Statistics and Critical Values (GSADF)", prefix = "Financial Stability: "),
      tab_panel("DT_bsadf_price", "Real House Prices Exuberance Statistics (BSADF)", prefix = "Financial Stability: "),
      tab_panel("DT_bsadf_afford", "Affordability Index Exuberance Statistics (BSADF)", prefix = "Financial Stability: "),
      "Uncertainty",
      tab_panel("DT_hpu", "House Price Uncertainty"),
      "New Price Indices - HOPI",
      tab_panel("DT_nuts1", "Level 1", "New Price Indices: "),
      tab_panel("DT_nuts2", "Level 2", "New Price Indices: "),
      tab_panel("DT_nuts3", "Level 3", "New Price Indices: ")#,
      # "---------",
      # tabPanel("Archive", icon = icon("angle-double-right"), 
      #          box2(width = 12, includeHTML("www/archive-table.html")))
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