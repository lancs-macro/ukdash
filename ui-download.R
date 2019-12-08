tabItem(
  tabName = "download",
  fluidPage(
    navlistPanel(
      well = TRUE,
      widths = c(3, 9),
      "Available Datasets",
      "Raw Data",
      tabPanel("Real House Prices", icon = icon("angle-double-right"), 
               box(width = 12, title = "Preview Data", dataTableOutput("dataOutput"))),
      tabPanel("Real House Price to Income", icon = icon("angle-double-right")),
      "Financial Stability",
      tabPanel("Real House Prices", icon = icon("angle-double-right")),
      tabPanel("Real House Price to Income", icon = icon("angle-double-right")),
      tabPanel("Critical Values", icon = icon("angle-double-right")),
      "Uncertainty",
      tabPanel("House Price Uncertainty", icon = icon("angle-double-right")),
      tabPanel("Economic Policy Uncertainty", icon = icon("angle-double-right")),
      "New House Price Index",
      tabPanel("NUTS 1", icon = icon("angle-double-right")),
      tabPanel("NUTS 2", icon = icon("angle-double-right")),
      tabPanel("NUTS 3", icon = icon("angle-double-right")), 
      "---------",
      tabPanel("Archive", icon = icon("angle-double-right"))
    )
  ),
  includeHTML("content/footer.html")
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