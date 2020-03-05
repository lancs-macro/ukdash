library(tidyverse)
library(shiny)
library(shinydashboard)

updt <- list.files("R", pattern = "update-", full.names = TRUE)
purrr::map(updt, source)

rsconnect::deployApp(
  appDir = "~/Housing Observatory/uk-housing-observatory-dashboard", 
  appFileManifest = "C:/Users/T460p/AppData/Local/Temp/7f43-118c-fb8a-005e", 
  account = "lancs-macro", 
  server = "shinyapps.io", 
  appName = "uk-housing-observatory-dashboard", 
  appId = 1589658, launch.browser = function(url) {
    message("Deployment completed: ", url)
    }, 
  lint = FALSE, 
  metadata = list(asMultiple = FALSE, asStatic = FALSE), 
  logLevel = "verbose")
