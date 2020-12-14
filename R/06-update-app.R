update_app <- function(git = FALSE, msg = NULL) {
  options(
    repos = structure(
      c(CRAN = 'https://cran.rstudio.com/'), RStudio = TRUE), 
    download.file.method = 'wininet')
  options(rsconnect.check.certificate = TRUE)
  
  appDir <- usethis::proj_path()
  manifestLines <- rsconnect:::bundleFiles(appDir)
  excludeFiles <- grep("update", manifestLines)
  appLines <- manifestLines[-excludeFiles]
  
  rsconnect::deployApp(
    appDir = appDir,      
    appFiles = appLines,
    account = "lancs-macro", 
    server = "shinyapps.io", 
    appName = "uk-housing-observatory-dashboard", 
    appId = 1589658, 
    launch.browser = function(url) {
      message("Deployment completed: ", url)
    }, 
    lint = FALSE, 
    forceUpdate = TRUE,
    metadata = list(asMultiple = FALSE, asStatic = FALSE), 
    logLevel = "verbose")
  
  if (isTRUE(git_commit)) {
    require(git2r)
    repo <- git2r::repository(appDir)
    add(repo, ".")
    if (is.null(msg)) {
      msg <- glue::glue("update to version {release_date}") 
    }
    commit(repo, message = msg)
    push(repo, credentials = cred_token()) ##ssh path
  }
} 


