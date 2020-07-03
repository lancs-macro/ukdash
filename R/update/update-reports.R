
load_rds <- function() {
  path_store_rds <- list.files("data/RDS", full.names = TRUE)
  store_rds <-  stringr::str_remove(list.files("data/RDS"), ".rds")
  for (i in seq_along(path_store_rds)) {
    assign(store_rds[i], readRDS(file = path_store_rds[i]), envir = .GlobalEnv)
  }
}

clear_meta <- function(filename) {
  texfile <- paste0("www/reports/", gsub(".pdf", ".tex", filename))
  if (file.exists(texfile)) {
    fs::file_delete(texfile)
  }
  logfile <- paste0("www/reports/", gsub(".pdf", ".log", filename))
  if (fs::file_exists(logfile)) {
    fs::file_delete(logfile)
  }
}

update_reports <- function(summary = TRUE, fstability = TRUE) {
  
  load_rds()
  vers <-  release_date
  
  # Load files --------------------------------------------------------------
  if (isTRUE(summary)) {
    source("R/src/src-hpu-index.R")
    stats <- update_statistics()
    filename <- paste0("UKHO-Summary-", gsub(" ", "", vers), ".html")
    report_rmd <- file.path("www/reports", "report-summary.Rmd")
    params <- list(
      vers = gsub(" ", ":", vers),
      uk_price = stats$uk_price,
      london_price = stats$london_price,
      uk_afford = stats$uk_afford,
      london_afford = stats$london_afford,
      plot_growth_UK_price = plot_growth_UK_price,
      plot_growth_UK_afford = plot_growth_UK_afford,
      autoplot_datestamp_price = autoplot_datestamp_price,
      autoplot_datestamp_afford = autoplot_datestamp_afford,
      hpu_index = hpu_index
    )
    rmarkdown::render(
      report_rmd, 
      output_file = filename, 
      params = params,
      envir = new.env(parent = globalenv())
    )
    clear_meta(filename)
  }
  
  # Financial Stability Reports ---------------------------------------------
  
  if (isTRUE(fstability)) {
    crit <-  tail(bsadf_table_price$`Critical Values`, 1)
    for (region in nms$names) {
      region_name <- gsub(" |&", "", region)
      filename <- paste0("UKHO-", region_name, "-", gsub(" ", "", vers), ".pdf")
      report_rmd <- file.path("www/reports", "report.Rmd")
      
      # Set up parameters to pass to Rmd document
      params <- list(
        region = region, 
        vers =  gsub(" ", ":", vers),
        price = price,
        afford = afford,
        plot_price = plot_price,
        plot_afford = plot_afford,
        bsadf_table_price = bsadf_table_price,
        bsadf_table_afford = bsadf_table_afford,
        autoplot_price = autoplot_price,
        autoplot_afford = autoplot_afford,
        crit = crit
      )
      # render the document  
      rmarkdown::render(
        report_rmd, 
        output_file = filename, 
        params = params,
        envir = new.env(parent = globalenv())
      )
      clear_meta(filename)
    }
  }
}
