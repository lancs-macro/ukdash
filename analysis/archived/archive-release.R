
update_archive <- function() {
  path_store_rds <- list.files("data/RDS", full.names = TRUE)
  store_rds <-  stringr::str_remove(list.files("data/RDS"), ".rds")
  for (i in seq_along(path_store_rds)) {
    assign(store_rds[i], readRDS(file = path_store_rds[i]))
  }
  suppressMessages({
    source("R/src/src-functions.R", local = TRUE)$value
    source("R/src/src-read-ntwd.R", local = TRUE)$value
    source("R/src/src-hpu-index.R", local = TRUE)$value
  })
  release <- gsub(" ", "-", release_date)
  path <- paste0("archive/docs/uk/", release)
  
  if (!fs::dir_exists(path)) {
    fs::dir_create(path)
  }
  # data-archive 
  write_csv_temp <- function(x, filename) {
    datafile <- paste0(path, "/", filename, ".csv")
    readr::write_csv(x, datafile)
  }
  
  items <- 
    list(price, afford, stat_table, bsadf_table_price, bsadf_table_afford, 
         hpu_index, nuts1_data, nuts2_data, nuts3_data)
  item_names <- c("data-prices", "data-afford", "fs-gsadf", "fs-bsadf-prices", "fs-bsadf-afford", 
                  "unc-hpu", "hopi-nuts1", "hopi-nuts2", "hopi-nuts3")
  map2(items, item_names, write_csv_temp)
  
  pdf_path <- paste0("archive/reports/uk/", release)
  if (!fs::dir_exists(pdf_path)) {
    fs::dir_create(pdf_path)
  }
  
  # report-archive
  pdf_files_full <- list.files("www/reports/", pattern = ".pdf", full.names = TRUE)
  pdf_files <- list.files("www/reports/", pattern = ".pdf")
  fs::file_copy(pdf_files_full, paste0(pdf_path,"/", pdf_files))
}
