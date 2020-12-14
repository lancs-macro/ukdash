# Preliminarires

update <- function() {
  devtools::load_all()
  source("analysis/src-read-ntwd.R")
  # update_files <- list.files("R/update", pattern = "update-", full.names = TRUE)
  # invisible(lapply(update_files, source))
  
  # run individually --------------------------------------------------------
  
  update_download(hopi = FALSE, boundaries = FALSE, cpi_epu = TRUE)
  
  update_exuber(save_rds = TRUE)
  
  update_statistics(save_graph = TRUE, write_json = TRUE)
  
  update_reports()
  
  update_app(TRUE)
}

