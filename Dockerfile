FROM rocker/geospatial:4.5.1
RUN install2.r --error --skipinstalled shiny shinyjs shinydashboard shinydashboardPlus shinyWidgets DT highcharter leaflet leaflet.extras exuber glue jsonlite zoo rmarkdown readxl here viridis RColorBrewer gridExtra zip
WORKDIR /app
COPY . /app
# shiny autoloads R/*.R: drop the data-update scripts that .rscignore keeps out of shinyapps.io too
RUN rm -f R/0[1-6]-update-*.R analysis/src-read-ntwd.R analysis/src-hpu-index.R && rm -rf analysis/archived
ENV PORT=3838 MALLOC_ARENA_MAX=2 MALLOC_TRIM_THRESHOLD_=131072
CMD ["sh", "-c", "R -e \"shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))\""]
