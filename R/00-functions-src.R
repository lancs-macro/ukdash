library(rlang)

box2 <- function(..., title = NULL, subtitle = NULL, footer = NULL, status = NULL, 
                  solidHeader = FALSE, background = NULL, width = 6, height = NULL, 
                  popover = FALSE, popover_title = NULL, popover_content = NULL,
                  data_toggle = "popover", collapsible = FALSE, collapsed = FALSE) 
{
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    shinydashboard:::validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    shinydashboard:::validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", title)
  }
  subtitleTag <- NULL
  if (!is.null(title)) {
    subtitleTag <- h5(class = "box-subtitle", subtitle)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- div(class = "box-tools pull-right", 
                       tags$button(class = paste0("btn btn-box-tool"), 
                                   `data-widget` = "collapse", shiny::icon(collapseIcon)))
  }
  popoverTag <- NULL
  if (popover) {
    popoverTag <- div(class = "box-tools pull-right", 
                       tags$button(class = paste0("btn btn-box-tool"), 
                                   `title` = popover_title,
                                   `data-content` = popover_content,
                                   `data-trigger` = "focus",
                                   `data-placement` = "right",
                                   # `data-html` = "true",
                                   `data-toggle` = data_toggle, shiny::icon("info")))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag) || !is.null(popoverTag)) {
    headerTag <- div(class = "box-header", titleTag, subtitleTag, collapseTag, popoverTag)
  }
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style)) 
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer)) 
        div(class = "box-footer", footer)))
}

exuber_note <- HTML('<span>There is exuberance when the </span> <span class="color-blue"> solid line </span> <span> surpasses the </span><span class="color-red"> dashed line </span>.')


column_4 <- function(...) column(width = 4, ...)

# Custom Labels  ----------------------------------------------------------

extract_yq <- function(object) {
  yq <- object %>% 
    select_if(lubridate::is.Date) %>% 
    setNames("Date") %>% 
    mutate(Quarter = lubridate::quarter(Date),
           Year = lubridate::year(Date)) %>% 
    tidyr::unite(labels, c("Year", "Quarter"), sep = " Q") %>% 
    rename(breaks = Date)
}

scale_custom <- function(object, div = 7) {
  require(lubridate)
  custom_date <- function(object, variable, div) {
    
    yq <- extract_yq(object)
    seq_slice <- seq(1, NROW(yq), length.out = div)
    yq %>% 
      slice(seq_slice) %>% 
      pull(!!parse_expr(variable))
  }
  
  scale_x_date(
    breaks = custom_date(fortify(object), variable = "breaks", div = div),
    labels = custom_date(fortify(object), variable = "labels", div = div)
  )
}

# Datestamp into yq

to_yq <- function(ds, radf_var, cv_var){
  
  index_yq <- extract_yq(fortify(radf_var, cv =  cv_var))
  
  ds_yq <- function(ds) {
    start <- ds[, 1]
    start_ind <- which(index_yq$breaks %in% start)
    start_label <- index_yq[start_ind ,2]
    
    end <- ds[, 2]
    end_ind <- which(index_yq$breaks %in% end)
    if (anyNA(end)) end_ind <- c(end_ind, NA)
    end_label <- index_yq[end_ind ,2]
    
    ds[, 1] <- start_label 
    ds[, 2] <- end_label
    ds
  }
  
  ds %>% 
    ds_yq()
}

# datatable-DT ------------------------------------------------------------


specify_buttons <- function(filename) {
  list(
    list(
      extend = "collection",
      buttons =
        list(
          list(extend = 'csv',
               filename = filename
               , exportOptions  =
                 list(
                   modifier = 
                     list(
                       page = "all",
                       search = 'none')
                 )
          ),
          list(extend = 'excel',
               filename = filename,
               title = "UK Housing Observatory")
        ),
      text = "Download"
    )
  )
}

make_DT <- function(x, filename, caption_string = ""){
  DT::datatable(
    x,
    rownames = FALSE,
    caption = caption_string,
    extensions = 'Buttons',
    options = list( 
      dom = 'Bfrtip', #'Blfrtip'
      searching = FALSE,
      pageLength = 13,
      autoWidth = TRUE,
      paging = TRUE,
      # scrollY = T,
      scrollX = T,
      columnDefs = list(
        list(
          targets = c(0), width = "80px")),
      buttons = specify_buttons(filename)
      )) %>%
    DT::formatRound(2:NCOL(x), 3) 
}

make_DT_general <- function(x, filename) {
  DT::datatable(x,
                rownames = FALSE,
                extensions = 'Buttons',
                options = list(dom = 'Bfrtip',#'Blfrtip',
                               searching = FALSE,
                               autoWidth = TRUE,
                               paging = TRUE,
                               scrollX = F,
                               # columnDefs = list(list(targets = c(0), width = "80px")),
                               buttons = specify_buttons(filename)
                )
  ) %>%
    DT::formatRound(2:NCOL(x), 3) 
}


# make_DT <- function(x, filename, caption_string = ""){
#   DT::datatable(x,
#                   # filter(Date >= as.Date(input$daterange[1]) & 
#                   #          Date <= as.Date(input$daterange[2])),
#                 rownames = FALSE,
#                 caption = caption_string,
#                 extensions = 'Buttons',
#                 options = list( dom = 'Bfrtip',#'Blfrtip',
#                                 searching = FALSE,
#                                 autoWidth = TRUE,
#                                 paging = FALSE,
#                                 pageLength = 20,#NROW(x),
#                                 # scrollY = T,
#                                 scrollX = T,
#                                 columnDefs = list(
#                                   list(
#                                     targets = c(0), width = "80px")),
#                                 buttons =  list(
#                                   list(
#                                     extend = "collection",
#                                     buttons = list(
#                                       list(extend = 'csv',
#                                            filename = filename),
#                                       list(extend = 'excel',
#                                            filename = filename,
#                                            title = "UK Housing Observatory")
#                                     ),
#                                     text = "Download"
#                                   )
#                                 )
#                 )
#   ) %>%
#     formatRound(2:NCOL(x), 3) 
# }

# make_DT_general <- function(x, filename) {
#   DT::datatable(x,
#                 rownames = FALSE,
#                 extensions = 'Buttons',
#                 options = list(dom = 'Bfrtip',#'Blfrtip',
#                                 searching = FALSE,
#                                 autoWidth = TRUE,
#                                 paging = FALSE,
#                                 pageLength = NROW(x),
#                                 scrollX = F,
#                                 # columnDefs = list(list(targets = c(0), width = "80px")),
#                                 buttons = list(
#                                   list(
#                                     extend = "collection",
#                                     buttons = list(list(extend = 'csv',
#                                                         filename = filename),
#                                                    list(extend = 'excel',
#                                                         filename = filename)),
#                                     text = "Download"
#                                   )
#                                 )
#                 )
#   ) %>%
#     formatRound(2:NCOL(x), 3) 
# }