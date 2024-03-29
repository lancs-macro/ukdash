
library(shiny)
library(ggplot2)
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
    popoverTag <- div(
      class = "box-tools pull-right", 
      tags$button(
        class = paste0("btn btn-box-tool"), 
        `title` = popover_title,
        `data-content` = popover_content,
        `data-trigger` = "focus",
        `data-placement` = "right",
        # `data-html` = "true",
        `data-toggle` = data_toggle, shiny::icon("info"))
    )
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

note_exuber <- 
  HTML('<span>There is exuberance when the </span> <span class="textbf"> solid line </span> <span> surpasses the </span><span class="color-red"> dashed line </span>.')

note_ds <- 
  HTML('Periods of time identified as exuberant by the financial stability analysis.')

note_shade <- 
  HTML('<span class="color-grey">Shaded areas</span> <span>indicate identified periods of exuberance.</span>')

note_bands <- 
  HTML('<span>The </span> <span class="color-grey">shaded bands </span><span> refer to the difference between the top and bottom decile of growth rates across all regions in the UK.</span>')

  
column_4 <- function(...) {
  column(width = 4, ...)
} 


icon_growth_box <- function(x) {
  if (x > 0) {
    return(icon("arrow-up"))
  }else{
    return(icon("arrow-down"))
  }
}

icon_exuberanec_box <- function(x, crit) {
  if (x > crit) {
    return(icon("exclamations"))
  }else{
    return(icon("flag"))
  }
}

text_exuberance_box <- function(x, crit) {
  if (x > crit) {
    return(HTML("<b>Exuberance</b>"))
  }else{
    return(HTML("<b>No Exuberance</b>"))
  }
}

DT_preview <- function(x, title = NULL) {
  box2(width = 12, title = title, dataTableOutput(x))
}

tab_panel <- function(x, title, prefix = "") {
  tabPanel(title, icon = icon("angle-double-right"), DT_preview(x, title = paste0(prefix, title)))
}


# Simple -----------------------------------------------------------------

ldiff <- function(x, n = 4) {
  log(x) - dplyr::lag(log(x), n = n)
} 

calc_growth <- function(x) {
  ldiff(x) %>% 
    tail(1) %>% 
    round(4) %>% 
    `*`(100)
}

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

custom_date <- function(object, variable, div) {
  yq <- extract_yq(object)
  seq_slice <- seq(1, NROW(yq), length.out = div)
  yq %>% 
    slice(as.integer(seq_slice)) %>% 
    pull(!!rlang::parse_expr(variable))
}

scale_custom <- function(object, div = 7) {
  require(lubridate)
  scale_x_date(
    breaks = custom_date(object, variable = "breaks", div = div),
    labels = custom_date(object, variable = "labels", div = div)
  )
}


# Plot Normal Series ------------------------------------------------------

my_theme <- theme_light() +
  theme(
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank() ,
    panel.grid.major = element_line(linetype = "dashed")
  )


# Datestamp into yq
to_yq <- function(ds, radf_var, cv_var){
  idx <- tibble(Date = index(radf_var, trunc = FALSE))
  index_yq <- extract_yq(idx)
  
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

ggarrange = function(...) {
  do.call(gridExtra::grid.arrange, c(...))
}

# datatable-DT ------------------------------------------------------------


specify_buttons <- function(filename) {
  list(
    list(
      extend = "collection",
      buttons =
        list(
          list(
            extend = 'csv',
            filename = filename, 
            exportOptions = list(
              modifier = list(
                page = "all", 
                search = 'none')
              )
            ),
          list(
            extend = 'excel',
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
      autoWidth = TRUE,
      paging = TRUE,
      pageLength = 12,
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
  DT::datatable(
    x,
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',#'Blfrtip',
      searching = FALSE,
      autoWidth = TRUE,
      pageLength = 12,
      paging = TRUE,
      scrollX = F,
      # columnDefs = list(list(targets = c(0), width = "80px")),
      buttons = specify_buttons(filename)
    )
  ) %>%
    DT::formatRound(2:NCOL(x), 3) 
}



# Get data from uk-house-prices -------------------------------------------

ukhp_get <- function(release = "2020-Q3", frequency = "monthly", classification = "nuts1") {
    endpoint <- "https://raw.githubusercontent.com/lancs-macro/hopi/master/data"
    query <- paste(endpoint, release, frequency, paste0(classification, ".csv"), sep = "/")
    readr::read_csv(query)
}


plot_ukhp_index <- function(hp_data, hp_data_agg, .y) {
  hp_data %>% 
    right_join(hp_data_agg, by = "Date") %>% 
    pivot_longer(-Date) %>% 
    filter(name %in% c(.y, "England and Wales")) %>% 
    mutate(name = fct_relevel(name, "England and Wales", .y)) %>% 
    ggplot(aes(Date, value)) +
    geom_line(aes(colour = name), size = 0.9) + 
    theme_bw() +
    scale_custom(hp_data) +
    scale_color_manual(
      values = c("black", "#B22222")) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid = element_line(linetype = 2),
      axis.title = element_blank()
    ) 
}

plot_ukhp_growth <- function(hp_data, hp_data_agg, .y) {
  hp_data %>% 
    right_join(hp_data_agg, by = "Date") %>% 
    pivot_longer(-Date) %>% 
    filter(name %in% c(.y, "England and Wales")) %>% 
    mutate(value = ldiff(value, n = 4)*100) %>% 
    drop_na() %>% 
    ggplot(aes(Date, value)) +
    geom_line(aes(colour = name), size = 0.9) + 
    ylab("Year on Year (%)") +
    theme_bw() +
    scale_custom(hp_data) +
    scale_color_manual(
      values = c("black", "#B22222")) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.title.x = element_blank(),
      panel.grid = element_line(linetype = 2)
    )
}



# utils -------------------------------------------------------------------

capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "", collapse = " ")
}
