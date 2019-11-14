tabItem(
  tabName = "exuberance",
  
  fluidRow(
    column(
      width = 4,
      h3("Exuberance Indicators"),
      p("The figures below display the real house prices and the affordability index (left) and the corresponding exuberance indicator (right) for the selected geographical location. There is exuberance when the statistic (blue line) exceeds the critical value (red line).")
    ),
    
    column(
      width = 3,
      div(
        class = "center",
        selectInput(
          inputId = "country",
          choices = slider_names,
          selected = slider_names[2],
          label = "Select Geographical Area:")
      )
    )#,
    # column(
    #   width = 5,
    #   div(
    #     class = "regional",
    #     h3("Regional Compostion"),
    #     textOutput("composition")
    #   )
    # )
  ),
  
  fluidRow(
    
    column(
      width = 4,
      box(
        title = "Real House Prices", 
        width = 12,
        plotOutput("plot_price"))
      ),
    column(
      width = 4,
      box(title = "Affordability Index",
          plotOutput("autoplot_price"),
          width = 12)),
    column(
      width = 4,
      box(
        solidHeader = FALSE,
        title = "Summary",
        background = NULL,
        width = 12,
        status = "danger",
        footer = fluidRow(
          column(
            width = 6,
            descriptionBlock(
              number = "17%", 
              number_color = "green", 
              number_icon = "fa fa-caret-up",
              header = "$35,210.43", 
              text = "Price", 
              right_border = TRUE,
              margin_bottom = FALSE
            )
          ),
          column(
            width = 6,
            descriptionBlock(
              number = "18%", 
              number_color = "red", 
              number_icon = "fa fa-caret-down",
              header = "1200", 
              text = "BSADF", 
              right_border = FALSE,
              margin_bottom = FALSE
            )
          )
        )
      ),
      # valueBox(width = 6, 2.5, "GSADF", color = "red", icon = icon("arrow-up")),
      # valueBox(width = 6, 0, "GSADF", color = "black", icon = icon("arrow-down")),
      # valueBoxOutput("box_gsadf_price"),
      # valueBoxOutput("box_bsadf_price"),
      box(
        title = "Date-stamping",
        solidHeader = FALSE,
        width = 12,
        dataTableOutput("table3")
      ))
  ),
  fluidRow(
    box(title = "Real House Prices", 
        width = 4,
        plotOutput("plot_income")),
    box(
      width = 4,
      dataTableOutput("table4")
    ),
    box(title = "Affordability Index",
        plotOutput("autoplot_income"),
        width = 4)
  )
)