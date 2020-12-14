ui_report <- function() {
  tabItem(
  tabName = "report",
  fluidPage(
    fluidRow(
      column(
        width = 3,
        box(
          title = "Reports",
          width = 12,
          p("Preview and download summary, national and regional financial stability reports."),
          radioButtons(
            inputId = "report_choice",
            label = "Select a category: ",
            choiceValues = c("Summary", "United Kingdom", "regional"),
            choiceNames = c("Summary", "National Market", "Regional Markets"),
          ),
          conditionalPanel(
            "input.report_choice === 'regional'",
            radioButtons(
              inputId = "region_choice",
              selected = nms$names[1],
              choices = nms$names[-11],
  # choiceValues = nms$abbr[-11],
              label = "Select Geographical Area:")
          ),
          verbatimTextOutput("pdf"),
          verbatimTextOutput("pdf2"),

          shiny::fluidRow(
            style = "text-align:center;",
            downloadButton("report", "Download")
          )
        ),
  # box(
  #   title = "Release",
  #   width = 12,
  #   shiny::fluidRow(
  #     style = "text-align:center;",
  #     p("Download the current release in a zip file."),
  #     downloadButton("report2", "Download")
  #   )
  # )
      ),
      column(
        width = 9,
        box(
          title = "Preview",
          width = 12,
          uiOutput("pdfview")
        ),
      )
    )
  )
  )
}