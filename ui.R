library("shiny")
library("shinydashboard")
library("RCurl")
library("jsonlite")
library("highcharter")
library("rvest")
library("purrr")
library("tidyr")
library("dplyr")
library("leaflet")
library("htmltools")
library("stringr")
library("RColorBrewer")
library("rmarkdown")
library("webshot")
library("shinyBS")
library("shinyjs")
library("shinyWidgets")

dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <-  tags$a(tags$img(src='header_logo.png',height='35',width='175'))

PDF_DownloadButton <- function(outputId, label = " Download PDF"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("file", lib = "font-awesome"), label)
}

function(request){
  dashboardPage(skin = "black",
                dbHeader,
                dashboardSidebar(
                  useShinyjs(),
                  sidebarMenu(id = "sidebarmenu",
                              menuItem("Data Filters",icon = icon("search"),
                                       div(id="form",
                                           dateRangeInput("daterange", "Date range:",
                                                          start = min(survey_data$date, na.rm = TRUE),
                                                          min = min(survey_data$date, na.rm = TRUE),
                                                          max = max(survey_data$date, na.rm = TRUE),
                                                          end = max(survey_data$date, na.rm = TRUE),
                                                          format = "mm/dd/yy",
                                                          separator = " - ")),
                                       pickerInput("duration_select","Duration Length:","",multiple = TRUE, options = list(`none-selected-text` = 'Choose one or several')),
                                       pickerInput("probtype_select","Problem type:","",multiple = TRUE, options = list(`none-selected-text` = 'Choose one or several')),
                                       br(),
                                       actionButton("filter_click", "SEARCH", width = '190px', 
                                                    style="color: #b5c7cf; background-color: #202d33; border-color: #ffffff"),
                                       actionButton("reset_click", "RESET FILTERS", width = '190px',
                                                    style="color: #b5c7cf; background-color: #202d33; border-color: #ffffff"),
                                       br()
                              ),
                              menuItem("Export",
                                       icon = icon("save-file", lib = "glyphicon"),
                                       br(),
                                       tags$div(id="download1",
                                                downloadButton("csv_download", "Download CSV")
                                       ),
                                       br(),
                                       bookmarkButton(label = "Get Permalink", 
                                                      style='margin:0px; color: #b5c7cf; background-color: #202d33; border-color: #ffffff'),
                                       br(),
                                       tags$div(id="download2", 
                                                PDF_DownloadButton("pdf_download")),
                                       br()
                                       )
                                       )),
                dashboardBody(
                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                            tags$style(HTML(".main-header .sidebar-toggle:before {
                                            content: '\\f002';}"))),
                  fluidRow(
                    column(width = 3,
                           valueBox(textOutput("total_reports"), HTML(paste("Total Reports",br(), textOutput("date_range"))), 
                                    icon = icon("bicycle"), color = "green",
                                    width = NULL),
                           box(title = "Problem Type Frequency", width = NULL, solidHeader = TRUE, collapsible = TRUE,
                               highchartOutput("ptbar", height = 130)),
                           box(title = "Frequency by Day", width = NULL, solidHeader = TRUE, collapsible = TRUE,
                               highchartOutput("weekcol", height = 100)),
                           box(title = "Bike Parking Demand by Time of Day (24h)", width = NULL, solidHeader = TRUE, collapsible = TRUE,
                               highchartOutput("heat_map", height = 140))
                    ),
                    column(width = 9,
                           box(title = NULL, width = NULL, solidHeader = TRUE,
                               leafletOutput("map", height = 740)) 
                    )
                  )
                  )
                  )
  }