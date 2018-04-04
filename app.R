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

##-- Retrieve and clean data --##

# Data from API

url <- "https://torontobikeparking-staging.herokuapp.com/api/survey?format=json"
doc <- getURL(url)
api_data <- fromJSON(doc, flatten = TRUE)

survey_data <- api_data[, c("survey.report_time","survey.comment", "survey.problem_type", 
                            "survey.duration", "latitude", "longitude", "survey.location.lat",
                            "survey.location.lng")]

# Rename lat and long vars
colnames(survey_data) <- c("report_time", "comment", "problem_type", "duration",
                           "problem_lat", "problem_long", "major_lat", "major_long")

# Clean problem_type field so that lists (multiple problem types) in the field are
# strings
survey_data$problem_type_collapse <- sapply(survey_data$problem_type, paste, collapse=", ")

# Clean up report_time variable
# Create date, weekday and hour variables
survey_data$report_time <- strptime(survey_data$report_time, "%Y-%m-%d %H:%M")

survey_data$date <- as.Date(survey_data$report_time)
survey_data$weekday <- weekdays(survey_data$report_time)
survey_data$minute <- format(survey_data$report_time, format = "%M")
survey_data$hour <- ifelse(survey_data$minute < 30,
                           as.numeric(format(survey_data$report_time, format="%H")),
                           as.numeric(format(survey_data$report_time, format="%H"))+1)
survey_data$time_group <- ifelse(survey_data$hour %in% c(6,7,8,9), "6am - 9am",
                                 ifelse(survey_data$hour %in% c(10,11,12), "10am - 12pm",
                                        ifelse(survey_data$hour %in% c(13,14,15,16), "1pm - 4pm",
                                               ifelse(survey_data$hour %in% c(17,18,19), "5pm - 7pm",
                                                      ifelse(is.na(survey_data$hour), NA,"8pm +")))))

# Drop report_time variable
survey_data <- survey_data[, !(colnames(survey_data) %in% c("report_time"))]

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Bikespace Analytics Dashboard", titleWidth = 325),
                    dashboardSidebar(
                      uiOutput('date_filter'),
                      uiOutput('duration_filter'),
                      uiOutput('probtype_filter'),
                      br(),
                      actionButton("filter_click", "Filter Data", icon("indent"), width = '105px'),
                      br(),
                      actionButton("reset_click", "Reset Filters", icon("refresh"))),
                    dashboardBody(
                      tags$head(tags$style(HTML('
                                                .box-body {
                                                padding-top: 0px;
                                                }'))),
    fluidRow(
      column(width = 4,
             box(title = "Problem Type Frequency", width = NULL, solidHeader = TRUE, collapsible = TRUE,
                 highchartOutput("ptbar", height = 150)),
             box(title = "Weekday Frequency", width = NULL, solidHeader = TRUE, collapsible = TRUE,
                 highchartOutput("weekcol", height = 150)),
             box(title = "Time vs. Parking Duration", width = NULL, solidHeader = TRUE, collapsible = TRUE,
                 highchartOutput("heat_map", height = 150))
      ),
      column(width = 8,
             box(title = "User-Submitted Issues", width = NULL, solidHeader = TRUE,
                 leafletOutput("map", height = 575)) 
      )
                      )
                      )
)

server <- function(input, output) {
  
  ## -- Sidebar Inputs -- ##
  
  output$date_filter <- renderUI({
    times <- input$reset_click
    div(id=letters[(times %% length(letters)) + 1],
        dateRangeInput("daterange", "Date range:",
                       start = min(survey_data$date, na.rm = TRUE),
                       min = min(survey_data$date, na.rm = TRUE),
                       max = max(survey_data$date, na.rm = TRUE),
                       end = max(survey_data$date, na.rm = TRUE),
                       format = "mm/dd/yy",
                       separator = " - "))
  })
  
  output$duration_filter <- renderUI({
    times <- input$reset_click
    div(id=letters[(times %% length(letters)) + 1],
        selectInput("duration_select", "Duration Length:", 
                    choices = c("Choose one or multiple" = "",
                                unique(survey_data[survey_data$date >= input$daterange[1] & survey_data$date <= input$daterange[2], "duration"])),
                    multiple = TRUE)) 
  })
  
  output$probtype_filter <- renderUI({
    times <- input$reset_click
    div(id=letters[(times %% length(letters)) + 1],
        selectInput("probtype_select", "Problem type:", 
                    choices = c("Choose one or multiple" = "",
                                unique(unlist(survey_data[survey_data$date >= input$daterange[1] & survey_data$date <= input$daterange[2] & grepl(paste(input$duration_select, collapse = "|"),survey_data$duration), "problem_type"]))),
                    multiple = TRUE))
  })
  
  ## -- Set up Data to Respond to Filters -- ##
  
  # Set up reactive data format
  values <- reactiveValues()
  # Load in global data as default
  values$data <- survey_data
  
  # Create dataframes for each filter
  
  # Date input
  dateinput_df <- reactive({
    survey_data %>%
      filter(date >= input$daterange[1]) %>%
      filter(date <= input$daterange[2])
  })
  
  # Parking duration  
  durinput_df <- eventReactive(input$filter_click,{
    if(is.null(input$duration_select)){
      dateinput_df()
    } else{
      dateinput_df() %>%
        filter(duration %in% input$duration_select)
    }
  })
  
  # Problem type  
  probinput_df <- eventReactive(input$filter_click,{
    if(is.null(input$probtype_select)){
      durinput_df()
    } else{
      durinput_df() %>%
        filter(grepl(paste(input$probtype_select, collapse="|"), problem_type_collapse))
    }
  })
  
  # Update data when "Filter Data" is clicked  
  newFilter <- observeEvent(input$filter_click, {
    values$data <- probinput_df()
  })
  
  # Set data back to global when "Reset" is clicked
  newReset <- observeEvent(input$reset_click, {
    values$data <- survey_data
  })
  
  ##-- Map Visualization --#
  
  # Create label for each marker on the map
  marker_labels <- reactive({sprintf(
    "Problem Type: %s<br/>Duration: %s<br/>Comment: %s",
    values$data[,"problem_type_collapse"], values$data[,"duration"], values$data[,"comment"]
  ) %>% lapply(htmltools::HTML)
  })
  
  # Parameters for MapBox basemap
  street_map <- "https://api.mapbox.com/styles/v1/arielag/cjf5ulybg2lh82rna8n6s8egz/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYXJpZWxhZyIsImEiOiJjamY1dTlseDYxZHB0Mnlsbndsb3BkaTV5In0.SiiSe0JU0cXc6sqeLA4Hcg"
  
  map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a>"
  
  output$map <- renderLeaflet({
    leaflet(values$data) %>% 
      addTiles(urlTemplate = street_map, attribution = map_attr) %>%
      addMarkers(lng = ~problem_long, lat = ~problem_lat, 
                 clusterOptions = markerClusterOptions(), label=marker_labels()) %>%
      setView(lng = -79.3892, lat = 43.6426, zoom = 12)
  })
  
  ##-- Problem Type Bar Chart --#
  probtype_df <- reactive({
    unlist_df <- data.frame(unlist(values$data[,"problem_type"]))
    colnames(unlist_df) <- c("problem_type")
    unlist_df
  })
  
  n_colors <- reactive({
    length(unique(probtype_df()$problem_type))
  })
  
  # Count problem type and convert to data series for chart
  ptbar_data <- reactive({
    if(n_colors() > 2){
      probtype_df() %>%
        count(problem_type) %>%
        ungroup() %>% 
        arrange(desc(n)) %>%
        mutate(x = row_number()) %>%
        mutate(color = brewer.pal(n_colors(), "Paired")) %>%
        rename(name = problem_type,
               y = n) %>% 
        select(y, name, color) %>% 
        list.parse3()
    } else if(n_colors() == 2){
      probtype_df() %>%
        count(problem_type) %>%
        ungroup() %>% 
        arrange(desc(n)) %>%
        mutate(x = row_number()) %>%
        mutate(color = c("#A6CEE3","#1F78B4")) %>%
        rename(name = problem_type,
               y = n) %>% 
        select(y, name, color) %>% 
        list.parse3()
    } else{
      probtype_df() %>%
        count(problem_type) %>%
        ungroup() %>% 
        arrange(desc(n)) %>%
        mutate(x = row_number()) %>%
        mutate(color = c("#A6CEE3")) %>%
        rename(name = problem_type,
               y = n) %>% 
        select(y, name, color) %>% 
        list.parse3()
    }
  })
  
  # Problem type bar chart
  output$ptbar <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type="category") %>%
      hc_yAxis(title = "Blank") %>% 
      hc_add_series(data = ptbar_data(), type = "bar", showInLegend = FALSE,
                    name = "Frequency")
  })
  
  ##-- Weekday Column Chart --##
  
  # Create dataframe to hold data for all possible weekdays
  weekcol_df <- data.frame(c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday"))
  colnames(weekcol_df) <- c("weekday")
  
  # Create frequency count from data by weekday, dropping NAs
  weekcol_data <- reactive({
    values$data %>%
      count(weekday) %>%
      ungroup() %>%
      drop_na()
  })
  
  # Join data with dataframe for all weekdays
  weekcol_df2 <- reactive({
    merge(weekcol_df, weekcol_data(), all.x = TRUE, by=c("weekday"))
  })
  
  # Convert the dataframe to a series for plotting
  weekcol_chart_data <- reactive({
    weekcol_df2() %>%
      replace_na(list(weekday = NA, n = 0)) %>%
      arrange(factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday"))) %>%
      mutate(x = row_number()) %>%
      rename(name = weekday, 
             y = n) %>% 
      select(y, name) %>% 
      list.parse3()
  })
  
  # Create the frequency plot
  output$weekcol <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type="category") %>%
      hc_yAxis(title = "Blank") %>% 
      hc_add_series(data = weekcol_chart_data(), type = "column", showInLegend = FALSE,
                    name = "Frequency")
  })
  
  ##-- Time v Duration Heat Map --##
  
  # Generate dataframe with all possible time v. duration combinations
  hours <- c("6am - 9am", "10am - 12pm", "1pm - 4pm", "5pm - 7pm", "8pm +")
  heat_hours <- sort(rep(hours,3))
  heat_duration <- rep(c("short", "med", "long"), 5)
  
  heat_df <- data.frame(heat_hours, heat_duration)
  colnames(heat_df) <- c("time_group", "duration")
  
  # Create frequency count by hour and parking duration from data, dropping NAs
  heat_count <- reactive({
    values$data %>%
      group_by(time_group, duration) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      drop_na()
  })
  
  # Merge the frequncy count with the dataframe with all hours and parking duration combinations  
  heat_data <- reactive({
    merge(heat_df, heat_count(), all.x = TRUE, by=c("time_group", "duration"))
  })
  
  heat_data_2 <- reactive({
    heat_data() %>%
      replace_na(list(time_group = NA, duration = NA, count = 0)) %>%
      arrange(factor(time_group, levels = c("6am - 9am", "10am - 12pm", "1pm - 4pm",
                                            "5pm - 7pm", "8pm +")), 
              factor(duration, levels = c("short", "med", "long")))
  })
  
  # Prepare values for the axes of the heat map
  y <- c("Short", "Medium", "Long")
  x <- c("6am - 9am", "10am - 12pm", "1pm - 4pm", "5pm - 7pm", "8pm +")
  
  # Convert the dataframe to a series for plotting
  heat_data_series <- reactive({
    heat_data_2() %>%
      mutate(yid = rep(c(0,1,2),5),
             xid = sort(rep(seq(1:5)-1,3))) %>%
      select(xid, yid, count) %>%
      list.parse2()
  })
  
  
  # Create a custom tooltip for the heat map
  fntltp <- JS("function(){
               return '<b>Time of Day: '+ this.series.xAxis.categories[this.point.x] +
               '<br></b><b>Parking Duration: ' + this.series.yAxis.categories[this.point.y] +
               '<br></b><b>Frequency: ' + this.point.value + '</b>';
               ; }")
  
  # Create color gradient for the heat map
  freq_colr <- list(list(0, '#2E86C1'),
                    list(0.5, '#F8F5F5'),
                    list(1, '#FF5733'))
  
  # Create the heat map
  output$heat_map <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "heatmap") %>% 
      hc_xAxis(categories = x) %>% 
      hc_yAxis(categories = y) %>% 
      hc_add_series(data = heat_data_series()) %>% 
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )) %>% 
      hc_tooltip(formatter = fntltp) %>% 
      hc_legend(align = "right", layout = "vertical",
                margin = 0, verticalAlign = "top",
                symbolHeight = 500, itemMarginTop = 5) %>% 
      hc_colorAxis(stops= freq_colr, showInLegend = FALSE)
  })
  
  }

shinyApp(ui, server)