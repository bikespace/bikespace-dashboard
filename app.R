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
url <- "https://app.bikespace.ca/api/survey?format=json"
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
survey_data$problem_type <- sapply(survey_data$problem_type, paste, collapse=", ")

# Clean up report_time variable
# Create date, weekday and hour variables
survey_data$report_time <- strptime(survey_data$report_time, "%Y-%m-%d %H:%M")

survey_data$date <- as.Date(survey_data$report_time)
survey_data$weekday <- weekdays(survey_data$report_time)
survey_data$hour <- format(survey_data$report_time, format="%H")

# Drop report_time variable
survey_data <- survey_data[, !(colnames(survey_data) %in% c("report_time"))]

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Bikespace Analytics Dashboard", titleWidth = 325),
  dashboardSidebar(
    dateRangeInput("daterange", "Date range:",
                                    min = min(survey_data$date, na.rm = TRUE),
                                    max = max(survey_data$date, na.rm = TRUE),
                                    format = "mm/dd/yy",
                                    separator = " - "),
    selectInput("duration_select", "Duration Length:", 
                choices = c("Choose one or multiple" = "",unique(survey_data$duration)),
                multiple = TRUE),
    selectInput("probtype_select", "Problem type:", 
                choices = c("Choose one or multiple" = "",unique(survey_data$problem_type)),
                multiple = TRUE)),
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
  
  ##-- Map Visualization --#
  
  # Create label for each marker on the map
  marker_labels <- sprintf(
    "Problem Type: %s<br/>Duration: %s<br/>Comment: %s",
    survey_data$problem_type, survey_data$duration, survey_data$comment
  ) %>% lapply(htmltools::HTML)
  
  # Parameters for MapBox basemap
  street_map <- "https://api.mapbox.com/styles/v1/arielag/cjf5ulybg2lh82rna8n6s8egz/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYXJpZWxhZyIsImEiOiJjamY1dTlseDYxZHB0Mnlsbndsb3BkaTV5In0.SiiSe0JU0cXc6sqeLA4Hcg"
  
  map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a>"
  
  output$map <- renderLeaflet({
    leaflet(survey_data) %>% 
      addTiles(urlTemplate = street_map, attribution = map_attr) %>%
      addMarkers(lng = ~problem_long, lat = ~problem_lat, 
                 clusterOptions = markerClusterOptions(), label=marker_labels) %>%
      setView(lng = -79.3892, lat = 43.6426, zoom = 12)
  })
  
  ##-- Problem Type Bar Chart --#
  
  ptbar_data <- survey_data %>%
    count(problem_type) %>%
    ungroup() %>% 
    arrange(desc(n)) %>% 
    filter(n>2) %>%
    mutate(x = row_number()) %>%
    mutate(color = brewer.pal(5, "Paired")) %>%
    rename(name = problem_type,
           y = n) %>% 
    select(y, name, color) %>% 
    list.parse3()
  
  output$ptbar <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type="category") %>%
      hc_yAxis(title = "Blank") %>% 
      hc_add_series(data = ptbar_data, type = "bar", showInLegend = FALSE,
                    name = "Frequency")
  })
  
  ##-- Weekday Column Chart --##
  
  # Create dataframe to hold data for all possible weekdays
  weekcol_df <- data.frame(c("Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday"))
  colnames(weekcol_df) <- c("weekday")
  
  # Create frequency count from data by weekday, dropping NAs
  weekcol_data <- survey_data %>%
    count(weekday) %>%
    ungroup() %>%
    drop_na()
  
  # Join data with dataframe for all weekdays
  weekcol_df <- merge(weekcol_df, weekcol_data, all.x = TRUE, by=c("weekday"))
  
  # Convert the dataframe to a series for plotting
  weekcol_chart_data <- weekcol_df %>%
    replace_na(list(weekday = NA, n = 0)) %>%
    arrange(factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                       "Thursday", "Friday", "Saturday"))) %>%
    mutate(x = row_number()) %>%
    rename(name = weekday, 
           y = n) %>% 
    select(y, name) %>% 
    list.parse3()
  
  # Create the frequency plot
  output$weekcol <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type="category") %>%
      hc_yAxis(title = "Blank") %>% 
      hc_add_series(data = weekcol_chart_data, type = "column", showInLegend = FALSE,
                    name = "Frequency")
  })
  
  ##-- Time v Duration Heat Map --##
  
  # Generate dataframe with all possible time v. duration combinations
  hours <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
             "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
             "20", "21", "22", "23")
  heat_hours <- sort(rep(hours,3))
  heat_duration <- rep(c("short", "med", "long"), 24)
  
  heat_df <- data.frame(heat_hours, heat_duration)
  colnames(heat_df) <- c("hour", "duration")
  
  # Create frequency count by hour and parking duration from data, dropping NAs
  heat_count <- survey_data %>%
    group_by(hour, duration) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    drop_na()
  
  # Merge the frequncy count with the dataframe with all hours and parking duration combinations  
  heat_data <- merge(heat_df, heat_count, all.x = TRUE, by=c("hour", "duration"))
  
  heat_data <- heat_data %>%
    replace_na(list(hour = NA, duration = NA, count = 0)) %>%
    arrange(hour, factor(duration, levels = c("short", "med", "long")))
  
  # Prepare values for the axes of the heat map
  x <- c("Short", "Medium", "Long")
  y <- c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00",
         "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00",
         "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00",
         "24:00")
  
  # Convert the dataframe to a series for plotting
  heat_data_series <- heat_data %>%
    mutate(xid = rep(c(0,1,2),24),
           yid = sort(rep(seq(1:24)-1,3))) %>%
    select(xid, yid, count) %>%
    list.parse2()
  
  # Create a custom tooltip for the heat map
  fntltp <- JS("function(){
               return '<b>Parking Duration: '+ this.series.xAxis.categories[this.point.x] +
               '<br></b><b> Time of Day: ' + this.series.yAxis.categories[this.point.y] +
               '<br></b><b> Frequency: ' + this.point.value + '</b>';
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
      hc_add_series(data = heat_data_series) %>% 
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE)
        )) %>% 
      hc_tooltip(formatter = fntltp) %>% 
      hc_legend(align = "right", layout = "vertical",
                margin = 0, verticalAlign = "top",
                symbolHeight = 500, itemMarginTop = 5) %>% 
      hc_colorAxis(stops= freq_colr)
  })

}

shinyApp(ui, server)