library("shiny")
library("shinydashboard")
library("RCurl")
library("jsonlite")
library("igraph")
library("highcharter")
library("rvest")
library("purrr")
library("tidyr")
library("dplyr")
library("stringr")
library("leaflet")
library("htmltools")
library("stringr")
library("RColorBrewer")
library("rmarkdown")
library("webshot")
library("shinyBS")
library("shinyjs")
library("shinyWidgets")
library("ggplot2")
library("Hmisc")
library("httr")

##-- Retrieve and clean data --##

# Data from API

url <- "https://s3.amazonaws.com/bikespace-dashboard-assets/dashboard.json"
doc <- getURL(url)
api_data <- as.data.frame(fromJSON(doc, flatten = TRUE))

survey_data <- api_data[, c("dashboard.id","dashboard.problem","dashboard.latitude","dashboard.longitude",
                            "dashboard.intersection","dashboard.comments","dashboard.duration", "dashboard.time", 
                            "dashboard.pic")]

# TEMPORARY SOLUTION FOR DURATION, CHANGE WHEN UX SET
# Issue is that there are multiple values for duration, should be mutually exclusive
# Exclude entries with multiple duration values (n=7)
survey_data$dashboard.duration <- unlist(survey_data$dashboard.duration)[1:nrow(survey_data)]
survey_data <-survey_data[!grepl(",", survey_data$dashboard.duration), ]

# Also old duration categories in the data
survey_data$dashboard.duration <- if_else(grepl("hour", survey_data$dashboard.duration), "hours", survey_data$dashboard.duration)

survey_data$dashboard.duration <- capitalize(survey_data$dashboard.duration)

# Extract date and time from datetime variable
survey_data$date <- substr(survey_data$dashboard.time,1,10)
survey_data$time <- substr(survey_data$dashboard.time,12,19)

# Format date and time variables
survey_data$date <- as.Date(strptime(survey_data$date, "%Y-%m-%d"))
survey_data$time <- strptime(survey_data$time, "%H:%M:%S")

# Drop dashboard.time variables
survey_data <- survey_data[, !(colnames(survey_data) %in% c("dashboard.time"))]

# Clean problem_type field so that lists (multiple problem types) in the field are
# strings
survey_data$problem_type_collapse <- sapply(survey_data$dashboard.problem, paste, collapse="; ")

# Also replace commas with semi-colons for CSV export
survey_data$problem_type_collapse <- gsub(",", ";", survey_data$problem_type_collapse)

# Capitalize each problem type in field using function
maketitle = function(txt){
  theletters = strsplit(txt,'')[[1]]
  wh = c(1,which(theletters  == ' ') + 1)
  theletters[wh] = toupper(theletters[wh])
  paste(theletters,collapse='')
}

survey_data$problem_type_collapse <- sapply(survey_data$problem_type_collapse, maketitle)

# Replace 'Badly' with 'Abandonded'
survey_data$problem_type_collapse <- gsub("Badly", "Abandoned", survey_data$problem_type_collapse)

# Drop NA (n=1)
survey_data <- survey_data[!grepl("NA", survey_data$problem_type_collapse),]

# Create date, weekday and hour variables
survey_data$weekday <- weekdays(survey_data$date, abbreviate = TRUE)
survey_data$hour <- as.numeric(format(survey_data$time, format="%H"))

survey_data$time_group <- ifelse(survey_data$hour %in% c(7,8,9), "07-10",
                                 ifelse(survey_data$hour %in% c(10,11,12), "10-13",
                                        ifelse(survey_data$hour %in% c(13,14,15), "13-16",
                                               ifelse(survey_data$hour %in% c(16,17,18), "16-19",
                                                      ifelse(is.na(survey_data$hour), NA,"19-07")))))

# Rename variables
colnames(survey_data) <- c("id","problem_type", "problem_lat","problem_long","intersection",
                           "comment","duration","pic","date", "time","problem_type_collapse", 
                           "weekday", "hour", "time_group")

# Drop report time variable
survey_data <- survey_data[, !(colnames(survey_data) %in% c("time"))]

enableBookmarking(store = "url")