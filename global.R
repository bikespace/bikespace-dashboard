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
survey_data$problem_type_collapse <- sapply(survey_data$problem_type, paste, collapse="; ")

# Clean up report_time variable
# Create date, weekday and hour variables
survey_data$report_time <- strptime(survey_data$report_time, "%Y-%m-%d %H:%M")

survey_data$date <- as.Date(survey_data$report_time)
survey_data$weekday <- weekdays(survey_data$report_time, abbreviate = TRUE)
survey_data$minute <- format(survey_data$report_time, format = "%M")
survey_data$hour <- ifelse(survey_data$minute < 30,
                           as.numeric(format(survey_data$report_time, format="%H")),
                           as.numeric(format(survey_data$report_time, format="%H"))+1)
survey_data$time_group <- ifelse(survey_data$hour %in% c(7,8,9), "07-09",
                                 ifelse(survey_data$hour %in% c(10,11,12), "10-12",
                                        ifelse(survey_data$hour %in% c(13,14,15), "13-15",
                                               ifelse(survey_data$hour %in% c(16,17,18), "16-18",
                                                      ifelse(is.na(survey_data$hour), NA,"19+")))))

# Drop report_time variable
survey_data <- survey_data[, !(colnames(survey_data) %in% c("report_time"))]

enableBookmarking(store = "url")