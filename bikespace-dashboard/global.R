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

url <- "https://torontobikeparking-staging.herokuapp.com/api/survey?format=json"
doc <- getURL(url)
api_data <- fromJSON(doc, flatten = TRUE)

survey_data <- api_data[, c("survey.happening", "survey.problem_type", 
                            "latitude", "longitude", "survey.comment")]

survey_data$datetime <- unlist(sapply(survey_data$survey.happening, function (x) x$date))

# TEMPORARY SOLUTION FOR DURATION, CHANGE WHEN UX SET
# Issue is that there are multiple values for duration, should be mutually exclusive
survey_data$duration <- unlist(sapply(survey_data$survey.happening, function (x) x$time))[1:nrow(survey_data)]

# Also old duration categories in the data
survey_data$duration <- if_else(grepl("hour", survey_data$duration), "hours",
                                if_else(grepl("overnight", survey_data$duration), "overnight",
                                        survey_data$duration))

survey_data$duration <- capitalize(survey_data$duration)

# Extract date and time from datetime variable
survey_data$date <- substr(survey_data$datetime,1,10)
survey_data$time <- substr(survey_data$datetime,12,19)

# Format date and time variables
survey_data$date <- as.Date(strptime(survey_data$date, "%Y-%m-%d"))
survey_data$time <- strptime(survey_data$time, "%H:%M:%S")

# TEMPORARY SOLUTION FOR DATE, CHANGE WHEN UX SET
# Issue is that dates occuring after present date are present
survey_data <- survey_data[survey_data$date <= Sys.Date(),]

# Drop happening and datetime variables
survey_data <- survey_data[, !(colnames(survey_data) %in% c("survey.happening", "datetime"))]

# TEMPORARY SOLUTION FOR PROBLEM_TYPE, CHANGE WHEN UX SET
# Remove problem types not part of current app, and take out NULL values
prob_type_exclude <- c("badly", "vandalized", "broken", "unusable")

survey_data <- survey_data[!grepl(paste(prob_type_exclude, collapse="|"), survey_data$survey.problem_type) 
                           & survey_data$survey.problem_type != "NULL",]

# Clean problem_type field so that lists (multiple problem types) in the field are
# strings
survey_data$problem_type_collapse <- sapply(survey_data$survey.problem_type, paste, collapse="; ")

# Capitalize each problem type in field using function
maketitle = function(txt){
  theletters = strsplit(txt,'')[[1]]
  wh = c(1,which(theletters  == ' ') + 1)
  theletters[wh] = toupper(theletters[wh])
  paste(theletters,collapse='')
}

survey_data$problem_type_collapse <- sapply(survey_data$problem_type_collapse, maketitle)

# Create date, weekday and hour variables
survey_data$weekday <- weekdays(survey_data$date, abbreviate = TRUE)
survey_data$hour <- as.numeric(format(survey_data$time, format="%H"))

survey_data$time_group <- ifelse(survey_data$hour %in% c(7,8,9), "07-10",
                                 ifelse(survey_data$hour %in% c(10,11,12), "10-13",
                                        ifelse(survey_data$hour %in% c(13,14,15), "13-16",
                                               ifelse(survey_data$hour %in% c(16,17,18), "16-19",
                                                      ifelse(is.na(survey_data$hour), NA,"19-07")))))

# Rename variables
colnames(survey_data) <- c("problem_type", "problem_lat","problem_long","comment","duration",
                           "date", "time","problem_type_collapse", "weekday", "hour", "time_group")

# Drop report time variable
survey_data <- survey_data[, !(colnames(survey_data) %in% c("time"))]

enableBookmarking(store = "url")