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
library("ggplot2")

function(input, output, session) {
  
  ## -- Sidebar Inputs -- ##
  
  duration_choices <- reactive({
    unique(survey_data[survey_data$date >= input$daterange[1] & survey_data$date <= input$daterange[2], "duration"])
  })
  
  observe({
    updatePickerInput(session, "duration_select", choices = c(duration_choices()))
  })
  
  probtype_choices <- reactive({
    unique(unlist(survey_data[survey_data$date >= input$daterange[1] & survey_data$date <= input$daterange[2] & grepl(paste(input$duration_select, collapse = "|"),survey_data$duration), "problem_type"]))
  })
  
  observe({
    updatePickerInput(session, "probtype_select", choices = c(probtype_choices()))
  })
  
  ## -- Set up Data to Respond to Filters -- ##
  
  # Set up reactive data format
  values <- reactiveValues()
  # Load in global data as default
  values$data <- survey_data
  
  # Create dataframes for each filter
  
  # Date input
  dateinput_df <- eventReactive(input$filter_click, {
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
  
  prob_choices <- reactive({
    df <- data.frame(unlist(values$data[,"problem_type"]))
    nrow(df)
  })
  
  # Update data when "Filter Data" is clicked  
  newFilter <- observeEvent(input$filter_click, {
    values$data <- probinput_df()
  })
  
  # Set data back to global when "Reset" is clicked
  observeEvent(input$reset_click, {
    reset("form")
    values$data <- survey_data
    updatePickerInput(session, "duration_select", choices = unique(survey_data$duration))
    updatePickerInput(session, "probtype_select", choices = unique(unlist(survey_data$problem_type)))
  })
  
  observeEvent(input$filter_click,{
    if(!isTruthy(input$daterange)){
      showNotification("Please enter a valid date range", type = "error", 
                       duration = 30)
    }
  })
  
  ## -- Bookmark -- ##
  
  onBookmark(function(state) {
    state$values$duration <- input$duration_select
    state$values$probtype <- input$probtype_select
  })
  
  survey_filter <- function(df, date_input, dur_input, prob_input){
    if(!isTruthy(date_input)){
      return()
    } else if (!isTruthy(dur_input) & !isTruthy(prob_input)){
      df %>%
        filter(date >= date_input[1]) %>%
        filter(date <= date_input[2])
    } else if (isTruthy(dur_input) & !isTruthy(prob_input)){
      df %>%
        filter(date >= date_input[1]) %>%
        filter(date <= date_input[2]) %>%
        filter(duration %in% dur_input)
    } else if (!isTruthy(dur_input) & isTruthy(prob_input)){
      df %>%
        filter(date >= date_input[1]) %>%
        filter(date <= date_input[2]) %>%
        filter(grepl(paste(prob_input, collapse="|"), problem_type_collapse))
    } else{
      df %>%
        filter(date >= date_input[1]) %>%
        filter(date <= date_input[2]) %>%
        filter(duration %in% dur_input) %>%
        filter(grepl(paste(prob_input, collapse="|"), problem_type_collapse))
    }
  }
  
  ## Execute filter function within onRestored function, set values$data as result
  
  onRestored(function(state) {
    updatePickerInput(session,"duration_select",selected=state$values$duration)
    delay(200, updatePickerInput(session,"probtype_select",selected=state$values$probtype))
    observe({
      values$data <- survey_filter(survey_data, input$daterange, input$duration_select,input$probtype_select)
    })
  })
  
  ##-- Value Box --#
  
  output$total_reports <- renderText({
    nrow(values$data)
  })
  
  output$date_range <- renderText({
    paste(format(min(values$data[,"date"], na.rm = TRUE), "%Y/%m/%d"), 
          "-", format(max(values$data[,"date"], na.rm = TRUE), "%Y/%m/%d"))
  })
  
  ##-- Map Visualization --#
  
  # Create label for each marker on the map
  marker_labels <- reactive({sprintf(
    "Problem Type: %s<br/>Duration: %s<br/>Comment: %s",
    values$data[,"problem_type_collapse"], values$data[,"duration"], values$data[,"comment"]
  ) %>% lapply(htmltools::HTML)
  })
  
  # Parameters for MapBox basemap
  street_map <- "https://api.mapbox.com/styles/v1/arielag/cjhl8uwjg084r2sopg0stjoob/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYXJpZWxhZyIsImEiOiJjamY1dTlseDYxZHB0Mnlsbndsb3BkaTV5In0.SiiSe0JU0cXc6sqeLA4Hcg"
  
  map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a>"
  
  output$map <- renderLeaflet({
    if(prob_choices() > 0){
      leaflet(values$data) %>% 
        addTiles(urlTemplate = street_map, attribution = map_attr) %>%
        #addProviderTiles(providers$CartoDB.Positron) %>%
        addMarkers(lng = ~problem_long, lat = ~problem_lat, 
                   clusterOptions = markerClusterOptions(), label=marker_labels()) %>%
        setView(lng = -79.3892, lat = 43.6426, zoom = 12)
    } else{
      leaflet() %>% 
        addTiles(urlTemplate = street_map, attribution = map_attr) %>%
        #addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -79.3892, lat = 43.6426, zoom = 12)
    }
  })
  
  # Define the map zoom level to pass into PDF report
  user_zoom <- reactive({
    if(is.null(input$map_zoom)){
      12
    }else{
      input$map_zoom
    }
  })
  
  ##-- Problem Type Bar Chart --#
  probtype_df <- reactive({
    unlist_df <- data.frame(unlist(values$data[,"problem_type"]))
    if(nrow(unlist_df) > 0){
      colnames(unlist_df) <- c("problem_type")
      unlist_df %>%
        count(problem_type) %>%
        arrange(desc(n)) %>%
        slice(1:6)
    } else{
      return()
    }
  })
  
  n_colors <- reactive({
    nrow(probtype_df())
  })
  
  # Count problem type and convert to data series for chart
  ptbar_data <- reactive({
    if(prob_choices() > 0){
      if(n_colors() == 6){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff0000", "#ff3300", "#fcb600", "#f2f200", "#9ba39d", "#46e08c")) %>%
          rename(name = problem_type,
                 y = n) %>% 
          select(y, name, color) %>% 
          list.parse3()
      } else if(n_colors() == 5){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff0000", "#ff3300", "#fcb600", "#f2f200", "#46e08c")) %>%
          rename(name = problem_type,
                 y = n) %>% 
          select(y, name, color) %>% 
          list.parse3()
      } else if(n_colors() == 4){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff0000", "#fcb600", "#f2f200", "#46e08c")) %>%
          rename(name = problem_type,
                 y = n) %>% 
          select(y, name, color) %>% 
          list.parse3()
      } else if(n_colors() == 3){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff0000", "#f2f200", "#46e08c")) %>%
          rename(name = problem_type,
                 y = n) %>% 
          select(y, name, color) %>% 
          list.parse3()
      } else if(n_colors() == 2){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff3300", "#46e08c")) %>%
          rename(name = problem_type,
                 y = n) %>% 
          select(y, name, color) %>% 
          list.parse3()
      } else{
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#00cb47")) %>%
          rename(name = problem_type,
                 y = n) %>% 
          select(y, name, color) %>% 
          list.parse3()
      }
    } else{
      return()
    }
  })
  
  # Problem type bar chart
  output$ptbar <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type="category", lineColor = "#ffffff", tickColor = "#ffffff") %>%
      hc_yAxis(title = "Blank", visible = FALSE) %>% 
      hc_add_series(data = ptbar_data(), type = "bar", showInLegend = FALSE,
                    name = "Frequency") %>%
      hc_plotOptions(bar = list(
        dataLabels = list(
          align = "left",
          enabled = TRUE,
          crop = FALSE,
          overflow = "none"
        ))
      )
  })
  
  ##-- Weekday Column Chart --##
  
  # Create dataframe to hold data for all possible weekdays
  weekcol_df <- data.frame(c("Sun", "Mon", "Tue", "Wed",
                             "Thu", "Fri", "Sat"))
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
  
  days_range <- reactive({
    max(values$data[,"date"], na.rm = TRUE) - min(values$data[,"date"], na.rm = TRUE)
  })
  
  # Convert the dataframe to a series for plotting
  weekcol_chart_data <- reactive({
    if(days_range() > 5){
      weekcol_df2() %>%
        replace_na(list(weekday = NA, n = 0)) %>%
        arrange(factor(weekday, levels = c("Sun", "Mon", "Tue", "Wed",
                                           "Thu", "Fri", "Sat"))) %>%
        mutate(x = row_number()) %>%
        mutate(color = c("#00cb47")) %>%
        rename(name = weekday, 
               y = n) %>% 
        select(y, name, color) %>% 
        list.parse3()
    } else if(days_range() > -Inf){
      values$data %>%
        count(date) %>%
        mutate(date2 = datetime_to_timestamp(date)) %>%
        drop_na()
    } else{
      return()
    }
  })
  
  # Create the frequency plot
  output$weekcol <- renderHighchart({
    if(days_range() > 5 | days_range() == -Inf){
      highchart() %>% 
        hc_xAxis(type="category", lineColor = "#ffffff", tickColor = "#ffffff") %>%
        hc_yAxis(title = "Blank", visible = FALSE) %>% 
        hc_add_series(data = weekcol_chart_data(), type = "column", showInLegend = FALSE,
                      name = "Frequency") %>%
        hc_plotOptions(column = list(
          dataLabels = list(
            align = "center",
            enabled = TRUE,
            crop = FALSE,
            overflow = "none"
          ))
        )
    } else{
      highchart() %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b'), 
                 lineColor = "#ffffff", tickColor = "#ffffff") %>%
        hc_yAxis(title = "Blank", visible = FALSE) %>%
        hc_add_series(weekcol_chart_data(), "column", hcaes(date, n), 
                      showInLegend = FALSE, name = "Frequency") %>%
        hc_plotOptions(column = list(
          color = "#00cb47",
          dataLabels = list(
            align = "center",
            enabled = TRUE,
            crop = FALSE,
            overflow = "none"
          ))
        )
    }
  })
  
  ##-- Time v Duration Heat Map --##
  
  # Generate dataframe with all possible time v. duration combinations
  hours <- c("07-10", "10-13", "13-16", "16-19", "19+")
  heat_hours <- sort(rep(hours,4))
  heat_duration <- rep(c("minutes", "hours", "days", "overnight"), 5)
  
  heat_df <- data.frame(heat_hours, heat_duration)
  heat_df$heat_duration <- factor(heat_df$heat_duration, levels = c("overnight", "days", "hours", "minutes"))
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
      replace_na(list(time_group = NA, duration = NA, count = 0))
  })
  
  # Prepare values for the axes of the heat map
  y <- c("Overnight", "Days", "Hours", "Minutes")
  x <- c("07-10", "10-13", "13-16", "16-19", "19+")
  
  # Convert the dataframe to a series for plotting
  heat_data_series <- reactive({
    heat_data_2() %>%
      mutate(yid = rep(c(0,1,2,3),5),
             xid = sort(rep(seq(1:5)-1,4))) %>%
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
  freq_colr <- list(list(0, '#ffffff'),
                    list(0.20, '#46e08c'),
                    list(0.40, '#f2f200'),
                    list(0.60, '#fcb600'),
                    list(0.80, '#ff3300'),
                    list(1.0, '#ff0000'))
  
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
  
  ##-- Export Data --##
  
  df_csv <- reactive({
    values$data[, !(colnames(values$data) %in% c("problem_type"))]
  })
  
  output$csv_download <- downloadHandler(
    filename = "BikeSpace Data.csv",
    content = function(file) {
      write.csv(df_csv(), file, row.names = FALSE)
    }
  )
  
  output$pdf_download <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(date_1 = input$daterange[1], date_2 = input$daterange[2],
                     dur_input = input$duration_select, prob_input = input$probtype_select,
                     street_input = input$street_name, intersection_input = input$intersection_name,
                     data = values$data, zoom = user_zoom())
      
      # Knit the document
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  }