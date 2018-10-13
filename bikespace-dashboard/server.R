
function(input, output, session) {
  
  ## -- Sidebar Inputs -- ##
  
  probtype_choices <- reactive({
    unique(str_trim(unlist(str_split(survey_data[survey_data$date >= input$daterange[1] & survey_data$date <= input$daterange[2], "problem_type_collapse"], "[;]"))))
  })
  
  observe({
    updatePickerInput(session, "probtype_select", choices = c(probtype_choices()))
  })
  
  duration_choices <- reactive({
    unique(survey_data[survey_data$date >= input$daterange[1] & survey_data$date <= input$daterange[2] & grepl(paste(input$probtype_select, collapse = "|"),survey_data$problem_type_collapse), "duration"])
  })
  
  observe({
    updatePickerInput(session, "duration_select", choices = c(duration_choices()))
  })
  
  intersection_choices <- reactive({
    unique(survey_data[survey_data$date >= input$daterange[1] & survey_data$date <= input$daterange[2] 
                       & grepl(paste(input$probtype_select, collapse = "|"),survey_data$problem_type_collapse) 
                       & grepl(tolower(input$street_select), tolower(survey_data$intersection)), "intersection"])
  })
  
  observe({
    updatePickerInput(session, "intersection_select", choices = c(intersection_choices()))
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
  
  # Problem type  
  probinput_df <- eventReactive(input$filter_click,{
    if(is.null(input$probtype_select)){
      dateinput_df()
    } else{
      dateinput_df() %>%
        filter(grepl(paste(input$probtype_select, collapse="|"), problem_type_collapse))
    }
  })
  
  # Parking duration  
  durinput_df <- eventReactive(input$filter_click,{
    if(is.null(input$duration_select)){
      probinput_df()
    } else{
      probinput_df() %>%
        filter(duration %in% input$duration_select)
    }
  })
  
  # Street intersection
  street_df <- eventReactive(input$filter_click,{
    if(is.null(input$street_select) & is.null(input$intersection_select)){
      durinput_df()
    }else if(is.null(input$intersection_select)){
      durinput_df() %>%
        filter(grepl(tolower(input$street_select), tolower(intersection)))
    } else{
      durinput_df() %>%
        filter(grepl(tolower(input$intersection_select), tolower(intersection)))
    }
  })
  
  prob_choices <- reactive({
    df <- data.frame(unlist(values$data[,"problem_type"]))
    nrow(df)
  })
  
  # Update data when "Filter Data" is clicked  
  newFilter <- observeEvent(input$filter_click, {
    values$data <- street_df()
  })
  
  # Set data back to global when "Reset" is clicked
  observeEvent(input$reset_click, {
    reset("form")
    values$data <- survey_data
    updatePickerInput(session, "probtype_select", choices = unique(str_trim(unlist(str_split(survey_data$problem_type_collapse, "[;]")))))
    updatePickerInput(session, "duration_select", choices = unique(survey_data$duration))
    reset("form2")
  })
  
  # Create error message if dates are blank
  observeEvent(input$filter_click,{
    if(!isTruthy(input$daterange)){
      showNotification("Please enter a valid date range", type = "error", 
                       duration = 30)
    }
  })
  
  ## -- Bookmark -- ##
  
  onBookmark(function(state) {
    state$values$probtype <- input$probtype_select
    state$values$duration <- input$duration_select
    state$values$street <- input$street_select
    state$values$intersection <- input$intersection_select
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
        filter(grepl(paste(prob_input, collapse="|"), problem_type_collapse)) %>%
        filter(duration %in% dur_input)
    }
  }
  
  ## Execute filter function within onRestored function, set values$data as result
  
  onRestored(function(state) {
    updatePickerInput(session,"probtype_select",selected=state$values$probtype)
    delay(200, updatePickerInput(session,"duration_select",selected=state$values$duration))
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
  
  ##-- Open Data Text --#
  odbl_link <- a("Open Database License", href="http://opendatacommons.org/licenses/odbl/1.0/")
  dbcl_link <- a("Database Contents License", href="https://opendatacommons.org/licenses/dbcl/1.0/")
  
  output$odbl_text <- renderUI({
    tagList("The BikeSpace database", 
            tags$br(),
            "is made available",
            tags$br(),
            "under the", 
            tags$br(),
            odbl_link)
  })
  
  output$dbcl_text <- renderUI({
    tagList("Any rights in individual", 
            tags$br(),
            "contents of the database",
            tags$br(),
            "are licensed under the",
            tags$br(),
            dbcl_link)
  })
  
  ##-- Map Visualization --#
  
  # Create bikespace icon for map
  bikespaceIcon <- makeIcon(
    iconUrl = "https://s3.amazonaws.com/bikespace-dashboard-assets/pins/DB_Logo_Pin.png",
    iconWidth = 25, iconHeight = 25,
    iconAnchorX = 0, iconAnchorY = 0
  )
  
  # Parameters for MapBox basemap
  street_map <- "https://api.mapbox.com/styles/v1/bikespace/cjjonnw9n477z2sl76c0xo0r8/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYmlrZXNwYWNlIiwiYSI6ImNqY2w5OWVlbDA3YXkycW8ydXd5eXN2MG8ifQ.ue_zOQmPEmHbNa-vG7BwLA"
  
  map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a>"
  
  # Popup image link
  pop_img_base <- "https://s3.amazonaws.com/bikeparking/"
  
  output$map <- renderLeaflet({
    if(prob_choices() > 0){
      leaflet(values$data) %>% 
        addTiles(urlTemplate = street_map, attribution = map_attr, options = providerTileOptions(minZoom = 10, maxZoom = 17)) %>%
        addMarkers(lng = ~problem_long, lat = ~problem_lat, icon = bikespaceIcon,
                   clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE), 
                   popup = paste0("<img src = ", pop_img_base,values$data[,"pic"], ">", 
                                  "<br>", "<br>",
                                  "<b>Problem Type: </b>",
                                  values$data[,"problem_type_collapse"],
                                  "<br>",
                                  "<b> Duration: </b>",
                                  values$data[,"duration"],
                                  "<br>",
                                  "<b> Comment: </b>",
                                  values$data[,"comment"])) %>%
        setView(lng = -79.3892, lat = 43.6426, zoom = 12)
    } else{
      leaflet() %>% 
        addTiles(urlTemplate = street_map, attribution = map_attr) %>%
        setView(lng = -79.3892, lat = 43.6426, zoom = 12)
    }
  })
  
  # Define the map zoom level and map bounds to pass into PDF report
  user_zoom <- reactive({
    if(is.null(input$map_zoom)){
      12
    }else{
      input$map_zoom
    }
  })
  
  user_center <- reactive({
    bounds <- input$map_bounds
    user_zoom <- input$map_zoom
    lon_factor <- ifelse(user_zoom==10,1.007, ifelse(user_zoom==11, 1.004,
                    ifelse(user_zoom==12, 1.002, ifelse(user_zoom==13,1.0009,
                      ifelse(user_zoom==14,1.0005, ifelse(user_zoom==15,1.0002,
                        ifelse(user_zoom==16,1.0001, 1.00005)))))))
    lat_factor <- ifelse(user_zoom==10,0.993, ifelse(user_zoom==11, 0.997,
                    ifelse(user_zoom==12, 0.998, ifelse(user_zoom==13,0.9992,
                      ifelse(user_zoom==14,0.9994, ifelse(user_zoom==15,0.9997,
                        ifelse(user_zoom==16,0.9999, 0.99995)))))))
    center <- c(mean(bounds$east, bounds$west)*lon_factor, mean(bounds$north, bounds$south)*lat_factor)
    center
  })
  
  ##-- Problem Type Bar Chart --#
  probtype_df <- reactive({
    unlist_df <- data.frame(str_trim(unlist(str_split(values$data[,"problem_type_collapse"], "[;]"))))
    if(nrow(unlist_df) > 0){
      colnames(unlist_df) <- c("problem_type_collapse")
      unlist_df %>%
        count(problem_type_collapse) %>%
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
          rename(name = problem_type_collapse,
                 y = n) %>% 
          select(y, name, color) %>% 
          list_parse()
      } else if(n_colors() == 5){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff0000", "#ff3300", "#fcb600", "#f2f200", "#46e08c")) %>%
          rename(name = problem_type_collapse,
                 y = n) %>% 
          select(y, name, color) %>% 
          list_parse()
      } else if(n_colors() == 4){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff0000", "#fcb600", "#f2f200", "#46e08c")) %>%
          rename(name = problem_type_collapse,
                 y = n) %>% 
          select(y, name, color) %>% 
          list_parse()
      } else if(n_colors() == 3){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff0000", "#f2f200", "#46e08c")) %>%
          rename(name = problem_type_collapse,
                 y = n) %>% 
          select(y, name, color) %>% 
          list_parse()
      } else if(n_colors() == 2){
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#ff3300", "#46e08c")) %>%
          rename(name = problem_type_collapse,
                 y = n) %>% 
          select(y, name, color) %>% 
          list_parse()
      } else{
        probtype_df() %>%
          mutate(x = row_number()) %>%
          mutate(color = c("#00cb47")) %>%
          rename(name = problem_type_collapse,
                 y = n) %>% 
          select(y, name, color) %>% 
          list_parse()
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
    if(days_range() > 3){
      weekcol_df2() %>%
        replace_na(list(weekday = NA, n = 0)) %>%
        arrange(factor(weekday, levels = c("Sun", "Mon", "Tue", "Wed",
                                           "Thu", "Fri", "Sat"))) %>%
        mutate(x = row_number()) %>%
        mutate(color = c("#00cb47")) %>%
        rename(name = weekday, 
               y = n) %>% 
        select(y, name, color) %>% 
        list_parse()
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
    if(days_range() > 3 | days_range() == -Inf){
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
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b %d'), 
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
  hours <- c("07-10", "10-13", "13-16", "16-19", "19-07")
  heat_hours <- sort(rep(hours,4))
  heat_duration <- rep(c("Minutes", "Hours", "Overnight", "Days"), 5)
  
  heat_df <- data.frame(heat_hours, heat_duration)
  heat_df$heat_duration <- factor(heat_df$heat_duration, levels = c("Days", "Overnight", "Hours", "Minutes"))
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
  y <- c("Days", "Overnight", "Hours", "Minutes")
  x <- c("07-10", "10-13", "13-16", "16-19", "19-07")
  
  # Convert the dataframe to a series for plotting
  heat_data_series <- reactive({
    heat_data_2() %>%
      mutate(yid = rep(c(0,1,2,3),5),
             xid = sort(rep(seq(1:5)-1,4))) %>%
      select(xid, yid, count) %>%
      list_parse2()
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
      hc_xAxis(categories = x, title = list(text = "24 hour time", 
                                            offset = 0, rotation = 0, y = 42)) %>% 
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
    values$data[, c("id","problem_type_collapse", "duration", "date", "weekday", "hour", 
                    "time_group", "problem_lat", "problem_long", "comment")]
  })
  
  output$csv_download <- downloadHandler(
    filename = "BikeSpace Data.csv",
    content = function(file) {
      write.csv(df_csv(), file, row.names = FALSE)
    }
  )
  
  output$pdf_download <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      withProgress(message = 'Generating PDF report',{
        setProgress(value = 0.2)
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        setProgress(value = 0.3)
        # Set up parameters to pass to Rmd document
        params <- list(date_1 = input$daterange[1], date_2 = input$daterange[2],
                       dur_input = input$duration_select, prob_input = input$probtype_select,
                       data = values$data, zoom = user_zoom(), center = user_center())
        
        setProgress(value = 0.7, detail = "~10 seconds")
        # Knit the document
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )
  
}