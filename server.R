library(shiny)
library(shinythemes)
library(readr)
library(tidyverse)
library(Rmisc)

function(input, output) {
  
  dataset_temp <- reactive({
    validate(
      need(input$csv_data != "", "Please upload a csv file with temperature data")
    )
    infile = input$csv_data  
    
    if (is.null(infile))
      return(NULL)
    
    readr::read_delim(infile$datapath, delim = ";", escape_double = FALSE, col_names = FALSE, na = "blank", trim_ws = TRUE)
  })
  
  dataset_temp_header <- reactive({
    dataset_temp <- dataset_temp()
    dataset_temp_header_ready <- dplyr::rename(dataset_temp, "measure_index" = X1) %>% 
      dplyr::rename("date" = X2) %>% 
      dplyr::rename("time_zone" = X3) %>% 
      dplyr::rename("temp_lower" = X4) %>% 
      dplyr::rename("temp_middle" = X5) %>% 
      dplyr::rename("temp_upper" = X6) %>% 
      dplyr::rename("moisture" = X7) %>% 
      dplyr::rename("shake" = X8) %>% 
      dplyr::rename("errflag" = X9)
  })
  
  dataset_temp_parsed <- reactive({
    dataset_temp_header <- dataset_temp_header()
    dataset_temp_parsed <- mutate(dataset_temp_header, date_parsed = lubridate::parse_date_time(dataset_temp_header$date, "%d/%m/%Y %H:%M"))
  })
  
  dataset_temp_posixct <- reactive({
    dataset_temp_parsed <- dataset_temp_parsed()
    dataset_temp_header_posixct <- mutate(dataset_temp_parsed, date_only = base::as.POSIXct(lubridate::as_date(dataset_temp_parsed$date_parsed, tz = "UTC"), tz = "UTC"))
  })
  
  dataset_temp_filtered <- reactive({
    dataset_temp_posixct <- dataset_temp_posixct()
    dataset_temp_filtered <- dplyr::filter(dataset_temp_posixct, date_only >= input$date_range[1] & date_only <= paste(as.character(input$date_range[2]), "23:45")) %>% 
      select(-date_only) %>% 
      gather(position, temp, temp_lower:temp_upper)
  })
  
  temp_average_upper <- reactive({
    temp <- summarySE(dataset_temp_filtered(), measurevar = "temp", groupvars = "position", conf.interval = 0.95)
    temp_upper <- temp[[3,3]]
  })
  
  temp_average_middle <- reactive({
    temp <- summarySE(dataset_temp_filtered(), measurevar = "temp", groupvars = "position", conf.interval = 0.95)
    temp_middle <- temp[[2,3]]
  })
  
  temp_average_lower <- reactive({
    temp <- summarySE(dataset_temp_filtered(), measurevar = "temp", groupvars = "position", conf.interval = 0.95) 
    temp_lower <- temp[[1,3]]
  })
  
  moisture_average <- reactive({
    moist <- filter(dataset_temp_filtered(), position == "temp_lower") %>% 
      summarySE(measurevar = "moisture", groupvars = "position", conf.interval = 0.95)
    moist_aver <- moist[[1,3]]
  })
  
  sensor <- renderText({
    input$sensor
  })
  
  ggplot_final <- reactive({
    dataset_temp_filtered <- dataset_temp_filtered()
    dataset_temp_filtered$position <- factor(dataset_temp_filtered$position, levels = c("temp_upper", "temp_middle", "temp_lower"))
    temp_average_upper <- temp_average_upper()
    temp_average_middle <- temp_average_middle()
    temp_average_lower <- temp_average_lower()
    moisture_average <- moisture_average()
    sensor <- sensor()
    if(input$plot_type == "temperature"){
      ggplot(data = dataset_temp_filtered, aes(y = temp, x = date_parsed)) +
      {if (sensor == "upper") {
        geom_line(aes(colour = position), size = 2, subset(dataset_temp_filtered, position == "temp_upper"))   
      } else {}} +
      {if (sensor == "middle") {
        geom_line(aes(colour = position), size = 2, subset(dataset_temp_filtered, position == "temp_middle"))   
      } else {}} +
      {if (sensor == "lower") {
        geom_line(aes(colour = position), size = 2, subset(dataset_temp_filtered, position == "temp_lower"))   
      } else {}} +
      {if (sensor == "upper middle") {
        geom_line(aes(colour = position), size = 2, subset(dataset_temp_filtered, position == "temp_upper" | position == "temp_middle"))
      } else {}} +
      {if (sensor == "middle lower") {
        geom_line(aes(colour = position), size = 2, subset(dataset_temp_filtered, position == "temp_middle" | position == "temp_lower"))   
      } else {}} +
      {if (sensor == "upper lower") {
        geom_line(aes(colour = position), size = 2, subset(dataset_temp_filtered, position == "temp_upper" | position == "temp_lower"))
      } else {}} +
      {if (sensor == "upper middle lower") {
        geom_line(aes(colour = position), size = 2, subset(dataset_temp_filtered, position == "temp_upper" | position == "temp_middle" | position == "temp_lower"))
      } else {}} +
      scale_color_viridis_d() + # color in the case of discrete values    
      scale_x_datetime(date_breaks = "1 day") +
      labs(title = input$plot_title, subtitle = paste("mean upper sensor - ", round(temp_average_upper, digits = 1), "°C\n", "mean middle sensor - ", round(temp_average_middle, digits = 1), "°C\n", "mean lower sensor - ", round(temp_average_lower, digits = 1), "°C\n"), x = "date", y = "temperature [°C]") +
#      {if (input$sensor == "upper" & input$average == TRUE)
#        geom_hline(yintercept = temp_average_upper, size = 1) 
#      } +
#      {if (input$sensor == "upper" & input$average == TRUE)
#       annotate("text", x = as.POSIXct(input$date_range[1] + 0.5), y = temp_average_upper + 0.01, label = "upper sensor", size = 2)
#      } +
#      {if (input$sensor == "middle" & input$average == TRUE)
#        geom_hline(yintercept = temp_average_middle, size = 1) 
#      } +
#      {if (input$sensor == "middle" & input$average == TRUE)
#          annotate("text", x = as.POSIXct(input$date_range[1] + 0.5), y = temp_average_middle + 0.01, label = "middle sensor", size = 2) 
#      } +
#      {if (input$sensor == "lower" & input$average == TRUE)
#       geom_hline(yintercept = temp_average_lower, size = 1) 
#         } +
#     {if (input$sensor == "lower" & input$average == TRUE)
#        annotate("text", x = as.POSIXct(input$date_range[1] + 0.5), y = temp_average_lower + 0.01, label = "lower sensor", size = 2) 
#      } +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 10, colour = "black"), axis.text.y = element_text(size = 13, colour = "black"), axis.title = element_text(size = 14, face = "bold", colour = "black"), plot.title = element_text(size = 14, face = "bold", colour = "black"),  plot.subtitle = element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  } else {
    ggplot(data = dataset_temp_filtered, aes(y = moisture, x = date_parsed)) +
      geom_line(aes(colour = position), size = 2, subset(dataset_temp_filtered, position == "temp_lower")) +
      scale_color_viridis_d() + # color in the case of discrete values    
      scale_x_datetime(date_breaks = "1 day") +
      labs(title = input$plot_title, subtitle = paste("mean moisture - ", round(moisture_average, digits = 1)), x = "date", y = "moisture") +
    theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 10, colour = "black"), axis.text.y = element_text(size = 13, colour = "black"), axis.title = element_text(size = 14, face = "bold", colour = "black"), plot.title = element_text(size = 14, face = "bold", colour = "black"),  plot.subtitle = element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }
  })
  
  output$ui <- renderUI({
    if (is.null(input$plot_type))
      return()
    
    switch(input$plot_type,
           "moisture" = checkboxInput("moisture", "Moisture data",
                                      value = TRUE),
           "temperature" = checkboxGroupInput("sensor", "Sensor",
                                                choices = c('Upper sensor' = 'upper',
                                                            'Middle sensor' = 'middle',
                                                            'Lower sensor' = 'lower'),
                                                selected = "lower"
           )
    )
  })
  
  
  output$contents5 <- renderPlot({
    ggplot_final()
  })
  output$contents1 <- renderTable({
    head(temp_average_upper(), 5)
  })
  output$contents6 <- renderTable({
    head(moisture_average(), 5)
  })
  output$contents7 <- renderTable({
    head(dataset_temp_filtered(), 5)
  })
  output$contents2 <- renderTable({
    tail(dataset_temp_filtered(), 5)
  })
  output$contents3 <- renderText({
    sensor()
  })
  output$contents4 <- renderText({
    paste(as.character(input$date_range[2]))
  })
  output$download_plot <- downloadHandler(
    filename = function() { paste(input$dataset, '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = ggplot_final(), device = "pdf", dpi = 300, height = 210, width = 297, units = "mm")
    }
  )
  
}