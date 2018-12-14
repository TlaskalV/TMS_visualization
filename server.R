library(shiny)
library(shinythemes)
library(readr)
library(tidyverse)
library(Rmisc)
library(tools)
library(gridExtra)
library(grid)

function(input, output) {
  
  dataset_temp <- reactive({
    validate(
      need(input$csv_data != "", "Please upload a csv file with temperature data")
    )
    infile = input$csv_data  
    
    if (is.null(infile))
      return(NULL)
    
    read.csv2(infile$datapath, header = FALSE, sep = ";", dec = ".")
  })

# get name of uploaded file
  file_name <- reactive({
    inFile <- input$csv_data
    
    if (is.null(inFile))
      return(NULL) else return (tools::file_path_sans_ext(inFile$name))
  })
  
# add column names to table  
  dataset_temp_header <- reactive({
    dataset_temp <- dataset_temp()
    dataset_temp_header_ready <- dplyr::rename(dataset_temp, "measure_index" = V1) %>% 
      dplyr::rename("date" = V2) %>% 
      dplyr::rename("time_zone" = V3) %>% 
      dplyr::rename("temp_lower" = V4) %>% 
      dplyr::rename("temp_middle" = V5) %>% 
      dplyr::rename("temp_upper" = V6) %>% 
      dplyr::rename("moisture" = V7) %>% 
      dplyr::rename("shake" = V8) %>% 
      dplyr::rename("errflag" = V9)
  })
  
# recognize date 
  dataset_temp_parsed <- reactive({
    dataset_temp_header <- dataset_temp_header()
    dataset_temp_parsed <- mutate(dataset_temp_header, date_parsed = lubridate::parse_date_time(dataset_temp_header$date, "ymd HM"))
  })
  
# make column date_only for filtering, as.POSIXct is important  
  dataset_temp_posixct <- reactive({
    dataset_temp_parsed <- dataset_temp_parsed()
    dataset_temp_header_posixct <- mutate(dataset_temp_parsed, date_only = base::as.POSIXct(lubridate::as_date(dataset_temp_parsed$date_parsed, tz = "UTC"), tz = "UTC"))
  })
  
# date filtering  
  dataset_temp_filtered <- reactive({
    dataset_temp_posixct <- dataset_temp_posixct()
    dataset_temp_filtered <- dplyr::filter(dataset_temp_posixct, date_only >= input$date_range[1] & date_only <= paste(as.character(input$date_range[2]), "23:45")) %>% 
      select(-date_only) %>% 
      gather(position, temp, temp_lower:temp_upper, moisture) 
  })
  
# average temperature upper  
  temp_average_upper <- reactive({
    temp <- summarySE(dataset_temp_filtered(), measurevar = "temp", groupvars = "position", conf.interval = 0.95)
    temp_upper <- temp[[4,3]]
  })

# average temperature middle  
  temp_average_middle <- reactive({
    temp <- summarySE(dataset_temp_filtered(), measurevar = "temp", groupvars = "position", conf.interval = 0.95)
    temp_middle <- temp[[3,3]]
  })
  
# average temperature upper  
  temp_average_lower <- reactive({
    temp <- summarySE(dataset_temp_filtered(), measurevar = "temp", groupvars = "position", conf.interval = 0.95) 
    temp_lower <- temp[[2,3]]
  })
  
# average moisture
  moisture_average <- reactive({
    moist <- summarySE(dataset_temp_filtered(), measurevar = "temp", groupvars = "position", conf.interval = 0.95) 
    moist_mean <- moist[[1,3]]
  })
  
# sensor option  
  sensor <- renderText({
    input$sensor
  })
  
# ggplot  
  ggplot_final <- reactive({
    dataset_temp_filtered <- dataset_temp_filtered()
    dataset_temp_filtered$position <- factor(dataset_temp_filtered$position, levels = c("temp_upper", "temp_middle", "temp_lower", "moisture"))
    temp_average_upper <- temp_average_upper()
    temp_average_middle <- temp_average_middle()
    temp_average_lower <- temp_average_lower()
    moisture_average <- moisture_average()
    sensor <- sensor()
    if(input$plot_type == "temperature"){
      ggplot(data = dataset_temp_filtered, aes(y = temp, x = date_parsed)) +
      {if (sensor == "upper") {
        geom_line(aes(colour = position), size = 1, alpha = 0.75, subset(dataset_temp_filtered, position == "temp_upper"))   
      } else {}} +
      {if (sensor == "middle") {
        geom_line(aes(colour = position), size = 1, alpha = 0.75, subset(dataset_temp_filtered, position == "temp_middle"))   
      } else {}} +
      {if (sensor == "lower") {
        geom_line(aes(colour = position), size = 1, alpha = 0.75, subset(dataset_temp_filtered, position == "temp_lower"))   
      } else {}} +
      {if (sensor == "upper middle") {
        geom_line(aes(colour = position), size = 1, alpha = 0.75, subset(dataset_temp_filtered, position == "temp_upper" | position == "temp_middle"))
      } else {}} +
      {if (sensor == "middle lower") {
        geom_line(aes(colour = position), size = 1, alpha = 0.75, subset(dataset_temp_filtered, position == "temp_middle" | position == "temp_lower"))   
      } else {}} +
      {if (sensor == "upper lower") {
        geom_line(aes(colour = position), size = 1, alpha = 0.75, subset(dataset_temp_filtered, position == "temp_upper" | position == "temp_lower"))
      } else {}} +
      {if (sensor == "upper middle lower") {
        geom_line(aes(colour = position), size = 1, alpha = 0.75, subset(dataset_temp_filtered, position == "temp_upper" | position == "temp_middle" | position == "temp_lower"))
      } else {}} +
      scale_color_viridis_d() + # color in the case of discrete values    
      {if (input$x_scale == "day") {
        scale_x_datetime(date_breaks = "1 day")
      } else {}} +
      {if (input$x_scale == "week") {
        scale_x_datetime(date_breaks = "1 week")
      } else {}} +
      {if (input$x_scale == "month") {
        scale_x_datetime(date_breaks = "1 month")
      } else {}} +
      {if (input$x_scale == "year") {
        scale_x_datetime(date_breaks = "1 year")
      } else {}} +
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
        theme(axis.text.x = element_text(angle = 90, size = 10, colour = "black"), axis.text.y = element_text(size = 13, colour = "black"), axis.title = element_text(size = 14, face = "bold", colour = "black"), plot.title = element_text(size = 14, face = "bold", colour = "black"),  plot.subtitle = element_text(colour = "black"), panel.grid.minor = element_blank(), legend.position = "top", legend.text = element_text(size = 11, colour = "black", face = "plain"), legend.title = element_text(size = 12, colour = "black", face = "bold"), legend.key.size = unit(3, 'lines'), legend.spacing.x = unit(0.3, 'cm'), legend.direction = "horizontal")
  } else if (input$plot_type == "moisture"){
# if moisture data are checked
    ggplot(data = dataset_temp_filtered, aes(y = temp, x = date_parsed)) +
      geom_line(aes(colour = position), size = 1, alpha = 1, subset(dataset_temp_filtered, position == "moisture")) +
      scale_color_viridis_d(labels = "moisture") + # color in the case of discrete values    
      {if (input$x_scale == "day") {
        scale_x_datetime(date_breaks = "1 day")
      } else {}} +
      {if (input$x_scale == "week") {
        scale_x_datetime(date_breaks = "1 week")
      } else {}} +
      {if (input$x_scale == "month") {
        scale_x_datetime(date_breaks = "1 month")
      } else {}} +
      {if (input$x_scale == "year") {
        scale_x_datetime(date_breaks = "1 year")
      } else {}} +
      labs(title = input$plot_title, subtitle = paste("mean moisture - ", round(moisture_average, digits = 1)), x = "date", y = "moisture") +
    theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 10, colour = "black"), axis.text.y = element_text(size = 13, colour = "black"), axis.title = element_text(size = 14, face = "bold", colour = "black"), plot.title = element_text(size = 14, face = "bold", colour = "black"),  plot.subtitle = element_text(colour = "black"), panel.grid.minor = element_blank(), legend.position = "top", legend.text = element_text(size = 11, colour = "black", face = "plain"), legend.title = element_text(size = 12, colour = "black", face = "bold"), legend.key.size = unit(3, 'lines'), legend.spacing.x = unit(0.3, 'cm'), legend.direction = "horizontal")
  } else {
# if combined data are checked
    temp_plot <- ggplot(data = dataset_temp_filtered, aes(y = temp, x = date_parsed)) +
      geom_line(aes(colour = position), size = 1, alpha = 0.75, subset(dataset_temp_filtered, position == "temp_lower" | position == "temp_upper" | position == "temp_middle")) +
      scale_color_viridis_d() + # color in the case of discrete values    
      {if (input$x_scale == "day") {
        scale_x_datetime(date_breaks = "1 day")
      } else {}} +
      {if (input$x_scale == "week") {
        scale_x_datetime(date_breaks = "1 week")
      } else {}} +
      {if (input$x_scale == "month") {
        scale_x_datetime(date_breaks = "1 month")
      } else {}} +
      {if (input$x_scale == "year") {
        scale_x_datetime(date_breaks = "1 year")
      } else {}} +
      labs(subtitle = paste("mean upper sensor - ", round(temp_average_upper, digits = 1), "°C\n", "mean middle sensor - ", round(temp_average_middle, digits = 1), "°C\n", "mean lower sensor - ", round(temp_average_lower, digits = 1), "°C\n"), x = "date", y = "temperature [°C]") +
    theme_bw() +
      theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 13, colour = "black"), axis.title.y = element_text(size = 14, face = "bold", colour = "black"), axis.title.x = element_blank(), plot.title = element_blank(),  plot.subtitle = element_text(colour = "black"), panel.grid.minor = element_blank(), legend.position = "top", legend.text = element_text(size = 11, colour = "black", face = "plain"), legend.title = element_text(size = 12, colour = "black", face = "bold"), legend.key.size = unit(3, 'lines'), legend.spacing.x = unit(0.3, 'cm'), legend.direction = "horizontal")
    moist_plot <- ggplot(data = dataset_temp_filtered, aes(y = temp/100, x = date_parsed)) +
      geom_line(aes(colour = position), size = 1, alpha = 1, subset(dataset_temp_filtered, position == "moisture")) +
      scale_color_viridis_d(labels = "moisture") + # color in the case of discrete values    
      {if (input$x_scale == "day") {
        scale_x_datetime(date_breaks = "1 day")
      } else {}} +
      {if (input$x_scale == "week") {
        scale_x_datetime(date_breaks = "1 week")
      } else {}} +
      {if (input$x_scale == "month") {
        scale_x_datetime(date_breaks = "1 month")
      } else {}} +
      {if (input$x_scale == "year") {
        scale_x_datetime(date_breaks = "1 year")
      } else {}} +
      labs(subtitle = paste("mean moisture - ", round(moisture_average, digits = 1)), x = "date", y = "moisture (*100)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 10, colour = "black"), axis.text.y = element_text(size = 13, colour = "black"), axis.title = element_text(size = 14, face = "bold", colour = "black"), plot.title = element_blank(),  plot.subtitle = element_text(colour = "black"), panel.grid.minor = element_blank(), legend.position = "none", legend.text = element_text(size = 11, colour = "black", face = "plain"), legend.title = element_blank(), legend.key.size = unit(3, 'lines'), legend.spacing.x = unit(0.3, 'cm'), legend.direction = "horizontal")
    plots_both <- grid.arrange(temp_plot, moist_plot, ncol = 1, top = textGrob(input$plot_title, gp = gpar(cex = 1.3, fontface = "bold", col = "black"), x = 0.08,  y = 0.55))
  }
  })
  
# switching between temperature and moisture plots  
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
                                                selected = c("upper", "middle", "lower")
           ),
           "combined" = checkboxInput("combined", "Combined data",
                                      value = TRUE)
    )
  })
  
  
# rendering  
  output$contents1 <- renderPlot({
    ggplot_final()
  })
  
  output$contents2 <- renderTable({
    (temp_average_upper())
  })
  
# download  
  output$download_plot <- downloadHandler(
    filename = function() {
      if (input$plot_type == "temperature")
        paste(file_name(), "_temperature.pdf", sep="")
      else if (input$plot_type == "moisture")
        paste(file_name(), "_moisture.pdf", sep="")
      else
        paste(file_name(), "_combined.pdf", sep="")
      },
    content = function(file) {
      ggsave(file, plot = ggplot_final(), device = "pdf", dpi = 300, height = 210, width = 297, units = "mm")
    }
  )
  
}