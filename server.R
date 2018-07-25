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
  
  ggplot_final <- reactive({
    ggplot(data = dataset_temp_filtered(), aes(y = temp, x = date_parsed)) +
      geom_line(aes(color = position), size = 2) +
      scale_color_viridis_d() + # color in the case of discrete values    
      scale_x_datetime(date_breaks = "1 day") +
      labs(title = input$plot_title, subtitle = "average", x = "date", y = "temperature [°C]") +
      #geom_hline(yintercept = 22.39062, color = "#440154") +
      #geom_hline(yintercept = 22.40365, color = "#29788E") +
      #geom_hline(yintercept = 22.39714, color = "#FDE724") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 10, colour = "black"), axis.text.y = element_text(size = 13, colour = "black"), axis.title = element_text(size = 14, face = "bold", colour = "black"), plot.title = element_text(size = 14, face = "bold", colour = "black"),  plot.subtitle = element_text(colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  })
  
  output$contents5 <- renderPlot({
    ggplot_final()
  })
  output$contents1 <- renderTable({
    head(dataset_temp_filtered(), 5)
  })
  output$contents2 <- renderTable({
    tail(dataset_temp_filtered(), 5)
  })
  output$contents3 <- renderText({
    paste(as.character(input$date_range[1]))
  })
  output$contents4 <- renderText({
    paste(as.character(input$date_range[2]))
  })
  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$otu, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset_temp_filtered(), file, row.names = FALSE, sep = ";")
    })
  
}