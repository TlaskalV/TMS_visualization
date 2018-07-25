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
    dataset_temp_filtered <- dplyr::filter(dataset_temp_posixct, date_only >= '2018-06-02' & date_only <= '2018-06-05')
  })
  
  output$contents1 <- renderTable({
    head(dataset_temp_filtered(), 5)
  })
  output$contents2 <- renderTable({
    tail(dataset_temp_filtered(), 5)
  })
  output$contents3 <- renderText({
    paste(as.character(input$start_date))
  })
  output$contents4 <- renderText({
    paste(as.character(input$end_date))
  })
  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$otu, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset_temp_filtered(), file, row.names = FALSE, sep = ";")
    })
  
}