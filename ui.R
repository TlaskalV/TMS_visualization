library(shiny)
library(shinythemes)
library(shinycssloaders)
library(readr)
library(tidyverse)
library(Rmisc)


fluidPage(
  theme = shinytheme("sandstone"),
  title = "Temperature probes",
  plotOutput("contents5") %>% withSpinner(type = getOption("spinner.type", default = 4)),
  tableOutput("contents1") %>% withSpinner(type = getOption("spinner.type", default = 4)),
  tableOutput("contents2") %>% withSpinner(type = getOption("spinner.type", default = 4)),
  tableOutput("contents6") %>% withSpinner(type = getOption("spinner.type", default = 4)),
  tableOutput("contents7") %>% withSpinner(type = getOption("spinner.type", default = 4)),
  textOutput("contents3"),
  textOutput("contents4"),
  hr(),
  fluidRow(
    column(3,
           h3("Temperature probes"),
           h4("1."),
           fileInput('csv_data', 
                     'Upload csv file',
                     accept = c('sheetName', 'header'), 
                     multiple = FALSE)
    ),
    column(4, 
           h4("2."),
           offset = 1,
           textInput("plot_title", 'Write plot title',
                     placeholder = "probe serial no. 93164188"),
           dateRangeInput('date_range',
                          label = 'Filter by date',
                          start = Sys.Date() - 3, end = Sys.Date() + 3,
                          separator = " to ", format = "yy/mm/dd",
                          startview = 'month', language = 'en', weekstart = 1
           )
    ),
    column(4,
           h4("3."),
           checkboxInput('upper', 'Upper sensor', value = FALSE),
           checkboxInput('middle', 'Middle sensor', value = FALSE),
           checkboxInput('lower', 'Lower sensor', value = TRUE),
           br(),
           checkboxInput('moisture', 'Moisture', value = TRUE),
           br(),
           checkboxInput('average', 'Display average line', value = FALSE),
           br(),
           downloadButton("download_plot", 
                          "Download final plot")
    )
  )
)
