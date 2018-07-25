library(shiny)
library(shinythemes)
library(shinycssloaders)
library(readr)
library(tidyverse)
library(Rmisc)


fluidPage(
  theme = shinytheme("sandstone"),
  title = "Temperature probes",
  #plotOutput('plot'),
  tableOutput("contents1") %>% withSpinner(type = getOption("spinner.type", default = 4)),
  tableOutput("contents2") %>% withSpinner(type = getOption("spinner.type", default = 4)),
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
           dateInput('start_date',
                     label = "Choose starting date",
                     value = as.character(Sys.Date()-1),
                     format = "dd.mm.yy",
                     startview = 'month', language = 'en', weekstart = 1
           ),
           dateInput('end_date',
                     label = "Choose ending date",
                     value = as.character(Sys.Date()),
                     format = "dd.mm.yy",
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
           downloadButton("download_plot", 
                          "Download final plot")
    )
  )
)
