# TMS visualization
App for fast plots based on temperature and moisture data from [TMS dataloggers](https://tomst.com/web/en/systems/tms/tms-3/) produced by [TOMST](https://tomst.com/web/en/).

## Table of contents

* [How to upload data](#data-upload)
* [Example plots](#example-plots)
* [Where to try app](#where-to-try-app)

## Data upload
To visualize data just upload raw **.csv** file with prefix **data_** downloaded from TMS datalogger ([see example csv file here](https://github.com/Vojczech/TMS_visualization/blob/master/data_94174102_0.csv)). Temperature and moisture data can be easily filtered by date.

There is an option to plot data from different combinations of sensors.

## Example plots

### Combined temperature and moisture data ###

<img src="/data_94174102_0_combined.png" width="600">

---

### Temperature data only ###

<img src="/data_94174102_0_temperature.png" width="600">

## Where to try app

There are two options to access the app:
* online web app hosted on [labenvmicro.shinyapps.io](https://labenvmicro.shinyapps.io/TMS_app/) 
* or start your local installation of **R** language and paste following code which automatically downloads prerequisties and starts app:
```
install.packages(c("shiny", "shinythemes", "readr", "tidyverse", "Rmisc", "tools", "gridExtra", "grid", "shinycssloaders"))
library(shiny)
runGitHub("TMS_visualization", "Vojczech") 
```