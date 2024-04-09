
# Shiny
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)

# Analysis
library(tidyverse)
library(timetk)

port <- Sys.getenv('PORT')

shiny::runApp(
    appDir = getwd(),
    host = '0.0.0.0',
    port = as.numeric(port)
)