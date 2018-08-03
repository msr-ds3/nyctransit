#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)

# this drops a few stops compared to the GTFS data, let's add them back in at some point
stations <- read_csv('stations.csv') %>%
  select(stop_id = `GTFS Stop ID`,
         stop_name = `Stop Name`, 
         routes = `Daytime Routes`,
         lat = `GTFS Latitude`,
         long = `GTFS Longitude`) %>%
  unnest_tokens(route, routes) %>%
  mutate(route = toupper(route),
         label = sprintf('%s (%s)', stop_name, route))


#stops <- read_csv("stops.txt")
#ns_stops <- stops %>%
#  filter(grepl('[NS]$', stop_id)) %>%
#  mutate(label = sprintf('%s %s', stop_name, stop_))

stop_names <- c("", stations$label)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Title here"),
  
  # Sidebar with a selectize input
  sidebarPanel(
    h3("Option 1"),
    radioButtons("direction_1", "Direction:",
                 c("Uptown" = "N",
                   "Downtown" = "S"),
                 selected = 0),
    selectizeInput("start_station_1",
                   label="Select a start station\n",
                   choices = stop_names,
                   selected = NULL),
    selectizeInput("end_station_1",
                   label="Select an end station\n",
                   choices = stop_names,
                   selected = NULL),
    
    h3("Option 2"),
    radioButtons("direction_2", "Direction:",
                 c("Uptown" = "N",
                   "Downtown" = "S"),
                 selected = 0),
    selectizeInput("start_station_2",
                   label="Select a start station\n",
                   choices = stop_names,
                   selected = NULL),
    selectizeInput("end_station_2",
                   label="Select an end station\n",
                   choices = stop_names,
                   selected = NULL)
  ),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    helpText(
      'Text here if you want it'
    ), 
    fluidRow(
      splitLayout(cellWidths = c("0%", "75%"), 
                  plotOutput("animatedPlot", height="400"),
                  plotOutput("distributionPlot", height="400")),
      textOutput("aucText")
    )
  )
))
