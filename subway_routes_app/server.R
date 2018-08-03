#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)

theme_set(theme_bw())

load('trips.RData')
trips <- one_day_data
rm(one_day_data)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  get_stop_id <- function(stop_label, direction) {
    data.frame(stop_label) %>%
      extract(stop_label, c("stop_name", "route"), "(.*) \\((.*)\\)$") %>%
      left_join(stations) %>%
      mutate(stop_id  =paste(stop_id, direction, sep =""))
  }
  
  output$distributionPlot <- renderPlot({
    req(input$direction_1)
    req(input$start_station_1)
    req(input$end_station_1)
    req(input$direction_2)
    req(input$start_station_2)
    req(input$end_station_2)
    
    start_station_1 <- get_stop_id(input$start_station_1, input$direction_1)
    end_station_1 <- get_stop_id(input$end_station_1, input$direction_1)
    start_station_2 <- get_stop_id(input$start_station_2, input$direction_2)
    end_station_2 <- get_stop_id(input$end_station_2, input$direction_2)
    
    print(start_station_1)
    print(end_station_1)
    print(start_station_2)
    print(end_station_2)
    
    df <- data.frame(x = rnorm(100))
    
    ggplot(df, aes(x = x)) +
      geom_density(alpha = 0.25) +
      coord_flip() +
      ggtitle(' ')
  })
  
})
