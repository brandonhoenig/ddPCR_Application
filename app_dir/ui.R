## ddPCR App

# load libraries 
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(ggthemes)
library(plotly)
library(janitor)

# Define UI for application
ui <- 
  
  # Fluid page for dashboard
  fluidPage(theme = shinytheme("yeti"),
    title = "ddPCR Analysis App",
    
    titlePanel(title = "ddPCR Analysis App"),
    
    # File upload button
    fluidRow(
      column(4,
             fileInput(inputId = "upload", 
                       "Upload a ddPCR .csv file here",
                       multiple = F,
                       accept = '.csv')
             ),
      column(4, 
             tableOutput("file"))),
    fluidRow(
      column(4, 
             actionButton(
               "example", "Or use 'example' dataset", 
               class = "btn-block"
             ))
    ),
    
    # Fluid row for handling slider inputs
    fluidRow(
      
      # choosing which channel you want for an x axis. 
      column(3, 
             uiOutput("show_x_axis"),
             
             uiOutput("show_button_x_threshold"),
             
             uiOutput("default_x_threshold_button"), 
             
             ),
      
      # choosing the threshold for a positive/negative result
      column(3, 
             uiOutput("show_button_x_axis_thresh")),
      
      # choosing which channel you want for the x axis
      column(3, 
             uiOutput("show_y_axis"),
             
             uiOutput("show_button_y_threshold"),
             
             uiOutput("default_y_threshold_button"),
             ),
      
      # choosing the threshold for a positive negative result on y axis. 
      column(3, 
             uiOutput("show_button_y_axis_thresh")),
             
      ),
    # plots out the ggplot of the 2 channel ddpcr assay
    fluidRow(
      column(12, 
             align = 'center',
             plotOutput("plotout"),
             
             uiOutput("download_the_plot")),
    )
    )
    
