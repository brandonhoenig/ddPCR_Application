#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# load libraries 

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(ggthemes)
library(plotly)
library(janitor)

# Clean some names function 
clean_some_names <- function(dat, idx, ...) {
  names(dat)[idx] <- janitor::make_clean_names(names(dat)[idx], ...)
  dat
}

# Define UI for application
ui <- 
  
  # Fluid page for dashboard
  fluidPage(theme = shinytheme("yeti"),
    title = "ddPCR Analysis App",
    
    titlePanel(title = "ddPCR Analysis App"),
    
    # File upload button
    fluidRow(
      column(8,
             fileInput(inputId = "upload", 
                       "Upload a ddPCR .csv file here",
                       multiple = F,
                       accept = '.csv'),
             tableOutput("file"))),
    
    # Fluid row for handling slider inputs
    fluidRow(
      
      # choosing which channel you want for an x axis. 
      column(3, 
             selectInput(inputId = "x_axis",
                       "Which Channel on the x axis?", 
                       choices = list("", 
                                      "FAM/EvaGreen" = "Ch1Amplitude",
                                      "HEX/VIC" = "Ch2Amplitude",
                                      "Cy5" = "Ch3Amplitude",
                                      "Cy5.5" = "Ch4Amplitude",
                                      "ROX" = "Ch5Amplitude",
                                      "ATTO 590" = "Ch6Amplitude"), 
                       selected = c("FAM/EvaGreen" = "Ch1Amplitude")),
             numericInput(inputId = "button_x_threshold", 
                          "Manually Set X Threshold",
                          value = NULL), 
             actionButton(inputId = "default_x_threshold",
                          "K-means Cluster X Threshold"), 
             
             ),
      
      # choosing the threshold for a positive/negative result
      column(3, 
             sliderInput(inputId = "x_axis_thresh", 
                         "Select your threshold for positive droplets on the x axis",
                         min = 0, 
                         max = 7000, 
                         value = 0, 
                         step = 1)),
      
      # choosing which channel you want for the x axis
      column(3, 
             selectInput(inputId = "y_axis",
                         "Which Channel on the y axis?", 
                         choices = list("", 
                                        "FAM/EvaGreen" = "Ch1Amplitude",
                                        "HEX/VIC" = "Ch2Amplitude",
                                        "Cy5" = "Ch3Amplitude",
                                        "Cy5.5" = "Ch4Amplitude",
                                        "ROX" = "Ch5Amplitude",
                                        "ATTO 590" = "Ch6Amplitude"),
                         selected = c("HEX/VIC" = "Ch2Amplitude")),
             numericInput(inputId = "button_y_threshold", 
                          "Manually Set Y Threshold", 
                          value = NULL), 
             actionButton(inputId = "default_y_threshold",
                          "K-means Cluster Y Threshold"), 
             ),
      
      # choosing the threshold for a positive negative result on y axis. 
      column(3, 
             sliderInput(inputId = "y_axis_thresh", 
                         "Select your threshold for positive droplets on the y axis",
                         min = 0, 
                         max = 7000, 
                         value = 0,
                         step = 1)),
      ),
    # plots out the ggplot of the 2 channel ddpcr assay
    fluidRow(
      column(8, 
             plotOutput("plot")), 
      # plots out the summary table for number of copies. 
      column(4, 
             tableOutput("counts"))
    )
    )

# Define server logic
server <- 
  function(input, output, session) {
    
    # renders the information on the table that is input. 
    output$file <- {
      observe(req(input$upload))
      renderTable(input$upload) 
    }
    
    # creates auto thresholds using k-means clustering
    auto_x_threshold <-
      reactive({read_csv(input$upload$datapath, skip = 3) %>%
          select(input$x_axis) %>%
          kmeans(., 2) %>%
          .$cluster %>%
          cbind(read_csv(input$upload$datapath, skip = 3) %>% 
                  select(input$x_axis)) %>%
          clean_some_names(1) %>%
          group_by(x) %>%
          summarise_all(c(max = max, 
                          min = min)) %>%
          arrange(max) %>%
          mutate(x = c("Negative", "Positive")) %>%
          filter(x == "Negative") %>%
          select(max) %>%
          as.numeric()})
    
    # creates auto thresholds using k-means clustering
    auto_y_threshold <- 
      reactive({read_csv(input$upload$datapath, skip = 3) %>%
          select(input$y_axis) %>%
          kmeans(., 2) %>%
          .$cluster %>%
          cbind(read_csv(input$upload$datapath, skip = 3) %>% 
                  select(input$y_axis)) %>%
          clean_some_names(1) %>%
          group_by(x) %>%
          summarise_all(c(max = max, 
                          min = min)) %>%
          arrange(max) %>%
          mutate(x = c("Negative", "Positive")) %>%
          filter(x == "Negative") %>%
          select(max) %>%
          as.numeric()})
    
    # Set default thresholds for positive / negative results using k-means clustering.
    {observe(req(input$upload))
    observeEvent(input$default_x_threshold,
                 updateSliderInput(session,'x_axis_thresh',
                                   value = auto_x_threshold()))}
    
    observeEvent(input$default_y_threshold,
                 updateSliderInput(session,'y_axis_thresh',
                                   value = auto_y_threshold()))
    
    # Add input option to type in threshold instead of using slider or setting default
    observeEvent(input$button_x_threshold,
                 updateSliderInput(session,'x_axis_thresh',
                                   value = input$button_x_threshold))
    
    observeEvent(input$button_y_threshold,
                 updateSliderInput(session,'y_axis_thresh',
                                   value = input$button_y_threshold))
    
    # makes input thresholds a reactive
    x_threshold <- reactive({
      input$x_axis_thresh
    })
    y_threshold <- reactive({
      input$y_axis_thresh
    })
    
    # makes the main input a reactive that can be called instead of reread each time. 
    dat <- reactive({
      read_csv(input$upload$datapath, skip = 3) %>%
        
        # puts threshold into the table. 
        mutate(x_threshold = x_threshold(), 
               y_threshold = y_threshold()) %>%
        
        # selects only the relevant data. 
        select(x_value = input$x_axis, 
               y_value = input$y_axis, 
               x_threshold, 
               y_threshold) %>%
        
        # determines droplet status for plot below coloring. 
        mutate(droplet_status = if_else(x_value > x_threshold &
                                         y_value > y_threshold, "double_pos", 
                                       if_else(x_value > x_threshold &
                                                 y_value < y_threshold, "only_x_pos", 
                                               if_else(x_value < x_threshold &
                                                         y_value > y_threshold, "only_y_pos", 
                                                       if_else(x_value < x_threshold & 
                                                                 y_value < y_threshold, "double_neg", NA)))))
    })
    
    # renders the ggplot for showing ddPCR droplets and their droplet statuses. 
    output$plot <- renderPlot({
      req(input$upload)
      
      ggplot() +
      geom_point(data = dat(), 
                 aes(x = x_value, 
                        y = y_value,
                     colour = droplet_status)) +
      geom_vline(data = dat(), 
                 aes(xintercept = x_threshold)) +
      geom_hline(data = dat(), 
                 aes(yintercept = y_threshold)) +
        scale_color_colorblind(breaks = c("double_pos", 
                                        "only_x_pos", 
                                        "only_y_pos", 
                                        "double_neg"), 
                             labels = c("Double Positive",
                                        "Only X Axis Positive", 
                                        "Only Y Axis Positive", 
                                        "Double Negative")) +
        labs(x = input$x_axis, 
             y = input$y_axis,
             colour = "Droplet Status") +
        theme_bw() 
    }
    )
    
    output$counts <- renderTable({
      
      req(input$upload)
      # Table for determining concentration of DNA in copies per reaction
      dat() %>%
        mutate(x_axis_call = if_else(x_value > x_threshold, 1, 0), 
               y_axis_call = if_else(y_value > y_threshold, 1 , 0)) %>%
        select(x_axis_call, y_axis_call) %>%
        group_by(x_axis_call, y_axis_call) %>%
        tally() %>%
        ungroup() %>%
        pivot_wider(names_from = c(x_axis_call, y_axis_call),
                    values_from = "n") %>%
        # calculates the copies per uL using poisson statistics
        mutate(total = sum(select(., contains("_")))) %>%
        # calculate lambda for each axis
        mutate(lambda_x = -log((total - sum(select(., contains("1_"))))/ total),
               lambda_y = -log((total - sum(select(., contains("_1"))))/ total)) %>%
        # calculate count of droplets 
        mutate(count_x = (lambda_x / 1) * 1000,
               count_y = (lambda_y / 1) * 1000) %>%
        # calculate confidence interval around each count
        mutate(confint_x = (sqrt((lambda_x / total)) * 1.96) * 1000, 
               confint_y = (sqrt((lambda_y / total)) * 1.96) * 1000) %>%
        mutate(count_x = paste0(round(count_x, 2), " [", round(count_x - confint_x, 2), ", ", round(count_x + confint_x, 2), "]"),
               count_y = paste0(round(count_y, 2), " [", round(count_y - confint_y, 2), ", ", round(count_y + confint_y, 2), "]")) %>%
        select(`Concentration of X Axis (copies/uL) [95% CI]` = count_x, 
               `Concentration of Y Axis (copies/uL) [95% CI]` = count_y)
    }
    
    )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

## Upcoming Edits
# make k-means fail and default to INF if clusters cannot be separated by 95% confidence. 
# Add a download button that downloads a figure with relevant information on it. 
