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

# Define server logic
server <- 
  function(input, output, session) {
    
    # renders the information on the table that is input. 
    output$file <- {
      observe(req(input$upload))
      renderTable(input$upload) 
    }
    
    # make buttons appear only when file has been uploaded. 
    output$show_x_axis <- renderUI({
      req(input$upload)
      selectInput(inputId = "x_axis",
                  "Which Channel on the x axis?", 
                  choices = list("", 
                                 "FAM/EvaGreen" = "Ch1Amplitude",
                                 "HEX/VIC" = "Ch2Amplitude",
                                 "Cy5" = "Ch3Amplitude",
                                 "Cy5.5" = "Ch4Amplitude",
                                 "ROX" = "Ch5Amplitude",
                                 "ATTO 590" = "Ch6Amplitude"), 
                  selected = c("FAM/EvaGreen" = "Ch1Amplitude")) })
    # make buttons appear only when file has been uploaded.  
    output$show_y_axis <- renderUI({
      req(input$upload)
      selectInput(inputId = "y_axis",
                  "Which Channel on the y axis?", 
                  choices = list("", 
                                 "FAM/EvaGreen" = "Ch1Amplitude",
                                 "HEX/VIC" = "Ch2Amplitude",
                                 "Cy5" = "Ch3Amplitude",
                                 "Cy5.5" = "Ch4Amplitude",
                                 "ROX" = "Ch5Amplitude",
                                 "ATTO 590" = "Ch6Amplitude"), 
                  selected = c("HEX/VIC" = "Ch2Amplitude")) })
    
    # make buttons appear only when file has been uploaded. 
    output$show_button_x_threshold <- renderUI({
      req(input$upload)
      numericInput(inputId = "button_x_threshold", 
                   "Manually Set X Threshold",
                   value = NULL)
    })
    
    # make buttons appear only when file has been uploaded. 
    output$show_button_y_threshold <- renderUI({
      req(input$upload)
      numericInput(inputId = "button_y_threshold", 
                   "Manually Set Y Threshold",
                   value = NULL)
    })
    
    # make buttons appear only when file has been uploaded. 
    output$show_button_y_axis_thresh <- renderUI({
      req(input$upload)
      sliderInput(inputId = "y_axis_thresh", 
                  "Select your threshold for positive droplets on the y axis",
                  min = 0, 
                  max = 7000, 
                  value = 0,
                  step = 1)
    })
    
    # make buttons appear only when file has been uploaded. 
    output$show_button_x_axis_thresh <- renderUI({
      req(input$upload)
      sliderInput(inputId = "x_axis_thresh", 
                  "Select your threshold for positive droplets on the x axis",
                  min = 0, 
                  max = 7000, 
                  value = 0,
                  step = 1)
    })
    
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
    observeEvent(input$default_x_threshold,
                 updateSliderInput(session,'x_axis_thresh',
                                   value = auto_x_threshold()))
    
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
    
    # Makes K-means button only appear after a file has been uploaded
    output$default_y_threshold_button <- renderUI({
      req(input$upload)
      actionButton('default_y_threshold', 
                   label = 'K-means Cluster Y Threshold') })
    
    # Makes K-means button only appear after a file has been uploaded
    output$default_x_threshold_button <- renderUI({
      req(input$upload)
      actionButton('default_x_threshold', 
                   label = 'K-means Cluster X Threshold') })
    
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
    myplot <- reactive({
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
    })
    
    output$plotout <- renderPlot({
      req(input$upload)
      
      myplot()
    }
    )
    
    # this makes the download plot button only appear when and input is uploaded. 
    output$download_the_plot <- renderUI({
      req(input$upload)
      downloadButton('downloadPlot', label = 'Download Plot') })
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste(input$upload, '.png', sep='') },
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = myplot(), device = device)
      }
    )
    
    counts <- reactive({dat() %>%
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
        
        # calculate DNA concentration based 
        mutate(count_x = (lambda_x / 1) * 1000,
               count_y = (lambda_y / 1) * 1000) %>%
        
        # calculate 95% confidence interval around DNA concentrations
        mutate(confint_x = (sqrt((lambda_x / total)) * 1.96) * 1000, 
               confint_y = (sqrt((lambda_y / total)) * 1.96) * 1000) %>%
        mutate(count_x = paste0(round(count_x, 2), " [", round(count_x - confint_x, 2), ", ", round(count_x + confint_x, 2), "]"),
               count_y = paste0(round(count_y, 2), " [", round(count_y - confint_y, 2), ", ", round(count_y + confint_y, 2), "]")) %>%
        select(`Concentration of X Axis (copies/uL) [95% CI]` = count_x, 
               `Concentration of Y Axis (copies/uL) [95% CI]` = count_y)})
    
    output$counts <- renderTable({
      
      req(input$upload)
      # Table for determining concentration of DNA in copies per reaction
      
      counts()
    }
    
    )
    
    output$download_the_table <- renderUI({
      req(input$upload)
      downloadButton('downloadCounts', label = 'Download Table') })
    
    output$downloadCounts <- downloadHandler(
      filename = function() { paste(input$upload, "_counts_",  '.csv', sep='') },
      content = function(file) {
        write.csv(counts(), file)
      }
    )
    
  }
