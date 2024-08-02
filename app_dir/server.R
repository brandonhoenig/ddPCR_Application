## ddPCR App

## Load libraries 

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(ggthemes)
library(plotly)
library(janitor)

## Needed functions
# Clean some names function 
clean_some_names <- function(dat, idx, ...) {
  names(dat)[idx] <- janitor::make_clean_names(names(dat)[idx], ...)
  dat
}

# randomly order column
randomly <- function(x) sample(xtfrm(x))

## Needed objects.

# move channel choices here so we don't have to retype them everytime. 
channel_choices <- 
  list("FAM/EvaGreen" = "Ch1Amplitude",
       "HEX/VIC" = "Ch2Amplitude",
       "Cy5" = "Ch3Amplitude",
       "Cy5.5" = "Ch4Amplitude",
       "ROX" = "Ch5Amplitude",
       "ATTO 590" = "Ch6Amplitude")

#make up some fake data for an example
example_data <-
  tibble(Ch1Amplitude = c(rnorm(5000, 5000, 500),
                          rnorm(15000, 500, 50))) %>%
    arrange(randomly(Ch1Amplitude)) %>%
    cbind(
      tibble(Ch2Amplitude = c(rnorm(2000, 4500, 500),
                              rnorm(18000, 1000, 50)))
    ) %>% 
    cbind(
      tibble(Ch3Amplitude = rnorm(20000, 300, 2000), 
             Ch4Amplitude = rnorm(20000, 300, 2000), 
             Ch5Amplitude = rnorm(20000, 300, 2000), 
             Ch6Amplitude = rnorm(20000, 300, 2000))
    )

# Define server logic
server <- 
  function(input, output, session) {
    
    #
    sample_data <- reactiveVal()
    
    observeEvent(
      input$upload, 
      {data_2 <- read_csv(input$upload$datapath, skip = 3)
      sample_data(data_2)})
    
    observeEvent(
      input$example, 
      {sample_data(example_data)}
    )
    
    # sample_data <-
    #   reactive({
    #     Data()
    #   }) |> bindEvent(Data())
    
    # renders the information on the table that is input. 
    output$file <- {
      observe(req(sample_data()))
      renderTable(input$upload$name) 
    }
    
    # make buttons appear only when file has been uploaded. 
    output$show_x_axis <- renderUI({
      req(sample_data())
      selectInput(inputId = "x_axis",
                  "Which Channel on the x axis?", 
                  choices = channel_choices, 
                  selected = c("FAM/EvaGreen" = "Ch1Amplitude")) })
   
     # make buttons appear only when file has been uploaded.  
    output$show_y_axis <- renderUI({
      req(sample_data())
      selectInput(inputId = "y_axis",
                  "Which Channel on the y axis?", 
                  choices = channel_choices, 
                  selected = c("HEX/VIC" = "Ch2Amplitude")) })
    
    # make buttons appear only when file has been uploaded. 
    output$show_button_x_threshold <- renderUI({
      req(sample_data())
      numericInput(inputId = "button_x_threshold", 
                   "Type to Manually Set X Threshold",
                   value = if_else(auto_x_threshold_ss() > 0.9, round(auto_x_threshold(), 0), 0))
    })
    
    # make buttons appear only when file has been uploaded. 
    output$show_button_y_threshold <- renderUI({
      req(sample_data())
      numericInput(inputId = "button_y_threshold", 
                   "Type to Manually Set Y Threshold",
                   value = if_else(auto_y_threshold_ss() > 0.9, round(auto_y_threshold(), 0), 0))
    })
    
    # run kmeans on x axis
    x_axis_k_means <- reactive({sample_data() %>%
        select(input$x_axis) %>%
        kmeans(., 2, iter.max = 10000)})
    
    # creates auto thresholds using k-means clustering
    auto_x_threshold_check <-
      reactive({x_axis_k_means() %>%
          .$cluster %>%
          cbind(sample_data() %>% 
                  select(input$x_axis)) %>%
          clean_some_names(1) %>%
          group_by(x) %>%
          summarise_all(c(max = max, 
                          min = min)) %>%
          arrange(min) %>%
          mutate(x = c("Negative", "Positive")) %>%
          filter(x == "Positive") %>%
          select(min) %>%
          as.numeric() - 1})
    
    # Determine if clusters are accurate or not by looking at sum of squares
    
    auto_x_threshold_ss <- reactive({
    (x_axis_k_means() %>%
        .$betweenss
      /
       x_axis_k_means() %>%
        .$totss)
      })
    
    # create a new reactive in the event that a threshold can't be created. 
    auto_x_threshold <- reactive({
      ifelse(auto_x_threshold_ss() > 0.9, auto_x_threshold_check(), 0)
        
    })
      
    
    # run kmeans on y axis
    y_axis_k_means <- reactive({sample_data() %>%
        select(input$y_axis) %>%
        kmeans(., 2, iter.max = 10000)})
    
    # creates auto thresholds using k-means clustering
    auto_y_threshold_check <- 
      reactive({y_axis_k_means() %>%
          .$cluster %>%
          cbind(sample_data() %>% 
                  select(input$y_axis)) %>%
          clean_some_names(1) %>%
          group_by(x) %>%
          summarise_all(c(max = max, 
                          min = min)) %>%
          arrange(min) %>%
          mutate(x = c("Negative", "Positive")) %>%
          filter(x == "Positive") %>%
          select(min) %>%
          as.numeric() - 1})
    
    # Determine if clusters are accurate or not by looking at sum of squares
    auto_y_threshold_ss <- reactive({
      (y_axis_k_means() %>%
         .$betweenss
       /
         y_axis_k_means() %>%
         .$totss)})
    
    # create a new reactive in the event that a threshold can't be created. 
    auto_y_threshold <- reactive({
      ifelse(auto_y_threshold_ss() > 0.9, auto_y_threshold_check(), 0)
      
    })
    
    # make buttons appear only when file has been uploaded. 
    output$show_button_y_axis_thresh <- renderUI({
      req(sample_data())
      sliderInput(inputId = "y_axis_thresh", 
                  "Select your threshold for positive droplets on the y axis",
                  min = 0, 
                  max = 20000, 
                  value = if_else(auto_y_threshold_ss() > 0.9, auto_y_threshold(), 0),
                  step = 5)
    })
    
    # make buttons appear only when file has been uploaded. 
    output$show_button_x_axis_thresh <- renderUI({
      req(sample_data())
      sliderInput(inputId = "x_axis_thresh", 
                  "Select your threshold for positive droplets on the x axis",
                  min = 0, 
                  max = 20000, 
                  value = if_else(auto_x_threshold_ss() > 0.9, auto_x_threshold(), 0),
                  step = 5)
    })
    
    # Get number of droplets and display it on the ggplot
    number_of_droplets <- reactive(
      {
      sample_data() %>%
            nrow()
      })
    
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
      req(sample_data())
      actionButton('default_y_threshold', 
                   label = paste('Reset', 
                                 names(channel_choices[channel_choices == input$y_axis]), 
                                 "Threshold")) 
      })
    
    # Makes K-means button only appear after a file has been uploaded
    output$default_x_threshold_button <- renderUI({
      req(sample_data())
      actionButton('default_x_threshold', 
                   label = paste('Reset', 
                                 names(channel_choices[channel_choices == input$x_axis]), 
                                 "Threshold")) 
    })
    
    # makes the main input a reactive that can be called instead of reread each time. 
    dat <- reactive({
      sample_data() %>%
        
        # puts threshold into the table. 
        mutate(x_threshold = if_else(auto_x_threshold_ss() > 0.9 | input$x_axis_thresh != 0, x_threshold(), Inf), 
               y_threshold = if_else(auto_y_threshold_ss() > 0.9 | input$y_axis_thresh != 0, y_threshold(), Inf)) %>%
        
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
        geom_vline(data = NULL, 
                   aes(xintercept = if_else(auto_x_threshold_ss() > 0.9 | input$x_axis_thresh != 0, x_threshold(), NA))) +
        geom_hline(data = NULL, 
                   aes(yintercept = if_else(auto_y_threshold_ss() > 0.9 | input$y_axis_thresh != 0, y_threshold(), NA))) +
        annotate(geom = 'label', 
                 label = paste("Total Droplets:", number_of_droplets(), "\n" ,
                               names(channel_choices[channel_choices == input$x_axis]), "Threshold:", x_threshold(), "\n",
                               names(channel_choices[channel_choices == input$y_axis]), "Threshold:", y_threshold(), "\n",
                               names(channel_choices[channel_choices == input$x_axis]), "copies/uL:", counts()[1], "\n",
                               names(channel_choices[channel_choices == input$y_axis]), "copies/uL:", counts()[2]),
                 x = Inf, 
                 y = Inf, 
                 hjust = 1, 
                 vjust = 1) +
        scale_color_colorblind(breaks = c("double_pos", 
                                          "only_x_pos", 
                                          "only_y_pos", 
                                          "double_neg"), 
                               labels = c("Double Positive",
                                          paste("Only",names(channel_choices[channel_choices == input$x_axis])  ,"Positive"), 
                                          paste("Only", names(channel_choices[channel_choices == input$y_axis]),"Positive"), 
                                          "Double Negative")) +
        labs(x = names(channel_choices[channel_choices == input$x_axis]), 
             y = names(channel_choices[channel_choices == input$y_axis]),
             colour = "Droplet Status") +
        theme_bw() +
        theme(legend.position = 'none')
    })
    
    output$plotout <- renderPlot({
      req(sample_data())
      
      myplot()
    }
    )
    
    # this makes the download plot button only appear when and input is uploaded. 
    output$download_the_plot <- renderUI({
      req(sample_data())
      downloadButton('downloadPlot', label = 'Download Plot') })
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste(input$upload, '.png', sep='') },
      content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = myplot(), device = device)
      }
    )
    
    counts <- reactive({
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
      
      req(sample_data())
      # Table for determining concentration of DNA in copies per reaction
      
      counts()
    }
    
    )
    
    output$download_the_table <- renderUI({
      req(sample_data())
      downloadButton('downloadCounts', label = 'Download Table') })
    
    output$downloadCounts <- downloadHandler(
      filename = function() { paste(input$upload, "_counts_",  '.csv', sep='') },
      content = function(file) {
        write.csv(counts(), file)
      }
    )
    
  }
