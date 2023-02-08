
library(shiny)
library(ggplot2)
library(readxl)

shinyServer(function(input, output, session) {
  data <- reactive({
    file <- input$file
    if(is.null(file)) return(NULL)
    
    # Read the data file
    if (grepl(".csv$", file$name)) {
      read.csv(file$datapath, header = TRUE)
    } else if (grepl(".xlsx$", file$name)) {
      read_excel(file$datapath, sheet = 1, col_names = TRUE)
    } else {
      return(NULL)
    }
  })
  
  output$scatterplot <- renderPlot({
    if(is.null(data())) return()
    x <- data()[[input$x]]
    y <- data()[[input$y]]
    
    # Check if the selected variables are numeric
    if (!is.numeric(x) || !is.numeric(y)) {
      return(NULL)
    }
    
    # Rescale the variables so they have similar scales centered at 0
    x_rescaled <- (x - mean(x)) / sd(x)
    y_rescaled <- (y - mean(y)) / sd(y)
    
    # Create the scatter plot with a loess smooth
    ggplot(data.frame(x_rescaled, y_rescaled), aes(x_rescaled, y_rescaled)) +
      geom_point() +
      geom_smooth(method = "loess") +
      geom_hline(yintercept = 0, size = 2) +
      geom_vline(xintercept = 0, size = 2) +
      xlab("Rescaled x") +
      ylab("Rescaled y") +
      coord_equal()
  })
  
  observe({
    if(is.null(data())) return()
    updateSelectInput(session, "x", choices = names(data()), selected = names(data())[1])
    updateSelectInput(session, "y", choices = names(data()), selected = names(data())[2])
  })
})

