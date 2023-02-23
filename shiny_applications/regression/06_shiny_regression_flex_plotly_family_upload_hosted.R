# Hosted: https://jchase.shinyapps.io/Regression/

## Generate fake data to test upload functional
# set.seed(1234)
# student_athletes <- data.frame(
#  student_id = 1:100,
#  gender = sample(c("M", "F"), 100, replace = TRUE),
#  age = rnorm(100, mean = 18, sd = 1),
#  grade = sample(1:12, 100, replace = TRUE),
#  height = rnorm(100, mean = 68, sd = 3),
#  weight = rnorm(100, mean = 150, sd = 20)
# )

## Review
# head(student_athletes)

## Locate where it will save
# getwd()

## Save data
# write.csv(student_athletes, "student_athletes.csv", row.names = F)
library(performance)
library(tidyverse)
library(broom)
library(ggfortify)
library(shiny)
library(DT)
library(plotly)
library(mgcv)
library(see)
library(patchwork)

data(mtcars)

ui <- fluidPage(
  titlePanel("Model"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      selectInput('x', 'X', choices = names(mtcars), selected = names(mtcars)[1]),
      selectInput('y', 'Y', choices = names(mtcars), selected = names(mtcars)[2]),
      selectInput("family", "Family Type:",
                  choices = c("gaussian", "poisson", "binomial"),
                  selected = "gaussian"),
      checkboxInput("loess", "Show LOESS Fit", value = FALSE),
      checkboxInput("lm", "Show Linear Model Fit", value = FALSE),
      checkboxInput("gam", "Show GAM Fit", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", plotlyOutput("scatterplot")),
        tabPanel("Table", DT::dataTableOutput("reg_table")),
        tabPanel("Assumptions", plotOutput("diagnostic"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath, header = TRUE)
    } else {
      mtcars
    }
  })
  
  # Update input choices based on data
  observe({
    if (!is.null(data())) {
      updateSelectInput(session, "x", choices = names(data()), selected = names(data())[1])
      updateSelectInput(session, "y", choices = names(data()), selected = names(data())[2])
    }
  })
  
  # Filter data
  filtered_data <- reactive({
    data_subset <- data() %>%
      select(input$x, input$y)
    
    # Rename variables to "x" and "y", respectively
    names(data_subset) <- c("x", "y")
    
    data_subset %>% 
      filter(!is.na(x) & !is.na(y))
  })
  
  # Construct model
  model <- reactive({
    if (input$family == "gaussian") {
      if (input$gam) {
        gam(y ~ s(x), data = filtered_data(), family = gaussian())
      } else {
        lm(y ~ x, data = filtered_data())
      }
    } else if (input$family == "poisson") {
      if (input$gam) {
        gam(y ~ s(x), data = filtered_data(), family = poisson())
      } else {
        glm(y ~ x, data = filtered_data(), family = poisson())
      }
    } else if (input$family == "binomial") {
      glm(y ~ x, data = filtered_data(), family = binomial())
    }
  })
  
  # View model
  output$scatterplot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x, y)) +
      geom_point()
    
    if (input$loess) {
      p <- p + geom_smooth(method = "loess")
    }
    
    if (input$lm) {
      p <- p + geom_abline(intercept = coef(model())[1],
                           slope = coef(model())[2], col = "red")
    }
    
    if (input$gam) {
      p <- p + geom_smooth(method = "gam", formula = y ~ s(x))
    }
    
    gg <- ggplotly(p)
    gg
  })
  
  # Regression table
  output$reg_table <- DT::renderDataTable({
    if (input$gam) {
      broom::tidy(model(), exponentiate = TRUE) %>% 
        DT::datatable()
    } else {
      broom::tidy(model()) %>% 
        DT::datatable()
    }
  })
  
  # Assumptions plot
  output$diagnostic <- renderPlot({
    if (input$family == "gaussian") {
      if (input$gam) {
        check_model(model())
      } else {
        autoplot(model())
      }
    } else if (input$family == "poisson") {
      plot(residuals(model()) ~ fitted(model()), data = filtered_data())
    } else if (input$family == "binomial") {
      plot(residuals(model(), type = "deviance") ~ fitted(model()), data = filtered_data())
    }
  })
  
  
}

shinyApp(ui, server)
