library(shiny)
library(easystats)
library(BayesFactor)
library(see)
library(dplyr)

# Save correlation results
results <- correlation(iris)

ui <- fluidPage(
  titlePanel("Correlation Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("correlation_type", "Correlation Type:",
                  c("Pearson", "Spearman", "Kendall", "Bayesian"),
                  selected = "Pearson"),
      checkboxInput("include_factors", "Include Factors", value = FALSE),
      checkboxInput("multilevel", "Multi-level Correlation", value = FALSE),
      checkboxInput("partial", "Partial Correlation", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

server <- function(input, output) {
  results_correlation <- reactive({
    iris %>%
      correlation(method = input$correlation_type,
                  bayesian = input$correlation_type == "Bayesian",
                  include_factors = input$include_factors,
                  multilevel = input$multilevel,
                  partial = input$partial)
  })
  
  output$summary <- renderPrint({
    summary(results_correlation(), redundant = TRUE)
  })
  
  output$plot <- renderPlot({
    results_correlation() %>%
      summary(redundant = TRUE) %>%
      plot()
  })
}

shinyApp(ui, server)
