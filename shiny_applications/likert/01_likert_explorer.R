library(shiny)
library(likert)

data <- data.frame(Employee = paste("Employee", 1:100),
                   matrix(sample(1:5, 100*4, replace = TRUE), ncol = 4))
colnames(data)[2:5] = paste("Variable", 1:4)
data[2:5] = lapply(data[2:5], factor, levels = 1:5)

ui <- fluidPage(
  titlePanel("Likert Plot Generator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("ordered", "Ordered:",
                  min = 0, max = 1, value = 0, step = 1),
      sliderInput("centered", "Centered:",
                  min = 0, max = 1, value = 0, step = 1)
    ),
    
    mainPanel(
      plotOutput("likert_plot")
    )
  )
)

server <- function(input, output) {
  output$likert_plot <- renderPlot({
    data_likert = likert(data[2:5])
    plot(data_likert, ordered = as.logical(input$ordered), centered = as.logical(input$centered), group.order = names(data[2:5]))
  })
}

shinyApp(ui, server)
