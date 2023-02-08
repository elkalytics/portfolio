library(shiny)
library(GGally)

ui <- fluidPage(
  titlePanel("Scatterplot Matrix with ggpairs"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Variable 1:", names(mtcars), selected = names(mtcars)[1]),
      selectInput("var2", "Variable 2:", names(mtcars), selected = names(mtcars)[2]),
      selectInput("var3", "Variable 3:", names(mtcars), selected = names(mtcars)[3]),
      selectInput("var4", "Variable 4:", names(mtcars), selected = names(mtcars)[4]),
      selectInput("var5", "Variable 5:", names(mtcars), selected = names(mtcars)[5]),
      selectInput("var6", "Variable 6:", names(mtcars), selected = names(mtcars)[6]),
      selectInput("group", "Grouping variable:", names(mtcars), selected = "vs")
    ),
    mainPanel(
      plotOutput("scatterplot_matrix")
    )
  )
)

server <- function(input, output) {
  selected_vars <- reactive({
    c(input$var1, input$var2, input$var3, input$var4, input$var5, input$var6)
  })
  
  output$scatterplot_matrix <- renderPlot({
    ggpairs(mtcars[, selected_vars()],
            columnLabels = c(input$var1, input$var2, input$var3, input$var4, input$var5, input$var6),
            aes(color = factor(mtcars[[input$group]])),
            upper = list(continuous = wrap('cor', size = 3)),
            lower = list(combo = wrap("facethist", bins = 30)),
            diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
            title = "Scatterplot matrix of `mtcars` Grouped by Engine")
  })
}

shinyApp(ui, server)
