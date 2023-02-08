library(shiny)
library(naniar)

ui <- fluidPage(
  titlePanel("Visualize Missing Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Dataset:", 
                  c("Airquality" = "airquality",
                    "Riskfactors" = "riskfactors",
                    "Pedestrian" = "pedestrian")),
      selectInput("plot_type", "Plot Type:", 
                  c("Visually Missing" = "vis_miss",
                    "GGplot Missing Upset" = "gg_miss_upset",
                    "GGplot Missing Point" = "geom_miss_point",
                    "GGplot Missing Facets" = "facet_wrap",
                    "GGplot Missing Variable" = "gg_miss_var",
                    "GGplot Missing Case" = "gg_miss_case",
                    "GGplot Missing Factor" = "gg_miss_fct",
                    "GGplot Missing Span" = "gg_miss_span",
                    "GGplot Missing Case Cumulative Sum" = "gg_miss_case_cumsum",
                    "GGplot Missing Which" = "gg_miss_which")),
      conditionalPanel(condition = "input.plot_type == 'gg_miss_upset'",
                       numericInput("nsets", "Number of Sets:", 10, min = 1, max = 100),
                       numericInput("nintersects", "Number of Intersects:", 50, min = 1, max = 100)),
      conditionalPanel(condition = "input.plot_type == 'geom_miss_point'",
                       selectInput("x", "X Variable:", c("Ozone", "Solar.R"), selected = "Ozone"),
                       selectInput("y", "Y Variable:", c("Ozone", "Solar.R"), selected = "Solar.R")),
      conditionalPanel(condition = "input.plot_type == 'facet_wrap'",
                       selectInput("facet", "Facet Variable:", c("Month", "Day"), selected = "Month")),
      conditionalPanel(condition = "input.plot_type == 'gg_miss_fct'",
                       selectInput("fct", "Factor Variable:", c("Marital", "Gender"), selected = "Marital")),
      conditionalPanel(condition = "input.plot_type == 'gg_miss_span'",
                       selectInput("data", "Data:", c("Pedestrian", "Hourly Counts"), selected = "Pedestrian"),
                       numericInput("span_every", "Span Every:", 3000, min = 1, max = 10000)),
      actionButton("update", "Update Plot")
    ),
    mainPanel(
      plotOutput("missing_plot")
    )
  )
)

server <- function(input, output) {
  
  plot_data <- reactive({
    switch(input$dataset,
           "airquality" = airquality,
           "riskfactors" = riskfactors,
           "pedestrian" = pedestrian)
  })
  
  observeEvent(input$update, {
    output$missing_plot <- renderPlot({
      switch(input$plot_type,
             "vis_miss" = vis_miss(plot_data()),
             "gg_miss_upset" = gg_miss_upset(plot_data(), nsets = input$nsets, nintersects = input$nintersects),
             "geom_miss_point" = ggplot(plot_data(), aes_string(x = input$x, y = input$y)) +
               geom_miss_point(),
             "facet_wrap" = ggplot(plot_data(), aes_string(x = input$x, y = input$y)) +
               geom_miss_point() + 
               facet_wrap(~ input$facet),
             "gg_miss_var" = gg_miss_var(plot_data(), show_pct = TRUE),
             "gg_miss_case" = gg_miss_case(plot_data()),
             "gg_miss_fct" = gg_miss_fct(plot_data(), fct = input$fct),
             "gg_miss_span" = gg_miss_span(input$data, hourly_counts, span_every = input$span_every),
             "gg_miss_case_cumsum" = gg_miss_case_cumsum(plot_data()),
             "gg_miss_which" = gg_miss_which(plot_data())
      )
    })
  })
}

shinyApp(ui, server)
             