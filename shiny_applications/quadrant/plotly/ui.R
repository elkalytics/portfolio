library(shiny)
library(ggplot2)
library(readxl)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Rescaled Scatter Plot with Loess Smooth"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload data file (csv or xlsx)"),
      selectInput("x", "Select X variable:", ""),
      selectInput("y", "Select Y variable:", "")
    ),
    mainPanel(
      plotlyOutput("scatterplot")
    )
  )
))