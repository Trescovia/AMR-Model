#make a basic shiny app

library(shiny)
library(xlsx)
library(rsconnect)
library(tidyr)
library(data.table)
library("shinydashboard")

ui <- dashboardPage(
  dashboardHeader(title = "Page Title"),
  dashboardSidebar(),
  dashboardBody(
    box(plotOutput("plot name"), width = 8)
  )
  
)

server <- function(input, output) {
  output$our_plot <- renderPlot({
    plot(iris$Sepal.Length, iris$Petal.Length)
  })
}

shinyApp(ui, server)