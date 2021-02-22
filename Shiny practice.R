#make a basic shiny app

library(shiny)
library(xlsx)
library(rsconnect)
library(tidyr)
library(data.table)
library("shinydashboard")


ui <- dashboardPage(
  dashboardHeader(title = "Cost Effectiveness"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Deterministic", tabName = "determ", icon = icon("balance-scale")),
      menuItem("Single Draw", tabName = "rand_draw", icon = icon("dice")),
      menuItem("Montecarlo Microimulation", tabName = "CEAC", icon = icon("chart-area")),
      menuItem("Randomised Willingness to Pay", tabName = "portion", icon = icon("dice-d20"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("rand_draw",
              box(
                title = "Incremental Cost-Effectiveness Ratio (Single Draw from a Distribution)",
                background = "fuchsia",
                width = 4,
                textOutput("ICER")
              ),
              box(numericInput("int_cost", 
                               em("Expected Healthcare Cost with Intervention:"), 
                               500, min = 0, max = 10000), width = 4),
              box(numericInput("bau_cost", 
                               em("Expected Healthcare Cost without Intervention:"), 
                               100, min = 0, max = 10000), width = 4)       
      ),
      tabItem("CEAC",
              box(
                title = "Cost-Effectiveness Acceptability Curve",
                background = "teal",
                width = 12,
                plotOutput("CEAC")
              ),
              box(numericInput("CEAC_int_cost", 
                               em("Expected Healthcare Cost with Intervention:"), 
                               500, min = 0, max = 10000), width = 6),
              box(numericInput("CEAC_bau_cost", 
                               em("Expected Healthcare Cost without Intervention:"), 
                               100, min = 0, max = 10000), width = 4) 
      ),
      tabItem("determ",
              box(
                title = "Incremental Cost-Effectiveness Ratio (Deterministic)",
                background = "red",
                width = 4,
                textOutput("ICER_determ")
              ),
              box(numericInput("determ_int_cost", 
                               em("Healthcare Cost with Intervention:"), 
                               500, min = 0, max = 10000), width = 4),
              box(numericInput("determ_bau_cost", 
                               em("Healthcare Cost without Intervention:"), 
                               100, min = 0, max = 10000), width = 4)
              ),
      tabItem("portion",
              box(
                title = "Portion of Interventions that are Cost Effective",
                background = "yellow",
                width = 4,
                textOutput("portion_effective")
              ),
              box(numericInput("portion_wtp", 
                               em("Expected Willingness to Pay for an Additional QALY:"), 
                               10000, min = 0, max = 100000000), width = 4),
              box(numericInput("portion_int_cost", 
                               em("Expected Healthcare Cost with Intervention:"), 
                               500, min = 0, max = 10000), width = 4),
              box(numericInput("portion_bau_cost", 
                               em("Expected Healthcare Cost without Intervention:"), 
                               100, min = 0, max = 10000), width = 4)
              )
    )

  )
  
)

server <- function(input, output) {
  ICER_rand <- function(inputdata){
    setwd("C:/Users/tresc/Dropbox/LSHTM")
    inputdata <- read.csv(inputdata)
    inputdata <- as.data.frame(inputdata)
    
    #replace intervention cost with a random draw
    inputdata$state_costs_intervention[1] <- rnorm(1,input$int_cost,100)
    
    #replace BAU cost with a random draw
    inputdata$state_costs_control[1] <- rnorm(1,input$bau_cost,100)
    
    costs_intervention <- inputdata$state_distribution_intervention * inputdata$state_costs_intervention
    total_cost_intervention <- costs_intervention[1]+costs_intervention[2]+costs_intervention[3]
    QALYs_intervention <- inputdata$state_distribution_intervention * inputdata$state_QALYs
    total_QALYs_intervention <- QALYs_intervention[1]+QALYs_intervention[2]+QALYs_intervention[3]
    
    costs_control <- inputdata$state_distribution_control * inputdata$state_costs_control
    total_cost_control <- costs_control[1]+costs_control[2]+costs_control[3]
    QALYs_control <- inputdata$state_distribution_control * inputdata$state_QALYs
    total_QALYs_control <- QALYs_control[1]+QALYs_control[2]+QALYs_control[3]
    
    ICER <- (total_cost_intervention - total_cost_control) / (total_QALYs_intervention - total_QALYs_control)
    
    return(ICER)
  } 
  
  ICER_determ <- function(inputdata){
    setwd("C:/Users/tresc/Dropbox/LSHTM")
    inputdata <- read.csv(inputdata)
    inputdata <- as.data.frame(inputdata)
    
    #replace intervention cost with our input
    inputdata$state_costs_intervention[1] <- input$determ_int_cost
    
    #replace BAU cost with our input
    inputdata$state_costs_control[1] <- input$determ_bau_cost
    
    costs_intervention <- inputdata$state_distribution_intervention * inputdata$state_costs_intervention
    total_cost_intervention <- costs_intervention[1]+costs_intervention[2]+costs_intervention[3]
    QALYs_intervention <- inputdata$state_distribution_intervention * inputdata$state_QALYs
    total_QALYs_intervention <- QALYs_intervention[1]+QALYs_intervention[2]+QALYs_intervention[3]
    
    costs_control <- inputdata$state_distribution_control * inputdata$state_costs_control
    total_cost_control <- costs_control[1]+costs_control[2]+costs_control[3]
    QALYs_control <- inputdata$state_distribution_control * inputdata$state_QALYs
    total_QALYs_control <- QALYs_control[1]+QALYs_control[2]+QALYs_control[3]
    
    ICER <- (total_cost_intervention - total_cost_control) / (total_QALYs_intervention - total_QALYs_control)
    
    return(ICER)
  }
  
  CEAC_ICER_function <- function(inputdata){
    setwd("C:/Users/tresc/Dropbox/LSHTM")
    inputdata <- read.csv(inputdata)
    inputdata <- as.data.frame(inputdata)
    
    #replace intervention cost with our input
    inputdata$state_costs_intervention[1] <- rnorm(1,input$CEAC_int_cost,100)
    
    #replace BAU healthcare cost with our input
    inputdata$stata_costs_control[1] <- rnorm(1,input$CEAC_bau_cost,100)
    
    costs_intervention <- inputdata$state_distribution_intervention * inputdata$state_costs_intervention
    total_cost_intervention <- costs_intervention[1]+costs_intervention[2]+costs_intervention[3]
    QALYs_intervention <- inputdata$state_distribution_intervention * inputdata$state_QALYs
    total_QALYs_intervention <- QALYs_intervention[1]+QALYs_intervention[2]+QALYs_intervention[3]
    
    costs_control <- inputdata$state_distribution_control * inputdata$state_costs_control
    total_cost_control <- costs_control[1]+costs_control[2]+costs_control[3]
    QALYs_control <- inputdata$state_distribution_control * inputdata$state_QALYs
    total_QALYs_control <- QALYs_control[1]+QALYs_control[2]+QALYs_control[3]
    
    CEAC_ICER <- (total_cost_intervention - total_cost_control) / (total_QALYs_intervention - total_QALYs_control)
    
    return(CEAC_ICER)
  }
  
  CEAC <- function(inputdata){
    
    ICER_Vector <- c(rep(0,10000))
    for(i in 1:10000){
      ICER_Vector[i] <- CEAC_ICER_function(inputdata)
      rm(i)
    }
    
    density <- ecdf(ICER_Vector)
    
    CEAC <- plot(density,
                 xlab = 'Willingness to Pay to Avert an Additional DALY',
                 ylab = "Portion of Interventions Cost-Effective",
                 main = 'Cost Effectiveness Acceptability Curve')
    
    return(CEAC)
    
  }
  
  ICER_portion <- function(inputdata){
    setwd("C:/Users/tresc/Dropbox/LSHTM")
    inputdata <- read.csv(inputdata)
    inputdata <- as.data.frame(inputdata)
    
    #replace intervention cost with our input
    inputdata$state_costs_intervention[1] <- rnorm(1,input$portion_int_cost,100)
    
    #replace BAU healthcare cost with our input
    inputdata$stata_costs_control[1] <- rnorm(1,input$portion_bau_cost,100)
    
    costs_intervention <- inputdata$state_distribution_intervention * inputdata$state_costs_intervention
    total_cost_intervention <- costs_intervention[1]+costs_intervention[2]+costs_intervention[3]
    QALYs_intervention <- inputdata$state_distribution_intervention * inputdata$state_QALYs
    total_QALYs_intervention <- QALYs_intervention[1]+QALYs_intervention[2]+QALYs_intervention[3]
    
    costs_control <- inputdata$state_distribution_control * inputdata$state_costs_control
    total_cost_control <- costs_control[1]+costs_control[2]+costs_control[3]
    QALYs_control <- inputdata$state_distribution_control * inputdata$state_QALYs
    total_QALYs_control <- QALYs_control[1]+QALYs_control[2]+QALYs_control[3]
    
    portion_ICER <- (total_cost_intervention - total_cost_control) / (total_QALYs_intervention - total_QALYs_control)
    
    return(portion_ICER)
  }
  
  portion_effective <- function(inputdata){
    
    ICER_Vector_Portion <- c(rep(0,10000))
    
    for(i in 1:10000){
      CET <- rnorm(1,input$portion_wtp,2500)
      if(CET <= ICER_portion("inputdata.csv")){
        ICER_Vector_Portion[i] <- 1} 
      rm(i)
    }
    
    Answer <- c(100*mean(ICER_Vector_Portion), " % of Interventions are Cost-Effective")
    
    return(Answer)
  }
  
  output$ICER <- renderText({
    ICER_rand("inputdata.csv")
    })
  
  output$ICER_determ <- renderText ({
    ICER_determ("inputdata.csv")
  })
  
  output$CEAC <- renderPlot ({
    CEAC("inputdata.csv")
  })
  
  output$portion_effective <- renderText ({
    portion_effective("inputdata.csv")
  })
}

shinyApp(ui, server)