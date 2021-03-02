#make a basic shiny app

library(shiny)
library(xlsx)
library(rsconnect)
library(tidyr)
library(data.table)
library("shinydashboard")
library("rsconnect")
library("PKI")
library("packrat")
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Cost Effectiveness"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Deterministic", tabName = "determ", icon = icon("balance-scale")),
      menuItem("Single Draw", tabName = "rand_draw", icon = icon("dice")),
      menuItem("Montecarlo MicroSimulation", tabName = "CEAC", icon = icon("chart-area")),
      menuItem("Randomised Willingness to Pay", tabName = "portion", icon = icon("dice-d20")),
      menuItem("The Cost-Effective Plane", tabName = "plane", icon = icon("plane")),
      menuItem("Tornado Plot", tabName = "tornado", icon = icon("poo-storm"))
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
                               100, min = 0, max = 10000), width = 4),
              box(fileInput("rand_file", 
                            "Upload Parameter Spreadsheet",
                            multiple = F,
                            accept= c("csv", "comma-separated-values", ".csv")))
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
                               100, min = 0, max = 10000), width = 6),
              box(fileInput("CEAC_file", 
                            "Upload Parameter Spreadsheet",
                            multiple = F,
                            accept= c("csv", "comma-separated-values", ".csv")))
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
                               100, min = 0, max = 10000), width = 4),
              box(fileInput("determ_file", 
                            "Upload Parameter Spreadsheet",
                            multiple = F,
                            accept= c("csv", "comma-separated-values", ".csv")))
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
                               100, min = 0, max = 10000), width = 4),
              box(fileInput("portion_file", 
                            "Upload Parameter Spreadsheet",
                            multiple = F,
                            accept= c("csv", "comma-separated-values", ".csv")))
              ),
      tabItem("plane",
              box(
                title = "For What Parameter Values Is the Intervention Cost-Effective?",
                background = "purple",
                width = 8,
                plotOutput("plane")
              ),
              box(fileInput("plane_file",
                            "Upload Parameter Spreadsheet",
                            multiple = F,
                            accept = c("csv", "comma-separated-values", ".csv")), width = 4),
              box(selectInput("xparam", "X-Parameter:",
                              c("BAU_Cost_Vector", "Int_Cost_vector", "Helth_QALY_Vector", "Sick_QALY_Vector")), width = 6),
              box(selectInput("yparam", "Y-Parameter:",
                              c("Int_Cost_vector", "BAU_Cost_Vector", "Helth_QALY_Vector", "Sick_QALY_Vector")), width = 6),
              box(numericInput("QHmin",
                               em("QoL in Healthy State - Minimum Plausible Value"),
                               0.9, min = 0, max = 1), width = 3),
              box(numericInput("QHmax",
                               em("QoL in Healthy State - Maximum Plausible Value"),
                               0.9, min = 0, max = 1), width = 3),
              box(numericInput("QSmin",
                               em("QoL in Sick State - Minimum Plausible Value"),
                               0.6, min = 0, max = 1), width = 3),
              box(numericInput("QSmax",
                               em("QoL in Sick State - Maximum Plausible Value"),
                               0.6, min = 0, max = 1), width = 3),
              box(numericInput("CImin",
                               em("Healthcare Cost with Intervention - Minimum Plausible Value"),
                               0, min = 0, max = 10000), width = 3),
              box(numericInput("CImax",
                               em("Healthcare Cost with Intervention - Maximum Plausible Value"),
                               1000, min = 0, max = 10000), width = 3),
              box(numericInput("CCmin",
                               em("Healthcare Cost without Intervention - Minimum Plausible Value"),
                               0, min = 0, max = 10000), width = 3),
              box(numericInput("CCmax",
                               em("Healthcare Cost without Intervention - Maximum Plausible Value"),
                               1000, min = 0, max = 10000), width = 3)
              
              ),
      tabItem("tornado",
              box(
                title = "Sensitivity of ICER to Each Parameter",
                background = "green",
                width = 12,
                plotOutput("Tornado")
              ),
              box(fileInput("tornado_file",
                            "Upload Parameter Spreadsheet",
                            multiple = F,
                            accept = c("csv", "comma-separated-values", ".csv")), width = 12),
              box(numericInput("t_QHmin",
                               em("QoL in Healthy State - Minimum Plausible Value"),
                               0.7, min = 0, max = 1), width = 3),
              box(numericInput("t_QHmax",
                               em("QoL in Healthy State - Maximum Plausible Value"),
                               1, min = 0, max = 1), width = 3),
              box(numericInput("t_QSmin",
                               em("QoL in Sick State - Minimum Plausible Value"),
                               0.3, min = 0, max = 1), width = 3),
              box(numericInput("t_QSmax",
                               em("QoL in Sick State - Maximum Plausible Value"),
                               0.8, min = 0, max = 1), width = 3),
              box(numericInput("t_CImin",
                               em("Healthcare Cost with Intervention - Minimum Plausible Value"),
                               0, min = 0, max = 10000), width = 3),
              box(numericInput("t_CImax",
                               em("Healthcare Cost with Intervention - Maximum Plausible Value"),
                               1000, min = 0, max = 10000), width = 3),
              box(numericInput("t_CCmin",
                               em("Healthcare Cost without Intervention - Minimum Plausible Value"),
                               0, min = 0, max = 10000), width = 3),
              box(numericInput("t_CCmax",
                               em("Healthcare Cost without Intervention - Maximum Plausible Value"),
                               1000, min = 0, max = 10000), width = 3)
              
        
      )
    )

  )
  
)

server <- function(input, output) {
  ICER_rand <- function(inputdata){
    
    inputfile <- input$rand_file
    
    if (is.null(inputfile))
      return("Same as last time, you need to choose a file")
    
    inputdata <- read.csv(inputfile$datapath)
    
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
    
    inputfile <- input$determ_file
    
    if (is.null(inputfile))
      return("You need to choose a file, I'm not a mind reader")
    
    inputdata <- read.csv(inputfile$datapath)
    
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
    
    inputfile <- input$CEAC_file
    
    inputdata <- read.csv(inputfile$datapath)
    
    inputdata <- as.data.frame(inputdata)
    
    #replace intervention cost with our input
    inputdata$state_costs_intervention[1] <- rnorm(1,input$CEAC_int_cost,100)
    
    #replace BAU healthcare cost with our input
    inputdata$state_costs_control[1] <- rnorm(1,input$CEAC_bau_cost,100)
    
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
    
    inputfile <- input$portion_file
    
    inputdata <- read.csv(inputfile$datapath)
    
    inputdata <- as.data.frame(inputdata)
    
    #replace intervention cost with our input
    inputdata$state_costs_intervention[1] <- rnorm(1,input$portion_int_cost,100)
    
    #replace BAU healthcare cost with our input
    inputdata$state_costs_control[1] <- rnorm(1,input$portion_bau_cost,100)
    
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
      if(CET >= ICER_portion(inputdata)){
        ICER_Vector_Portion[i] <- 1} 
      rm(i)
    }
    
    Answer <- c(100*mean(ICER_Vector_Portion), " % of Interventions are Cost-Effective")
    
    return(Answer)

  }
  
  cost_effective <- function(inputdata){
    
    inputfile <- input$plane_file
    
    inputdata <- read.csv(inputfile$datapath)
    
    inputdata <- as.data.frame(inputdata)
    
    BAU_Cost_Vector <-c(rep(NA,30000))
    Int_Cost_vector <-c(rep(NA,30000))
    Sick_QALY_Vector <- c(rep(NA,30000))
    Helth_QALY_Vector <- c(rep(NA,30000))
    
    CEspace <- as.data.frame(cbind(BAU_Cost_Vector, Int_Cost_vector, Sick_QALY_Vector, Helth_QALY_Vector))
    
    for(i in 1:30000){
      CET <- 10000
      inputdata$state_costs_intervention[1] <- runif(1,input$CImin,input$CImax)
      inputdata$state_costs_control[1] <- runif(1,input$CCmin,input$CCmax)
      inputdata$state_QALYs[1] <- runif(1,input$QHmin,input$QHmax)
      inputdata$state_QALYs[2] <- runif(1,input$QSmin,input$QSmax)
      
      costs_intervention <- inputdata$state_distribution_intervention * inputdata$state_costs_intervention
      total_cost_intervention <- costs_intervention[1]+costs_intervention[2]+costs_intervention[3]
      QALYs_intervention <- inputdata$state_distribution_intervention * inputdata$state_QALYs
      total_QALYs_intervention <- QALYs_intervention[1]+QALYs_intervention[2]+QALYs_intervention[3]
      
      costs_control <- inputdata$state_distribution_control * inputdata$state_costs_control
      total_cost_control <- costs_control[1]+costs_control[2]+costs_control[3]
      QALYs_control <- inputdata$state_distribution_control * inputdata$state_QALYs
      total_QALYs_control <- QALYs_control[1]+QALYs_control[2]+QALYs_control[3]
      
      ICER <- (total_cost_intervention - total_cost_control) / (total_QALYs_intervention - total_QALYs_control)
      
      if(CET >= ICER){
        CEspace$BAU_Cost_Vector[i] <- inputdata$state_costs_control[1]
        CEspace$Int_Cost_vector[i] <- inputdata$state_costs_intervention[1]
        CEspace$Helth_QALY_Vector[i] <- inputdata$state_QALYs[1]
        CEspace$Sick_QALY_Vector[i] <- inputdata$state_QALYs[2] 
      } 
      rm(i)
    }
    
    Cost_Effective_Points <- ggplot(CEspace, aes(x=CEspace[[input$xparam]], y=CEspace[[input$yparam]]))+
      geom_point()+
      xlab(input$xparam)+
      ylab(input$yparam)+
      ggtitle("Cost-Effective Plane")
    
    return(Cost_Effective_Points)
  }
  
 # tornado = function(inputdata){
 #   
 # }
  
  output$ICER <- renderText({
    ICER_rand(inputdata)
    })
  
  output$ICER_determ <- renderText ({
    ICER_determ(inputdata)
  })
  
  output$CEAC <- renderPlot ({
    CEAC(inputdata)
  })
  
  output$portion_effective <- renderText ({
    portion_effective(inputdata)
  })
  
  output$plane <- renderPlot({
    cost_effective(inputdata)
  })
  
 # output$tornado <- renderPlot({
 #   tornado(inputdata)
 # })
}

shinyApp(ui, server)