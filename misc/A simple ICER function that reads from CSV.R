setwd("C:/Users/tresc/Dropbox/LSHTM")

ICER_csv <- function(inputdata){
  setwd("C:/Users/tresc/Dropbox/LSHTM")
  inputdata <- read.csv(inputdata)
  inputdata <- as.data.frame(inputdata)
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

ICER_csv("inputdata.csv")
ICER_csv("pessimistic inputs.csv")
ICER_csv("optimistic inputs.csv")

################################################################################
#Try randomising part of the function, and storing simulation results

ICER_rand <- function(inputdata){
  setwd("C:/Users/tresc/Dropbox/LSHTM")
  inputdata <- read.csv(inputdata)
  inputdata <- as.data.frame(inputdata)
  
  #replace intervention cost with a random draw
  inputdata$state_costs_intervention[1] <- rnorm(1,500,100)
  
  #replace bau cost with a random draw
  inputdata$state_costs_control[1] <- rnorm(1,100,100)
  
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

ICER_rand("inputdata.csv")

ICER_Vector <- c(rep(0,10000))

for(i in 1:10000){
  ICER_Vector[i] <- ICER_rand("inputdata.csv")
  rm(i)
}

hist(ICER_Vector)

density <- density(ICER_Vector)

plot(density)

CEAC <- ecdf(ICER_Vector)

plot(CEAC,
     xlab = 'Willingness to Pay to Avert an Additional DALY',
     ylab = "Portion of Interventions Cost-Effective",
     main = 'Cost Effectiveness Acceptability Curve')


################################################################################
#Also randomise the CET/WTP

set.seed(420)

ICER_Vector <- c(rep(0,10000))

for(i in 1:10000){
  CET <- rnorm(1,10000,2500)
  if(CET <= ICER_rand("inputdata.csv")){
  ICER_Vector[i] <- 1} 
  rm(i)
}

sum(ICER_Vector)

################################################################################
#turn this into a function as well
Chance_effective <- function(inputdata){
  
  ICER_Vector <- c(rep(0,10000))
  
  for(i in 1:10000){
    CET <- rnorm(1,10000,2500)
    if(CET >= ICER_rand("inputdata.csv")){
      ICER_Vector[i] <- 1} 
    rm(i)
  }
  
  Answer <- paste(100*mean(ICER_Vector), "% of Interventions are Cost-Effective")
    
  return(Answer)
}

Chance_effective("inputdata.csv")

################################################################################
#build a function that plots the cost-effective space, where you can choose which two variables to plot

ICER_rand_2 <- function(inputdata){
  setwd("C:/Users/tresc/Dropbox/LSHTM")
  inputdata <- read.csv(inputdata)
  inputdata <- as.data.frame(inputdata)
  
  #replace intervention cost with a random draw
  inputdata$state_costs_intervention[1] <- rnorm(1,500,100)
  
  #replace bau cost with a random draw
  inputdata$state_costs_control[1] <- rnorm(1,100,100)
  
  #replace the healthy quality of life with a random draw
  inputdata$state_QALYs[1] <- runif(1,0.8,1)
  
  #replace the sick quality of life with a random draw
  inputdata$state_QALYs[2] <- runif(1,0.3,0.7)
  
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

library(tidyverse)

cost_effective <- function(inputdata){
  
  setwd("C:/Users/tresc/Dropbox/LSHTM")
  inputdata <- read.csv(inputdata)
  inputdata <- as.data.frame(inputdata)
  
  BAU_Cost_Vector <-c(rep(NA,100000))
  Int_Cost_vector <-c(rep(NA,100000))
  Sick_QALY_Vector <- c(rep(NA,100000))
  Helth_QALY_Vector <- c(rep(NA,100000))
  
  CEspace <- as.data.frame(cbind(BAU_Cost_Vector, Int_Cost_vector, Sick_QALY_Vector, Helth_QALY_Vector))
  
  for(i in 1:100000){
    CET <- 10000
    inputdata$state_costs_intervention[1] <- runif(1,0,1000)
    inputdata$state_costs_control[1] <- rnorm(1,0,1000)
    
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
  
  Cost_Effective_Points <- ggplot(CEspace, aes(x=BAU_Cost_Vector, y=Int_Cost_vector))+
  geom_point()+
  coord_cartesian(xlim = c(0,1000), ylim = c(0,1000))+
  xlab("Health Cost without Intervention")+
  ylab("Health Cost with Intervention")+
  ggtitle("Cost-Effective Plane")
  
  return(Cost_Effective_Points)
}

cost_effective("inputdata.csv")

################################################################################
#Tornado plots (create a different dataset for each ICER)

ICER_tornado <- function(inputdata){

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


ICER_compare <- ICER_tornado(read.csv("inputdata.csv"))

data_BAUcost_max <- read.csv("inputdata.csv") 
data_BAUcost_max$state_costs_control[1] <- 500
ICER_BAUcost_max <- ICER_tornado(data_BAUcost_max)
BAUcost_max <- ICER_BAUcost_max - ICER_compare

data_BAUcost_min <- read.csv("inputdata.csv") 
data_BAUcost_min$state_costs_control[1] <- 0
ICER_BAUcost_min <- ICER_tornado(data_BAUcost_min)
BAUcost_min <- ICER_BAUcost_min - ICER_compare

data_intcost_max <- read.csv("inputdata.csv") 
data_intcost_max$state_costs_intervention[1] <- 1000
ICER_intcost_max <- ICER_tornado(data_intcost_max)
intcost_max <- ICER_intcost_max - ICER_compare

data_intcost_min <- read.csv("inputdata.csv") 
data_intcost_min$state_costs_intervention[1] <- 100
ICER_intcost_min <- ICER_tornado(data_intcost_min)
intcost_min <- ICER_intcost_min - ICER_compare

data_healthyqol_max <- read.csv("inputdata.csv")
data_healthyqol_max$state_QALYs[1] <- 1
ICER_healthyqol_max <- ICER_tornado(data_healthyqol_max)
healthyqol_max <- ICER_healthyqol_max - ICER_compare

data_healthyqol_min <- read.csv("inputdata.csv") 
data_healthyqol_min$state_QALYs[1] <- 0.7
ICER_healthyqol_min <- ICER_tornado(data_healthyqol_max)
healthyqol_min <- ICER_healthyqol_min - ICER_compare

data_sickqol_max <- read.csv("inputdata.csv") 
data_sickqol_max$state_QALYs[2] <- 0.8
ICER_sickqol_max <- ICER_tornado(data_sickqol_max)
sickqol_max <- ICER_sickqol_max - ICER_compare

data_sickqol_min <- read.csv("inputdata.csv") 
data_sickqol_min$state_QALYs[2] <- 0.3
ICER_sickqol_min <- ICER_tornado(data_sickqol_max) 
sickqol_min <- ICER_sickqol_min - ICER_compare












Tornado <- function(inputdata){
  setwd("C:/Users/tresc/Dropbox/LSHTM")
  inputdata <- read.csv(inputdata)
  inputdata <- as.data.frame(inputdata)
  
  ICER_compare <- ICER_tornado(inputdata)
  
  data_BAUcost_max <- read.csv(inputdata) 
  data_BAUcost_max$state_costs_control[1] <- 500
  ICER_BAUcost_max <- ICER_tornado(data_BAUcost_max)
  BAUcost_max <- ICER_BAUcost_max - ICER_compare
  
  data_BAUcost_min <- read.csv(inputdata) 
  data_BAUcost_min$state_costs_control[1] <- 0
  ICER_BAUcost_min <- ICER_tornado(data_BAUcost_min)
  BAUcost_min <- ICER_BAUcost_min - ICER_compare
  
  data_intcost_max <- read.csv(inputdata) 
  data_intcost_max$state_costs_control[1] <- 1000
  ICER_intcost_max <- ICER_tornado(data_intcost_max)
  intcost_max <- ICER_intcost_max - ICER_compare
  
  data_intcost_min <- read.csv(inputdata) 
  data_intcost_min$state_costs_control[1] <- 100
  ICER_intcost_min <- ICER_tornado(data_intcost_min)
  intcost_min <- ICER_intcost_min - ICER_compare
  
  data_healthyqol_max <- read.csv(inputdata)
  data_healthyqol_max$state_QALYs[1] <- 1
  ICER_healthyqol_max <- ICER_tornado(data_healthyqol_max)
  healthyqol_max <- ICER_healthyqol_max - ICER_compare

  data_healthyqol_min <- read.csv(inputdata) 
  data_healthyqol_min$state_QALYs[1] <- 0.7
  ICER_healthyqol_min <- ICER_tornado(data_healthyqol_max)
  healthyqol_min <- ICER_healthyqol_min - ICER_compare
  
  data_sickqol_max <- read.csv(inputdata) 
  data_sickqol_max$state_QALYs[2] <- 0.8
  ICER_sickqol_max <- ICER_tornado(data_sickqol_max)
  sickqol_max <- ICER_sickqol_max - ICER_compare
  
  data_sickqol_min <- read.csv(inputdata) 
  data_sickqol_min$state_QALYs[2] <- 0.3
  ICER_sickqol_min <- ICER_tornado(data_sickqol_max) 
  sickqol_min <- ICER_sickqol_min - ICER_compare
  
  tornado_plot <- ggplot(data.frame(y=c(0,5)), aes(y))+
    geom_segment(aes(x = sickqol_min, y = 1, xend = sickqol_max, yend = 1, colour = "blue"))+
    geom_segment(aes(x = healthyqol_min, y = 2, xend = healthyqol_max, yend = 2, colour = "green"))+
    geom_segment(aes(x = intcost_min, y = 3, xend = intcost_max, yend = 3, colour = "pink"))+
    geom_segment(aes(x = BAUcost_min, y = 4, xend = BAUcost_min, yend = BAUcost_max, colour = "orange"))
    
  return(tornado_plot)
}

Tornado("inputdata.csv")
