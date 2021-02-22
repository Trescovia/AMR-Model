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

################################################################################

cost_effective <- function(inputdata){
  
  setwd("C:/Users/tresc/Dropbox/LSHTM")
  inputdata <- read.csv(inputdata)
  inputdata <- as.data.frame(inputdata)
  
  BAU_Cost_Vector <-c(rep(NA,10000))
  Int_Cost_vector <-c(rep(NA,10000))
  Sick_QALY_Vector <- c(rep(NA,10000))
  Helth_QALY_Vector <- c(rep(NA,10000))
  
  CEspace <- as.data.frame(cbind(BAU_Cost_Vector, Int_Cost_vector, Sick_QALY_Vector, Helth_QALY_Vector))
  
  for(i in 1:10000){
    CET <- rnorm(1,10000,2500)
    inputdata$state_costs_intervention[1] <- rnorm(1,500,100)
    inputdata$state_costs_control[1] <- rnorm(1,100,100)
    inputdata$state_QALYs[1] <- runif(1,0.8,1)
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
    
    if(abs(CET - ICER) < 250){
      CEspace$BAU_Cost_Vector[i] <- inputdata$state_costs_control[1]
      CEspace$Int_Cost_vector[i] <- inputdata$state_costs_intervention[1]
      CEspace$Helth_QALY_Vector[i] <- inputdata$state_QALYs[1]
      CEspace$Sick_QALY_Vector[i] <- inputdata$state_QALYs[2] 
    } 
    rm(i)
  }
  
  Cost_Effective_Points <- plot(CEspace$BAU_Cost_Vector, CEspace$Int_Cost_vector,
                                abline(lm(BAU_Cost_Vector ~ Int_Cost_vector, data = CEspace)),
                                xlab = "Healthcare Cost without Intervention",
                                ylab = "Healthcare Cost with Intervention",)
  
  return(Cost_Effective_Points)
}

cost_effective("inputdata.csv")

a <- c(1,2,3,4,5,6,7,8,9)
b <- c(9,8,7,6,5,4,3,2,1)

plot(a,b,
     abline(lm(a~b)))
