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

##New branch
#Lorem ipsum dolor sit amet

#fhreuiheruifherufirehi