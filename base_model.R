####### LIBRARIES & DATA INPUts #######
library(data.table)
library("readxl")
library("stargazer")
library("tidyverse")
library("tseries")
library("forecast")
library("dynlm")
library("seastests")
library("forecast")
library("TSA")
library("epiR")
library("extraDistr")
library("MonoInc")
library("pksensi")
library("sensitivity")

pop <- read.csv("C:/Users/tresc/Desktop/AMR-Model/Population data for ARIMA/Vietnam Population.csv")
pop <- ts(pop$Population, start = 1960, frequency = 1)#
ARIMApop <- auto.arima(pop, stepwise = F, approximation = F)
plot(forecast(ARIMApop, 50))
forecast(ARIMApop, 50)
predict <- forecast(ARIMApop, 50)
predict$mean
futurepop <- predict$mean

births <- numeric()
for (i in 1:49) {
  births[i] <- futurepop[i+1] - futurepop[i]
} #atm births[1] shows NET births in 2020

births <- births[2:49] #now births[1] is NET births in 2021

birthrate <- numeric()
for (i in 1:49) {
  birthrate[i] <- (futurepop[i+1] - futurepop[i])/futurepop[i]
} #atm birthrate[1] shows NET birthrate in 2020

birthrate <- birthrate[2:49] #now birthrate[1] shows NET birthrate in 2021

#upper bound population projection
futurepop_up <- predict$upper
futurepop_up <- futurepop_up[,2]

birthrate_up <- numeric()
for (i in 1:49) {
  birthrate_up[i] <- (futurepop_up[i+1] - futurepop_up[i])/futurepop_up[i]
} #atm birthrate_up[1] shows rate for 2020

birthrate_up <- birthrate_up[2:49] #now it shows upper rates for 2021

#lower bound population projection
futurepop_low <- predict$lower
futurepop_low <- futurepop_low[,2]

birthrate_low <- numeric()
for (i in 1:49){
  birthrate_low[i] <- (futurepop_low[i+1] - futurepop_low[i])/futurepop_low[i]
} #atm birthrate_low[1] shows rate for 2020

birthrate_low <- birthrate_low[2:49] #now it shows lower rates for 2021

##here I have predicted net births and will use this as the 'birth' parameter. This means that the background 
##mortality will be set to 0 because the net birthrate already accounts for background mortality

####dependency ratio

dependency <- read.csv("C:/Users/tresc/Desktop/AMR-Model/Population data for ARIMA/Viet Nam dependency ratio.csv")
dependency <- ts(dependency$Dependency.Ratio, start = 1960, frequency = 1)
arimadependency <- auto.arima(dependency, stepwise = F, approximation = F)
plot(forecast(arimadependency, 50))
predictdependency <- forecast(arimadependency, 50)
predictdependency$mean
futuredepend <- predictdependency$mean

dependency <- numeric()
for(i in 1:49) {
  dependency[i] <- futuredepend[i + 1]
} #now dependency[1] is the dependency in 2021, not 2020

rm(i)

#now get portion working age
dependency <- dependency * 0.01

portion_working_age <- 1/(1+dependency)

plot(portion_working_age)

portion_working <- portion_working_age * 0.98162 * 0.767 #assuming unemployment rate equal to mean of 2016-2020 and LFPR equal to mean of 2016-2019

plot(portion_working)


#population and working population from WB projections

##working population
wb_working_pop <- read.csv("C:/Users/tresc/Desktop/AMR-Model/Population data for ARIMA/Viet Nam Working Population WB.csv")

wb_working_pop <- ts(wb_working_pop$Working.Population, start = 1960, frequency = 1)

plot(wb_working_pop)

arimawb <- auto.arima(wb_working_pop, stepwise = F, approximation = F)

summary(arimawb) #5th degree autoregressive, 2nd degree integration, no moving average component

plot(forecast(arimawb, 17))

forecast(arimawb,17)

working_population <- c(rep(0,47))

for (i in 1:30) {
  working_population[i] <- wb_working_pop[i + 61] #first value is for 1960, so now working_population[1] is for 2021
}

working_population[30]

future_wb <- forecast(arimawb,17)
future_wb$mean[1] #2051 is first year

for (i in 31:47) {
  working_population[i] <- future_wb$mean[i-30]
}

working_population ## this one for working population

working_population[3]

##total population
wb_pop <- read.csv("C:/Users/tresc/Desktop/AMR-Model/Population data for ARIMA/Viet Nam Population WB.csv")

wb_pop <- ts(wb_pop$Population, start = 1960, frequency = 1)

plot(wb_pop)

arimawbpop <- auto.arima(wb_pop, stepwise = F, approximation = F)

summary(arimawbpop) #3rd degree autoregressive, 2nd degree integration, no moving average component

plot(forecast(arimawbpop, 17))

forecast(arimawbpop,17)

wb_population <- c(rep(0,47))

for (i in 1:30) {
  wb_population[i] <- wb_pop[i + 61] #first value is for 1960, so now wb_population[1] is for 2021
}

wb_population[30]

future_wb_pop <- forecast(arimawbpop,17)
future_wb_pop$mean[1] #2051 is first year

for (i in 31:47) {
  wb_population[i] <- future_wb_pop$mean[i-30]
}

wb_population #this one for total pop

wb_population[3]

################################################################################


###### defined parameters across the sectors ######
n.t <- 47 ## time horizon - 46 years + cycle 0 (initial states)
dr <- 0.08 ## discount rate
wtp <- 2000 ## willingness to pay per QALY gained
scenario <- "HCA"

############# model functions
inputs <- read.csv("C:/Users/tresc/Desktop/AMR-Model/input_V.csv")
inputs <- as.data.table(inputs)

model <- function(inputs){
  
  inputs[ , value := as.numeric(as.character(value))]
  
  human <- inputs[scenario=="human_0" | scenario=="human_1"]
  animal <- inputs[scenario=="animal_0" | scenario=="animal_1"]
  intervention <- inputs[scenario=="intervention"]
  
  
  ##### functions used across the sectors##########
  f_expvalue <- function(x,y,z){
    ## x is the epi matrix
    ## y is the cost matrix
    ## z is the reward matrix
    results <- matrix(c(sum(x*y),sum(x*z)),1,2)
    return(results)
    
  }
  
  f_di <- function(x,y){
    # function to apply a discount rate
    # x is cost
    # y is discount rate 
    x2 <- x - (x*y)
    return(x2)
  }
  
  
  ###################*****HUMAN MODEL*****###########################
  #for HCA and FCA, we only care about the losses in productivity, so we set the 
  #reward for 'well' to be zero. Going into the 'dead' state incurs a productivity
  #loss equal to the discounted value of future earnings, going into the 'res' or 'sus
  #states incurs a loss equal to the earnings that would have been made during the 
  #time in hospital. After 1 period, all people in 'dead' go to 'afterlife', which
  #has a reward of zero
  
  state_names <- c("well", "res","sus","dead", "afterlife") ## the compartments
  transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s", "dead_aft")  ## the transition probabilities
  parameter_names <- c(state_names, transition_names)
  
  ## initial state vector (population starting in well)
  state_i <- c(intervention[parameter=="n_population",value], rep(0,length=length(state_names)-1))
  
  ## matrix of parameter values over cycles
  m_param <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_param) <- parameter_names
  rownames(m_param) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  
  m_param[ , "r"] <- rep(human[parameter=="well_r",value], n.t)
  m_param[ , "s"] <- rep(human[parameter=="well_s",value], n.t)
  m_param[ , "mort_r"] <- rep(human[parameter=="r_dead",value], n.t)
  m_param[ , "mort_s"] <- rep(human[parameter=="s_dead",value], n.t)
  m_param[ , "rec_r"] <- rep(1-(m_param[1,"mort_r"]), n.t)
  m_param[ , "rec_s"] <- rep(1-(m_param[1,"mort_s"]), n.t)
  m_param[ , "birth"] <- birthrate[1:n.t] ##set to be predicted net births
  m_param[ , "mort_w"] <- rep(0, n.t) ##set to zero because background mortality is included in net births
  m_param[ , "dead_aft"] <- rep(1, n.t) #all those who die go to the afterlife
  
  m_param[1, 1:length(state_names)] <- state_i ## adding initial cycle 0 values
  
  ## the difference equation function: 
  f_human_epi <- function(m_param, n.t){
    for (i in 2:(n.t)){
      m_param[i,"well"] <- m_param[i-1,"well"] -(m_param[i-1,"r"]*m_param[i-1,"well"]) -
        (m_param[i-1,"s"]*m_param[i-1,"well"]) + (m_param[i-1,"birth"]*m_param[i-1,"well"])-
        (m_param[i-1,"mort_w"]*m_param[i-1,"well"])+(m_param[i-1,"rec_r"]*m_param[i-1,"res"])+
        (m_param[i-1,"rec_s"]*m_param[i-1,"sus"])
      
      m_param[i,"res"] <- m_param[i-1,"res"] + (m_param[i-1,"r"]*m_param[i-1,"well"]) - 
        (m_param[i-1,"mort_r"]*m_param[i-1,"res"]) - (m_param[i-1,"rec_r"]*m_param[i-1,"res"])
      
      m_param[i,"sus"] <- m_param[i-1,"sus"] + (m_param[i-1,"s"]*m_param[i-1,"well"])
      - (m_param[i-1,"mort_s"]*m_param[i-1,"sus"]) - (m_param[i-1,"rec_s"]*m_param[i-1,"sus"])
      
      m_param[i,"dead"] <- (m_param[i-1,"mort_r"]*m_param[i-1,"res"]) + (m_param[i-1,"mort_s"]*m_param[i-1,"sus"])+
        (m_param[i-1,"mort_w"]*m_param[i-1,"well"])
      ## note that this is the incidence of death due to how we then multiply with QALY loss but 
      # if change that should also add in + m_param[i-1,"dead"] 
      
      m_param[i, "afterlife"] <- m_param[i-1, "afterlife"] + m_param[i-1, "dead"] #just keeps growing
    }
    return(m_param)
  }
  
  m_param <- f_human_epi(m_param,n.t) ## currently population growth over time
  
  #### HC system cost ###########
  m_cost <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_cost) <- parameter_names
  rownames(m_cost) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_r <- human[parameter=="r_cost",value]
  c_s <- human[parameter=="s_cost",value]
  
  cost_i <- c(0,c_r,c_s,0, 0)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost[2, 1:length(state_names)] <- cost_i
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost[i,j] <- f_di(m_cost[i-1,j],dr)
    }  
  }
  
  #### HC system rewards ##################
  m_rwd <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_rwd) <- parameter_names
  rownames(m_rwd) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  pv_fut_life <- c(rep(0,46)) #expected remaining life years
  for (i in 1:46){
    pv_fut_life[i] <- human[parameter=="background_qol",value] * (1-dr)^(i-1)
  }
  pv_life <- sum(pv_fut_life)
  
  r_s <- human[parameter=="background_qol",value]*(human[parameter=="hrqol_ill",value]-1) ## QoL lost from time in hospital
  r_r <- human[parameter=="background_qol",value]*(human[parameter=="hrqol_res",value]-1) ## same but adjusted for longer LoS
  r_d <- -1 * pv_life #discounted QoL loss from death
  
  rwd_i <- c(0,r_r,r_s,r_d, 0) 
  
  ##############################################################################
  #have changed health state rewards to be 1, 0.98 0.975, 0, 0 
  ##############################################################################
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd[2, 1:length(state_names)] <- rwd_i
  
  ### accounting for discounting
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd[i,j] <- f_di(m_rwd[i-1,j],dr)
    }  
  }
  
  #### Productivity Costs ###########
  #for HCA and FCA, we only care about the losses in productivity, so we set the 
  #reward for 'well' to be zero. Going into the 'dead' state incurs a productivity
  #loss equal to the discounted value of future earnings, going into the 'res' or 'sus
  #states incurs a loss equal to the earnings that would have been made during the 
  #time in hospital. After 1 period, all people in 'dead' go to 'afterlife', which
  #has a reward of zero
  
  m_cost_prod <- matrix(rep(0),nrow = n.t, ncol = length(parameter_names))
  colnames(m_cost_prod) <- parameter_names
  rownames(m_cost_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  c_r_prod <- 0
  c_s_prod <- 0
  
  cost_i_prod <- rep(0,(length(state_names)))
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_cost_prod[2,1:length(state_names)] <- cost_i_prod
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost_prod[i,j] <- f_di(m_cost_prod[i-1,j],dr)
    }
  }
  
  
  #### Productivity Rewards #########
  
  #calculate the discounted value of future work
  #
  
  m_rwd_prod <- matrix(rep(0), nrow = n.t, ncol = length(parameter_names))
  colnames(m_rwd_prod) <- parameter_names
  rownames(m_rwd_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  r_r_prod <- -1*(human[parameter=="prod",value]+human[parameter=="unpaid_prod",value])*(1-0.923504449) #accounting for longer stay if res
  
  r_s_prod <- -1*(human[parameter=="prod",value]+human[parameter=="unpaid_prod",value])*(1-0.94)
  
  r_w_prod <- 0
  
  r_aft_prod <- 0
  
  yearly_prod <- (human[parameter=="prod",value] + human[parameter=="unpaid_prod", value])
  pv_fut_prod <- c(rep(0,34)) #expected remaining working years
  for (i in 1:34){
    pv_fut_prod[i] <- yearly_prod * (1-dr)^(i-1)
  }
  pv_life_prod <- sum(pv_fut_prod)
  
  if(scenario == "HCA"){
    r_d_prod <- -1 * pv_life_prod
  } else if(scenario == "FCA"){
    r_d_prod <- -0.5 * yearly_prod
  } else{
    paste("ERROR: PLEASE CHOOSE AN APPROACH TO ESTIMATING PRODUCTIVITY OUTCOMES")
  }
  
  
  #multiply the initial productivity rewards by the portion of people who are working
  rwd_i_prod <- c(portion_working[1]*r_w_prod, portion_working[1]*r_r_prod, portion_working[1]*r_s_prod, portion_working[1]*r_d_prod, r_aft_prod)
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_rwd_prod[2,1:length(state_names)] <- rwd_i_prod 
  
  ### discount, but also sneakily add the labour productivity growth
  
  dr_pgrowth <- dr - human[parameter=="prod_growth", value] ##discount rate net of productivity growth (in this case prod growth exceeds discount rate)
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd_prod[i,j] <- f_di(m_rwd_prod[i-1,j],dr_pgrowth)
    }
  }
  
  #now we multiply the productivity rewards across all states and time steps
  #(apart from initial, which we already did) by the portion of people working
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)) {
      m_rwd_prod[i,j] <- m_rwd_prod[i,j]*portion_working[i]
    }
  } 
  
  ###################*****ANIMAL MODEL*****###########################
  
  state_names_a <- c("well", "res","sus","fallen","sold") ## the compartments
  transition_names_a  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
  ## birth = replacement in this scenario
  parameter_names_a <- c(state_names_a, transition_names_a)
  
  state_i_a <- c(intervention[parameter=="n_animals",value], rep(0,length=length(state_names_a)-1))
  
  m_param_a_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_param_a_base) <- parameter_names_a
  rownames(m_param_a_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  m_param_a_base[ , "r"] <- rep(animal[parameter=="well_r",value], n.t)
  m_param_a_base[ , "s"] <- rep(animal[parameter=="well_s",value], n.t)
  m_param_a_base[ , "mort_s"] <- rep(animal[parameter=="s_dead",value], n.t)
  m_param_a_base[ , "mort_r"] <- rep(animal[parameter=="r_dead",value], n.t)
  m_param_a_base[ , "rec_r"] <- rep(1-(m_param_a_base[1,"mort_r"]), n.t)
  m_param_a_base[ , "rec_s"] <- rep(1-(m_param_a_base[1,"mort_s"]), n.t)
  m_param_a_base[ , "birth"] <- rep(animal[parameter=="birth_well",value], n.t) ## !!! doesn't really do anything for now given restructure 
  m_param_a_base[ , "mort_w"] <- rep(animal[parameter=="well_dead",value], n.t)
  m_param_a_base[ , "w_sold"] <- rep(1, n.t) 
  
  m_param_a_base[1, 1:length(state_names_a)] <- state_i_a
  
  f_animal_epi <- function(m_param_a_base, n.t){
    ### !!! started process to update to include tunnel states
    ## but need to go through and check this
    ### this has for each cycle;
    ## row 1: all the new animals in well bought
    ## row 2: all the cycle transitions to ill 
    ## row 3: they recover/die from ill
    ## row 4: all the well go to sold
    ### then need to sum up across these for each cycle
    ## so you have number of bought, ill, and sold for each cycle
    ## currently assuming those who die of illness/prematurely are not sold !!! probably needs investigating
    
    ## current structure assumes nothing changes over time
    ## if things do can build each individual cycle aggregates
    ## based on the changes wanted
    
    ## create mini matrix to represent the 4 rows per cycle
    
    m_param_a_temp <- m_param_a_base[1:4,]
    rownames(m_param_a_temp) <- NULL ##removing rownames
    
    ### split over time to allow for transition probabilities to sum to 1
    ### first transitions
    i<- 2 ## row 2 definitions ## background mortality happens at beginning of cycle 
    m_param_a_temp[i,"well"] <- m_param_a_temp[i-1,"well"] -(m_param_a_temp[i-1,"r"]*m_param_a_temp[i-1,"well"]) -
      (m_param_a_temp[i-1,"s"]*m_param_a_temp[i-1,"well"]) - (m_param_a_temp[i-1,"mort_w"]*m_param_a_temp[i-1,"well"])
    m_param_a_temp[i,"res"] <- m_param_a_temp[i-1,"res"] + (m_param_a_temp[i-1,"r"]*m_param_a_temp[i-1,"well"]) 
    m_param_a_temp[i,"sus"] <- m_param_a_temp[i-1,"sus"] + (m_param_a_temp[i-1,"s"]*m_param_a_temp[i-1,"well"])
    m_param_a_temp[i,"fallen"] <- (m_param_a_temp[i-1,"mort_w"]*m_param_a_temp[i-1,"well"]) 
    
    i <- 3 ###
    m_param_a_temp[i,"well"] <-  m_param_a_temp[i-1,"well"]+(m_param_a_temp[i-1,"rec_r"]*m_param_a_temp[i-1,"res"])+ 
      (m_param_a_temp[i-1,"rec_s"]*m_param_a_temp[i-1,"sus"])
    m_param_a_temp[i,"res"] <- m_param_a_temp[i-1,"res"] -  (m_param_a_temp[i-1,"mort_r"]*m_param_a_temp[i-1,"res"]) -
      (m_param_a_temp[i-1,"rec_r"]*m_param_a_temp[i-1,"res"])
    m_param_a_temp[i,"sus"] <- m_param_a_temp[i-1,"sus"] - 
      (m_param_a_temp[i-1,"mort_s"]*m_param_a_temp[i-1,"sus"]) - (m_param_a_temp[i-1,"rec_s"]*m_param_a_temp[i-1,"sus"])
    m_param_a_temp[i,"fallen"] <- (m_param_a_temp[i-1,"mort_r"]*m_param_a_temp[i-1,"res"]) + 
      (m_param_a_temp[i-1,"mort_s"]*m_param_a_temp[i-1,"sus"]) ## didn't include previous number in fallen as want to sum
    # so avoids double counting
    
    i <-4 ## moving to sold/not-sold states
    m_param_a_temp[i,"sold"] <- (m_param_a_temp[i-1,"w_sold"]*m_param_a_temp[i-1,"well"])
    
    ### aggregate
    m_a_sum <- colSums(m_param_a_temp) ## sum over the 4 rows
    m_a_sum[1] <- state_i_a[1] ## reset Well sum (as this is the only one currently double counting..hopefully)
    
    m_a_sum <- animal[parameter=="annual_cycles",value] * m_a_sum #multiply by the number of annual cycles
    
    ### repeat to get 10 cycles....
    m_param_a <- matrix(rep(m_a_sum), nrow=n.t, ncol =length(parameter_names_a))
    m_param_a <- t(replicate(n.t,m_a_sum))
    colnames(m_param_a) <- parameter_names_a
    rownames(m_param_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
    
    return(m_param_a)
  }
  
  m_param_a <- f_animal_epi(m_param_a_base,n.t)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  #### farm cost ###########
  m_cost_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_a) <- parameter_names_a
  rownames(m_cost_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_w <- animal[parameter=="c_animal",value] ## defining cost of having the animal
  c_s <- animal[parameter=="s_cost",value] ## defining cost of treating infections
  c_r <- animal[parameter=="r_cost",value]
  
  cost_i_a <- c(c_w,c_w + c_r,c_w + c_s,0,0) #changed so farm still pays upkeep c_w on infected animals
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost_a[2, 1:length(state_names_a)] <- cost_i_a 
  
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_cost_a[i,j] <- f_di(m_cost_a[i-1,j],dr)
    }  
  }
  
  #### farm rewards ##################
  m_rwd_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_rwd_a) <- parameter_names_a
  rownames(m_rwd_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  
  r_sold <- animal[parameter=="i_animal",value] ## defining the income from a sold animal
  rwd_i_a <- c(0,0,0,0,r_sold)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_a[2, 1:length(state_names_a)] <- rwd_i_a
  
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_rwd_a[i,j] <- f_di(m_rwd_a[i-1,j],dr)
    }  
  }
  
  
  ##########*****INTERVENTION****** ############################
  
  ### reduction in incidence of drug resistant infections
  ### humans
  m_param2 <- m_param ## parameter matrix for scenario 2
  
  m_param2[ , "r"] <- rep(human[parameter=="well_r",value]-(human[parameter=="well_r",value]*intervention[parameter=="u_RH",value]), 
                          n.t)
  ## this is assuming a reduction in drug resistant infections
  ## if you want to then just change the proportion that are resistant
  ## but keep total infection numbers constant
  ## would also have to change the well2susceptible transition prob here also
  
  ## clear state values
  m_param2[ , 1:length(state_names)] <- 0
  m_param2[1, 1:length(state_names)] <- state_i
  
  m_param2 <- f_human_epi(m_param2, n.t)
  
  ## animals
  m_param_a2 <- m_param_a_base
  m_param_a2[ , "r"] <- rep(animal[parameter=="well_r",value]-(animal[parameter=="well_r",value]*intervention[parameter=="u_RA",value]), 
                            n.t)
  m_param_a2[ , 1:length(state_names_a)] <- 0
  m_param_a2[1, 1:length(state_names_a)] <- state_i_a
  
  m_param_a2 <- f_animal_epi(m_param_a2, n.t)
  ### !!! need to check this because giving minus values
  
  ### costs and rewards are the same for healthcare system
  ## rewards are the same for farms
  # costs change for each well add a cost of intervention
  
  m_cost_a2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
  colnames(m_cost_a) <- parameter_names_a
  rownames(m_cost_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_s <- animal[parameter=="s_cost",value]
  c_r <- c_s+(c_s*animal[parameter=="r_cost",value])
  c_interv <- intervention[parameter=="int_cost_per",value]
  
  cost_i_a2 <- c(c_w + c_interv,c_w + c_r,c_w + c_s,0,0) #changed it so you still pay upkeep on animals who are treated or infected
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost_a2[2, 1:length(state_names_a)] <- cost_i_a2
  
  for (j in 1:length(state_names_a)) {
    for (i in 3:(n.t)){
      m_cost_a2[i,j] <- f_di(m_cost_a2[i-1,j],dr)
    }  
  }
  
  
  
  ############ RESULTS #######################
  results_base_h <- f_expvalue(m_param,m_cost,m_rwd)
  results_base_a <- f_expvalue(m_param_a,m_cost_a,m_rwd_a)
  results_interv_h <- f_expvalue(m_param2,m_cost,m_rwd)
  results_interv_a <- f_expvalue(m_param_a2,m_cost_a2,m_rwd_a)
  
  total_results_HC<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_HC) <- c("Costs (£)", "QALYs")
  rownames(total_results_HC) <- c("Base Case", "Intervention")
  
  total_results_HC[1,] <- results_base_h[1,]
  total_results_HC[2,] <- results_interv_h[1,]
  
  results_base_prod <- f_expvalue(m_param,m_cost_prod,m_rwd_prod)
  results_interv_prod <- f_expvalue(m_param2,m_cost_prod,m_rwd_prod)
  
  total_results_prod <- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_prod) <- c("Productivity", "QALYs")
  rownames(total_results_prod) <- c("Base Case", "Intervention")
  
  #total_results_prod[1,] <- results_base_prod[1,]
  #total_results_prod[2,] <- results_interv_prod[1,]
  
  total_results_prod[1,2] <- results_base_h[,2]
  total_results_prod[2,2] <- results_interv_h[,2]
  total_results_prod[1,1] <- results_base_prod[,2]   #will be negative
  total_results_prod[2,1] <- results_interv_prod[,2] #will be negative but hopefully closer to zero
  
  #### HC 
  incr_cost <- (results_interv_h[1,1] - results_base_h[1,1])
  incr_benefit <-  (results_interv_h[1,2]-results_base_h[1,2])
  icer <- incr_cost/incr_benefit
  NMB_H <- (incr_benefit*wtp)-(incr_cost)  # per person in the population
  
  #############################################################################
  #because there technically aren't any costs for the productivity side,
  #we use the negative productivity gain from base to intervention (a productivity gain would incur a negative 'cost')
  #############################################################################
  
  ### Productivity
  incr_cost_prod <- total_results_prod[1,1] - total_results_prod[2,1] #hopefully negative
  incr_benefit_prod <- total_results_prod[2,2] - total_results_prod[1,2] #hopefully positive
  icer_prod <- incr_cost_prod/incr_benefit_prod #hopefully negative, if intervention improves productivity and saves QALYs
  NMB_prod <- total_results_prod[2,1] - total_results_prod[1,1] #hopefully positive
  
  ## Farm level
  incr_cost_a <- (results_interv_a[1,1] - results_base_a[1,1])
  incr_benefit_a <-  (results_interv_a[1,2]-results_base_a[1,2])
  
  total_results_Ag<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_Ag) <- c("Costs (£)", "Benefits (£)")
  rownames(total_results_Ag) <- c("Base Case", "Intervention")
  
  total_results_Ag[1,] <- results_base_a[1,]
  total_results_Ag[2,] <- results_interv_a[1,]
  
  CBR <- incr_benefit_a/incr_cost_a
  NMB_A <- incr_benefit_a-incr_cost_a # per farm in the population
  
  NMB_A_all <- NMB_A*intervention[parameter=="n_farms",value]
  
  incr_cost_macro <- incr_cost_prod - NMB_A_all + incr_cost
  
  icer_macro <- incr_cost_macro / incr_benefit
  
  net_monetary_gain_macro <- -1 * incr_cost_macro
  
  NMB_macro <- (incr_benefit * wtp) - (incr_cost_macro)
  
  outputs <- data.table(incr_cost=incr_cost, incr_benefit=incr_benefit,
                        incr_cost_a=incr_cost_a, incr_benefit_a=incr_benefit_a,
                        icer=icer, CBR = CBR, NMB_H=NMB_H, NMB_A=NMB_A, icer_prod = icer_prod, 
                        NMB_prod = NMB_prod, net_monetary_gain_macro = net_monetary_gain_macro,
                        NMB_macro = NMB_macro, icer_macro = icer_macro)
  outputs
  
  return(outputs)
  #   
}
# 
model(inputs)

##check the cost-effectiveness as a function of intervention cost per chicken
##and plot the cost-effective space
inputs2 <- as.data.table(read.csv("C:/Users/tresc/Desktop/AMR-Model/input_V.csv"))
interv_cost_vector <- c(rep(0,10000))
wtp_vector <- c(rep(0,10000))

for (i in 1:10000) {
  inputs2[24,4] <- runif(1,0,2)
  x <- as.numeric(inputs2[24,4])
  wtp <- runif(1,0,5000)
  if(model(inputs2)[1,12] >= 0){
    interv_cost_vector[i] <- x
    wtp_vector[i] <- wtp
  }
} 

ceplot <- as.data.frame(cbind(interv_cost_vector, wtp_vector))

cost_effective_points <- ggplot(ceplot, aes(x=wtp_vector, y=interv_cost_vector))+
  geom_point()+
  xlab("Willingness to Pay for a QALY")+
  ylab("Intervention Cost per Chickin")+
  ggtitle("Cost-Effective Plane")

plot(cost_effective_points)

##create a CEAC using the distributions in our parameter sheet

inputs3 <- as.data.table(read.csv("C:/Users/tresc/Desktop/AMR-Model/input_V.csv"))
ceac_icer_vector <- c(rep(0,1000))

for(i in 1:1000){
  
  #load dataset
  inputs3 <- as.data.table(read.csv("C:/Users/tresc/Desktop/AMR-Model/input_V.csv"))
  
  #random draws of normally distributed variables
  inputs3[13,4] <- rnorm(1,as.numeric(inputs3[13,4]),as.numeric((inputs3[13,4] - inputs3[13,6])/1.96))
  inputs3[18,4] <- rnorm(1,as.numeric(inputs3[18,4]),as.numeric((inputs3[18,4] - inputs3[18,6])/1.96))
  inputs3[33,4] <- rnorm(1,as.numeric(inputs3[33,4]),as.numeric((inputs3[33,4] - inputs3[33,6])/1.96))
  
  #random draws of uniformly distributed variables
  inputs3[22,4] <- runif(1,as.numeric(inputs3[22,6]),as.numeric(inputs3[22,7]))
  inputs3[24,4] <- runif(1,as.numeric(inputs3[24,6]),as.numeric(inputs3[24,7]))
  
  #random draws of the beta-distributed variables
  inputs3[5,4] <- rbeta(1,as.numeric(inputs3[5,6]),as.numeric(inputs3[5,7]))
  inputs3[6,4] <- rbeta(1,as.numeric(inputs3[6,6]),as.numeric(inputs3[6,7]))
  inputs3[15,4] <- rbeta(1,as.numeric(inputs3[15,6]),as.numeric(inputs3[15,7]))
  inputs3[16,4] <- rbeta(1,as.numeric(inputs3[16,6]),as.numeric(inputs3[16,7]))
  inputs3[19,4] <- rbeta(1,as.numeric(inputs3[19,6]),as.numeric(inputs3[19,7]))
  inputs3[20,4] <- rbeta(1,as.numeric(inputs3[20,6]),as.numeric(inputs3[20,7]))
  inputs3[25,4] <- rbeta(1,as.numeric(inputs3[25,6]),as.numeric(inputs3[25,7]))
  inputs3[26,4] <- rbeta(1,as.numeric(inputs3[26,6]),as.numeric(inputs3[26,7]))
  
  #create a vector of ICER values
  ceac_icer_vector[i] <- as.data.frame(model(inputs3))[1,13]
}

#create and plot the CEAC
density <- ecdf(ceac_icer_vector)
CEAC <- plot(density,
             xlim = c(0,10000),
             xlab = 'Willingness to Pay per QALY',
             ylab = "Portion of Interventions Cost-Effective",
             main = 'Cost Effectiveness Acceptability Curve')
abline(v = c(200,3000), col = c("blue", "red"), lty = c(2,2), lwd = c(2,2)) 

#tornado plot
#get base case ICER
tornado_base <- as.data.frame(model(inputs))[1,13]

#prod growth
inputstornado <- inputs 
inputstornado[33,4] <- inputs[33,6]
prod_growth_low <- as.data.frame(model(inputstornado))[1,13]
prod_growth_low <- prod_growth_low - tornado_base
inputstornado[33,4] <- inputs[33,7]
prod_growth_high <- as.data.frame(model(inputstornado))[1,13]
prod_growth_high <- prod_growth_high - tornado_base

#reduction in animal resistance
inputstornado <- inputs
inputstornado[26,4] <- inputs[26,6]
u_RA_low <- as.data.frame(model(inputstornado))[1,13]
u_RA_low <- u_RA_low - tornado_base
inputstornado[26,4] <- inputs[26,7]
u_RA_high <- as.data.frame(model(inputstornado))[1,13]
u_RA_high <- u_RA_high - tornado_base

#reduction in human resistance
inputstornado <- inputs
inputstornado[25,4] <- inputs[25,6]
u_RH_low <- as.data.frame(model(inputstornado))[1,13]
u_RH_low <- u_RH_low - tornado_base
inputstornado[25,4] <- inputs[25,7]
u_RH_high <- as.data.frame(model(inputstornado))[1,13]
u_RH_high <- u_RH_high - tornado_base

#intervention cost per chicken
inputstornado <- inputs
inputstornado[24,4] <- inputs[24,6]
int_cost_low <- as.data.frame(model(inputstornado))[1,13]
int_cost_low <- int_cost_low - tornado_base
inputstornado[24,4] <- inputs[24,7]
int_cost_high <- as.data.frame(model(inputstornado))[1,13]
int_cost_high <- int_cost_high - tornado_base

#cost of treating a resistant infection in animals
inputstornado <- inputs
inputstornado[22,4] <- inputs[22,6]
res_treat_a_low <- as.data.frame(model(inputstornado))[1,13]
res_treat_a_low <- res_treat_a_low - tornado_base
inputstornado[22,4] <- inputs[22,7]
res_treat_a_high <- as.data.frame(model(inputstornado))[1,13]
res_treat_a_high <- res_treat_a_high - tornado_base

#cost of treating a susceptible infection in animals
inputstornado <- inputs
inputstornado[21,4] <- inputs[21,6]
sus_treat_a_low <- as.data.frame(model(inputstornado))[1,13]
sus_treat_a_low <- sus_treat_a_low - tornado_base
inputstornado[21,4] <- inputs[21,7]
sus_treat_a_high <- as.data.frame(model(inputstornado))[1,13]
sus_treat_a_high <- sus_treat_a_high - tornado_base

#animal mortality from resistant infection
inputstornado <- inputs
inputstornado[20,4] <- inputs[20,6]
res_mort_a_low <- as.data.frame(model(inputstornado))[1,13]
res_mort_a_low <- res_mort_a_low - tornado_base
inputstornado[20,4] <- inputs[20,7]
res_mort_a_high <- as.data.frame(model(inputstornado))[1,13] 
res_mort_a_high <- res_mort_a_high - tornado_base

#animal mortality from susceptible infection
inputstornado <- inputs
inputstornado[19,4] <- inputs[19,6]
sus_mort_a_low <- as.data.frame(model(inputstornado))[1,13]
sus_mort_a_low <- sus_mort_a_low - tornado_base
inputstornado[19,4] <- inputs[19,7]
sus_mort_a_high <- as.data.frame(model(inputstornado))[1,13] 
sus_mort_a_high <- sus_mort_a_high - tornado_base

#income per chicken sold
inputstornado <- inputs
inputstornado[18,4] <- inputs[18,6]
income_chicken_low <- as.data.frame(model(inputstornado))[1,13]
income_chicken_low <- income_chicken_low - tornado_base
inputstornado[18,4] <- inputs[18,7]
income_chicken_high <- as.data.frame(model(inputstornado))[1,13] 
income_chicken_high <- income_chicken_high - tornado_base

#cost (financial, not emotional) of raising a chicken
inputstornado <- inputs
inputstornado[17,4] <- inputs[17,6]
upkeep_chicken_low <- as.data.frame(model(inputstornado))[1,13]
upkeep_chicken_low <- upkeep_chicken_low - tornado_base
inputstornado[17,4] <- inputs[17,7]
upkeep_chicken_high <- as.data.frame(model(inputstornado))[1,13] 
upkeep_chicken_high <- upkeep_chicken_high - tornado_base

#probability of an animal getting a susceptible infection
inputstornado <- inputs
inputstornado[16,4] <- inputs[16,6]
sus_chicken_low <- as.data.frame(model(inputstornado))[1,13]
sus_chicken_low <- sus_chicken_low - tornado_base
inputstornado[16,4] <- inputs[16,7]
sus_chicken_high <- as.data.frame(model(inputstornado))[1,13] 
sus_chicken_high <- sus_chicken_high - tornado_base

#probability of an animal getting a resistant infection
inputstornado <- inputs
inputstornado[15,4] <- inputs[15,6]
res_chicken_low <- as.data.frame(model(inputstornado))[1,13]
res_chicken_low <- res_chicken_low - tornado_base
inputstornado[15,4] <- inputs[15,7]
res_chicken_high <- as.data.frame(model(inputstornado))[1,13] 
res_chicken_high <- res_chicken_high - tornado_base

#chicken background mortality
inputstornado <- inputs
inputstornado[13,4] <- inputs[13,6]
chicken_mort_low <- as.data.frame(model(inputstornado))[1,13]
chicken_mort_low <- chicken_mort_low - tornado_base
inputstornado[13,4] <- inputs[13,7]
chicken_mort_high <- as.data.frame(model(inputstornado))[1,13] 
chicken_mort_high <- chicken_mort_high - tornado_base

#hospital cost of treating a resistant infection in humans
inputstornado <- inputs
inputstornado[8,4] <- 0.5 * inputs[8,4]
res_treat_low <- as.data.frame(model(inputstornado))[1,13]
res_treat_low <- res_treat_low - tornado_base
inputstornado[8,4] <- 1.5 * inputs[8,4]
res_treat_high <- as.data.frame(model(inputstornado))[1,13] 
res_treat_high <- res_treat_high - tornado_base

#hospital cost of treating a susceptible infection in humans
inputstornado <- inputs
inputstornado[7,4] <- 0.5 * inputs[7,4]
sus_treat_low <- as.data.frame(model(inputstornado))[1,13]
sus_treat_low <- sus_treat_low - tornado_base
inputstornado[7,4] <- 1.5 * inputs[7,4]
sus_treat_high <- as.data.frame(model(inputstornado))[1,13] 
sus_treat_high <- sus_treat_high - tornado_base

#mortality of resistant cases
inputstornado <- inputs
inputstornado[6,4] <- 0.5 * inputs[6,4]
res_mort_low <- as.data.frame(model(inputstornado))[1,13]
res_mort_low <- res_mort_low - tornado_base
inputstornado[6,4] <- 1.5 * inputs[6,4]
res_mort_high <- as.data.frame(model(inputstornado))[1,13]
res_mort_high <- res_mort_high - tornado_base

#mortality of susceptible cases
inputstornado <- inputs
inputstornado[5,4] <- 0.5 * inputs[5,4]
sus_mort_low <- as.data.frame(model(inputstornado))[1,13]
sus_mort_low <- sus_mort_low - tornado_base
inputstornado[5,4] <- 1.5 * inputs[5,4]
sus_mort_high <- as.data.frame(model(inputstornado))[1,13]
sus_mort_high <- sus_mort_high - tornado_base

tornado <- data.frame(variable = c("productivity growth",
                                   "reduction in animal AMR",
                                   "reduction in human AMR",
                                   "intervention cost per chicken",
                                   "cost of treating a resistant infection in chickens",
                                   "cost of treating a susceptible infection in chickens",
                                   "chicken mortality from resistant infection",
                                   "chicken mortality from susceptible infection",
                                   "income per chicken sold",
                                   "cost (financial, not emotional) of raising a chicken",
                                   "probability of a chicken getting a susceptible infection",
                                   "probability of a chicken getting a resistant infection",
                                   "chicken background mortality",
                                   "hospital cost of treating a resistant infection in humans",
                                   "hospital cost of treating a susceptible infection in humans",
                                   "mortality of resistant cases",
                                   "mortality of susceptible cases"),
                      min = c(prod_growth_low, u_RA_low, u_RH_low, int_cost_low, res_treat_a_low, sus_treat_a_low, 
                              res_mort_a_low, sus_mort_a_low, income_chicken_low, upkeep_chicken_low, sus_chicken_low, res_chicken_low, 
                              chicken_mort_low, res_treat_low, sus_treat_low, res_mort_low, sus_mort_low),
                      max = c(prod_growth_high,u_RA_high, u_RH_high, int_cost_high, res_treat_a_high, sus_treat_a_high,
                              res_mort_a_high, sus_mort_a_high, income_chicken_high, upkeep_chicken_high, sus_chicken_high, res_chicken_high,
                              chicken_mort_high, res_treat_high, sus_treat_high, res_mort_high, sus_mort_high))


ggplot(tornado, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  ggtitle("Change in Macro-Level ICER along Range of Each Parameter")+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))

################################################################################
############################exploring discount rates############################
################################################################################
model(inputs)
dr <- 0.094
model(inputs) #becomes significantly less cost-effective with a higher discount rate
dr <- 0.035

dr_vector <- as.vector(seq(from = 0, to = 0.12, by = 0.0001))
dr_icer <- as.vector(rep(0,1201))

for(i in 1:1201){
  dr <- dr_vector[i]
  dr_icer[i] <- as.data.frame(model(inputs))[1,13]
}

dr_df <- as.data.frame(cbind(dr_vector, dr_icer))
colnames(dr_df) <- c("Discount Rate", "Macro-Level ICER")

plot(dr_df$`Discount Rate`, dr_df$`Macro-Level ICER`)

ggplot(dr_df, aes(x=dr_vector, y=dr_icer)) +
  geom_point()+
  geom_vline(xintercept = 0.03, linetype = "dotted")+
  geom_vline(xintercept = 0.066, lty = "dashed")+
  geom_vline(xintercept = 0.094)+
  ggtitle("Macro-Level ICER at Different Levels of the Discount Rate")+
  labs(title = "Macro-Level ICER at Different Levels of the Discount Rate", x = "Intertemporal Discount Rate", y = "Macro-Level ICER")

################################################################################
############################exploring farm costs################################
################################################################################

cost_vector <- as.vector(seq(from = -0.05, to = 0.15, by = 0.0001))
cost_icer <- as.vector(rep(0,2001))

inputs_cost <- inputs 

for(i in 1:1201){
  inputs_cost[24,4] <- cost_vector[i]
  cost_icer[i] <- as.data.frame(model(inputs_cost))[1,13]
  inputs_cost <- inputs 
}

cost_df <- as.data.frame(cbind(cost_vector, cost_icer))
colnames(cost_df) <- c("Cost per Chicken", "Macro-Level ICER")

plot(cost_df$`Cost per Chicken`, cost_df$`Macro-Level ICER`)

ggplot(cost_df, aes(x=cost_vector, y=cost_icer)) +
  geom_point()+
  geom_vline(xintercept = 0, linetype = "dashed", lwd = 1, col = "red")+
  #geom_vline(xintercept = 0.02173, linetype = "dashed", lwd = 1, col = "blue")+
  geom_vline(xintercept = 0.022205, linetype = "dashed", lwd = 1, col = "blue")+
  labs(title = "Macro-Level ICER at Different Levels of Intervention Cost", 
       x = "Intervention Cost per Chicken", y = "Macro-Level ICER", 
       subtitle = "Red: ICER at Zero Net Cost, Blue: ICER at Cost-Effective Threshold (0.022205 USD)")

################################################################################
################################################################################

#Partial Rank Correlation Coefficient
#parameters to vary: same as in CEAC, but now add discount rate
prcc_df <- data.table()

prcc_df$a_mort <- rep(0,10000)
prcc_df$a_inc <- rep(0,10000)
prcc_df$prod_growth <- rep(0,10000)
prcc_df$a_res_cost <- rep(0,10000)
prcc_df$a_int_cost <- rep(0,10000)
prcc_df$h_res_mort <- rep(0,10000)
prcc_df$h_sus_mort <- rep(0,10000)
prcc_df$a_res_prob <- rep(0,10000)
prcc_df$a_sus_prob <- rep(0,10000)
prcc_df$a_res_mort <- rep(0,10000)
prcc_df$a_sus_mort <- rep(0,10000)
prcc_df$h_amr_fall <- rep(0,10000)
prcc_df$a_amr_fall <- rep(0,10000)
prcc_df$dr <- rep(0,10000)
prcc_df$ICER <- rep(0,10000)

inputs_prcc <- inputs

set.seed(42069)

for(i in 1:10000){
  inputs_prcc[13,4] <- rnorm(1,as.numeric(inputs_prcc[13,4]),as.numeric((inputs_prcc[13,4] - inputs_prcc[13,6])/1.96)) #animal on-farm mortality
  inputs_prcc[18,4] <- rnorm(1,as.numeric(inputs_prcc[18,4]),as.numeric((inputs_prcc[18,4] - inputs_prcc[18,6])/1.96)) #income per animal
  inputs_prcc[33,4] <- rnorm(1,as.numeric(inputs_prcc[33,4]),as.numeric((inputs_prcc[33,4] - inputs_prcc[33,6])/1.96)) #productivity growth
  
  #random draws of uniformly distributed variables
  inputs_prcc[22,4] <- runif(1,as.numeric(inputs_prcc[22,6]),as.numeric(inputs_prcc[22,7])) #cost of treating a resistant infection in animals
  inputs_prcc[24,4] <- runif(1,as.numeric(inputs_prcc[24,6]),as.numeric(inputs_prcc[24,7])) #intervention cost per animal
  
  #random draws of the beta-distributed variables
  inputs_prcc[5,4] <- rbeta(1,as.numeric(inputs_prcc[5,6]),as.numeric(inputs_prcc[5,7])) #human mortality from resistant infection
  inputs_prcc[6,4] <- rbeta(1,as.numeric(inputs_prcc[6,6]),as.numeric(inputs_prcc[6,7])) #human mortality from susceptible infection
  inputs_prcc[15,4] <- rbeta(1,as.numeric(inputs_prcc[15,6]),as.numeric(inputs_prcc[15,7])) #animal probability of resistant infection
  inputs_prcc[16,4] <- rbeta(1,as.numeric(inputs_prcc[16,6]),as.numeric(inputs_prcc[16,7])) #animal probability of susceptible infection
  inputs_prcc[19,4] <- rbeta(1,as.numeric(inputs_prcc[19,6]),as.numeric(inputs_prcc[19,7])) #animal mortality from susceptible infection
  inputs_prcc[20,4] <- rbeta(1,as.numeric(inputs_prcc[20,6]),as.numeric(inputs_prcc[20,7])) #animal mortality from resistant infection
  inputs_prcc[25,4] <- rbeta(1,as.numeric(inputs_prcc[25,6]),as.numeric(inputs_prcc[25,7])) #intervention reduction in human AMR
  inputs_prcc[26,4] <- rbeta(1,as.numeric(inputs_prcc[26,6]),as.numeric(inputs_prcc[26,7])) #intervention reduction in animal AMR
  
  #randonly draw the discount rate
  dr <- rtriang(1, a = 0, b = 10, c = 8)

  prcc_df$a_mort[i] <- inputs_prcc[13,4]
  prcc_df$a_inc[i] <- inputs_prcc[18,4]
  prcc_df$prod_growth[i] <- inputs_prcc[33,4]
  prcc_df$a_res_cost[i] <- inputs_prcc[22,4]
  prcc_df$a_int_cost[i] <- inputs_prcc[24,4]
  prcc_df$h_res_mort[i] <- inputs_prcc[5,4]
  prcc_df$h_sus_mort[i] <- inputs_prcc[6,4]
  prcc_df$a_res_prob[i] <- inputs_prcc[15,4]
  prcc_df$a_sus_prob[i] <- inputs_prcc[16,4]
  prcc_df$a_res_mort[i] <- inputs_prcc[19,4]
  prcc_df$a_sus_mort[i] <- inputs_prcc[20,4]
  prcc_df$h_amr_fall[i] <- inputs_prcc[25,4]
  prcc_df$a_amr_fall[i] <- inputs_prcc[26,4]
  prcc_df$dr[i] <- dr 
  prcc_df$ICER[i] <- model(inputs_prcc)[1,13]
  
  inputs_prcc <- inputs
  
  if(i %% 100 == 0){
    print(i)
  }
  
}

safe <- prcc_df

a_mort <- as.numeric(unlist(safe$a_mort))
a_inc <- as.numeric(unlist(safe$a_inc))
prod_growth <- as.numeric(unlist(safe$prod_growth))
a_res_cost <- as.numeric(unlist(safe$a_res_cost))
a_int_cost <- as.numeric(unlist(safe$a_int_cost))
h_res_mort <- as.numeric(unlist(safe$h_res_mort))
h_sus_mort <- as.numeric(unlist(safe$h_sus_mort))
a_res_prob <- as.numeric(unlist(safe$a_res_prob))
a_sus_prob <- as.numeric(unlist(safe$a_sus_prob))
a_res_mort <- as.numeric(unlist(safe$a_res_mort))
a_sus_mort <- as.numeric(unlist(safe$a_sus_mort))
h_amr_fall <- as.numeric(unlist(safe$h_amr_fall))
a_amr_fall <- as.numeric(unlist(safe$a_amr_fall))
drr <- as.numeric(unlist(safe$dr))
ICER <- as.numeric(unlist(safe$ICER))

prcc_dataset <- as.data.frame(cbind(a_mort,a_inc,prod_growth,a_res_cost,a_int_cost,h_res_mort,h_sus_mort,a_res_prob,a_sus_prob,a_res_mort,a_sus_mort,h_amr_fall,a_amr_fall,drr,ICER))

write.csv(prcc_dataset,"C:/Users/tresc/Desktop/AMR-Model/outputs/prcc_data.csv", row.names = F)

#evaluate the monotonicity of the relationships by looking at scatterplots

plot(prcc_dataset$a_mort , prcc_dataset$ICER) #safe

plot(prcc_dataset$a_inc, prcc_dataset$ICER) #safe

plot(prcc_dataset$prod_growth , prcc_dataset$ICER) #safe

plot(prcc_dataset$a_res_cost , prcc_dataset$ICER) #safe

plot(prcc_dataset$a_int_cost , prcc_dataset$ICER) #safe

plot(prcc_dataset$h_res_mort , prcc_dataset$ICER) #possible non-monotonicity

plot(prcc_dataset$h_sus_mort , prcc_dataset$ICER) #possible non-monotonicity

plot(prcc_dataset$a_res_prob , prcc_dataset$ICER) #possible non-monotonicity

plot(prcc_dataset$a_sus_prob , prcc_dataset$ICER) #safe

plot(prcc_dataset$a_res_mort , prcc_dataset$ICER) #safe

plot(prcc_dataset$a_sus_mort , prcc_dataset$ICER) #safe

plot(prcc_dataset$h_amr_fall , prcc_dataset$ICER) #unclear

plot(prcc_dataset$a_amr_fall , prcc_dataset$ICER) #unclear

plot(prcc_dataset$drr , prcc_dataset$ICER) #unclear

id <- seq(from = 1, to = 10000, by = 1)

monotonic_test_dataset <- as.data.frame(cbind(id, prcc_dataset))

#gauge to PRCC

epi.prcc(prcc_dataset, sided.test = 2, conf.level = 0.95)
#significant values: 5 (animal intervention cost)(+), 7 (human mortality from sus)(-), 8 (animal probability of res)(-), 
#11 (animal mortality from sus)(-), 12 (fall in human AMR)(-), 13 (fall in animal AMR)(-), 14 (discount rate)(-)
#some of these don't make sense though --> we saw that a higher discount rate increased the ICER
#this is probably because PRCC requires a monotonic relationship

epi.prcc(prcc_dataset, sided.test = 1, conf.level = 0.95)
#in the one-sided test, the significant values are:
#2(-), 5(+), 7(-), 8(-), 11(-), 12(-), 13(-), 14(-)
#only difference is that income from animal sale is now significant, and negatively related to ICER (makes sense)


   