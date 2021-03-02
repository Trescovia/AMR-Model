####### LIBRARIES & DATA INPUts #######
library(data.table)

###### defined parameters across the sectors ######
n.t <- 11 ## time horizon - 10 years + cycle 0 (initial states)
dr <- 0.035 ## discount rate
wtp <- 20000 ## willingness to pay per QALY gained

############# model functions
inputs <- read.csv("data/input_V.csv")
inputs <- as.data.table(inputs)

# model <- function(inputs){

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

state_names <- c("well", "res","sus","dead") ## the compartments
transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s")  ## the transition probabilities
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
m_param[ , "birth"] <- rep(human[parameter=="birth_well",value], n.t)
m_param[ , "mort_w"] <- rep(human[parameter=="well_dead",value], n.t)

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

cost_i <- c(0,c_r,c_s,0)

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


r_r <- human[parameter=="hrqol_ill",value] ## e.g disability weight of being in hospital with BSI
r_s <- human[parameter=="hrqol_ill",value] ## this is probably the same value but * by longer length of hospital stay
r_d <- human[parameter=="hrqol_death",value] 

rwd_i <- c(1,r_r,r_s,r_d)

## start at cycle 1 so you do not multiply initial state vector 
m_rwd[2, 1:length(state_names)] <- rwd_i

### accounting for discounting
for (j in 1:length(state_names)) {
  for (i in 3:(n.t)){
    m_rwd[i,j] <- f_di(m_rwd[i-1,j],dr)
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

cost_i_a <- c(c_w,c_r,c_s,0,0)

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

cost_i_a2 <- c(c_interv,c_r,c_s,0,0)

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

#### HC 
incr_cost <- (results_interv_h[1,1] - results_base_h[1,1])
incr_benefit <-  (results_interv_h[1,2]-results_base_h[1,2])
icer <- incr_cost/incr_benefit
NMB_H <- (incr_benefit*wtp)-(incr_cost)  # per person in the population

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

outputs <- data.table(incr_cost=incr_cost, incr_benefit=incr_benefit,
                      incr_cost_a=incr_cost_a, incr_benefit_a=incr_benefit_a,
                      icer=icer, CBR = CBR, NMB_H=NMB_H, NMB_A=NMB_A)
outputs

###### HUMAN HEALTHCARE OUTCOMES
#### !!! it's currently counting deaths 
# not life years lost so need to incorporate
# 46 years for each death (but discount accordingly)
# I have some code from my PhD project I can dig out
# and we can try to adjust to fit this purpose

##### AGRICULTURAL OUTCOMES
### !!! it's currently looking at NMB for an "average" farm
# not all farms in the sector
# can get that by multiplying the agri outcomes (NMB_A)
# by the total number of those types of farms

#   return(outputs)
#   
# }
# 
# model(inputs)

outputs