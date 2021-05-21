
# Libraries ---------------------------------------------------------------
 

library("data.table")
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
library("xlsx")

# Global parameters and scenarios -----------------------------------------

n.t <- 47 ## time horizon - 46 years + cycle 0 (initial states)
tstop <- n.t + 3
dr <- 0.08 ## discount rate
wtp <- 2365 ## willingness to pay per QALY gained
emp_rate <- 0.98162
lfpr <- 0.767

#Scenarios
scenario <- "HCA" #must be "HCA" or "FCA"
scenario_transmission <- "med" #for now, must be {'hi', 'low', 'med', 'max'}
scenario_outcomes <- "All" #must be either "Enterobacteria" or "All"

# Population Projections --------------------------------------------------

###
# in this section, we use demographic data from Viet Nam (population, dependency ratio, etc.)
# to forecast the at-risk population and the working population throughout our time frame
# what we refer to as 'births' in this model is actually net births, and this is represented by the
# 'birth' transition parameter. We set background mortality to zero, as background mortality is already
# captured by net births. This does, however, mean that the rate of population growth  in our model
# is independent of the number of deaths from BSIs, although in reality the two may be related.
# This therefore relies on the assumption that the amount of deaths from BSIs is not systemically
# important in the sense that it can influence demographic trajectory
###

pop <- read.csv("C:/Users/tresc/Desktop/AMR-Model/Population data for ARIMA/Vietnam Population.csv")
pop <- ts(pop$Population, start = 1960, frequency = 1)#
ARIMApop <- auto.arima(pop, stepwise = F, approximation = F)
plot(forecast(ARIMApop, tstop))
forecast(ARIMApop, tstop)
predict <- forecast(ARIMApop, tstop)
predict$mean
futurepop <- predict$mean

popchange <- numeric()
for (i in 1:tstop) {
  popchange[i] <- futurepop[i+1] - futurepop[i]
} #atm births[1] shows NET births in 2020

popchange <- popchange[2:tstop] #now popchange[1] is NET popchange in 2021

popchange.r <- numeric()
for (i in 1:tstop) {
  popchange.r[i] <- (futurepop[i+1] - futurepop[i])/futurepop[i]
} #atm popchange.r[1] shows NET popchange in 2020

popchange.r <- popchange.r[2:tstop] #now popchange.r[1] shows NET popchange in 2021

#upper bound population projection
futurepop_up <- predict$upper
futurepop_up <- futurepop_up[,2]

popchange.r_up <- numeric()
for (i in 1:tstop) {
  popchange.r_up[i] <- (futurepop_up[i+1] - futurepop_up[i])/futurepop_up[i]
} #atm popchange.r_up[1] shows rate for 2020

popchange.r_up <- popchange.r_up[2:tstop] #now it shows upper rates for 2021

#lower bound population projection
futurepop_low <- predict$lower
futurepop_low <- futurepop_low[,2]

popchange.r_low <- numeric()
for (i in 1:tstop){
  popchange.r_low[i] <- (futurepop_low[i+1] - futurepop_low[i])/futurepop_low[i]
} #atm popchange.r_low[1] shows rate for 2020

birthrate_low <- birthrate_low[2:tstop] #now it shows lower rates for 2021

##here I have predicted net popchange and will use this as the 'birth' parameter. This means that the background 
##mortality will be set to 0 because the net popchange.r already accounts for background mortality

####dependency ratio

dependency <- read.csv("C:/Users/tresc/Desktop/AMR-Model/Population data for ARIMA/Viet Nam dependency ratio.csv")
dependency <- ts(dependency$Dependency.Ratio, start = 1960, frequency = 1)
arimadependency <- auto.arima(dependency, stepwise = F, approximation = F)
plot(forecast(arimadependency, tstop))
predictdependency <- forecast(arimadependency, tstop)
predictdependency$mean
futuredepend <- predictdependency$mean

dependency <- numeric()
for(i in 1:tstop) {
  dependency[i] <- futuredepend[i + 1]
} #now dependency[1] is the dependency in 2021, not 2020

rm(i)

#now get portion working age
dependency <- dependency * 0.01 ## *0.01 added to make it a portion rather than a %%

portion_working_age <- 1/(1+dependency)

plot(portion_working_age)

portion_working <- portion_working_age * emp_rate * lfpr #assuming unemployment rate equal to mean of 2016-2020 and LFPR equal to mean of 2016-2019

plot(portion_working)


###
# Here, we define our scenarios. We set the number of periods to be 46 (the expected
# remaining life years in our population). We let the discount rate be 0.08 and the 
# willingness to pay for a WALY be 2365 USD, both of which are derived from theory.
# We have possible scenarios for a) the way we estimate productivity outcomes from
# morbidity and mortality (either the human capital approach or the friction cost 
# approach), b) the link parameter between animal AMU and human AMR (either from Tang
# or from Booton), and c) the infection types looked at (either all BSIs or all BSIs
# caused by Enterobacteriaceae spp.)
###


# Main Model --------------------------------------------------------------


inputs <- read.csv("C:/Users/tresc/Desktop/AMR-Model/intervention 1/inputs.csv")
inputs <- as.data.table(inputs)

  inputs[ , value := as.numeric(as.character(value))]
  
  human <- inputs[scenario=="human"]
  chicken <- inputs[scenario=="chicken"]
  pig <- inputs[scenario=="pig"]
  intervention <- inputs[scenario=="intervention"]
  
  
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
  
  
  
  # Human Epi Model -------------------------------------------------------------
  
  #for HCA and FCA, we only care about the losses in productivity, so we set the 
  #reward for 'well' to be zero. Going into the 'dead' state incurs a productivity
  #loss equal to the discounted value of future earnings, going into the 'res' or 'sus
  #states incurs a loss equal to the earnings that would have been made during the 
  #time in hospital. After 1 period, all people in 'dead' go to 'afterlife', which
  #has a reward of zero. There is also a sequelae state - people in this state only 
  #stay there for one period before continuing peacefully into the afterlife. This
  #is only for convenience and does not imply that people with sequelae die after one 
  #year. Rather, the cost of being in the sequelae state for one turn is equal to the
  #difference in discounted future welfare between a pefectly healthy expected remainder
  #of life and an expected remainder of life with sequelae
  
  state_names <- c("well", "res","sus","dead", "afterlife", "seq") ## the compartments
  transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s", "dead_aft", "sick_seq")  ## the transition probabilities
  parameter_names <- c(state_names, transition_names)
  
  ## initial state vector (population starting in well)
  state_i <- c(human[parameter=="n_population",value], rep(0,length=length(state_names)-1))
  
  ## matrix of parameter values over cycles
  m_param <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_param) <- parameter_names
  rownames(m_param) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  if(scenario_outcomes == "All"){
    tuning <- 2
  }else if (scenario_outcomes == "Enterobacteria"){
    tuning <- 1
  }
  
  m_param[ , "sick_seq"] <- rep(human[parameter=="sick_seq", value], n.t)
  m_param[ , "r"] <- rep(tuning*human[parameter=="well_r",value], n.t)
  m_param[ , "s"] <- rep(tuning*human[parameter=="well_s",value], n.t)
  m_param[ , "mort_r"] <- rep(human[parameter=="r_dead",value], n.t)
  m_param[ , "mort_s"] <- rep(human[parameter=="s_dead",value], n.t)
  m_param[ , "rec_r"] <- rep(1-(m_param[1,"mort_r"]+m_param[1,"sick_seq"]), n.t)
  m_param[ , "rec_s"] <- rep(1-(m_param[1,"mort_s"]+m_param[1,"sick_seq"]), n.t)
  m_param[ , "birth"] <- birthrate[1:n.t] ##set to be predicted net births
  m_param[ , "mort_w"] <- rep(0, n.t) ##set to zero because background mortality is included in net births
  m_param[ , "dead_aft"] <- rep(1, n.t) #all those who die go to the afterlife
  
  m_param[1, 1:length(state_names)] <- state_i ## adding initial cycle 0 values
  
  #letting the chance of getting infected with resistant bacteria grow in each period
  for (i in 2:(n.t)){
    m_param[i, "r"] <- m_param[i-1, "r"]*human[parameter=="amr_growth", value]
  }
  
  #keeping the overall prevalence of disease constant, we make sure that the number
  #of infections with susceptible and resistant bacteria sum to the total number of
  #infections. Thus, the highest resistance outcome is one in which all infections are
  #resistant but the total number of infections remains the same
  disease_max <- human[parameter=="disease_risk", value]
  
  for(i in 1:n.t){
    if(m_param[i, "r"] > disease_max){
      m_param[i, "r"] <- disease_max
    }
    m_param[i, "s"] <- disease_max - m_param[i, "r"]
  }
  
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
      
      m_param[i, "afterlife"] <- m_param[i-1, "afterlife"] + m_param[i-1, "dead"] + m_param[i-1, "seq"] #just keeps growing
      
      m_param[i, "seq"] <- m_param[i-1, "sick_seq"]*(m_param[i-1, "res"]+m_param[i-1, "sus"]) #only spend one period in 'seq' then go straight to the shadow realm
    }
    return(m_param)
  }
  
  m_param <- f_human_epi(m_param,n.t) #applying the epi function for humans (base case)
  
  
  # Healthcare Costs --------------------------------------------------------
  
  
  m_cost <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_cost) <- parameter_names
  rownames(m_cost) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_r <- human[parameter=="r_cost",value]
  c_s <- human[parameter=="s_cost",value]
  
  cost_i <- c(0,c_r,c_s,0,0,0) #only infections incur a healthcare cost here
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost[2, 1:length(state_names)] <- cost_i
  
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_cost[i,j] <- f_di(m_cost[i-1,j],dr)
    }  
  }
  
  
  # Healthcare Rewards ------------------------------------------------------
  
  
  m_rwd <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
  colnames(m_rwd) <- parameter_names
  rownames(m_rwd) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #calculate the present value of expected remaining life years for a) a perfectly
  #healthy person and b) someone with sequelae. The negative difference is the 'reward' 
  #of being in the sequelae state 
  
  pv_fut_life <- c(rep(0,46)) #expected remaining life years
  for (i in 1:46){
    pv_fut_life[i] <- human[parameter=="background_qol",value] * (1-dr)^(i-1)
  }
  pv_life <- sum(pv_fut_life)
  
  pv_fut_life_seq <- c(rep(0,46)) #expected remaining life years
  for (i in 1:46){
    pv_fut_life_seq[i] <- human[parameter=="hrqol_seq",value] * (1-dr)^(i-1)
  }
  pv_life_seq <- sum(pv_fut_life_seq)
  
  #the 'reward' for death is the negative of the present value of remaining life 
  #years, and the 'reward' for being infected is the loss of welfare while hospitalised
  #therefore the scenario with more deaths, infections and sequelae will have a negative
  #'reward' of larger absolute value
  
  r_s <- human[parameter=="background_qol",value]*(human[parameter=="hrqol_ill",value]-1) ## QoL lost from time in hospital
  r_r <- human[parameter=="background_qol",value]*(human[parameter=="hrqol_res",value]-1) ## same but adjusted for longer LoS
  r_d <- -1 * pv_life #discounted QoL loss from death
  r_seq <- pv_life - pv_life_seq
  
  rwd_i <- c(0,r_r,r_s,r_d,0,r_seq) 
  
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd[2, 1:length(state_names)] <- rwd_i
  
  ### accounting for discounting
  for (j in 1:length(state_names)) {
    for (i in 3:(n.t)){
      m_rwd[i,j] <- f_di(m_rwd[i-1,j],dr)
    }  
  }
  
  
  
  # Productivity Costs ------------------------------------------------------
  
  #for HCA and FCA, we only care about the losses in productivity, so we set the 
  #reward for 'well' to be zero. Going into the 'dead' state incurs a productivity
  #loss equal to the discounted value of forgone future earnings, going into the
  #'res' or 'sus states incurs a loss equal to the earnings that would have
  #been made during the time in hospital. After 1 period, all people in 'dead' go
  #to 'afterlife', which has a reward of zero. 
  #Using the HCA, the forgone future earnings are those of expected remaining economically
  #active years. For FCA, it is the forgone earnings during the time that it takes
  #to find a replacement worker
  
  
  #all productivity rewards are set to zero, and we only see a difference between
  #the reward matrices of different scenarios
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
  
  #the 'reward' for being infected is equal to the negative forgone productivity
  #while hospitalised
  
  m_rwd_prod <- matrix(rep(0), nrow = n.t, ncol = length(parameter_names))
  colnames(m_rwd_prod) <- parameter_names
  rownames(m_rwd_prod) <- paste("cycle", 0:(n.t-1), sep = "")
  
  r_r_prod <- -1*(human[parameter=="prod",value]+human[parameter=="unpaid_prod",value])*(1-0.923504449) #accounting for longer stay if res
  
  r_s_prod <- -1*(human[parameter=="prod",value]+human[parameter=="unpaid_prod",value])*(1-0.94)
  
  r_w_prod <- 0
  
  r_aft_prod <- 0
  
  r_seq_prod <- 0 #importantly assumes that people with sequelae are equally productive
  
  #the reward for dead is the present discounted value of future work (either for the
  #remainder of economically active life, or for the 6 months needed to find a replacement)
  
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
  rwd_i_prod <- c(portion_working[1]*r_w_prod, portion_working[1]*r_r_prod, portion_working[1]*r_s_prod, portion_working[1]*r_d_prod, r_aft_prod, r_seq_prod)
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_rwd_prod[2,1:length(state_names)] <- rwd_i_prod 
  
  ### discount, but also account for labour productivity growth
  
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
  
  
  
  # Chicken Epi Model -----------------------------------------------------------
  
  
  #in this model, we assume that all animals are born on the farm. This will make 
  #no difference on aggregate, as the income from selling chicks is by necessity
  #equal to the expense of buying them. In future, we could incorporate this as well,
  #and make it so that a farm experiencing negative profits will disappear
  
  state_names_c <- c("well", "res","sus","fallen","sold") ## the compartments
  transition_names_c  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
  parameter_names_c <- c(state_names_c, transition_names_c)
  
  state_i_c <- c(chicken[parameter=="n_animals",value], rep(0,length=length(state_names_c)-1))
  
  m_param_c_base <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_param_c_base) <- parameter_names_c
  rownames(m_param_c_base) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #here, we have a set mortality rate for all states - 
  #it only matters if animals are in 'res' or 'sus' for the purpose of calculating 
  #the cost of therapeutic treatment
  #we are able to do this because the trial data only tells us the effect on overall mortality,
  #which inherently takes into account the portion which develop res and sus infections
  m_param_c_base[ , "r"] <- rep(chicken[parameter=="well_r",value], n.t)
  m_param_c_base[ , "s"] <- rep(chicken[parameter=="well_s",value], n.t)
  m_param_c_base[ , "mort_s"] <- rep(chicken[parameter=="all_dead",value], n.t) 
  m_param_c_base[ , "mort_r"] <- rep(chicken[parameter=="all_dead",value], n.t)
  m_param_c_base[ , "rec_r"] <- rep(1-(m_param_c_base[1,"mort_r"]), n.t)
  m_param_c_base[ , "rec_s"] <- rep(1-(m_param_c_base[1,"mort_s"]), n.t)
  m_param_c_base[ , "birth"] <- rep(chicken[parameter=="birth_well",value], n.t)
  m_param_c_base[ , "mort_w"] <- rep(chicken[parameter=="all_dead",value], n.t)
  m_param_c_base[ , "w_sold"] <- rep(1, n.t) 
  
  #make it so that the total incidence of infections stays the same, and only the
  #portion of them that are resistant changes
  for(i in 1:n.t){
    m_param_c_base[i, "s"] <- chicken[parameter=="disease_risk", value] - m_param_c_base[i, "r"]
  }
  
  m_param_c_base[1, 1:length(state_names_c)] <- state_i_c
  
  f_animal_epi <- function(m_param_c_base, n.t){
    ### this has for each cycle;
    ## row 1: all the new animals in well bought
    ## row 2: all the cycle transitions to ill 
    ## row 3: they recover/die from ill
    ## row 4: all the well go to sold
    ### then need to sum up across these for each cycle
    ## so you have number of bought, ill, and sold for each cycle
    ## currently assuming those who die of illness/prematurely are not sold !!! probably needs investigating
    
    ## create mini matrix to represent the 4 rows per cycle
    
    m_param_c_temp <- m_param_c_base[1:4,]
    rownames(m_param_c_temp) <- NULL ##removing rownames
    
    ### split over time to allow for transition probabilities to sum to 1
    ### first transitions
    i<- 2 ## row 2 definitions ## background mortality happens at beginning of cycle 
    m_param_c_temp[i,"well"] <- m_param_c_temp[i-1,"well"] -(m_param_c_temp[i-1,"r"]*m_param_c_temp[i-1,"well"]) -
      (m_param_c_temp[i-1,"s"]*m_param_c_temp[i-1,"well"]) - (m_param_c_temp[i-1,"mort_w"]*m_param_c_temp[i-1,"well"])
    m_param_c_temp[i,"res"] <- m_param_c_temp[i-1,"res"] + (m_param_c_temp[i-1,"r"]*m_param_c_temp[i-1,"well"]) 
    m_param_c_temp[i,"sus"] <- m_param_c_temp[i-1,"sus"] + (m_param_c_temp[i-1,"s"]*m_param_c_temp[i-1,"well"])
    m_param_c_temp[i,"fallen"] <- (m_param_c_temp[i-1,"mort_w"]*m_param_c_temp[i-1,"well"]) 
    
    i <- 3 ###
    m_param_c_temp[i,"well"] <-  m_param_c_temp[i-1,"well"]+(m_param_c_temp[i-1,"rec_r"]*m_param_c_temp[i-1,"res"])+ 
      (m_param_c_temp[i-1,"rec_s"]*m_param_c_temp[i-1,"sus"])
    m_param_c_temp[i,"res"] <- m_param_c_temp[i-1,"res"] -  (m_param_c_temp[i-1,"mort_r"]*m_param_c_temp[i-1,"res"]) -
      (m_param_c_temp[i-1,"rec_r"]*m_param_c_temp[i-1,"res"])
    m_param_c_temp[i,"sus"] <- m_param_c_temp[i-1,"sus"] - 
      (m_param_c_temp[i-1,"mort_s"]*m_param_c_temp[i-1,"sus"]) - (m_param_c_temp[i-1,"rec_s"]*m_param_c_temp[i-1,"sus"])
    m_param_c_temp[i,"fallen"] <- (m_param_c_temp[i-1,"mort_r"]*m_param_c_temp[i-1,"res"]) + 
      (m_param_c_temp[i-1,"mort_s"]*m_param_c_temp[i-1,"sus"]) ## didn't include previous number in fallen as want to sum
    # so avoids double counting
    
    i <-4 ## moving to sold/not-sold states
    m_param_c_temp[i,"sold"] <- (m_param_c_temp[i-1,"w_sold"]*m_param_c_temp[i-1,"well"])
    
    ### aggregate
    m_c_sum <- colSums(m_param_c_temp) ## sum over the 4 rows
    m_c_sum[1] <- state_i_c[1] ## reset Well sum (as this is the only one currently double counting..hopefully)
    
    m_c_sum <- chicken[parameter=="annual_cycles",value] * m_c_sum #multiply by the number of annual cycles
    
    #repeat to get all cycles - in this branch, the animal production side is the same in every year
    #and nothing changes
    m_param_c <- matrix(rep(m_c_sum), nrow=n.t, ncol =length(parameter_names_c))
    m_param_c <- t(replicate(n.t,m_c_sum))
    colnames(m_param_c) <- parameter_names_c
    rownames(m_param_c) <- paste("cycle", 0:(n.t-1), sep  =  "")
    
    return(m_param_c)
    
  }
  
  #apply the animal epi function:
  m_param_c <- f_animal_epi(m_param_c_base,n.t)
  ### ignore totals of transition probs etc. as they are over counted etc.
  ## just want to focus on health state totals
  
  
  # Farm Costs --------------------------------------------------------------
  
  m_cost_c <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_cost_c) <- parameter_names_c
  rownames(m_cost_c) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_w <- chicken[parameter=="c_animal",value] ## defining cost of keeping the animal
  c_s <- chicken[parameter=="s_cost",value] ## defining cost of treating infections
  c_r <- chicken[parameter=="r_cost",value]
  
  cost_i_c <- c(c_w,c_w + c_r,c_w + c_s,0,0) #the farm still pays upkeep for sick animals
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_cost_c[2, 1:length(state_names_c)] <- cost_i_c 
  
  #discount the farm costs
  for (j in 1:length(state_names_c)) {
    for (i in 3:(n.t)){
      m_cost_c[i,j] <- f_di(m_cost_c[i-1,j],dr)
    }  
  }
  
  
  # Farm Rewards ------------------------------------------------------------
  
  
  m_rwd_c <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_rwd_c) <- parameter_names_c
  rownames(m_rwd_c) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  #only get a reward for selling animals
  r_sold <- chicken[parameter=="i_animal",value] ## defining the income from a sold animal
  rwd_i_c <- c(0,0,0,0,r_sold)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c[2, 1:length(state_names_c)] <- rwd_i_c
  
  #discount
  for (j in 1:length(state_names_c)) {
    for (i in 3:(n.t)){
      m_rwd_c[i,j] <- f_di(m_rwd_c[i-1,j],dr)
    }  
  }
  
  
  # Intervention ------------------------------------------------------------
  
  
  ### reduction in incidence of drug resistant infections
  ### humans
  m_param2 <- m_param ## parameter matrix for scenario 2
  
  #reduce the chance of getting a resistant infection in humans, depending on the link parameter used
  if(scenario_transmission == "low"){
    m_param2[ , "r"] <- rep(tuning*human[parameter=="well_r",value]-(tuning*human[parameter=="well_r",value]*intervention[parameter=="u_RH_low",value]),
                            n.t)
  } else if(scenario_transmission == "med"){
    m_param2[ , "r"] <- rep(tuning*human[parameter=="well_r",value]-(tuning*human[parameter=="well_r",value]*intervention[parameter=="u_RH_med",value]),
                            n.t)
  } else if(scenario_transmission == "hi"){
    m_param2[ , "r"] <- rep(tuning*human[parameter=="well_r",value]-(tuning*human[parameter=="well_r",value]*intervention[parameter=="u_RH_hi",value]),
                            n.t)
  } else if(scenario_transmission == "max"){
    m_param2[ , "r"] <- rep(tuning*human[parameter=="well_r",value]-(tuning*human[parameter=="well_r",value]*intervention[parameter=="u_RH_max",value]),
                            n.t)
  } else{
    paste("ERROR: PLEASE CHOOSE AN APPROACH TO ESTIMATING THE EFFECT ON HUMAN AMR")
  }
  
  #make sure that the total number of infections remains constant
  for(i in 1:n.t){
    if(m_param[i, "r"] > disease_max){
      m_param[i, "r"] <- disease_max
    }
    m_param[i, "s"] <- disease_max - m_param[i, "r"]
  }
  
  ## clear state values
  m_param2[ , 1:length(state_names)] <- 0
  m_param2[1, 1:length(state_names)] <- state_i
  
  m_param2 <- f_human_epi(m_param2, n.t) #apply the human epi function to the intervention case
  
  ## chickens
  m_param_c2 <- m_param_c_base #create an animal parameter spreadsheet for the intervention case
  
  #change in chicken mortality
  c_mort_int <- chicken[parameter=="all_dead", value] + (chicken[parameter=="all_dead", value]*intervention[parameter=="chicken_mort_effect", value])
  m_param_c2[ , "mort_w"] <- rep(c_mort_int, n.t)
  #m_param_c2[ , "mort_W"] <- rep(chicken[parameter=="all_dead", value] + (chicken[parameter=="all_dead", value]*intervention[parameter=="chicken_mort_effect", value]), n.t)
  
  m_param_c2[ , "mort_s"] <- rep(chicken[parameter=="all_dead", value] + (chicken[parameter=="all_dead", value]*intervention[parameter=="chicken_mort_effect", value]), 
                                 n.t)
  
  m_param_c2[ , "mort_r"] <- rep(chicken[parameter=="all_dead", value] + (chicken[parameter=="all_dead", value]*intervention[parameter=="chicken_mort_effect", value]), 
                                 n.t)
  
  
  #make sure the total number of infections stays constant
  for(i in 1:n.t){
    m_param_c2[i, "s"] <- chicken[parameter=="disease_risk", value] - m_param_c2[i, "r"]
  }
  
  m_param_c2[ , 1:length(state_names_c)] <- 0
  m_param_c2[1, 1:length(state_names_c)] <- state_i_c
  
  #apply the animal epi function to the intervention case parameter spreadsheet
  m_param_c2 <- f_animal_epi(m_param_c2, n.t) 
  
  #rewards
  m_rwd_c2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_rwd_c2) <- parameter_names_c
  rownames(m_rwd_c2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  r_sold_2 <- (chicken[parameter=="i_animal",value])*(1+intervention[parameter=="chicken_income_effect", value]) 
  rwd_i_c2 <- c(0,0,0,0,r_sold_2)
  
  ## start at cycle 1 so you do not multiply initial state vector 
  m_rwd_c2[2, 1:length(state_names_c)] <- rwd_i_c2
  
  #discount
  for (j in 1:length(state_names_c)) {
    for (i in 3:(n.t)){
      m_rwd_c2[i,j] <- f_di(m_rwd_c2[i-1,j],dr)
    }  
  }
  
  #costs
  m_cost_c2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_c))
  colnames(m_cost_c2) <- parameter_names_c
  rownames(m_cost_c2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  c_s <- chicken[parameter=="s_cost",value]
  c_r <- chicken[parameter=="r_cost",value] #removed double-paying 
  
  cost_i_c2 <- c(c_w, c_w + c_r,c_w + c_s,0,0) #you still pay upkeep on animals who are treated or infected
  
  ## start at cycle 1 so you do not multiply initial state vector
  m_cost_c2[2, 1:length(state_names_c)] <- cost_i_c2
  
  #discount
  for (j in 1:length(state_names_c)) {
    for (i in 3:(n.t)){
      m_cost_c2[i,j] <- f_di(m_cost_c2[i-1,j],dr)
    }
  }
  
  
  
  # Results -----------------------------------------------------------------
  
  
  #get a results matrix for healthcare and chickens
  results_base_h <- f_expvalue(m_param,m_cost,m_rwd)
  results_base_c <- f_expvalue(m_param_c,m_cost_c,m_rwd_c)
  results_interv_h <- f_expvalue(m_param2,m_cost,m_rwd)
  results_interv_c <- f_expvalue(m_param_c2,m_cost_c2,m_rwd_c2)
  
  total_results_HC<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_HC) <- c("Costs (£)", "QALYs")
  rownames(total_results_HC) <- c("Base Case", "Intervention")
  
  total_results_HC[1,] <- results_base_h[1,]
  total_results_HC[2,] <- results_interv_h[1,]
  
  #get a results matrix for productivity
  results_base_prod <- f_expvalue(m_param,m_cost_prod,m_rwd_prod)
  results_interv_prod <- f_expvalue(m_param2,m_cost_prod,m_rwd_prod)
  
  total_results_prod <- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_prod) <- c("Productivity", "QALYs")
  rownames(total_results_prod) <- c("Base Case", "Intervention")
  
  total_results_prod[1,2] <- results_base_h[,2]
  total_results_prod[2,2] <- results_interv_h[,2]
  total_results_prod[1,1] <- results_base_prod[,2]   #will be negative
  total_results_prod[2,1] <- results_interv_prod[,2] #will be negative but hopefully closer to zero
  
  #### HC 
  incr_cost <- (results_interv_h[1,1] - results_base_h[1,1])
  incr_benefit <-  (results_interv_h[1,2]-results_base_h[1,2])
  icer <- incr_cost/incr_benefit
  NMB_H <- (incr_benefit*wtp)-(incr_cost)
  
  #because there technically aren't any costs for the productivity side,
  #we use the negative productivity gain from base to intervention (a productivity gain would incur a negative 'cost')
  
  ### Productivity
  incr_cost_prod <- total_results_prod[1,1] - total_results_prod[2,1] #hopefully negative
  incr_benefit_prod <- total_results_prod[2,2] - total_results_prod[1,2] #hopefully positive
  icer_prod <- incr_cost_prod/incr_benefit_prod #hopefully negative, if intervention improves productivity and saves QALYs
  NMB_prod <- total_results_prod[2,1] - total_results_prod[1,1] #hopefully positive
  
  ## Farm level (chickens)
  incr_cost_c <- (results_interv_c[1,1] - results_base_c[1,1])
  incr_benefit_c <-  (results_interv_c[1,2]-results_base_c[1,2])
  
  total_results_Ag_c<- matrix(rep(0), nrow=2, ncol=2)
  colnames(total_results_Ag_c) <- c("Costs (£)", "Benefits (£)")
  rownames(total_results_Ag_c) <- c("Base Case", "Intervention")
  
  total_results_Ag_c[1,] <- results_base_c[1,] 
  total_results_Ag_c[2,] <- results_interv_c[1,] 
  
  CBR <- incr_benefit_c/incr_cost_c #cost-benefit ratio for the poultry sector
  NMB_A <- incr_benefit_c-incr_cost_c #net monetary benefit for each chicken farm
  
  NMB_A_all <- NMB_A*chicken[parameter=="n_farms",value] #net monetary benefit for the entire poultry sector
  
  intervention_cost_year <- intervention[parameter=="farm_int_cost", value]
  dr_int_pgrowth <- dr - human[parameter=="prod_growth", value] ##discount rate net of productivity growth, as we assume that compensation for vet and farmer time increases to reflect wage growth
  int_cost_vector <- rep(0, n.t)
  for(i in 1:n.t){
    int_cost_vector[i] <- intervention_cost_year*((1-dr_int_pgrowth)^(i-1))
  }
  intervention_cost_all <- sum(int_cost_vector)
  
  implementation_cost <- intervention[parameter=="admin_cost", value] + intervention_cost_all*chicken[parameter=="n_farms", value]
  
  incr_cost_macro <- implementation_cost + incr_cost_prod - NMB_A_all + incr_cost
  
  icer_macro <- incr_cost_macro / incr_benefit #not really using this any more but we keep it in
  
  net_monetary_gain_macro <- -1 * incr_cost_macro #net monetary gain for poultry + healthcare + productivity (doesn't include valuation of QALYs)
  
  NMB_macro <- (incr_benefit * wtp) - (incr_cost_macro) #net monetary gain plus the value of QALYs saved
  
  #the final outputs (some, such as macro-level ICER, are not useful)
  outputs <- data.table(incr_cost=incr_cost, incr_benefit=incr_benefit,
                        incr_cost_c=incr_cost_c, incr_benefit_c=incr_benefit_c, 
                        NMB_A_all=NMB_A_all,
                        icer=icer, CBR = CBR, NMB_H=NMB_H, NMB_A=NMB_A, icer_prod = icer_prod, 
                        NMB_prod = NMB_prod, net_monetary_gain_macro = net_monetary_gain_macro,
                        NMB_macro = NMB_macro, icer_macro = icer_macro, implementation_cost = implementation_cost)
  outputs
  
# Scenario Analysis -------------------------------------------------------


scenario_analysis_all <- matrix(rep(0), nrow = 4, ncol = 2)
colnames(scenario_analysis) <- c("Human Capital Approach", "Friction Cost Approach")
rownames(scenario_analysis) <- c("-0.025", "-0.05", "-0.10", "-0.16")

scenario_outcomes <- "All"

scenario <- "HCA"
scenario_transmission <- "low"
scenario_analysis[1,1] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "med"
scenario_analysis[2,1] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "high"
scenario_analysis[3,1] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "max"
scenario_analysis[4,1] <- as.numeric(model(inputs)[1,13])

scenario <- "FCA"
scenario_transmission <- "low"
scenario_analysis[1,2] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "med"
scenario_analysis[2,2] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "high"
scenario_analysis[3,2] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "max"
scenario_analysis[4,2] <- as.numeric(model(inputs)[1,13])

write.xlsx(scenario_analysis, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Scenario Analysis All.xlsx")

scenario_analysis_Ebteribacteriaceae <- matrix(rep(0), nrow = 4, ncol = 2)
colnames(scenario_analysis) <- c("Human Capital Approach", "Friction Cost Approach")
rownames(scenario_analysis) <- c("-0.025", "-0.05", "-0.10", "-0.16")

scenario_outcomes <- "Enterobacteriaceae"

scenario <- "HCA"
scenario_transmission <- "low"
scenario_analysis[1,1] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "med"
scenario_analysis[2,1] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "high"
scenario_analysis[3,1] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "max"
scenario_analysis[4,1] <- as.numeric(model(inputs)[1,13])

scenario <- "FCA"
scenario_transmission <- "low"
scenario_analysis[1,2] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "med"
scenario_analysis[2,2] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "high"
scenario_analysis[3,2] <- as.numeric(model(inputs)[1,13])
scenario_transmission <- "max"
scenario_analysis[4,2] <- as.numeric(model(inputs)[1,13])

write.xlsx(scenario_analysis, "C:/Users/tresc/Desktop/AMR-Model/Intervention 1/Scenario Analysis Enterobacteriaceae.xlsx")