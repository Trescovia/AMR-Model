state_names_a <- c("well", "res","sus","fallen","sold") ## the compartments
transition_names_a  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
## birth = replacement in this scenario
parameter_names_a <- c(state_names_a, transition_names_a)

state_i_a <- c(intervention[parameter=="n_animals",value], rep(0,length=length(state_names_a)-1))

m_param_a_base2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
colnames(m_param_a_base2) <- parameter_names_a
rownames(m_param_a_base2) <- paste("cycle", 0:(n.t-1), sep  =  "")

if(scenario_transmission == "Tang"){
  m_param_a_base2[ , "r"] <- rep(animal[parameter=="well_r",value]-(animal[parameter=="well_r",value]*intervention[parameter=="u_RA",value]),
                                 n.t)
} else if(scenario_transmission == "Booton"){
  m_param_a_base2[ , "r"] <- rep(animal[parameter=="well_r",value]-(animal[parameter=="well_r",value]*intervention[parameter=="u_RA_Booton",value]),
                                 n.t)
} else{
  paste("ERROR: PLEASE CHOOSE AN APPROACH TO ESTIMATING THE EFFECT ON ANIMAL AMR")
}
m_param_a_base2[ , "mort_s"] <- rep(animal[parameter=="s_dead",value], n.t)
m_param_a_base2[ , "mort_r"] <- rep(animal[parameter=="r_dead",value], n.t)
m_param_a_base2[ , "rec_r"] <- rep(1-(m_param_a_base2[1,"mort_r"]), n.t)
m_param_a_base2[ , "rec_s"] <- rep(1-(m_param_a_base2[1,"mort_s"]), n.t)
m_param_a_base2[ , "birth"] <- rep(animal[parameter=="birth_well",value], n.t) ## !!! doesn't really do anything for now given restructure 
m_param_a_base2[ , "mort_w"] <- rep(animal[parameter=="well_dead",value], n.t)
m_param_a_base2[ , "w_sold"] <- rep(1, n.t) 

#new
for(i in 1:n.t){
  m_param_a_base2[i, "s"] <- animal[parameter=="disease_risk", value] - m_param_a_base2[i, "r"]
}
#new

m_param_a_base2[1, 1:length(state_names_a)] <- state_i_a

f_animal_epi2 <- function(m_param_a_base2, n.t){
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
  
  m_param_a_temp2 <- m_param_a_base2[1:4,]
  rownames(m_param_a_temp2) <- NULL ##removing rownames
  
  ### split over time to allow for transition probabilities to sum to 1
  ### first transitions
  i<- 2 ## row 2 definitions ## background mortality happens at beginning of cycle 
  m_param_a_temp2[i,"well"] <- m_param_a_temp2[i-1,"well"] -(m_param_a_temp2[i-1,"r"]*m_param_a_temp2[i-1,"well"]) -
    (m_param_a_temp2[i-1,"s"]*m_param_a_temp2[i-1,"well"]) - (m_param_a_temp2[i-1,"mort_w"]*m_param_a_temp2[i-1,"well"])
  m_param_a_temp2[i,"res"] <- m_param_a_temp2[i-1,"res"] + (m_param_a_temp2[i-1,"r"]*m_param_a_temp2[i-1,"well"]) 
  m_param_a_temp2[i,"sus"] <- m_param_a_temp2[i-1,"sus"] + (m_param_a_temp2[i-1,"s"]*m_param_a_temp2[i-1,"well"])
  m_param_a_temp2[i,"fallen"] <- (m_param_a_temp2[i-1,"mort_w"]*m_param_a_temp2[i-1,"well"]) 
  
  i <- 3 ###
  m_param_a_temp2[i,"well"] <-  m_param_a_temp2[i-1,"well"]+(m_param_a_temp2[i-1,"rec_r"]*m_param_a_temp2[i-1,"res"])+ 
    (m_param_a_temp2[i-1,"rec_s"]*m_param_a_temp2[i-1,"sus"])
  m_param_a_temp2[i,"res"] <- m_param_a_temp2[i-1,"res"] -  (m_param_a_temp2[i-1,"mort_r"]*m_param_a_temp2[i-1,"res"]) -
    (m_param_a_temp2[i-1,"rec_r"]*m_param_a_temp2[i-1,"res"])
  m_param_a_temp2[i,"sus"] <- m_param_a_temp2[i-1,"sus"] - 
    (m_param_a_temp2[i-1,"mort_s"]*m_param_a_temp2[i-1,"sus"]) - (m_param_a_temp2[i-1,"rec_s"]*m_param_a_temp2[i-1,"sus"])
  m_param_a_temp2[i,"fallen"] <- (m_param_a_temp2[i-1,"mort_r"]*m_param_a_temp2[i-1,"res"]) + 
    (m_param_a_temp2[i-1,"mort_s"]*m_param_a_temp2[i-1,"sus"]) ## didn't include previous number in fallen as want to sum
  # so avoids double counting
  
  i <-4 ## moving to sold/not-sold states
  m_param_a_temp2[i,"sold"] <- (m_param_a_temp2[i-1,"w_sold"]*m_param_a_temp2[i-1,"well"])
  
  ### aggregate
  m_a_sum2 <- colSums(m_param_a_temp2) ## sum over the 4 rows
  m_a_sum2[1] <- state_i_a[1] ## reset Well sum (as this is the only one currently double counting..hopefully)
  
  m_a_sum2 <- animal[parameter=="annual_cycles",value] * m_a_sum2 #multiply by the number of annual cycles
  
  ### repeat to get all cycles....
  m_param_a2 <- matrix(rep(m_a_sum2), nrow=n.t, ncol =length(parameter_names_a))
  m_param_a2 <- t(replicate(n.t,m_a_sum2))
  colnames(m_param_a2) <- parameter_names_a
  rownames(m_param_a2) <- paste("cycle", 0:(n.t-1), sep  =  "")
  
  return(m_param_a2)
  
}

m_param_a2 <- f_animal_epi2(m_param_a_base2,n.t)
### ignore totals of transition probs etc. as they are over counted etc.
## just want to focus on health state totals

#new
m_param_a_super2 <- matrix(rep(0), ncol = length(parameter_names_a), nrow = n.t)
m_param_a_super2[1,] <- m_param_a2[1,]

for(i in 2:n.t){
  m_param_a_base2[, "r"] <- rep(m_param_a2[1 , "r"]*animal[parameter=="amu_a_growth", value]^(i-1), n.t)
  if(m_param_a_base2[i, "r"] > animal[parameter=="disease_risk", value]){
    m_param_a_base2[, "r"] <- rep(animal[parameter=="disease_risk", value], n.t)
  }
  m_param_a_base2[, "s"] <- rep(animal[parameter=="disease_risk", value] - m_param_a_base2[i, "r"], n.t)
  
  m_param_a2 <- f_animal_epi(m_param_a_base2, n.t)
  
  m_param_a_super2[i,] <- m_param_a2[i,]
}

m_param_a2 <- m_param_a_super2