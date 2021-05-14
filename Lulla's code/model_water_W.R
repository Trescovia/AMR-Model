#####################################################################""
### Risk assessment model - WHO
### SUBMODEL 1 - acqusition from water consumption
### MODEL W (for water)
### author : L Opatowski
### date : 2017/11/28

### In this script we use a monte carlo approach to estimate the risk associated with a set of conditions
### and parameter values

### values can be modified by the user (Part A of the code)

#####################################################################""


#####################################
##  Distributions 
#####################################
W_distrib_risk <- function(){

  W_prev <- mcstoc(runif,type="VU", min=W_Mprev[1], max=W_Mprev[2])
  W_atb <- mcstoc(runif,type="VU",min=W_Matb[1], max=W_Matb[2])
  
  W_sel <- W_prev^(1/(1+W_atb)) # atb selection in water, amplifies atb rate
  
  W_treat <- mcstoc(runif,type="VU",min=W_Mtreat[1], max=W_Mtreat[2])
  W_access <- mcstoc(runif,type="VU", min=W_Maccess[1], max=W_Maccess[2])
  # W_access <- mcstoc(rnorm,type="VU",mean=W_Maccess, sd=0.05)
  W_acq <-mcstoc(rnorm,type="VU",mean=W_Macq, sd=0.01)
  
  
  W_conso <- W_access * exp(-W_treat) + (1-W_access)
  
  W_dose <- mcstoc(rnorm, type="VU", mean=W_Mdose, sd=1)

  # calculate point risk (probability of acqusition over a day of consumming water)
  PcolW <- W_sel * W_conso * W_acq

  W_risk <- 100*W_dose*PcolW # PcolW former W_expo in the text
  
  ## Two dimensional Monte Carlo simulation
  # ECW <- mc(W_prev,W_atb,W_sel,W_treat,W_access,W_conso, W_dose, W_acq, PcolW, W_risk)
  ECW <- mc(W_prev,W_atb,W_treat,W_access, W_dose, W_acq, W_risk)
  
  return(list(W_risk, ECW))
  
  }


