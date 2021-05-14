#####################################################################""
### Risk assessment model - WHO
### SUBMODEL 1 - acqusition from water consumption
### MODEL W (for water)
### author : L Opatowski
### date : 2017/11/28

### In this script we use a monte carlo approach to estimate the risk associated with a set of conditions
### and parameter values

#

F_distrib_risk <- function(){

  F_prev <- mcstoc(runif,type="VU", min=F_Mprev[1], max=F_Mprev[2])
  F_atb <- mcstoc(runif,type="VU",min=F_Matb[1], max=F_Matb[2])

  F_arb <- F_prev^(1/(1+F_atb))
  
  F_slaugh <- mcstoc(rnorm,type="VU",mean=F_Mslaught, sd=0.01)
  F_ret <- mcstoc(runif,type="VU",min=F_Mret[1], max=F_Mret[2])
  
  F_treat <- 1/((1+F_slaugh)*(1+F_ret))
  
  F_cook <- mcstoc(rempiricalD,type="VU",values=c(1,0.01), prob=c(F_Mraw,1-F_Mraw)) 
  F_dose <- mcstoc(rnorm,type="VU", mean=F_Mdose, sd=10)
  
  F_acq <-mcstoc(rnorm,type="VU",mean=F_Macq, sd=0.01)

  
  #####################################
  ## PART C: Risk calculation
  #####################################
  
  # calculate point risk
  PcolF <- (F_arb^F_treat)*F_cook*F_acq #PcolF former F_expo 
  
  F_risk <- PcolF*F_dose*100 # mean result of a Binomila (N=F_dose*100, p=F_expo)
  
  ## Two dimensional Monte Carlo simulation
  # ECF <- mc(F_prev,F_atb,F_arb,F_slaugh,F_ret,F_treat,F_cook,F_dose,F_acq,PcolF, F_risk)
  ECF <- mc(F_prev,F_atb,F_slaugh,F_ret,F_cook,F_dose,F_acq, F_risk)
  
  return(list(F_risk, ECF))
  
}
