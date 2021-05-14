#####################################################################""
### Risk assessment model - WHO
### SUBMODEL 5 - acqusition from humans through community transmission
### MODEL H2 (for human route number 2)
### author: L Opatowski
### date: 2017/11/28

### In this script we use a monte carlo approach to estimate the risk associated with a set of conditions
### and parameter values



C_distrib_risk <- function(prev=C_Mprev){
  
  # C_prev <- mcstoc(rnorm,type="VU", mean=C_Mprev, sd=0.05)
  C_prev <- mcstoc(rnorm,type="VU", mean=prev, sd=0.05)
  C_atb <- mcstoc(runif,type="VU",min=C_Matb[1], max=C_Matb[2]) 
  C_trans <- mcstoc(rnorm,type="VU",mean=C_Mtrans, sd=0.001)
  
  # impact of hygiene practice and sanitation access
  C_effpsan <- mcstoc(rempiricalD,type="VU",values=c(0,1), prob=c(C_Mpsan,1-C_Mpsan))
  C_effhyg <- mcstoc(rempiricalD,type="VU",values=c(0,1), prob=c(C_Mhyg,1-C_Mhyg))
  
  C_dose <- mcstoc(rnorm,type="VU",mean=C_Mdose, sd=1)
  
  #####################################
  ## PART C: Risk calculation
  #####################################
  
  # calculate point risk / exposure
  
  C_sel <- C_prev^(1/(1+C_atb))
  
  # proba colonization
  PcolC <- C_trans^(1/(1+C_effhyg * C_effpsan)) * C_sel # former R_expo
  
  C_risk <- C_dose*100*PcolC
  
  ## Two dimensional Monte Carlo simulation
  # ECC <- mc( C_prev, C_atb, C_trans, C_effpsan, C_effhyg, C_sel, PcolC, C_dose, C_risk)
  # ECC <- mc( C_prev, C_atb, C_trans, C_effpsan, C_effhyg, C_dose, C_risk)
  ECC <- mc( C_atb, C_trans, C_effpsan, C_effhyg, C_dose, C_risk)
  
  return(list(C_risk, ECC))
  
}



