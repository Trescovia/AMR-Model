#####################################################################""
### Risk assessment model - WHO
### SUBMODEL 4 - acqusition from humans through hospital transmission
### MODEL H1 (for human route number 1)
### author: L Opatowski
### date: 2017/11/28

### In this script we use a monte carlo approach to estimate the risk associated with a set of conditions
### and parameter values



H_distrib_risk <- function(prev = H_Mprev){
  H_Nhospit <- mcstoc(rnorm,type="VU", mean=H_MNhospit, sd=0.05)
  # H_prev <- mcstoc(rnorm,type="U", mean=H_Mprev, sd=0.05)
  H_Padm <- mcstoc(rnorm,type="VU", mean=prev, sd=0.05)
  H_atb <- mcstoc(runif,type="VU",min=H_Matb[1], max=H_Matb[2]) 
  
  H_trans <- mcstoc(rnorm,type="VU",mean=H_Mtrans, sd=0.01)
  H_Fmr <-  mcstoc(runif,type="VU",min=H_MFmr[1], max=H_MFmr[2])
  # H_MtransFmr <- mcstoc(rnorm,type="U",mean=H_MtransFmr, sd=0.05)
  
  H_hyg <-  mcstoc(runif,type="VU",min=H_Mhyg[1], max=H_Mhyg[2])
  
  
  #####################################
  ## PART C: Risk calculation
  #####################################
  
  # calculate point risk / exposure
  
  PcolH <- (H_Padm^(1/(1+H_atb))) * (H_trans^(1/(1+H_Fmr))) * exp(-H_hyg) # former R_hospit

  
    # R_hospit <- H_prev * exp(H_select*H_atb) * H_trans * exp(H_Fmr) * exp(H_hyg)
  
  # R_expo <- (H_prev * exp(H_atb) * H_trans * exp(H_hyg))* ((1-H_Fmr) + 2* H_Fmr) # other way of modelling
  
  
  H_dose <- H_Nhospit
  
  H_risk <- H_Nhospit*100*PcolH
  
  ## Two dimensional Monte Carlo simulation
  # ECH <- mc(H_Nhospit, H_Padm, H_atb, H_trans, H_Fmr, H_hyg, PcolH, H_risk)
  # ECH <- mc(H_Nhospit, H_Padm, H_atb, H_trans, H_Fmr, H_hyg, H_risk)
  ECH <- mc(H_Nhospit, H_atb, H_trans, H_Fmr, H_hyg, H_risk)
  
   return(list(H_risk, ECH))
  
}
