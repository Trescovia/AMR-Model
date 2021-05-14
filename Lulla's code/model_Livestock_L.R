#####################################################################""
### Risk assessment model - WHO
### SUBMODEL 3 - acqusition from contact with Livestock
### MODEL L (for Livestock)
### author : L Opatowski
### date : 2017/11/30

### In this script we use a monte carlo approach to estimate the risk associated with a set of condtions
### and parameter values


L_distrib_risk <- function(){
  
  # prevalence
  L_prev <- mcstoc(runif,type="VU", min=L_Mprev[1], max=L_Mprev[2])
  # antibiotic frequency
  #atb <- mcstoc(rnorm,type="VU",mean=Matb, sd=0.05)
  L_atbF <- mcstoc(runif,type="VU",min=L_MatbF[1], max=L_MatbF[2]) # Matb is a percentage
  L_atbI <- mcstoc(runif,type="VU",min=L_MatbI[1], max=L_MatbI[2]) # Matb is a percentage
  # percentage of inidivudals exposed to husbandry
  L_husb <- mcstoc(rnorm,type="VU",mean=L_Mhusb, sd=0.1)
  # Distribution of husbandries according to their type: family-like and indsutry-like
  L_PHFam <- mcstoc(runif,type="VU", min=L_MPHFam[1], max=L_MPHFam[2])
  # Density of animals in the two types of husbandry : family-like and industry-like
  L_NaF <- mcstoc(rnorm,type="VU",mean=L_MNFHa, sd=2)
  L_NaI <- mcstoc(rnorm,type="VU",mean=L_MNIHa, sd=20)
  # Human-animal contacts 
  L_CtcAHF <- mcstoc(rnorm,type="VU",mean=L_MNcontacts[1], sd=1) # family-like 
  L_CtcAHI <- mcstoc(rnorm,type="VU",mean=L_MNcontacts[2], sd=1) # industry-like
  
  L_tAH <-  mcstoc(runif,type="VU",min=L_MtAH[1], max=L_MtAH[2]) # transmission risk
  # Hygiene level in farms when contacts with animals
  L_HygF <- mcstoc(runif,type="VU",min=L_MHygF[1], max=L_MHygF[2]) # affecting transmission in family-like farms
  L_HygI <- mcstoc(runif,type="VU",min=L_MHygI[1], max=L_MHygI[2]) # affecting transmission in insdustry-like farms
  
  # antibiotic selection
  L_pBMRF = L_prev^(1/(1+L_atbF))
  L_pBMRI = L_prev^(1/(1+L_atbI))
  
  # dose : number of contacts
  L_NCtctsF <- L_NaF * L_CtcAHF #former L_expoF
  L_NCtctsI <-  L_NaI * L_CtcAHI #former L_expoI
  
  # acquisition risk during a contact => EXPOSURE / Point risk
  L_RCtcF <- L_pBMRF * L_tAH * exp(-L_HygF)
  L_RCtcI <- L_pBMRI * L_tAH * exp(-L_HygI)
  
  # raw <- mcstoc(rempiricalD,type="VU",values=c(1,0.01), prob=c(Mraw,1-Mraw))
  #####################################
  ## PART C: Risk calculation
  #####################################
  
  # calculate risk
  # L_risk <- 1 - (1-L_RcontactF)^L_NcontactsF - (1-L_RcontactI)^L_NcontactsI
  L_risk <- L_husb * (L_PHFam *L_NCtctsF*100*L_RCtcF
                    + (1- L_PHFam) * L_NCtctsI *100 * L_RCtcI)
    
  ## Two dimensional Monte Carlo simulation
  # ECL <- mc(L_prev,L_atbF, L_atbI, L_husb, L_PHFam, L_NaF, L_NaI, L_CtcAHF, L_CtcAHI, L_tAH, L_HygF, L_HygI, L_pBMRF, 
            # L_pBMRI,L_NCtctsF, L_NCtctsI, L_RCtcF, L_RCtcI, L_risk)
  ECL <- mc(L_prev,L_atbF, L_atbI, L_husb, L_PHFam, L_NaF, L_NaI, L_CtcAHF, L_CtcAHI, L_tAH, L_HygF, L_HygI,L_risk)
  
  return(list(L_risk, ECL))
  

}

