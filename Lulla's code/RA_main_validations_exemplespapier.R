#####################################################################""
### Risk assessment model - WHO
### GLOBAL MODEL of risk assessment of acqusition of AMR in humans 
### author: L Opatowski
### date: 2019/12/31

### In this program we use a monte carlo approach to estimate the risk associated 
### with a set of conditions
### and parameter values

# 2 applications of submodels to "validate" predictions on numbers from recent 
# reports

### submodels considered here :
### - F: acqusition through food consumption
### - L: acqusition through contacts with Livestock


### All values can be modified by the user in script names: 
###     RA_variables_valid.R
### Models and distributions are described in the corresponding model files
###     model_food_consumption_F.R; model_contact_Livestock_L.R; 
###

#####################################################################
#to run only the first time:
# install.packages("mc2d")
# to run at each use
library(mc2d)

#####################################
ndvar(1001) # default size of Monte carlo simulation for variability
ndunc(101) # for uncertainty
# mcnode: V : variability; U : uncertainty; VU : variability and uncertainty
#####################################

setwd("C:/Users/Lulla/Dropbox/PROJETS/WHO_Sirenda/MODEL_AMR_SEAR/Models/final_model_paper")

source("variables.R")
source("variables_validThailand.R")

# Case 1 - food model for Thailand, assess the impact of retail etc

F_prev <- mcstoc(runif,type="VU", min=F_Mprev[1], max=F_Mprev[2])
F_atb <- mcstoc(runif,type="VU",min=F_Matb[1], max=F_Matb[2])

F_arb <- F_prev^(1/(1+F_atb))

F_slaugh <- mcstoc(rnorm,type="VU",mean=F_Mslaught, sd=0.01)
F_ret <- mcstoc(runif,type="VU",min=F_Mret[1], max=F_Mret[2])

F_treat <- 1/((1+F_slaugh)*(1+F_ret))

F_cook <- mcstoc(rempiricalD,type="VU",values=c(1,0.01), prob=c(F_Mraw,1-F_Mraw))
F_dose <- mcstoc(rnorm,type="VU", mean=F_Mdose, sd=10)

F_acq <-mcstoc(rnorm,type="VU",mean=F_Macq, sd=0.01)

#prev after slaughter
Prev_sl <-F_arb^(1/((1+F_slaugh)))
#prev after retail
Prev_ret <-Prev_sl^(1/((1+F_ret)))

PcolF <- (F_arb^F_treat)*F_cook*F_acq #PcolF former F_expo 

F_risk <- PcolF*F_dose*100 # mean result of a Binomila (N=F_dose*100, p=F_expo)

## Two dimensional Monte Carlo simulation
ECF <- mc(F_prev,F_atb,F_arb,F_slaugh,F_ret,F_treat,F_cook,F_dose,F_acq,PcolF, F_risk)

#################
## Case 2 - Livestock model for Thailand - assess the impact of contacts with animals

# prevalence
L_prev <- mcstoc(runif,type="VU", min=L_Mprev[1], max=L_Mprev[2])
# antibiotic frequency
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
L_risk_F <- L_husb  *L_NCtctsF*100*L_RCtcF
L_risk_I <- L_husb  * L_NCtctsI *100 * L_RCtcI

# Final prevalence in humans
input_prev_F <- (Mcolo_duration * L_risk_F/100)/(Mcolo_duration * L_risk_F/100+1) # duration in year-1
input_prev_I <- (Mcolo_duration * L_risk_I/100)/(Mcolo_duration * L_risk_I/100+1) # duration in year-1


## Two dimensional Monte Carlo simulation
ECL <- mc(L_prev,L_atbF, L_atbI, L_husb, L_PHFam, L_NaF, L_NaI, L_CtcAHF, L_CtcAHI, L_tAH, L_HygF, L_HygI, L_pBMRF, 
          L_pBMRI,L_NCtctsF, L_NCtctsI, L_RCtcF, L_RCtcI, L_risk)

