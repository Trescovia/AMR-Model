#####################################################################""
### Risk assessment model - WHO
### GLOBAL MODEL of risk assessment of acqusition of AMR in humans 
### author: L Opatowski
### date: 2018/09/14

### In this program we use a monte carlo approach to estimate the risk associated 
### with a set of conditions
### and parameter values

### In the Global model, the risk is calculated based on the combination of risks
### computed from 5 different submodels:
### - W : acqusition through water consumption
### - F: acqusition through food consumption
### - L: acqusition through contacts with Livestock
### - H1: acqusition from human contacts within the hospital
### - H2: acqusition from human contacts within the community


### All values can be modified by the user in script names: 
###     RA_variables.R
### Models and distributions are described in the corresponding model files
###     model_water_contamination_W; model_food_consumption_F.R; model_contact_Livestock_L.R; 
###     model_hospital_transmission_H1.R; model_community_transmission_H2.R
###

### In this particular script, we run a specific scenario described in the article
### Scenario 1 - lower income country

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

setwd("C:/Users/tresc/Desktop/AMR-Model/Lulla's code")


# Models
source("variables.R")
source("variables_scen1.R")
source("model_water_W.R")
source("model_Livestock_L.R")
source("model_food_F.R")
source("model_hospital_H1.R")
source("model_community_H2.R")
source("analyse_output.R")

# Execute each submodel independently
W_res <- W_distrib_risk()
L_res <- L_distrib_risk()
F_res <- F_distrib_risk()
# H1_res <- H_distrib_risk() # H_distrib_risk(indiv_acq)
# H2_res <- C_distrib_risk() # C_distrib_risk(indiv_acq)


# Get models outputs
ECW <- W_res[[2]]
ECL <- L_res[[2]]
ECF <- F_res[[2]]
# ECH <- H1_res[[2]]
# ECC <- H2_res[[2]]

RW <- W_res[[1]]
RL <- L_res[[1]]
RF <- F_res[[1]]
# RH <- H1_res[[1]]
# RC <- H2_res[[1]]


# Caculate the global risk

# risk entering in humans
R1 <- (RL + RF + RW)/100

input_prev <- (Mcolo_duration * R1)/(Mcolo_duration * R1+1) # duration in year-1

# could use this R1 as inidividual risk of acquisition in models for human risk assessment, eg.
H1_res <- H_distrib_risk(input_prev) # H_distrib_risk(indiv_acq) # equivalent to H_Mprev, mean prevalence in hospitals at admission
H2_res <- C_distrib_risk(input_prev) # C_distrib_risk(indiv_acq) # equivalent to C_Mprev, mean prevalence in community

RH1 <- H1_res[[1]]
RH2 <- H2_res[[1]]

ECH <- H1_res[[2]]
ECC <- H2_res[[2]]

Rfinal <-  RH1 + RH2 # incidence per 100 individuals per year

# print outputs
output_save(W_res,L_res, F_res,H1_res, H2_res, spec_dir="scen1")

# hist(ECW)
# tornadounc(ECW, output=length(ECW), quant=0.95)
# 
# hist(ECF)
# tornadounc(ECF, output=length(ECF), quant=0.95)
# 
# hist(ECL)
# tornadounc(ECL, output=length(ECL), quant=0.95)
# 
# hist(ECH)
# tornadounc(ECW, output=length(ECH), quant=0.95)
# 
# hist(ECC)
# tornadounc(ECC, output=length(ECH), quant=0.95)
# 
# 

