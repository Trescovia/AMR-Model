#load packages
library(mc2d)

##set global values, working directory, etc.
ndvar(1001) # default size of Monte carlo simulation for variability
ndunc(101) # for uncertainty
# mcnode: V : variability; U : uncertainty; VU : variability and uncertainty
setwd("C:/Users/tresc/Desktop/AMR-Model/Lulla's code")

################################################################################
#RUN THE BASELINE MODEL (NO INTERVENTION)
################################################################################

set.seed(420)

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
H1_res <- H_distrib_risk() # H_distrib_risk(indiv_acq)
H2_res <- C_distrib_risk() # C_distrib_risk(indiv_acq)

# Get models outputs
ECW <- W_res[[2]]
ECL <- L_res[[2]]
ECF <- F_res[[2]]
ECH <- H1_res[[2]]
ECC <- H2_res[[2]]

RW <- W_res[[1]]
RL <- L_res[[1]]
RF <- F_res[[1]]
RH <- H1_res[[1]]
RC <- H2_res[[1]]

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
output_save(W_res,L_res, F_res,H1_res, H2_res, spec_dir="baseline")

hist(ECW)
tornadounc(ECW, output=length(ECW), quant=0.95)

hist(ECF)
tornadounc(ECF, output=length(ECF), quant=0.95)

hist(ECL)
tornadounc(ECL, output=length(ECL), quant=0.95)

hist(ECH)
tornadounc(ECW, output=length(ECH), quant=0.95)

hist(ECC)
tornadounc(ECC, output=length(ECH), quant=0.95)

# Results for incidence / risk:
# mean: 169.029794696752
# median: 128.096201656516 [min = 66.7784594767253, max = 1105.56731732568]

################################################################################
#RUN THE INTERVENTION (FAMILY AND INDUSTRIAL FARMS)
################################################################################

#load packages
library(mc2d)

##set global values, working directory, etc.
ndvar(1001) # default size of Monte carlo simulation for variability
ndunc(101) # for uncertainty
# mcnode: V : variability; U : uncertainty; VU : variability and uncertainty
setwd("C:/Users/tresc/Desktop/AMR-Model/Lulla's code")

set.seed(420)

# Models
source("variables.R")
source("variables_scen1.R")
source("variables_intervene_067.R")
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
H1_res <- H_distrib_risk() # H_distrib_risk(indiv_acq)
H2_res <- C_distrib_risk() # C_distrib_risk(indiv_acq)

# Get models outputs
ECW <- W_res[[2]]
ECL <- L_res[[2]]
ECF <- F_res[[2]]
ECH <- H1_res[[2]]
ECC <- H2_res[[2]]

RW <- W_res[[1]]
RL <- L_res[[1]]
RF <- F_res[[1]]
RH <- H1_res[[1]]
RC <- H2_res[[1]]

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
output_save(W_res,L_res, F_res,H1_res, H2_res, spec_dir="interv_family_industrial")

hist(ECW)
tornadounc(ECW, output=length(ECW), quant=0.95)

hist(ECF)
tornadounc(ECF, output=length(ECF), quant=0.95)

hist(ECL)
tornadounc(ECL, output=length(ECL), quant=0.95)

hist(ECH)
tornadounc(ECW, output=length(ECH), quant=0.95)

hist(ECC)
tornadounc(ECC, output=length(ECH), quant=0.95)

# Results for incidence / risk:
# mean: 153.37264646213
# median: 116.508861446226 [min = 54.8328523431575, max = 995.626715061975]

################################################################################
#RUN THE INTERVENTION (FAMILY FARMS ONLY)
################################################################################

#load packages
library(mc2d)

##set global values, working directory, etc.
ndvar(1001) # default size of Monte carlo simulation for variability
ndunc(101) # for uncertainty
# mcnode: V : variability; U : uncertainty; VU : variability and uncertainty
setwd("C:/Users/tresc/Desktop/AMR-Model/Lulla's code")

set.seed(420)

# Models
source("variables.R")
source("variables_scen1.R")
source("variables_intervene_067_family.R")
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
H1_res <- H_distrib_risk() # H_distrib_risk(indiv_acq)
H2_res <- C_distrib_risk() # C_distrib_risk(indiv_acq)

# Get models outputs
ECW <- W_res[[2]]
ECL <- L_res[[2]]
ECF <- F_res[[2]]
ECH <- H1_res[[2]]
ECC <- H2_res[[2]]

RW <- W_res[[1]]
RL <- L_res[[1]]
RF <- F_res[[1]]
RH <- H1_res[[1]]
RC <- H2_res[[1]]

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
output_save(W_res,L_res, F_res,H1_res, H2_res, spec_dir="interv_family")

hist(ECW)
tornadounc(ECW, output=length(ECW), quant=0.95)

hist(ECF)
tornadounc(ECF, output=length(ECF), quant=0.95)

hist(ECL)
tornadounc(ECL, output=length(ECL), quant=0.95)

hist(ECH)
tornadounc(ECW, output=length(ECH), quant=0.95)

hist(ECC)
tornadounc(ECC, output=length(ECH), quant=0.95)

# Results for incidence / risk:
# mean: 163.298672354718
# median: 123.832027464404 [min = 61.8828479933577, max = 1063.41037266073]

################################################################################
# FINAL RESULTS (MEAN RISK)
# BASELINE: 169.029794696752 (-0.00)
# FAMILY AND INDUSTRIAL: 153.37264646213 (-0.092629517)
# FAMILY ONLY: 163.298672354718 (-0.0339059889)
