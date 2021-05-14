#######################################################################
### Risk assessment model - WHO
### GLOBAL MODEL of risk assessment of acqusition of AMR in humans 
### author: L Opatowski
### date: 2017/11/28


### Script including all variables definitions
###
### Model variables are described for each of the 5 models

########################################################################

## Water model

W_Mprev <- c(0.29, 0.6) # prevalence in water, specify (min, max)
W_Matb  <- c(0,1) # concentration level of antibiotics in natural waters, contamination from hospital release, pharma industries and livestock
W_Mtreat <- c(1,2) # Level of managment of water in the country. Treatment is supposed to eradicate ARB. 0: neutral; -1: perfect;  +1: very poor 
W_Maccess <- c(0.3,0.8) # % of the population having access to safely managed water. 
W_Macq <- 0.05 # acquisition risk when drinking contaminated water on a given day
W_Mdose <- 360 # frequency of water consumption over a year
  
## Food model

F_Mprev <- c(0.01, 0.15) # prevalence in animal, specify (min, max)
F_Matb  <- c(2,3) # Level of percentage of farms using antibiotics
F_Mslaught <- 0 # slaught = hygiene level at slaughter house. 0: neutral; -1: perfect;  +1: very poor 
F_Mret <- c(-0.5,0.5) # ret = hygiene level at retail. 0: neutral; -1: perfect;  +1: very poor 
F_Mraw <- 0.2 # raw = proportion of meat which is eaten raw
F_Mdose <- 50 # conso = number of times one individual eats meat in average over the year
F_Macq <- 0.1 # acquisition risk when eaten contaminated product

# Livestock model

L_Mhusb <- 0.7 # percentage of inidivuals involved in husbandry in the country
L_MtAH <- c(0.001,0.005) # baseline transmission risk during one human-animal contact
L_Mprev <- c(0.01, 0.05) # prevalence in animal, specify (min, max)

L_MPHFam <- c(0.35,0.95) # percentage of farms set in a family context (by opposition to industrial context), specify(min, max)
L_MNFHa <- 10 # average number of animals  in family husbandry
L_MNIHa <- 200 # average number of animal in idndustrial husbandry

L_MatbF  <- c(1, 2) # antibiotic use in family farming (= % of animals exposed to atb)
L_MatbI  <- c(2, 3) # antibiotic use in industrial farming (= % of animals exposed to atb)
L_MNcontacts <- c(365, 52) # number of contacts per year with a given animal from the farm in family-farms and industrial-like farms 
L_MHygF <- c(0.1, 0.3) # average hygiene level in the farm, in family-like farms. 0: neutral; -1: perfect;  +1: very poor, specify (min, max) 
L_MHygI <- c(0.4, 0.6) # average hygiene level in the farm, in  instrial-like farms. 0: neutral; -1: perfect;  +1: very poor 

# Hospital model

H_MNhospit <- 0.45 # number of hospital days per year and per person (~ yearly rate of hospitalization)
H_Mprev <- 0.3 # mean prevalence in hospitals at admission
H_Matb  <- c(1,2) # percentage of patients exposed to antibiotics in hospitals, specify (min, max)
H_Mtrans <- 0.2 # Average risk of transmission (baseline) during patient contacts within a hospital 
H_MFmr <- c(1,2) # % of multiple rooms in hospitals, specify (min, max)
# H_MtransFmr <- 2 # increased factor of transmission in multiple rooms
H_Mhyg <- c(0.2,0.4) # Level of hygiene control during contacts (~ % of contacts for which hygiene control is implemented), specify (min, max)
# H_select <- 2 # selective force of antibiotic pressure on ARB

# Community model

C_Mprev <- 0.3 # mean prevalence in community
C_Matb  <- c(0.15, 0.55) # percentage of conterfactuals + misuse
C_Mhyg <- 0.6#c(0.4,0.9) # % of individuals with access to hygiene installation 
C_Mpsan <- 0.9#c(0.02, 0.22) # Percentage of population with access to private sanitation
C_Mtrans <- 0.01 # Average risk of transmission (baseline) between individuals per day within the community 
C_Mdose <- 360 # frequency of exposure to the risk over a year


# Global variable for the bacteria
Mcolo_duration <- 8/365 # duration of carriage of the bacteria, in years. D needs to be calibrated from the literature for each ARB species
