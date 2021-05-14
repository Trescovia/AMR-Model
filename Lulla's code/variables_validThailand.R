#######################################################################
### Risk assessment model - WHO
### GLOBAL MODEL of risk assessment of acqusition of AMR in humans 
### author: L Opatowski
### date: 2019/12/31


### Definition of parameters values specific to the two cases studies 
### used for validation in the discussion section
###
### All other model parameters are defined elsewhere (script variables.R)

########################################################################

## Food model - case 1

F_Mprev <- c(0.14, 0.16) # prevalence in animal, specify (min, max)
F_Matb  <- c(3,3) # Level of percentage of farms using antibiotics
F_Mslaught <- -0.7 # slaught = hygiene level at slaughter house. 0: neutral; -1: perfect;  +1: very poor 
F_Mret <- c(0.45,0.55) # ret = hygiene level at retail. 0: neutral; -1: perfect;  +1: very poor 
F_Mraw <- 0.2 # raw = proportion of meat which is eaten raw
F_Mdose <- 50 # conso = number of times one individual eats meat in average over the year
F_Macq <- 0.1 # acquisition risk when eaten contaminated product

# Livestock model - case 2

L_Mhusb <- 1 # percentage of inidivuals involved in husbandry in the country
L_MtAH <- c(0.0039,0.0041) # baseline transmission risk during one human-animal contact
L_Mprev <- c(0.14, 0.16) # prevalence in animal, specify (min, max)

L_MPHFam <- c(0.35,0.95) # percentage of farms set in a family context (by opposition to industrial context), specify(min, max)
L_MNFHa <- 100 # average number of animals  in family husbandry
L_MNIHa <- 200 # average number of animal in idndustrial husbandry

L_MatbF  <- c(3, 3) # antibiotic use in family farming (= level)
L_MatbI  <- c(3, 3) # antibiotic use in industrial farming (= level)
L_MNcontacts <- c(365, 365) # number of contacts per year with a given animal from the farm in family-farms and industrial-like farms 
L_MHygF <- c(0.25, 0.35) # average hygiene level in the farm, in family-like farms. in 0-1
L_MHygI <- c(0.25, 0.35) # average hygiene level in the farm, in  instrial-like farms.  in 0-1

