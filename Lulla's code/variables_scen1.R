#######################################################################
### Risk assessment model - WHO
### GLOBAL MODEL of risk assessment of acqusition of AMR in humans 
### author: L Opatowski
### date: 2018/09/13


### Definition of parameters values specific to scenario 1
###
### All other model parameters are defined elsewhere (script variables.R)

########################################################################

## Water model
W_Maccess <- c(0.3,0.4) # % of the population having access to safely managed water. 

## Food model
F_Mret <- c(0.1,0.5) # ret = hygiene level at retail. 0: neutral; -1: perfect;  +1: very poor 

# Livestock model
# L_MPHFam <- c(0.8,0.95) # percentage of farms set in a family context (by opposition to industrial context), specify(min, max)
L_Mhusb <- 0.8 # percentage of inidivuals involved in husbandry in the country


# Hospital model
H_Mhyg <- c(0.2,0.3) # Level of hygiene control during contacts (~ % of contacts for which hygiene control is implemented), specify (min, max)

# Community model
C_Matb  <- c(0.4, 0.55) # percentage of conterfactuals + misuse

