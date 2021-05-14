#######################################################################
### Risk assessment model - WHO
### GLOBAL MODEL of risk assessment of acqusition of AMR in humans 
### author: L Opatowski
### date: 2018/09/13


### Definition of parameters values specific to scenario 2 (higher income country)
###
### All other model parameters are defined elsewhere (script variables.R)

########################################################################

## Water model
W_Maccess <- c(0.7,0.8) # % of the population having access to safely managed water. 

## Food model
F_Mret <- c(-0.5,-0.1) # ret = hygiene level at retail. 0: neutral; -1: perfect;  +1: very poor 

# Livestock model
# L_MPHFam <- c(0.35,0.5) # percentage of farms set in a family context (by opposition to industrial context), specify(min, max)
L_Mhusb <- 0.3 # percentage of inidivuals involved in husbandry in the country


# Hospital model
H_Mhyg <- c(0.5,0.6) # Level of hygiene control during contacts (~ % of contacts for which hygiene control is implemented), specify (min, max)

# Community model
C_Matb  <- c(0.15, 0.30) # percentage of conterfactuals + misuse

