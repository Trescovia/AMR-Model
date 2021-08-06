#Per-farm intervention cost

#LIC
farmers_per_seminar <- 10
hourly_compensation <- 1.15
seminar_length <- 3
visits_per_year <- 3
visit_length_individual <- 2
transport_cost <- 5
seminar_cost <- (4+seminar_length)*hourly_compensation
visit_cost <- (4+visit_length_individual)*hourly_compensation

##village-level (LIC)

group_size <- 10
additional_time_per_farm <- 0.5
visit_length_village <- 2

cost_village_LIC <- seminar_cost/farmers_per_seminar + 
  seminar_length*hourly_compensation + transport_cost +
  (visit_cost + ((group_size - 1)*additional_time_per_farm*hourly_compensation))*(visits_per_year/group_size) +
  visit_length_village*hourly_compensation*visits_per_year

##individual (LIC)

cost_farm_LIC <- visit_cost*visits_per_year + 
  visit_length_individual*hourly_compensation*visits_per_year +
  seminar_cost/farmers_per_seminar +
  seminar_length*hourly_compensation +
  transport_cost

#MIC
farmers_per_seminar <- 10
hourly_compensation <- 4.89
seminar_length <- 3
visits_per_year <- 3
visit_length_individual <- 2
transport_cost <- 10
seminar_cost <- (4+seminar_length)*hourly_compensation
visit_cost <- (4+visit_length_individual)*hourly_compensation

##village-level (MIC)

group_size <- 10
additional_time_per_farm <- 0.5
visit_length_village <- 2

cost_village_MIC <- seminar_cost/farmers_per_seminar + 
  seminar_length*hourly_compensation + transport_cost +
  (visit_cost + ((group_size - 1)*additional_time_per_farm*hourly_compensation))*(visits_per_year/group_size) +
  visit_length_village*hourly_compensation*visits_per_year

##individual (MIC)

cost_farm_MIC <- visit_cost*visits_per_year + 
  visit_length_individual*hourly_compensation*visits_per_year +
  seminar_cost/farmers_per_seminar +
  seminar_length*hourly_compensation +
  transport_cost

#HIC
farmers_per_seminar <- 10
hourly_compensation <- 31.98
seminar_length <- 3
visits_per_year <- 3
visit_length_individual <- 2
transport_cost <- 20
seminar_cost <- (4+seminar_length)*hourly_compensation
visit_cost <- (4+visit_length_individual)*hourly_compensation

##village-level (HIC)

group_size <- 10
additional_time_per_farm <- 0.5
visit_length_village <- 2

cost_village_HIC <- seminar_cost/farmers_per_seminar + 
  seminar_length*hourly_compensation + transport_cost +
  (visit_cost + ((group_size - 1)*additional_time_per_farm*hourly_compensation))*(visits_per_year/group_size) +
  visit_length_village*hourly_compensation*visits_per_year

##individual (HIC)

cost_farm_HIC <- visit_cost*visits_per_year + 
  visit_length_individual*hourly_compensation*visits_per_year +
  seminar_cost/farmers_per_seminar +
  seminar_length*hourly_compensation +
  transport_cost


#Macro-level intervention cost

smallholder_farm_size_p <- 10
smallholder_farm_size_c <- 2500
industrial_farm_size_p <- 2000
industrial_farm_size_c <- 40000

portion_farms_per_year <- 1

#LICs
n_pigs <- 3426081
n_chickens <- 89964865 #in the final model, we could turn n_pigs and n_chickens into
# a single parameter of how much agricultural production is created relative to the 
# world average, e.g. meat_production. However, the two might affect things separately
# and we might want to decompose the effects of number of chickens vs. pigs
portion_smallholder <- 1
cost_farm <- cost_farm_LIC
cost_village <- cost_village_LIC

#MICs
n_pigs <- 10479823
n_chickens <- 275187810
portion_smallholder <- 0.5
cost_farm <- cost_farm_MIC
cost_village <- cost_village_MIC

#HICs
n_pigs <- 28839828
n_chickens <- 757299960
portion_smallholder <- 0
cost_farm <- cost_farm_HIC
cost_village <- cost_village_HIC


#annual_intervention_cost <- function(n_pigs, n_chickens, portion_smallholder, cost_farm, cost_village){
# in the final model, we would combine these 5 function inputs into a simple table

###smallholder cost
smallholder_pigs <- n_pigs * portion_smallholder
smallholder_pig_farms <- smallholder_pigs / smallholder_farm_size_p

smallholder_chickens <- n_chickens * portion_smallholder
smallholder_chicken_farms <- smallholder_chickens / smallholder_farm_size_c

smallholder_farms <- smallholder_pig_farms + smallholder_chicken_farms
smallholder_cost <- smallholder_farms * cost_village

###industrial cost
industrial_pigs <- n_pigs * (1 - portion_smallholder)
industrial_pig_farms <- industrial_pigs / industrial_farm_size_p

industrial_chickens <- n_chickens * (1-portion_smallholder)
industrial_chicken_farms <- industrial_chickens / industrial_farm_size_c

industrial_farms <- industrial_chicken_farms + industrial_pig_farms
industrial_cost <- industrial_farms * cost_farm 

###total cost
annual_cost <- (industrial_cost + smallholder_cost) * portion_farms_per_year

#return(annual_cost)
#}

