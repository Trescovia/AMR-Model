cd "C:\Users\tresc\Desktop\AMR-Model\General model\input data"

import delimited using "global-meat-production.csv"

ren entity country

label var country "country"

drop code

egen max_year = max(year), by(country)

drop if year != max_year

drop year max_year

ren livestockprimarymeattotal1765pro meat_production

gen income_status = "M"

label var income_status "country income status (low, mid, high)"

drop if country == "Africa" | country == "Asia" | country == "Asia, Central" | country == "Australia & New Zealand" | country == "Belgium-Luxembourg" | country == "Caribbean" | country == "Central America" | country == "Czechoslovakia" | country == "Eastern Africa" | country == "Eastern Asia" | country == "Eastern Europe" | country == "Europe" | country == "Europe, Western" | country == "European Union" | country == "" | country == "Land Locked Developing Countries" | country == "Low Income Food Deficit Countries" | country == "Micronesia (region)" | country == "Melanesia" | country == "Middle Africa" | country == "Net Food Importing Developing Countries" | country == "Northern Africa" | country == "Northern America" | country == "Northern Europe" | country == "Oceania" | country == "Polynesia" | country == "Small island developing States" | country == "South America" | country == "South Eastern Asia" | country == "Southern Africa" | country == "Southern Asia" | country == "Southern Europe" | country == "Sudan (former)" | country == "USSR" | country == "Western Africa" | country == "Western Asia" | country == "World" | country == "Yugoslavia" | country == "Ethiopia PDR" | country == "Serbia and Montenegro" | country == "Netherlands Antilles" | country == "Americas" | country == "French Polynesia" | country == "Hong Kong" | country == "Macao" | country == "New Caledonia" | country == "Least Developed Countries" 

replace income_status = "L" if country == "Afghanistan" | country == "Burkina Faso" | country == "Central African Republic" | country == "Chad" | country == "Ethiopia" | country == "Gambia" | country == "Guinea" | country == "Guinea-Bissau" | country == "North Korea" | country == "Liberia" | country == "Madagascar" | country == "Malawi" | country == "Mozambique" | country == "Niger" | country == "Rwanda" | country == "Sierra Leone" | country == "Togo" | country == "Uganda" | country == "Yemen"

replace income_status = "H" if country == "Antigua and Barbuda" | country == "Australia" | country == "Austria" | country == "Bahamas" | country == "Belgium" | country == "Bermuda" | country == "Brunei" | country == "Canada" | country == "Chile" | country == "Croatia" | country == "Cyprus" | country == "Denmark" | country == "Estonia" | country == "Finland" | country == "France" | country == "Germany" | country == "Greece" | country == "Hungary" | country == "Iceland" | country == "Ireland" | country == "Israel" | country == "Italy" | country == "Japan" | country == "South Korea" | country == "Kuwait" | country == "Latvia" | country == "Luxembourg" | country == "Malta" | country == "Netherlands" | country == "New Zealand" | country == "Norway" | country == "Poland" | country == "Slovakia" | country == "Slovenia" | country == "Spain" | country == "Sweden" | country == "Oman" | country == "Portugal" | country == "Saudi Arabia" | country == "Switzerland" | country == "Taiwan" | country == "Trinidad and Tobago" | country == "United Arab Emirates" | country == "United Kingdom" | country == "United States" | country == "Uruguay"

save meat_supply.dta, replace

import excel using "wbpop.xls", sheet("Clean") firstrow clear

ren Year year

egen max_year = max(year), by(Country)

drop if year != max_year

drop year max_year

drop if Country == "Africa Eastern and Southern" | Country == "Africa Western and Central" | Country == "Arab World" | Country == "British Virgin Islands" | Country == "American Samoa" | Country == "Caribbean small states" | Country == "Cayman Islands" | Country == "Central Europe and the Baltics" | Country == "Channel Islands" | Country == "Early-demographic dividend" | Country == "East Asia & Pacific (excluding high income)" | Country == "East Asia & Pacific" | Country == "Europe & Central Asia (excluding high income)" | Country == "Europe & Central Asia" | Country == "European Union" | Country == "Fragile and conflict affected situations" | Country == "Gibraltar" | Country == "IBRD only" | Country == "High income" | Country == "Hong Kong SAR, China" | Country == "Heavily indebted poor countries (HIPC)" | Country == "IDA & IBRD total" | Country == "IDA total" | Country == "IDA blend" | Country == "IDA only" | Country == "Isle of Man" | Country == "Latin America & Caribbean (excluding high income)" | Country == "Latin America & Caribbean" | Country == "Least developed countries: UN classification" | Country == "Low income" | Country == "Lower middle income" | Country == "Low & middle income" | Country == "Late-demographic dividend" | Country == "Macao SAR, China" | Country == "St. Martin (French part)" | Country == "Middle East & North Africa" | Country == "Middle income" | Country == "Middle East & North Africa (excluding high income)" | Country == "Northern Mariana Islands" | Country == "North America" | Country == "OECD members" | Country == "Other small states" | Country == "Pre-demographic dividend" | Country == "Pacific island small states" | Country == "Post-demographic dividend" | Country == "French Polynesia" | Country == "South Asia" | Country == "Sub-Saharan Africa (excluding high income)" | Country == "Sub-Saharan Africa" | Country == "Small states" | Country == "Sint Maarten (Dutch part)" | Country == "East Asia & Pacific (IDA & IBRD countries)" | Country == "Europe & Central Asia (IDA & IBRD countries)" | Country == "Latin America & the Caribbean (IDA & IBRD countries)" | Country == "Middle East & North Africa (IDA & IBRD countries)" | Country == "South Asia (IDA & IBRD)" | Country == "Sub-Saharan Africa (IDA & IBRD countries)" | Country == "Upper middle income" | Country == "British Virgin Islands" | Country == "Virgin Islands (U.S.)" | Country == "World" | Country == "Guam" | Country == "Turks and Caicos Islands" | Country == "Euro area" | Country == "Faroe Islands"

ren Country country

replace country = "Bahamas" if country == "Bahamas, The"
replace country = "Brunei" if country == "Brunei Darussalam"
replace country = "Cabo Verde" if country == "Cape Verde"
replace country = "Democratic Republic of Congo" if country == "Congo, Dem. Rep."
replace country = "Czechia" if country == "Czech Republic"
replace country = "Egypt" if country == "Egypt, Arab Rep."
replace country = "Gambia" if country == "Gambia, The"
replace country = "Iran" if country == "Iran, Islamic Rep."
replace country = "South Korea" if country == "Korea, Rep."
replace country = "North Korea" if country == "Korea, Dem. People's Rep."
replace country = "Laos" if country == "Lao PDR"
replace country = "Kyrgyzstan" if country == "Kyrgyz Republic"
replace country = "Micronesia (country)" if country == "Micronesia, Fed. Sts."
replace country = "Palestine" if country == "West Bank and Gaza"
replace country = "Russia" if country == "Russian Federation"
replace country = "Slovakia" if country == "Slovak Republic"
replace country = "Syria" if country == "Syrian Arab Republic"
replace country = "Timor" if country == "Timor-Leste"
replace country = "Venezuela" if country == "Venezuela, RB"
replace country = "Yemen" if country == "Yemen, Rep."

merge 1:1 country using "meat_supply.dta"

gen pc_meat_production = meat_production / Population

replace pc_meat_production = pc_meat_production * 100000

label var pc_meat_production "meat production (tonnes per 100,000 population)"

drop if pc_meat_production == .

drop _merge

egen mean_production_pc = mean(pc_meat_production), by(income_status)

label var mean_production_pc "average per-capita meat production by country income status"

egen weighted_mean_production_pc = wtmean(pc_meat_production), weight(Population) by(income_status)

label var weighted_mean_production_pc "population-weighted average per-capita meat production by country income status"

save meat_supply.dta, replace

/*
world made 342422466 tonnes and had population of 7752840547, giving 

4416.7356 tonnes per 100,000 population 

for LICs it is 1186.831, or .26871226 the world average

for MICs it is 3630.322, or .82194687 the world average

for HICs it is 9990.423, or 2.2619473 the world average

There were 25.69bn chickens and 0.9783bn pigs worldwide, or 3.348 and 0.1275 per person

assuming that pigs/capita and chickens/capita are roughly proportional to meat supply/capita, there would be 

Chickens:
- 7.5729996 per person in HICs 
- 2.7518781 per person in MICs
- .89964865 per person in LICs

Pigs
- .28839828 per person in HICs 
- .10479823 per person in MICs
- .03426081 per person in LICs

for a hypothetical country of 100,000,000 people, this means 

Chickens
- 757,299,960 in HICs 
- 275,187,810 in MICs
- 89,964,865 in LICs

Pigs:
- 28,839,828 in HICs 
- 10,479,823 in MICs
- 3,426,081 in LICs
*/

//from old dataset
/*
egen mean_production_pc = mean(meat_supply), by(income_status)

label var mean_production_pc "mean meat production (kg / capita / year) by country income status"
*/

/*
results: 14.2kg for LIC, 41.3kg for MIC, 78.1kg for HIC

recall that for the world as a whole, it was 43.22, 
meaning that HICs have 1.807 times as much as the world average, 
MICs have 0.9556 times as much, 
and LICs have 0.3286 times as much.

There were 25.69bn chickens and 0.9783bn pigs worldwide, or 3.348 and 0.1275 per person

assuming that pigs/capita and chickens/capita are roughly proportional to meat supply/capita, there would be 

Pigs:
- 0.2304 per person in HICs 
- 
- 

Chickens
- 6.050 per person in HICs
- 
- 

*/
                                   
import delimited using "productivity.csv", clear

ren entity country

label var country "country"

drop code

egen max_year = max(year), by(country)

drop if year != max_year

drop year max_year

ren productivitypwt912019 productivity

label var productivity "hourly productivity per person, nominal USD"

gen income_status = "M"

label var income_status "country income status (low, mid, high)"

drop if country == "Hong Kong" 

replace income_status = "H" if country == "Australia" | country == "Austria" | country == "Barbados" | country == "Belgium" | country == "Canada" | country == "Chile" | country == "Croatia" | country == "Cyprus" | country == "Czechia" | country == "Denmark" | country == "Estonia" | country == "Finland" | country == "France" | country == "Germany" | country == "Greece" | country == "Hungary" | country == "Iceland" | country == "Ireland" | country == "Israel" | country == "Italy" | country == "Japan " | country == "South Korea" | country == "Latvia" | country == "Lithuania" | country == "Luxembourg" | country == "Malta" | country == "Netherlands" | country == "New Zealand" | country == "Norway" | country == "Poland" | country == "Portugal" | country == "Singapore" | country == "Slovakia" | country == "Slovenia" | country == "Spain" | country == "Sweden" | country == "Switzerland" | country == "Taiwan" | country == "Trinidad and Tobago" | country == "United Kingdom" | country == "United States" | country == "Uruguay"

egen mean_productivity = mean(productivity), by(income_status)

label var mean_productivity "mean hourly productivity per person (nominal USD) by income status"

//45.74USD for HICs, 15.43USD for MICs 

replace income_status = "L" if country == "Bangladesh" | country == "Cambodia" | country == "India" | country == "Indonesia" | country == "Myanmar" | country == "Pakistan" | country == "Philippines" | country == "Sri Lanka" | country == "Vietnam"

drop mean_productivity

egen mean_productivity = mean(productivity), by(income_status)

//7.84USD for lower-MICs (standin for LICs)

drop if country == "British Virgin Islands" | country == "China, Hong Kong SAR" | country == "China, Macao SAR" | country == "Curaçao" | country == "Montserrat" | country == "Sint Maarten (Dutch part)" | country == "Turks and Caicos Islands"

replace country = "South Korea" if country == "Republic of Korea"
replace country = "Russia" if country == "Russian Federation"
replace country = "Viet Nam" if country == "Vietnam"

save prod.dta, replace

use pwt.dta, clear

keep year pop emp avh country

egen max_year = max(year), by(country)

drop if year != max_year

drop year max_year

gen lfpr = emp/pop

label var lfpr "labour force participation rate"

label var country "Country"

drop if country == "British Virgin Islands" | country == "China, Hong Kong SAR" | country == "China, Macao SAR" | country == "Curaçao" | country == "Montserrat" | country == "Sint Maarten (Dutch part)" | country == "Turks and Caicos Islands"

replace country = "South Korea" if country == "Republic of Korea"
replace country = "Russia" if country == "Russian Federation"
replace country = "Viet Nam" if country == "Vietnam"

merge 1:1 country using "prod.dta"

gen annual_productivity = avh*productivity 

replace income_status = "M"

replace income_status = "H" if country == "Australia" | country == "Austria" | country == "Barbados" | country == "Belgium" | country == "Canada" | country == "Chile" | country == "Croatia" | country == "Cyprus" | country == "Czechia" | country == "Denmark" | country == "Estonia" | country == "Finland" | country == "France" | country == "Germany" | country == "Greece" | country == "Hungary" | country == "Iceland" | country == "Ireland" | country == "Israel" | country == "Italy" | country == "Japan " | country == "South Korea" | country == "Latvia" | country == "Lithuania" | country == "Luxembourg" | country == "Malta" | country == "Netherlands" | country == "New Zealand" | country == "Norway" | country == "Poland" | country == "Portugal" | country == "Singapore" | country == "Slovakia" | country == "Slovenia" | country == "Spain" | country == "Sweden" | country == "Switzerland" | country == "Taiwan" | country == "Trinidad and Tobago" | country == "United Kingdom" | country == "United States" | country == "Uruguay"

/*
Antigua and Barbuda Bahamas Bahrain Barbados Brunei Darussalam Kuwait Luxembourg Oman Qatar Saudi Arabia Seychelles Singapore United Arab Emirates
*/

replace income_status = "L" if country == "Bangladesh" | country == "Cambodia" | country == "India" | country == "Indonesia" | country == "Myanmar" | country == "Pakistan" | country == "Philippines" | country == "Sri Lanka" | country == "Viet Nam"

/*
Burundi Central African Republic Chad D.R. of the Congo Ethiopia Gambia Guinea Guinea-Bissau Liberia Madagascar Malawi Mali Mozambique Niger Rwanda Sierra Leone Sudan Syrian Arab Republic Togo Uganda Yemen
*/

/*
Angola Algeria Bangladesh Belize Benin Bolivia (Plurinational State of) Cabo Verde Cambodia Cameroon Comoros Congo Côte d'Ivoire Djibouti Egypt El Salvador Eswatini Ghana Haiti Honduras India Indonesia Iran (Islamic Republic of) Kenya Kyrgyzstan Lao People's DR Lesotho Mauritania Mongolia Morocco Myanmar Nepal Nicaragua Nigeria Pakistan Philippines Sao Tome and Principe Senegal Sri Lanka U.R. of Tanzania: Mainland Tajikistan Tunisia Ukraine Uzbekistan Viet Nam State of Palestine Zambia Zimbabwe
*/

egen mean_annual_productivity = mean(annual_productivity), by(income_status)

egen mean_lfpr = mean(lfpr), by(income_status)

/*
annual productivity (worked hours times hourly productivity)
l-MIC: 16365.19
u-MIC: 39920
HIC: 79893

LFPR
l-MIC: .4199239
u-MIC: .4055108
HIC: .4939464
*/

display 0.5*(16365.19+39920)

save prod.dta, replace 







import delimited using "pig weight.csv", clear

drop item element unit

ren date year

egen max_year = max(year), by(country)

drop if year != max_year

drop year max_year

ren value weight

replace weight = weight * 0.1

label var weight "average pig weight, KG"

gen income_status = ""

replace income_status = "M"

replace income_status = "L" if country == "Afghanistan" | country == "Burkina Faso" | country == "Burundi" | country == "Central African Republic" | country == "Chad" | country == "Democratic Republic of the Congo" | country == "Eritrea" | country == "Ethiopia PDR" | country == "Gambia" | country == "Guinea" | country == "Guinea-Bissau" | country == "Democratic People's Republic of Korea" | country == "Liberia" | country == "Madagascar" | country == "Malawi" | country == "Mali" | country == "Mozambique" | country == "Niger" | country == "Rwanda" | country == "Sierra Leone" | country == "Somalia" | country == "South Sudan" | country == "Sudan" | country == "Syria" | country == "Togo" | country == "Uganda" | country == "Yemen"

replace income_status = "H" if country == "Antigua and Barbuda" | country == "Australia" | country == "Austria" | country == "Bahamas" | country == "Barbados" | country == "Belgium" | country == "Brunei Darussalam" | country == "Canada" | country == "Chile" | country == "Croatia" | country == "Cyprus" | country == "Czechia" | country == "Denmark" | country == "Estonia" | country == "Finland" | country == "France" | country == "Germany" | country == "Greece" | country == "Hungary" | country == "Iceland" | country == "Ireland" | country == "Israel" | country == "Italy" | country == "Japan " | country == "Republic of Korea" | country == "Latvia" | country == "Lithuania" | country == "Luxembourg" | country == "Malta" | country == "Netherlands" | country == "New Zealand" | country == "Norway" | country == "Poland" | country == "Portugal" | country == "Saint Kitts and Nevis" | country == "Seychelles" | country == "Singapore" | country == "Slovakia" | country == "Slovenia" | country == "Spain" | country == "Sweden" | country == "Switzerland" | country == "China, Taiwan Province of" | country == "Trinidad and Tobago" | country == "United Kingdom" | country == "United States of America" | country == "Uruguay"

/*
Mean weight:
LICs: 45.5kg
MICs: 63.34kg
HICs: 84.47kg
*/ 

import delimited using "chicken weights.csv", clear 	
keep area item value
keep if item == "Meat, chicken"
drop item

ren area country
label var country "Country"
ren value weight 
label var weight "average chicken weight, kg"
replace weight = weight / 10000

drop if country == "China, Macao SAR" | country == "China, Hong Kong SAR" | country == "New Caledonia" | country == "French Polynesia"

gen income_status = ""

replace income_status = "M"

replace income_status = "L" if country == "Afghanistan" | country == "Burkina Faso" | country == "Burundi" | country == "Central African Republic" | country == "Chad" | country == "Democratic Republic of the Congo" | country == "Eritrea" | country == "Ethiopia PDR" | country == "Gambia" | country == "Guinea" | country == "Guinea-Bissau" | country == "Democratic People's Republic of Korea" | country == "Liberia" | country == "Madagascar" | country == "Malawi" | country == "Mali" | country == "Mozambique" | country == "Niger" | country == "Rwanda" | country == "Sierra Leone" | country == "Somalia" | country == "South Sudan" | country == "Sudan" | country == "Syria" | country == "Togo" | country == "Uganda" | country == "Yemen"

replace income_status = "H" if country == "Antigua and Barbuda" | country == "Australia" | country == "Austria" | country == "Bahamas" | country == "Barbados" | country == "Belgium" | country == "Brunei Darussalam" | country == "Bahrain" | country == "Canada" | country == "Chile" | country == "Croatia" | country == "Cyprus" | country == "Czechia" | country == "Denmark" | country == "Estonia" | country == "Finland" | country == "France" | country == "Germany" | country == "Greece" | country == "Hungary" | country == "Iceland" | country == "Ireland" | country == "Israel" | country == "Italy" | country == "Japan" | country == "Republic of Korea" | country == "Latvia" | country == "Lithuania" | country == "Luxembourg" | country == "Malta" | country == "Netherlands" | country == "New Zealand" | country == "Norway" | country == "Poland" | country == "Portugal" | country == "Puerto Rico" | country == "Qatar" | country == "Saint Kitts and Nevis" | country == "Seychelles" | country == "Singapore" | country == "Slovakia" | country == "Slovenia" | country == "Spain" | country == "Sweden" | country == "Switzerland" | country == "China, Taiwan Province of" | country == "Trinidad and Tobago" | country == "United Kingdom of Great Britain and Northern Ireland" | country == "United Arab Emirates" | country == "United States of America" | country == "Uruguay"

egen mean_weight_c = mean(weight), by(income_status)

/*
Chicken weight
LICs: 1.200792
MICs: 1.335566
HICs: 1.599206
*/

import delimited using "sepsis incidence.csv", clear

ren ïcountry country 
label var country "country"

ren sepsisasirper100000population sepsis_incidence 
ren sepsisasmrper100000population sepsis_mortality

ren mortalityrate fatality_rate

destring sepsis_incidence, replace ignore(",")
destring sepsis_mortality, replace ignore(",")

replace fatality_rate = sepsis_mortality / sepsis_incidence

replace income_status = "X" if country == "High-income"

egen mean_incidence = mean(sepsis_incidence), by(income_status)
egen mean_mortality = mean(sepsis_mortality), by(income_status)
egen mean_fatality_rate = mean(fatality_rate), by(income_status)

import delimited using "resistomap DRI.csv", clear

ren ïcountry country

label var country "Country" 

label var dri "Resistomap Drug-Resistance Index"

label var gdppcppp "GDP per Capita, PPP"

egen mean_dri = mean(dri), by(income_status)

/*
HIC: 35.44828
uMIC: 54.2
lMIC: 65
mean of lMIC and uMIC is 59.6
*/ 

twoway scatter dri gdppcppp

gen above_45 = 1 if gdppcppp >= 45000
replace above_45 = 0 if gdppcppp < 45000

reg dri gdppcppp
reg dri gdppcppp above_45
reg dri gdppcppp if above_45 == 0
reg dri gdppcppp if above_45 == 1 & country != "Ireland" //excluded due to outlier level of income per person
reg dri gdppcppp if country != "Ireland"

/*
generally, AMR seems to have a negative relationship with income/person up until 45,000 USD per person, after which point it seems to have no significant relationship 

extrapolating back to GDPPCPPP of 2524 (average of LICs) using the regression for under-45k, we would get
65.25824 + 2524 * (-.000579) = 63.796844

extrapolating to GDPPCPPP of 11816, we woud get
65.25824 + 11816 * (-.000579) = 58.416776

again, average of HICs is 35.44828
extrpolating to average of HICs (GDPPCPPP of 50,752) using the whole-sample regression without Ireland gives 
69.09229 + 50752*(-.0007564) = 30.703477 but we have seen that the relationship is not significant over 45k

*/

import delimited "hospital bed day.csv", clear

ren ïcountry country 
label var country "Country"

ren incomecapitappp GDPPCPPP 
ren hospitalbeddaycostnominalusd bed_day_cost

destring GDPPCPPP, replace ignore(",")

twoway scatter bed_day_cost GDPPCPPP

reg bed_day_cost GDPPCPPP

reg bed_day_cost GDPPCPPP, nocons

/*
for the average GDPPCPPP of LIC, MIC, HIC:
LIC: .0076175*2524 = 
MIC: .0076175*11816 = 
HIC: .0076175*50572 = 