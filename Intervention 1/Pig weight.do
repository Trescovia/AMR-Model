cd "C:\Users\tresc\Desktop\AMR-Model\Intervention 1"

import excel "pig weight.xlsx", firstrow sheet(original)

drop All EcoliCFU EcoliLogCFU Sample PigsampleID id

gen weight_mo4 = .
replace weight_mo4 = Weightkg if Month ==  "M4"
gen weight_mo3 = .
replace weight_mo3 = Weightkg if Month ==  "M3"
gen weight_mo2 = .
replace weight_mo2 = Weightkg if Month ==  "M2"
gen weight_mo1 = .
replace weight_mo1 = Weightkg if Month ==  "M1"
gen weight_mo0 = .
replace weight_mo0 = Weightkg if Month ==  "M0"

drop if Group == "Control"

gen Probiotic = 0
replace Probiotic = 1 if Group == "Probiotic"

replace Sex = "1" if Sex == "Male"
replace Sex = "0" if Sex == "Female"
destring Sex, replace float

collapse (mean) weight_mo4 weight_mo0 Sex Pigpen Probiotic, by (PigID)

replace Pigpen = Pigpen*10

reg weight_mo4 weight_mo0 Sex Probiotic
reg weight_mo4 weight_mo0 Sex Probiotic, robust
reg weight_mo4 weight_mo0 Sex Probiotic, vce(cluster Pigpen)
reg weight_mo4 weight_mo0 Sex Probiotic i.Pigpen, vce(cluster Pigpen)
//compared with pigs taking antibiotic, those taking probiotics were 462 to 538 grammes lighter. Howver, the difference was far from statistically significant