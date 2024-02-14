cd "****"

*insheet using "data.csv", comma clear
insheet using "sez_edu.csv", comma clear

gen sez = 0
replace sez = 1 if ent > 0

gen pop1 = pop/100000 
gen lpop = ln(pop)


xtset id year

* local variables 
gen crimepc = crime/lpop
gen lcrimepc = ln(crimepc)
gen sqent = sqrt(ent)
gen sqinv = sqrt(inv)
gen inv1 = inv/10000000
gen tec_salary1 = tec_salary/1000

* lagged variables
gen dcrimepc = crimepc - l1.crimepc
gen dinv = inv - l1.inv
gen dent = ent - l1.ent
gen dlcrimepc = lcrimepc - l1.lcrimepc
gen dtec_salary = tec_salary - l1.tec_salary

* eq1 ===================================================
eststo mod1: quietly reg crimepc sez 
quietly estadd local FE_province  "No", replace
quietly estadd local FE_year      "No", replace

eststo mod2: quietly reg crimepc sez year
quietly estadd local FE_province  "No", replace
quietly estadd local FE_year      "Yes", replace

eststo mod3: quietly xtreg crimepc sez  year, fe
quietly estadd local FE_province  "Yes", replace
quietly estadd local FE_year      "Yes", replace

esttab mod1 mod2 mod3, title(Crime - sez)	

* eq2 ==========================================================
eststo mod4: quietly reg crimepc ent inv1 tec_salary1
quietly estadd local FE_province  "No", replace
quietly estadd local FE_year      "No", replace

eststo mod5: quietly reg crimepc ent inv1 tec_salary1 year 
quietly estadd local FE_province  "No", replace
quietly estadd local FE_year      "Yes", replace

eststo mod6: quietly xtreg crimepc ent inv1 tec_salary1 year, fe
quietly estadd local FE_province  "Yes", replace
quietly estadd local FE_year      "Yes", replace

esttab mod4 mod5 mod6, title(Crime - ent)	
