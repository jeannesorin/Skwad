clear all
set more off

use "/Users/Manav/Documents/Empirical Analysis/Empirical Anlaysis 3/Problem Set 2/lalonde2.dta"

*Summary for treatment data
tabstat age educ black married nodegree re74, by(treated)

*TOT(=ATE in this case)
reg re78 treated

**Use treated from NSW and controls from CPS sample**
*define pseudo treatment
gen psudo_treated = 1 if treated==1
replace psudo_treated = 0 if sample == 2

*run naive observational regression
reg re78 psudo_treated

*check covariate balancing with psudo_treated
tabstat age educ black married nodegree re74, by(psudo_treated)


*Support comparison graphs graphs
foreach var of varlist age educ black married nodegree re74 {
	twoway histogram `var', by(psudo_treated) name(`var')
}


*matching algorithm (with one-to-one matching, or nearest neighbour with K=1)
psmatch2 psudo_treated age educ black married nodegree re74, out(re78)

*Porpensity scores and local linear matching
psmatch2 psudo_treated age educ black married nodegree re74, llr out(re78)
