clear all
set more off
set seed 3

*working directory
global wdir "Users/Manav/Documents/Empirical Analysis/Empirical Anlaysis 3"

***	Set N = 10,000 and define parameters***
set obs 10000
local beta1 = 2
local beta2 = 3
local sigma = 2

*** Generate X = (x1, x1(~N[0,1])
g x1 = 1
g x2 = rnormal(0,1)

***	Draw U~N(0,4)
g u = rnormal(0,`sigma')


*** Compute y's
g y = `beta1'*x1 + `beta2'*x2 + u
reg y x1 x2, nocons

*save draws in tempfile
tempfile data_1
save `data_1', replace

***************Part b**********************
/*
*create temporary file where we will store all the values
clear
tempfile betas
set obs 0
g beta1 = .
g beta2 = .
save `betas'

forvalues i = 1(1)10000{

	use `data_1', clear
	bsample 10000
	quietly reg y x1 x2, nocons


	*define paramerters and save
	quietly g beta1 = _b[x1]
	quietly g beta2 = _b[x2]
	quietly collapse beta1 beta2
	quietly append using `betas'
	quietly save `betas', replace
}

save "/Users/Manav/Documents/Empirical Analysis/Empirical Anlaysis 3/betas_sim ps1 q3.dta", replace
*/

***Plot Histogram***
clear all
use "/Users/Manav/Documents/Empirical Analysis/Empirical Anlaysis 3/betas_sim ps1 q3.dta"

histogram beta1, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("Constant") title("Histogram of Constant") ///
note("Based on 10,000 simulations, 10,000 obs per simulation") color(navy) width(.001) ///
name(beta1_hist)

histogram beta2, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("beta 2") title("Histogram of beta 2") ///
note("Based on 10,000 simulations, 10,000 obs per simulation") color(navy) width(.0001) ///
name(beta2_hist)

*calculate bootstrap standard error
egen beta1_se = sd(beta1)
egen beta2_se = sd(beta2)


***************************************************
****************Non-parametric Bootstrap***********
***************************************************
clear all

set obs 10000

g u1 = rnormal(0,1)
g u2 = rnormal(0,1)

g y1 = 5 + u1
g y0 = 2 + u2

g d = rnormal(0,1) >0

g y = d*y1 + (1-d)*y0

reg y d

*save draws in tempfile
tempfile data_2
save `data_2', replace

****bootstrap a sample of 10,000 with replacment

/*clear
tempfile betas2
set obs 0
g beta1 = .
g beta2 = .
save `betas2'

forvalues i = 1(1)10000{

	use `data_2', clear
	bsample 10000
	quietly reg y d

	*define paramerters and save
	quietly g beta1 = _b[_cons]
	quietly g beta2 = _b[d]
	quietly collapse beta1 beta2
	quietly append using `betas2'
	quietly save `betas2', replace
}

save "/Users/Manav/Documents/Empirical Analysis/Empirical Anlaysis 3/betas_sim ps1 q4.dta", replace
*/
***Plot Histogram***
clear all
use "/Users/Manav/Documents/Empirical Analysis/Empirical Anlaysis 3/betas_sim ps1 q4.dta"

histogram beta1, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("Constant") title("Histogram of Constant") ///
note("Based on 10,000 simulations, 10,000 obs per simulation") color(navy) width(.001) ///
name(beta1_hist)

histogram beta2, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("beta 2") title("Histogram of beta 2") ///
note("Based on 10,000 simulations, 10,000 obs per simulation") color(navy) width(.0001) ///
name(beta2_hist)

*calculate bootstrap standard error
egen beta1_se = sd(beta1)
egen beta2_se = sd(beta2)


