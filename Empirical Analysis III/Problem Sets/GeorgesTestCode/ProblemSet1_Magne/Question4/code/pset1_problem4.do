***Question 4 STATA Code***
clear all
set more off


***Change to your path***

***George***
global whoamI "/Users/georgevojta/Dropbox/SkwadGit"

***Manav***
*global whoamI "Users/georgevojta/Dropbox/SwkadGit"
***Sid***
*global whoamI "Users/georgevojta/Dropbox/SwkadGit"

global git "$whoamI/Skwad/Empirical Analysis III/Problem Sets/GeorgesTestCode"
global problem "$git/ProblemSet1_Magne/Question4"

global input "$problem/input"
global output "$problem/output"
global code "$problem/code"
global processing "$problem/processing"

/*
Empirical Analysis III - Part A - Problem Set 1 - Question 4

Current Status: Code Generation

Part A:															*/



***	Set N = 10,000 and create constant***
set obs 10000

*** Generate X = (cons, x_2(~N[0,100])
g cons = 1
g x_2 = rnormal(0,10)

***	Draw U~N(0,4)
g u=rnormal(0,2)

/*
Summarizing U to make sure the code is right, Step is here because stata
code I had read previously had called the second term the variance where its 
clearly the s.d.      */

sum u


/* Here we are just going to make our "true" beta a local variable to generate
the y variable */

local beta1 = 2
local beta2 = 3


/*
Now we generate		Y = \Beta'X + U

And then follow it up with a sanity check	
(Note I'm running a nocons regression because 
we are manually inputting constant
												*/ 
												
g y = `beta1'*cons + `beta2'*x_2 + u

												
*Step 1: Let's grab the se(mean) now so we can force it in the table*
tabstat y, stats(semean) save
return list

mat test = r(StatTotal)
local y_semean = round(test[1,1], .0001)		 
												
*Step 2: Run regression, export table*
eststo problem4a_regression: reg y cons x_2, nocons
estadd local y_se = `y_semean'
esttab problem4a_regression using "$output/Problem4_PartA.tex", keep(cons x_2) scalar(y_se) r2 star(* 0.10 ** 0.05 *** 0.01) replace se

/*   
This is going to be what we need to create the dataset
DO NOT UNCOMMENT UNLESS NEC BECAUSE YOU MIGHT SAVE OVER


reg y cons x_2, nocons
g beta_cons = _b[cons]
g beta_x_2 = _b[x_2]

collapse beta_cons beta_x_2
*save "$processing/montecarlo.dta", replace
*/

********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
/*
Part B
OK so now this wants us to loop through this process 10,000 times (yikes) and
save the \beta coefficients

*/
clear all

/*
Redo Part A: but 10,000 times
Note this takes xxx minutes to run so only run if absolutely nec
			
forval i = 1(1)10000{
	***	Generate the DAta
	clear all
	quietly set obs 10000
	g cons = 1
	g x_2 = rnormal(0,10)
	g u=rnormal(0,2)
	
	local beta1 = 2
	local beta2 = 3
													
	g y = `beta1'*cons + `beta2'*x_2 + u

	*This is going to be what we need to create the dataset
	quietly reg y cons x_2, nocons
	g beta_cons = _b[cons]
	g beta_x_2 = _b[x_2]

	quietly collapse beta_cons beta_x_2
	quietly append using "$processing/montecarlo.dta"
	quietly save "$processing/montecarlo.dta", replace
	
	*Added this counter because I wanted to make sure wasn't stuck in a loop*
	*Fe
	di `i'
}

save "$output/montecarlo.dta", replace
*/

clear all
use "$output/montecarlo.dta"

***Plot Histogram***
histogram beta_cons, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("β_0") title("Histogram of β_0") ///
note("Based on 10,000 simulations, 10,000 obs per simulation") color(navy) width(.001)
translate @Graph "$output/Problem4_partB_beta_0.png", replace name("Graph")

histogram beta_x_2, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("β_1") title("Histogram of β_1") ///
note("Based on 10,000 simulations, 10,000 obs per simulation") color(navy) width(.0001)
translate @Graph "$output/Problem4_partB_beta_1.png", replace name("Graph")



********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
/*
Part C
Let's do some non-parametric bootstrapping

*/
clear all
set obs 10000

********************************
***Generate Data****************
********************************
g u_1 = rnormal(0,1)
g u_2 = rnormal(0,1)

g y_1 = 5+u_1
g y_0 = 2+u_2
********************************
********************************
********************************

g d_random_parameter = runiform(0,1)
g d = 1 if d_random > .5
replace d = 0 if d_random <= .5

********************************
********************************
********************************
gen y = y_0 + d*(y_1-y_0)

*Step 1: Let's grab the se(mean) now so we can force it in the table*
tabstat y, stats(semean) save
return list

mat test = r(StatTotal)
local y_semean = round(test[1,1], .0001)		 
												
*Step 2: Run regression, export table*
eststo problem4b_regression: reg y d
estadd local y_se = `y_semean'
esttab problem4b_regression using "$output/Problem4_PartB.tex", keep(d _cons) scalar(y_se) r2 star(* 0.10 ** 0.05 *** 0.01) replace se

save "$processing/simdata_bootstrap.dta", replace

/*
Part D
*/

*** Sample is a STATA code that draws obs with replacement, note that bsample is with replacement ***
/* Again, doing this once is plenty
forval i = 1(1)10000{
	preserve
	
	***Sample of 10,000***
	bsample 10000
	
	keep y d
	quietly reg y d
	
	***Save regression coefficients***
	g beta_d = _b[d]
	g beta_cons = _b[_cons]

	quietly collapse beta_cons beta_d
	capture quietly append using "$processing/bootstrap.dta"
	quietly save "$processing/bootstrap.dta", replace
	di `i'
	restore
	}
*/

use "$processing/bootstrap.dta", clear
quietly save "$processing/bootstrap.dta", replace
quietly save "$output/bootstrap.dta", replace


***Quickly pump out some sum stats for the bootstrap***
preserve
collapse (mean) mean_beta_d = beta_d (semean) semean_beta_d = beta_d (sd) sd_beta_d = beta_d (min) min = beta_d (max) max = beta_d
g variance = sd*sd

label var mean "Mean"
label var semean "Std. Err"
label var sd "Std. Dev"
label var var "Variance"
label var min "Min"
label var max "Max"


texsave mean_beta_d semean_beta_d sd_beta_d variance min max ///
using "$output/SummaryStats_3_2_a.tex", replace title("Summary Stats") varlabels ///
footnote("Numbers are Means") ///
width(20cm) location("H") geometry(margin = .3in) frag
restore



histogram beta_cons, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("Y_0") title("Histogram of Y_0") ///
note("Based on 10,000 Random Samples w/ Replacement (N= 10,000)") color(navy) width(.001)
translate @Graph "$output/Problem4_partB_2_y_0.png", replace name("Graph")

histogram beta_d, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("β") title("Histogram of β") ///
note("Based on 10,000 Random Samples w/ Replacement (N= 10,000)") color(navy) width(.001)
translate @Graph "$output/Problem4_partB_2_beta.png", replace name("Graph")

/* Don't want to run this again
use "$processing/simdata_bootstrap.dta", clear
*** Sample is a STATA code that draws obs WITHOUT replacement, note sample is without replacement ***
forval i = 1(1)10000{
	preserve
	
	***Sample of 10,000, which is 100%...makes no sense***
	sample 100
	
	keep y d
	quietly reg y d
	
	***Save regression coefficients***
	g beta_d = _b[d]
	g beta_cons = _b[_cons]

	quietly collapse beta_cons beta_d
	capture quietly append using "$processing/bootstrap_no_rep.dta"
	quietly save "$processing/bootstrap_no_rep.dta", replace
	di `i'
	restore
	}
	
use "$processing/bootstrap_no_rep.dta", clear
quietly save "$processing/bootstrap_no_rep.dta", replace
quietly save "$output/bootstrap_no_rep.dta", replace


histogram beta_cons, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("Y_0") title("Histogram of Y_0") ///
note("Based on 10,000 Random Samples w/out Replacement (N= 10,000)") color(navy) width(.001)
translate @Graph "$output/not_rep_y_0.png", replace name("Graph")

histogram beta_d, fraction graphregion(color(white)) bgcolor(white) ///
xtitle("β") title("Histogram of β") ///
note("Based on 10,000 Random Samples w/out Replacement (N= 10,000)") color(navy) width(.001)
translate @Graph "$output/no_rep_beta.png", replace name("Graph")
*/



