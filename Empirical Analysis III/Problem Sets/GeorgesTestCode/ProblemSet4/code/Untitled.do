***Question 3 STATA Code***
clear all
set more off


***Change to your path***

***George***
global whoamI "/Users/georgevojta/Dropbox/Uchicago Notes/Year1/Magne/ProblemSets/ProblemSet4"

global problem "$whoamI"



global input "$problem/input"
global output "$problem/output"
global code "$problem/code"
global processing "$problem/processing"

use "$input/final5.dta", clear

*label variables so we know what they are
label var avgmath "Mean math score"
label var classize "Class size"
label var tipuach "Percent disadvantaged"
label var c_size "Enrollment"


***Part A***
eststo part_a1: reg avgmath classize
estadd local Question "Part A.1"

eststo part_a2: reg classize tipuach c_size
estadd local Question "Part A.2"


***Part B***

*class size between 20 and 60*
*Limit enrollment to 20-60 students(
keep if c_size > 19 & c_size < 61
count 
local datasize = `r(N)'

*dummy for large*
g largeclass = 0 if c_size > 40
replace largeclass = 1 if largeclass == .

*regression is now*
eststo part_b: reg avgmath classize tipuach c_size largeclass

esttab part_a1 part_a2 part_b using "$output/ols_reggressions.tex", label r2 star(* 0.10 ** 0.05 *** 0.01) replace se
estadd local Question "Part B"

***Part C***
/*
*** Sample is a STATA code that draws obs with replacement, note that bsample is with replacement ***
*Again, doing this once is plenty
forval i = 1(1)1000{
	preserve
	di `i'
	
	***Sample of 1,500***
	bsample `datasize'
	
	quietly lpoly avgmath c_size if c_size <= 40, gen(lpolest_l) at(c_size) nograph
	quietly lpoly avgmath c_size if c_size > 40, gen(lpolest_r) at(c_size) nograph
	
	quietly summarize lpolest_l if c_size == 40
	local lmean = r(mean)
	
	quietly summarize lpolest_r if c_size == 41
	local rmean = r(mean)
	
	quietly drop lpolest_r lpolest_l
	
	local SRDD =  `lmean' - `rmean'
	
	clear all
	quietly set obs 1 
	g SRDD = `SRDD'
	
	if `i' == 1{
			quietly save "$processing/bootstrap.dta", replace
			restore
			continue
		}
	capture quietly append using "$processing/bootstrap.dta"
	quietly save "$processing/bootstrap.dta", replace
	restore
	}
*/

*Part 4*
use "$input/final5.dta", clear

*label variables so we know what they are
label var avgmath "Mean math score"
label var classize "Class size"
label var tipuach "Percent disadvantaged"
label var c_size "Enrollment"

g largeclass = 0 if c_size > 40
replace largeclass = 1 if largeclass == .


preserve
keep if c_size > 20 & c_size < 60
ivregress 2sls avgmath tipuach c_size (classize = largeclass), vce(cluster schlcode)

*Part 5*
rdrobust avgmath c_size, c(40) deriv(0) scalepar(-1) 
rdrobust avgmath c_size, c(40) fuzzy(largeclass)

*Part 6*
restore
g predicted_class_size = c_size/(int((c_size-1)/40) + 1)
sort c_size

***collapse to get averages to plot***
preserve 
collapse (mean) classize predicted_class, by(c_size)
twoway (scatter classize c_size) (line predicted_class c_size)

***Part 7***
restore
ivregress 2sls avgmath (classize = predicted_class), vce(cluster schlcode)


***Part 8***
ivregress 2sls avgmath tipuach c_size (classize = predicted_class), vce(cluster schlcode)

***Part 9***
twoway (histogram c_size, width(1)) (kdensity c_size)

***Part 10***
g bin = .
forval i = 0(20)240{
		replace bin = `i'+10 if c_size > `i' & c_size <= `i'+20
	}
	
preserve
collapse (mean) classize avgmath, by(bin)
twoway (scatter classize bin) (scatter avgmath bin), xline(20 40 60 80 100 120 140 160 180 200 220)

***Part 11***
sort bin
twoway (scatter classize bin) (line classize bin) (scatter avgmath bin) (line avgmath bin), xline(20 40 60 80 100 120 140 160 180 200 220)
***Part 12***

***Part 13***
