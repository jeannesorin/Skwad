clear all
set more off
set seed 10

*cd "/Users/Manav/Documents/Empirical Analysis/Empirical Anlaysis 3/Problem Set 4"

*use "/Users/Manav/Documents/Empirical Analysis/Empirical Anlaysis 3/Problem Set 4/data/final5.dta"

global whoamI "/Users/georgevojta/Dropbox/Uchicago Notes/Year1/Magne/ProblemSets/ProblemSet4"

global problem "$whoamI"



global input "$problem/input"
global output "$problem/output"
global code "$problem/code"


use "$input/final5.dta", clear

cd "$output"

*label variables so we know what they are
label var avgmath "Mean math score"
label var classize "Class size"
label var tipuach "Percent disadvantaged"
label var c_size "Enrollment"

*****Part 1
**regressing class size on math scores using
reg avgmath classize, vce(cluster schlcode)
outreg2 using parta, tex replace nocons label ctitle(Math score) dec(3)

 
**control for percentage of disadvantaged kids and enrollement in class
reg avgmath classize tipuach c_size, vce(cluster schlcode)
outreg2 using parta, tex append nocons label ctitle(Math score) dec(3) 


*****Part 2
gen large_class = c_size <= 40
label var large_class "Large class"


**regress as if sharp RDD controlling for disadvantaged and enrollement
reg avgmath large_class tipuach c_size if c_size>20 & c_size<60, vce(cluster schlcode)
outreg2 using partb, tex replace nocons label ctitle(Math score) dec(3)


*****Part 3
*program that runs local regressions and extracts value to calculte SRDD estimates
capture program drop localreg_SRDD
program localreg_SRDD, rclass
	version 13
	args y x t1 t2 t3
	confirm var `y'
	confirm var `x'
	confirm number `t1'
	confirm number `t2'
	confirm number `t3'
	
	tempname lmean rmean
	
	*run two local regessions and save point estimates around discontinuity
	quietly lpoly `y' `x' if `x'>`t1' & `x'<=`t2', gen(lpolest_l) at(`x') nograph
	quietly lpoly `y' `x' if `x'>`t2' & `x'<`t3', gen(lpolest_r) at(`x') nograph
	
	quietly summarize lpolest_l if `x'==`t2', meanonly
	scalar `lmean' = r(mean)
	
	quietly summarize lpolest_r if `x'==`t2'+1, meanonly
	scalar `rmean' = r(mean)
	
	quietly drop lpolest_r lpolest_l
	
	return scalar SRDD =  `lmean' - `rmean'
end

*Point Estimate
localreg_SRDD avgmath c_size 20 40 60
di r(SRDD)

*Standard Error
/*
**dropping unecessary observations helps speed up the bootstrap
keep if c_size>20 & c_size<60
bootstrap SRDD=r(SRDD), reps(1000) nowarn: localreg_SRDD avgmath c_size 20 40 60
*/


********Part 4
ivregress 2sls avgmath tipuach c_size (classize = large_class) if c_size>20 & c_size<60, vce(cluster schlcode)
outreg2 using partd, tex replace nocons label ctitle(Math score) dec(3)


********Part 5
rdrobust avgmath c_size if c_size>20 & c_size<60, c(40) deriv(0) scalepar(-1) 
rdrobust avgmath c_size if c_size>20 & c_size<60, c(40) fuzzy(large_class)


*******Part 6
gen fsc = c_size/(int((c_size-1)/40)+1)
label var fsc "Predicted class size"
*save current dataset to call later
tempfile data
save `data', replace
*Create summary data for plotting
collapse classize fsc, by(c_size)
label var classize "Average class size"
label var fsc "Predicted class size"
scatter classize c_size || line fsc c_size, name(scatter1) graphregion(color(white)) bgcolor(white) ///
ytitle("(Real or Predicted) Class Size") title("Plotting Predicted Class Size") ///
 ///
note("")
translate @Graph "$output/class_size_pred.png", replace name("scatter1")



*******Part 7
use `data', clear
ivregress 2sls avgmath (classize = fsc), vce(cluster schlcode)
outreg2 using part7, tex replace nocons label ctitle(Math score) dec(3)


*******Part 8
use `data', clear
ivregress 2sls avgmath tipuach c_size (classize = fsc), vce(cluster schlcode)
outreg2 using part8, tex replace nocons label ctitle(Math score) dec(3)


*******Part 9
twoway histogram c_size, width(1) || ///
kdensity c_size, xline(40 80 120 160 200, lstyle(foreground)) name(hist1) graphregion(color(white)) bgcolor(white) ///
ytitle("Density") title("Analyzing Density of Running Variable") ///
 ///
note("")
translate @Graph "$output/density_part9.png", replace name("hist1")



*******Part 10
*generate indicators for the bins
gen bin_id = 0
forvalues k= 1(1)12{
	replace bin_id = `k' if 20*(`k'-1)<c_size & 20*(`k')>=c_size
}
*create summary variables
collapse classize avgmath c_size, by(bin_id)
label var classize "Average class size"
label var avgmath "Average math score"
label var c_size "Enrollment"

twoway scatter classize c_size || scatter avgmath c_size, xline(40 80 120 160 200 240, lstyle(foreground)) name(scatter2) ///
graphregion(color(white)) bgcolor(white) ///
ytitle("Scores (Red), and Class Size (Blue)") ///
 ///
note("")
translate @Graph "$output/bins_part10.png", replace name("scatter2")



*******Part 11
twoway scatter classize c_size || lfit classize c_size || fpfit classize c_size, xline(40 80 120 160 200 240, lstyle(foreground)) name(scatter3)
twoway scatter avgmath c_size || lfit avgmath c_size || fpfit avgmath c_size, xline(40 80 120 160 200 240, lstyle(foreground)) name(scatter4)


*******Part 12
use `data', clear
*Part (i) checking sensitivity to bandwith
*bandwith 3, 5, 7, 9
forvalues j = 3(2)9{
	gen bw`j'_id = 0
	forvalues k = 1(1)6{
		replace bw`j'_id = 1 if 20*(`k')-`j'<=c_size & 20*(`k')+`j'>=c_size
	}
}
	
ivregress 2sls avgmath (classize = fsc) if bw3_id == 1, vce(cluster schlcode)
outreg2 using part12, tex replace nocons label ctitle(Math Score (bw=3)) dec(3)

forvalues j = 5(2)9{
	ivregress 2sls avgmath (classize = fsc) if bw`j'_id == 1, vce(cluster schlcode)
	outreg2 using part12, tex append nocons label ctitle(Math Score (bw=`j')) dec(3)
}

*Part (ii) checking sensitivity to control non-linearities
gen c_size2 = c_size^2
label var c_size2 "Enrollment Squared"
ivregress 2sls avgmath (classize = fsc), vce(cluster schlcode)
outreg2 using part12b, tex replace nocons label ctitle(Math score) dec(3)

ivregress 2sls avgmath tipuach c_size (classize = fsc), vce(cluster schlcode)
outreg2 using part12b, tex append nocons label ctitle(Math score) dec(3)

ivregress 2sls avgmath tipuach c_size c_size2 (classize = fsc), vce(cluster schlcode)
outreg2 using part12b, tex append nocons label ctitle(Math score) dec(3)


*******Part 13
ivregress 2sls tipuach (classize = fsc), vce(cluster schlcode)
outreg2 using part13, tex replace nocons label ctitle(Percent of disadvantaged pupils) dec(3)
