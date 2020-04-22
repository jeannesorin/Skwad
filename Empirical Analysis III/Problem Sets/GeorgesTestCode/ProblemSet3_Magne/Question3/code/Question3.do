***Question 3 STATA Code***
clear all
set more off


***Change to your path***

***George***
global whoamI "/Users/georgevojta/Dropbox/SkwadGit"

global problem "$whoamI/Skwad/Empirical Analysis III/Problem Sets/GeorgesTestCode/ProblemSet3_Magne/Question3/"

***Manav***
*global whoamI "Users/georgevojta/Dropbox/SwkadGit"
***Sid***
*global whoamI "Users/georgevojta/Dropbox/SwkadGit"

*global git "$whoamI/Skwad/Empirical Analysis III/Problem Sets/GeorgesTestCode"
*global problem "$git/ProblemSet1_Magne/Question4"

global input "$problem/input"
global output "$problem/output"
global code "$problem/code"
global processing "$problem/processing"


use "$input/lottery.dta"
label var z "Won Lottery"
label var d "Attended Med"
label var lnw "Wages (Logged)"
/*
Empirical Analysis III - Part A - Problem Set 3 - Question 3

Current Status: Done

Part B:															*/

reg d z

eststo problem3b_regression: reg d z

esttab problem3b_regression using "$output/relevance.tex", label r2 star(* 0.10 ** 0.05 *** 0.01) replace se

/*
Empirical Analysis III - Part A - Problem Set 3 - Question 3

Current Status: Done

Part C:															*/
reg lnw d i.year
ivreg2 lnw (d=z)
outreg2 using "$output/iv_regression_basic.tex", replace label tex(frag pretty)

/*
Empirical Analysis III - Part A - Problem Set 3 - Question 3

Current Status: Done

Part D:															*/
preserve

g Treated_WonLottery = 1 if (z == 1 & d == 1) 
replace Treated_WonLottery  = 0 if Treated_WonLottery == .
g Treated_LostLottery  = 1 if z == 0 & d == 1
replace Treated_LostLottery = 0 if Treated_LostLottery == .
g Untreated_WonLottery = 1 if z == 1 & d == 0
replace Untreated_WonLottery = 0 if Untreated_WonLottery == .
g Untreated_LostLottery = 1 if z == 0 & d == 0
replace Untreated_LostLottery = 0 if Untreated_LostLottery == .


g Total = 1

g Gender = "Female" if female == 1
replace Gender = "Male" if female == 0

collapse (sum) Treated_WonLottery Treated_LostLottery Untreated_WonLottery Untreated_LostLottery Total, by(Gender)
set obs 3
replace Gender = "Total" if Gender == ""


foreach var in Total Treated_WonLottery Treated_LostLottery Untreated_WonLottery Untreated_LostLottery {
	g `var'_ratio = `var' / Total
	replace `var' = `var'[_n-1] + `var'[_n-2] if `var' == .
	}

sort Treated_WonLottery Treated_LostLottery Untreated_WonLottery Untreated_LostLottery Total


texsave Gender Treated_WonLottery Treated_LostLottery Untreated_WonLottery Untreated_LostLottery Total  ///
using "$output/SummaryStats_gender.tex", replace title("Summary Stats By Gender") ///
footnote("Counts") ///
width(18.5cm) location("H") frag
restore

***Confirm this method with ivreg2***

eststo female: reg d z if female == 1
estadd local gender "Female"
eststo male: reg d z if female == 0
estadd local gender "Male"
eststo population: reg d z
estadd local gender "Full Population" 
esttab female male population using "$output/gender_firststage.tex", scalar(gender) r2 star(* 0.10 ** 0.05 *** 0.01) replace se


/*
Empirical Analysis III - Part A - Problem Set 3 - Question 3

Current Status: Done

Part F: Using the weights I calculated from earlier     */

***Get Densities To Calculate Complier Densities***
***At command makes sure we save over same X values (ln wage in this case)***

kdensity lnw if z == 0 & d == 0, gen(z_0_d_0_x z_0_d_0_d)
kdensity lnw if z == 0 & d == 1, gen(z_0_d_1_x z_0_d_1_d) at(z_0_d_0_x)
kdensity lnw if z == 1 & d == 1, gen(z_1_d_1_x z_1_d_1_d) at(z_0_d_0_x)
kdensity lnw if z == 1 & d == 0, gen(z_1_d_0_x z_1_d_0_d)  at(z_0_d_0_x)

twoway (line z_0_d_0_d z_0_d_0_x) (line z_0_d_1_d z_0_d_1_x) ///
(line z_1_d_1_d z_1_d_1_x) (line z_1_d_0_d z_1_d_0_x), ///
graphregion(color(white)) bgcolor(white) ///
xtitle("Log Wages") title("Plotted Densities") ///
legend(label(1 "Z=0, D=0") label(2 "Z=0, D=1") label(3 "Z=1, D=1") label(4 "Z=1, D=0")) ///
note("")
translate @Graph "$output/four_main_densities.png", replace name("Graph")


preserve 

***keep just data set of densities and wages***
keep z_0_d_0_d z_0_d_0_x z_0_d_1_d z_0_d_1_x z_1_d_1_d z_1_d_1_x z_1_d_0_d z_1_d_0_x
sort z_0_d_0_x z_0_d_1_x z_1_d_1_x z_1_d_0_x

g density_compliers_notreat = (((.52+.07)/.52)*z_0_d_0_d) - ((.07/.52)*z_0_d_1_d)
g density_compliers_treat = (((.52+.41)/.52)*z_1_d_1_d) - ((.41/.52)*z_1_d_0_d)

twoway (line density_compliers_notreat z_0_d_0_x ) (line density_compliers_treat z_0_d_1_x), ///
graphregion(color(white)) bgcolor(white) ///
xtitle("Log Wages") title("Plotted Densities") ///
legend(label(1 "Untreated Compliers (Y0)") label(2 "Treated Compliers (Y1)")) ///
note("")
translate @Graph "$output/compliers_densities.png", replace name("Graph")

save "$processing/densities.dta", replace

restore

***We can now use slide 30 from topic 3 in class to back out the 
g y1 = lnw*d
ivreg2 y1 (d=z), robust noheader
outreg2 using "$output/y1.tex", label tex(frag pretty) replace

g y0 = lnw*(1-d)
g md = (1-d)
ivreg2 y0 (md = z), robust noheader
outreg2 using "$output/y0.tex", label tex(frag pretty) replace


/*
Empirical Analysis III - Part A - Problem Set 3 - Question 3

Current Status: Done

Part G: Basic Summary Statistic Code    */

preserve 
g Type = "Never Taker" if z == 1 & d == 0
replace Type = "Always Taker" if z == 0 & d == 1
drop if Type == ""
collapse (mean) mean = lnw (min) min = lnw (max) max=lnw (count) count = lnw, by(Type)

texsave Type mean min max count  ///
using "$output/SummaryStats_takers.tex", replace title("Summary Stats By Takers") ///
footnote("") ///
width(15cm) location("H") frag
restore

twoway (line z_0_d_1_d z_0_d_1_x) ///
(line z_1_d_0_d z_1_d_0_x), ///
graphregion(color(white)) bgcolor(white) ///
xtitle("Log Wages") title("Plotted Densities") ///
legend(label(1 "Always Takers") label(2 "Never Takers")) ///
note("")
translate @Graph "$output/consistent_takers.png", replace name("Graph")


/*
Empirical Analysis III - Part A - Problem Set 3 - Question 3

Current Status: Done

Part H: Basic Summary Statistic Code    */

**Interaction i.year##i.lotcateg
save "$processing/workingfile.dta",replace

tostring year, gen(Year)
tostring lotcateg, gen(lottery_category)
g year_and_lot = Year + lottery_category
encode year_and_lot, gen(numeric_year_and_lot)
tab year_and_lot, gen(fe_)

save "$processing/workingfile.dta",replace


levelsof year, clean local(yearlist)
levelsof lotcateg, clean local(lotterylist)

foreach year in `yearlist'{
	foreach lot in `lotterylist'{
		preserve
		
		***Get count for observation weighting***
		count if year == `year' & lotcateg == `lot'
		local observations = `r(N)'
		
		***Run and save first stage***
		reg d z if year == `year' & lotcateg == `lot'
		local pi = _b[z]
		
		***Get variance of Z for Angrist Imbens Weighting***
		sum z if year == `year' & lotcateg == `lot'
		local variance = `r(sd)'^2
		
		***Run LATE just at a year lottery category level**
		ivreg2 lnw (d=z) if year == `year' & lotcateg == `lot'
		local late = _b[d]
		
		***Save into new data set***
		clear all
		set obs 1
		g year = `year'
		tostring year, gen(Year)
		g lotcateg = `lot'
		g variance_z = `variance'
		g firststage_pi = `pi'
		g LATE = `late'
		g observations = `observations'
		
		if `year' == 1988 & `lot' == 3{
			save "$output/late_estimates.dta", replace
			restore
			continue
			}
			
		append using "$output/late_estimates.dta"
		save "$output/late_estimates.dta", replace
		restore
		
		
	}
}
count
local observations = `r(N)'

***Run IVREG as per question requests***
ivreg2 lnw i.numeric_year_and_lot (d = numeric_year_and_lot#z), first
local late = _b[d]

clear all
set obs 1
g Year = "2SLS" 
g LATE = `late'
g observations = `observations'

append using "late_estimates.dta"
save "$output/late_estimates.dta", replace
sort Year lotcateg
order Year lotcateg 


***Now Population Weight It***
drop if Year == "2SLS"
collapse (mean) LATE [aweight = observations]
g Year = "Pop Weighted" 
append using "$output/late_estimates.dta"
save "$output/late_estimates.dta", replace
sort Year lotcateg
order Year lotcateg 

***Finally let's do Angrist Imbens Weights***
use "$processing/workingfile.dta", clear

***Make locals to loop through**
levelsof year, clean local(yearlist)
levelsof lotcateg, clean local(lotterylist)

***For some reason STATA is dropping this year / category when I interact so I'm forcing it in
g force_dummy = 1 if year == 1988 & lotcateg == 3
replace force_dummy = 0 if force == .

***Get Pi_x
foreach year in `yearlist'{
	foreach lot in `lotterylist'{
		
		
		preserve
		reg d force_dummy i.year#i.lotcateg z#i.year#i.lotcateg, nocons
		local pi_x = _b[1.z#`year'.year#`lot'.lotcateg]
		
		clear all
		set obs 1
		g year = `year'
		g lotcateg = `lot'
		g pi_x = `pi_x'
		
		if `year' == 1988 & `lot' == 3{
			save "$processing/angristgarbage.dta", replace
			restore
			continue
			}
			
		append using "$processing/angristgarbage.dta"
		save "$processing/angristgarbage.dta", replace
		restore
		
			
		}
	}

***Get Variance 
foreach year in `yearlist'{
	foreach lot in `lotterylist'{
		
		preserve
		sum z if year == `year' & lotcateg == `lot'
		local variance = `r(sd)'^2
		
		clear all
		set obs 1
		g year = `year'
		g lotcateg = `lot'
		g variance_z = `variance'
		
		merge 1:1 year lotcateg using "$processing/angristgarbage.dta"
		drop _merge
		save "$processing/angristgarbage.dta", replace
		restore
		}
}

use "$processing/angristgarbage.dta", clear
g weight_num = variance * pi^2

***Merge In and Calculate Weights / LATE***
merge 1:m year lotcateg using "$output/late_estimates.dta"
drop _merge

save "$output/late_estimates.dta", replace
preserve

collapse (mean) denominator = weight_num [aweight = observations]
sum denom
local denom = `r(mean)'
restore

g weight_denom = `denom'

g weight_final = weight_num / weight_denom
save "$output/late_estimates.dta", replace
drop if weight_final == .

g LATE2 = LATE * weight_final

collapse (mean) LATE = LATE2 [aweight = observations]
g Year = "AI Weights" 

append using "$output/late_estimates.dta"
save "$output/late_estimates.dta", replace

use "$output/late_estimates.dta", replace
sort Year lotcateg
order Year lotcateg LATE pi_x variance weight_final observations

label var lotcateg "Lottery Category"

***Save this garbage in a table
texsave Year lotcateg LATE pi_x variance weight_final observations  ///
using "$output/parth.tex", replace title("LATE Calculations") ///
footnote("First 8 lines Estimate Category and Year Specific LATEs, 2SLS Line does 2SLS controlling for interactions in both stages, consistent with Angrist Impends Weights") ///
width(17cm) location("H") frag
