***Question 3 STATA Code***
clear all
set more off


***Change to your path***

***George***
global whoamI "/Users/georgevojta/Dropbox/Uchicago Notes/Year1/Magne/ProblemSets/ProblemSet5"

global problem "$whoamI"



global input "$problem/input"
global output "$problem/output"
global code "$problem/code"
global processing "$problem/processing"


/*
PSET 5 - Thanks for the Eigth! (Because it's half a quarter hehehe

*/

use "$input/PS5.dta", clear

*** Part 1 ***
codebook

*** Parts 2-5 ***
* Problem gives us that t = 0, here post, is the time frame we want so run the regression
set level 90
reg empft minwage nregs hrsopen d1 d2 d3 d4 if post == 0
reg empft minwage nregs hrsopen d1 d2 d3 d4 if post == 0, nocons

*** Part 5 ***
di _b[d2] - _b[d4]

*** Part 6 ***
test d2 d3

*** Part 7***
mat list e(V)

test d2 = d3
di _b[d2]-_b[d3]

*** Part 10 ***
preserve 
collapse (mean) Mean = empft (sd) StdErr = empft (count) N = empft, by(post state)
sort state post Mean StdErr N
order state post Mean StdErr N

texsave state post Mean StdErr N  ///
using "$output/Part10.tex", replace title("Summary Stats By Time and State") ///
footnote("") ///
width(18.5cm) location("H") frag
restore

*** Part 12 ***
g interact = post * state
reg empft post state interact

*** Part 14 ***
ivreg2 empft post state interact, cluster(state post) robust

*** Part 15 ***
reg empft post state interact hrsopen nregs d1 d2 d3 d4,  nocons robust
