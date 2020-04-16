clear
set more off
set seed 10

*Set number of draws
set obs 10000
*generate two standard normal variables
gen z1 = rnormal(0,1)
gen z2 = rnormal(0,1)


*Exercise 1 for sigma = 2 and rho = 0.5
******Generate random draws from joint distribution of (Y_1,Y_0)************
*The code constructs the joint distribution using a linear combination of
*of two standard independent normals

**Finding appropriate weights on independent normals
*Set parameters
loc sigma = 2
loc rho = 0.5

matrix Sigma = (`sigma'^2, `rho' *`sigma' \ `rho'*`sigma', 1)
mat L = cholesky(Sigma)
*generate (Y_1,Y_0) pairs
gen y1_1 = L[1,1]*z1
gen y0_1 = L[2,1]*z1 + L[2,2]*z2
*****************************************************************************

****************Calculate parameters of interest*****************************
gen d_1 = y1_1>y0_1
egen ATE_1 = mean(y1_1 - y0_1)
egen ATT_1 = mean(y1_1 - y0_1) if d_1==1
egen ATUT_1 = mean(y1_1 - y0_1) if d_1==0
egen temp1 = mean(y1_1) if d_1 == 1
egen temp0 = mean(y0_1) if d_1 == 0
sort temp0
replace temp0 = temp0[_n-1] if missing(temp0)
sort temp1
replace temp1 = temp1[_n-1] if missing(temp1)
gen OLS_1 = temp1 - temp0
drop temp1 temp0

*fill missing values
sort ATT_1
replace ATT_1 = ATT_1[_n-1] if missing(ATT_1)
sort ATUT_1
replace ATUT_1 = ATUT_1[_n-1] if missing(ATUT_1)



*Exercise 2 for sigma = 2 and rho = 0
******Generate random draws from joint distribution of (Y_1,Y_0)************
*The code constructs the joint distribution using a linear combination of
*of two standard independent normals

**Finding appropriate weights on independent normals
*Set parameters
loc sigma = 2
loc rho = 0

matrix Sigma = (`sigma'^2, `rho' *`sigma' \ `rho'*`sigma', 1)
mat L = cholesky(Sigma)
*generate (Y_1,Y_0) pairs
gen y1_2 = L[1,1]*z1
gen y0_2 = L[2,1]*z1 + L[2,2]*z2
*****************************************************************************

****************Calculate parameters of interest*****************************
gen d_2 = y1_2>y0_2
gen y_2 = y0_2 + (y1_2 - y0_2)*d_2
egen ATE_2 = mean(y1_2 - y0_2)
egen ATT_2 = mean(y1_2 - y0_2) if d_2==1
egen ATUT_2 = mean(y1_2 - y0_2) if d_2==0
egen temp1 = mean(y1_2) if d_2 == 1
egen temp0 = mean(y0_2) if d_2 == 0
sort temp0
replace temp0 = temp0[_n-1] if missing(temp0)
sort temp1
replace temp1 = temp1[_n-1] if missing(temp1)
gen OLS_2 = temp1 - temp0
drop temp1 temp0


*fill missing values
sort ATT_2
replace ATT_2 = ATT_2[_n-1] if missing(ATT_2)
sort ATUT_2
replace ATUT_2 = ATUT_2[_n-1] if missing(ATUT_2)






*Exercise 3 for sigma = 2 and rho = -0.5
******Generate random draws from joint distribution of (Y_1,Y_0)************
*The code constructs the joint distribution using a linear combination of
*of two standard independent normals

**Finding appropriate weights on independent normals
*Set parameters
loc sigma = 2
loc rho = -0.5

matrix Sigma = (`sigma'^2, `rho' *`sigma' \ `rho'*`sigma', 1)
mat L = cholesky(Sigma)
*generate (Y_1,Y_0) pairs
gen y1_3 = L[1,1]*z1
gen y0_3 = L[2,1]*z1 + L[2,2]*z2
*****************************************************************************

****************Calculate parameters of interest*****************************
gen d_3 = y1_3>y0_3
gen y_3 = y0_3 + (y1_3 - y0_3)*d_3
egen ATE_3 = mean(y1_3 - y0_3)
egen ATT_3 = mean(y1_3 - y0_3) if d_3==1
egen ATUT_3 = mean(y1_3 - y0_3) if d_3==0
egen temp1 = mean(y1_3) if d_3 == 1
egen temp0 = mean(y0_3) if d_3 == 0
sort temp0
replace temp0 = temp0[_n-1] if missing(temp0)
sort temp1
replace temp1 = temp1[_n-1] if missing(temp1)
gen OLS_3 = temp1 - temp0
drop temp1 temp0

*fill missing values
sort ATT_3
replace ATT_3 = ATT_3[_n-1] if missing(ATT_3)
sort ATUT_3
replace ATUT_3 = ATUT_3[_n-1] if missing(ATUT_3)








*Exercise 4 fix rho = 0.5 and try mupltiple sigma
******Generate random draws from joint distribution of (Y_1,Y_0)************
*The code constructs the joint distribution using a linear combination of
*of two standard independent normals
loc rho = 0.5
loc step_size = 0.2
loc steps =30

forvalues k = 1/`steps'{
	**Finding appropriate weights on independent normals
	*Set parameters
	loc sigma = `k'*`step_size'
	di `sigma'

	matrix Sigma = (`sigma'^2, `rho' *`sigma' \ `rho'*`sigma', 1)
	mat L = cholesky(Sigma)
	*generate (Y_1,Y_0) pairs
	gen y1_4_`k' = L[1,1]*z1
	gen y0_4_`k' = L[2,1]*z1 + L[2,2]*z2
	*****************************************************************************

	****************Calculate parameters of interest*****************************
	gen d_4_`k' = y1_4_`k'>y0_4_`k'
	gen y_4_`k' = y0_4_`k' + (y1_4_`k' - y0_4_`k')*d_4_`k'
	egen ATE_4_`k' = mean(y1_4_`k' - y0_4_`k')
	egen ATT_4_`k' = mean(y1_4_`k' - y0_4_`k') if d_4_`k'==1
	egen ATUT_4_`k' = mean(y1_4_`k' - y0_4_`k') if d_4_`k'==0
	egen temp1 = mean(y1_4_`k') if d_4_`k' == 1
	egen temp0 = mean(y0_4_`k') if d_4_`k' == 0
	sort temp0
	replace temp0 = temp0[_n-1] if missing(temp0)
	sort temp1
	replace temp1 = temp1[_n-1] if missing(temp1)
	gen OLS_4_`k' = temp1 - temp0
	drop temp1 temp0


	*fill missing values
	sort ATT_4_`k'
	replace ATT_4_`k' = ATT_4_`k'[_n-1] if missing(ATT_4_`k')
	sort ATUT_4_`k'
	replace ATUT_4_`k' = ATUT_4_`k'[_n-1] if missing(ATUT_4_`k')
}

keep if _n == 1
