*** Figures A.4-5 ***
local path_images "" // place here the path to store figures

sysuse nlsw88, clear

tabstat age hours ttl_exp wage, ///
	statistics(count mean median p25 p75 sd min max)
// misstable summarize age hours ttl_exp wage
graph matrix age hours ttl_exp wage, half msize(small) ///
	scheme(sj)
local filename "`path_images'/figappA_4.png"
graph export "`filename'", replace
graph close

generate logwage = log(wage)
label variable logwage "logarithm of hourly wage"
graph matrix age hours ttl_exp logwage, half msize(small) ///
	scheme(sj)
local filename "`path_images'/figappA_5.png"
graph export "`filename'", replace
graph close

// correlate age hours ttl_exp logwage, covariance
correlate age hours ttl_exp logwage
pwcorr age hours ttl_exp logwage, obs sig

*** Figures A.14-20 ***
sysuse nlsw88, clear

local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

regress wage grade
/*
scatter wage grade, scheme(sj)
rvfplot, scheme(sj)
estat hettest
predict resid, residuals
qnorm resid, grid scheme(sj)
swilk resid
*/

generate logwage = log(wage)
label variable logwage "logarithm of hourly wage"
regress logwage grade
estimates store model1
/*
scatter logwage grade, scheme(sj)
rvfplot, scheme(sj)
estat hettest
drop resid
predict resid, residuals
qnorm resid, grid scheme(sj)
swilk resid
*/

regress logwage grade i.smsa
estimates store model2
regress logwage c.grade##smsa

scatter logwage grade if smsa == 0, mcolor(gs5) || ///
	scatter logwage grade if smsa == 1, mcolor(gs10) || ///
	function .6466321 + .0886297*x, range(0 18) lpattern(solid) ///
	lwidth(medthick) lcolor(gs5) || ///
	function (.6466321 + .2381447) + .0886297*x, range(0 18) lpattern(dash) ///
	lwidth(medthick) lcolor(gs10) ||, ///
	xtitle("Current grade completed") ytitle("Logarithm of hourly wage") ///
	legend(label(1 "SMSA = 0") label(2 "SMSA = 1") ///
	label(3 "Fit for SMSA = 0") label(4 "Fit for SMSA = 1")) scheme(sj)
local filename "`path_images'/figappA_17.pdf"
graph export "`filename'", replace
graph close

// table occupation
regress logwage grade i.smsa age hours ttl_exp tenure i.race i.married ///
  i.union i.south i.never_married i.occupation ///
	if occupation != 9 & occupation != 10 & occupation != 12
estimates store model3
// rvfplot, scheme(sj)
estat hettest
estat vif

quietly regress logwage grade i.smsa age hours ttl_exp tenure i.race ///
  i.married i.union i.south i.never_married i.occupation ///
	if occupation != 9 & occupation != 10 & occupation != 12, ///
	vce(bootstrap, reps(2000) seed(101) ///
	saving("`path_data'/appA_nlws_reg.dta", replace))
estimates store model3b
estat bootstrap, all

estimates table model*, b(%9.4f) se(%9.4f) stats(N r2_a)

use "`path_data'/appA_nlws_reg.dta", clear
histogram _b_tenure, frequency normal normopts(lwidth(thick)) ///
	scheme(sj)
local filename "`path_images'/figappA_20.pdf"
graph export "`filename'", replace
graph close
/*
graph matrix _b_*, half scheme(sj)
local filename "`path_images'/figappA_21.png"
graph export "`filename'", replace
graph close
// mvtest normality _b*, all univariate
*/
