*** Figure 7.1 ***
local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

clear all
set obs 1000
set seed 101
local pi = .5
generate group = rbinomial(1, `pi')
local m1 = 10
local s1 = 1
generate x1 = rnormal(`m1', `s1')
local m2 = 18
local s2 = 3
generate x2 = rnormal(`m2', `s2')
generate x = x1
quietly replace x = x2 if group == 1

histogram x, bin(25) scheme(sj) color(gs13) xlabel(0(5)30) xscale(range(0 30)) ///
	kdensity kdenopts(lwidth(medthick) lpattern(solid)) legend(off)
local filename "`path_images'/fig7_1a.pdf"
graph export "`filename'", replace

histogram x, bin(25) scheme(sj) color(gs13) xlabel(0(5)30) xscale(range(0 30)) addplot( ///
	function y = (1 - `pi')*normalden(x, `m1', `s1'), range(0 30) ///
		lwidth(medthick) lpattern(longdash) || ///
	function y = `pi'*normalden(x, `m2', `s2'), range(0 30) ///
		lwidth(medthick) lpattern(shortdash)) legend(off)
local filename "`path_images'/fig7_1b.pdf"
graph export "`filename'", replace

graph close

summarize x
bysort group: summarize x
ttest x, by(group) unequal


*** Figures 7.6 and 7.8 - REBUS-PLS example (from Mehmetoglu, 2011) ***
local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

quietly use "`path_data'/ch7_Cruise.dta", clear

misstable summarize
misstable patterns

plssem (TANGIBLE > Service Food Hygiene) (ATMOSPHERIC > Lively Lighting) ///
	(LOYALTY > Positive_talk Recommend), structural(LOYALTY TANGIBLE ATMOSPHERIC) ///
	missing(mean)

set matsize 1000
estat unobshet, test reps(1000) seed(123456) plot

local filename "`path_images'/fig7_6.pdf"
graph export "`filename'", replace

graph close

plssem (TANGIBLE > Service Food Hygiene) (ATMOSPHERIC > Lively Lighting) ///
	(LOYALTY > Positive_talk Recommend), structural(LOYALTY TANGIBLE ATMOSPHERIC) ///
	group(rebus_class, method(bootstrap) reps(1000) plot seed(123456)) missing(mean)

local filename "`path_images'/fig7_8.pdf"
graph export "`filename'", replace

graph close

/* // analysis by cluster
bysort rebus_class: plssem (TANGIBLE > Service Food Hygiene) (ATMOSPHERIC > Lively Lighting) ///
	(LOYALTY > Positive_talk Recommend), structural(LOYALTY TANGIBLE ATMOSPHERIC) ///
	missing(mean)
*/


*** FIMIX-PLS example ***
/*
/* 
  warning: these examples use FIMIX-PLS and PLS-GAS methods, whose
  implementation is still experimental [01/09/2020]!
*/

local path_data ""   // place here the path to store data

quietly use "`path_data'/ch7_Cruise.dta", clear

plssem (TANGIBLE > Service Food Hygiene) (ATMOSPHERIC > Lively Lighting) ///
	(LOYALTY > Positive_talk Recommend), structural(LOYALTY TANGIBLE ATMOSPHERIC) ///
	missing(mean)

// estat unobshet, method(fimix) stop(.1) restart(3) groups(1/8) seed(101) maxiter(1000)
estat unobshet, method(fimix) restart(3) numclass(4)

set matsize 1000
estat unobshet, method(rebus) numclass(2)
estat unobshet, method(fimix) restart(10) numclass(2)

table *_class

plssem (TANGIBLE > Service Food Hygiene) (ATMOSPHERIC > Lively Lighting) ///
	(LOYALTY > Positive_talk Recommend), structural(LOYALTY TANGIBLE ATMOSPHERIC) ///
	group(rebus_class, method(bootstrap) reps(1000) plot seed(123456)) missing(mean)
*/
