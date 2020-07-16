*** Figure 2.1 ***
clear all

local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

set obs 5
generate Y = .
replace Y = 7 in 1
replace Y = 4 in 2
replace Y = -5 in 3
replace Y = 1 in 4
replace Y = -2 in 5

local rseed = 101
local B = 200
bootstrap r(mean), reps(`B') seed(`rseed') ///
	saving("`path_data'/ch2_table1.dta", replace): summarize Y, meanonly
estat bootstrap, all

use "`path_data'/ch2_table1.dta", clear
centile, centile(2.5 97.5)

histogram _bs_1, frequency xtitle("Bootstrap mean") scheme(sj)
local filename "`path_images'/fig2_1.pdf"
graph export "`filename'", replace
graph close

*** Figures 2.3-2.8 ***
clear all

local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

use "`path_data'/ch2_Rateprof.dta", clear

graph matrix quality-raterinterest, half msize(small) ///
	scheme(sj)
local filename "`path_images'/fig2_3.png"
graph export "`filename'", replace
graph close

pca quality-raterinterest
// pca quality-raterinterest, covariance
screeplot, scheme(sj)
local filename "`path_images'/fig2_5.pdf"
graph export "`filename'", replace
graph close

pca quality-raterinterest, components(2)
pca quality-raterinterest, components(3)

estat loadings, cnorm(eigen)
// estat loadings, cnorm(unit)

// scoreplot, components(3) half msize(small) scheme(sj)
// loadingplot, components(3) half scheme(sj)

predict pc*, scores


*** Figure 2.9a ***
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter2/figures"

clear all
set obs 300
set seed 101
local cum_pi1 = 1/3
local cum_pi2 = 2/3
generate random  = uniform()
generate group = cond(random < `cum_pi1', 1, cond(random < `cum_pi2', 2, 3)) 
generate x = .
generate y = .

tempname m1 sd1
matrix `m1' = (1, 1)
matrix `sd1' = (.5, .5)
drawnorm tmpx tmpy, means(`m1') sds(`sd1')
quietly replace x = tmpx if group == 1
quietly replace y = tmpy if group == 1
drop tmpx tmpy

tempname m2 sd2
matrix `m2' = (5, 1)
matrix `sd2' = (.5, .5)
drawnorm tmpx tmpy, means(`m2') sds(`sd2')
quietly replace x = tmpx if group == 2
quietly replace y = tmpy if group == 2
drop tmpx tmpy

tempname m3 sd3
matrix `m3' = (3, 6)
matrix `sd3' = (.5, .5)
drawnorm tmpx tmpy, means(`m3') sds(`sd3')
quietly replace x = tmpx if group == 3
quietly replace y = tmpy if group == 3
drop tmpx tmpy

twoway scatter y x, scheme(sj) aspectratio(1) ///
	xlabel(#10, grid nolabels noticks gstyle(major)) xtitle("") ///
	ylabel(#10, grid nolabels noticks gstyle(major)) ytitle("") ///
	graphregion(color(white)) plotregion(lcolor(black))

local filename "`path_images'/fig2_9a.pdf"
graph export "`filename'", replace

graph close

*** Figure 2.9b ***
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter2/figures"

clear all
set obs 200
set seed 101

tempname m sd C
matrix `m' = (1, 1)
matrix `sd' = (1, 1)
matrix `C' = (1, .9, 1)
drawnorm x y, means(`m') sds(`sd') corr(`C') cstorage(lower)

twoway scatter y x, scheme(sj) aspectratio(1) ///
	xlabel(#7, grid nolabels noticks gstyle(major)) xtitle("") ///
	ylabel(#7, grid nolabels noticks gstyle(major)) ytitle("") ///
	graphregion(color(white)) plotregion(lcolor(black))

local filename "`path_images'/fig2_9b.pdf"
graph export "`filename'", replace

graph close

*** Figure 2.9c ***
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter2/figures"

clear all
set obs 200
set seed 101
local pi = 1/2
generate group = rbinomial(1, `pi')
generate x = .
generate y = .

tempname m1 sd1
matrix `m1' = (1, 1)
matrix `sd1' = (.5, .5)
drawnorm tmpx tmpy, means(`m1') sds(`sd1')
quietly replace x = tmpx if group == 1
quietly replace y = tmpy if group == 1
drop tmpx tmpy

tempname m2 sd2
matrix `m2' = (5, 1)
matrix `sd2' = (.5, .5)
drawnorm tmpx tmpy, means(`m2') sds(`sd2')
quietly replace x = tmpx if group == 0
quietly replace y = tmpy if group == 0
drop tmpx tmpy

twoway scatter y x, scheme(sj) aspectratio(1) ///
	xlabel(#10, grid nolabels noticks gstyle(major)) xtitle("") ///
	ylabel(#10, grid nolabels noticks gstyle(major)) ytitle("") ///
	graphregion(color(white)) plotregion(lcolor(black))

local filename "`path_images'/fig2_9c.pdf"
graph export "`filename'", replace

graph close

*** Figure 2.9d ***
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter2/figures"

clear all
set obs 500
set seed 101
local pi = 1/4
generate group = rbinomial(1, `pi')
generate x = .
generate y = .

tempname m1 sd1
matrix `m1' = (0, 0)
matrix `sd1' = (.4, .4)
drawnorm tmpx tmpy, means(`m1') sds(`sd1')
quietly replace x = tmpx if group == 1
quietly replace y = tmpy if group == 1
drop tmpx tmpy

local r = 2.5
generate theta = runiform(0, 2*c(pi))
local m2 = 0
local sd2 = .25
generate eps = rnormal(`m2', `sd2')
generate tmpx = `r'*cos(theta) + eps 
drop eps
generate eps = rnormal(`m2', `sd2')
generate tmpy = `r'*sin(theta) + eps
quietly replace x = tmpx if group == 0
quietly replace y = tmpy if group == 0
drop tmpx tmpy

twoway scatter y x, scheme(sj) aspectratio(1) ///
	xlabel(#10, grid nolabels noticks gstyle(major)) xtitle("") ///
	ylabel(#10, grid nolabels noticks gstyle(major)) ytitle("") ///
	graphregion(color(white)) plotregion(lcolor(black))

local filename "`path_images'/fig2_9d.pdf"
graph export "`filename'", replace

graph close


*** Figure 2.12 ***
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter2/figures"

clear all
matrix input D = (9 1 7 6 5 9 10 11 2 8)

clustermat singlelinkage D, shape(llower) clear name(single_linkage)
cluster dendrogram, scheme(sj) title("")

local filename "`path_images'/fig2_12a.pdf"
graph export "`filename'", replace

graph close

clustermat completelinkage D, shape(llower) clear name(complete_linkage)
cluster dendrogram, scheme(sj) title("")

local filename "`path_images'/fig2_12b.pdf"
graph export "`filename'", replace

graph close

clustermat averagelinkage D, shape(llower) clear name(average_linkage)
cluster dendrogram, scheme(sj) title("")

local filename "`path_images'/fig2_12c.pdf"
graph export "`filename'", replace

graph close

clustermat wardslinkage D, shape(llower) clear name(ward_linkage)
cluster dendrogram, scheme(sj) title("")

local filename "`path_images'/fig2_12d.pdf"
graph export "`filename'", replace

graph close


*** Figure 2.13 ***
local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

* The simulated data are for 3 4-dimensional multivariate normal groups
clear all
set obs 50
set seed 101
local cum_pi1 = .2
local cum_pi2 = .7
generate random  = uniform()
generate truegroup = cond(random < `cum_pi1', 1, cond(random < `cum_pi2', 2, 3)) 
generate x1 = .
generate x2 = .
generate x3 = .
generate x4 = .

tempname m1 sd1
matrix `m1' = (1, 1, 1, 1)
matrix `sd1' = (1, 1, 1, 1)
drawnorm tmpx1 tmpx2 tmpx3 tmpx4, means(`m1') sds(`sd1')
quietly replace x1 = tmpx1 if truegroup == 1
quietly replace x2 = tmpx2 if truegroup == 1
quietly replace x3 = tmpx3 if truegroup == 1
quietly replace x4 = tmpx4 if truegroup == 1
drop tmp*

tempname m2 sd2
matrix `m2' = (7, 5, 7, 5)
matrix `sd2' = (1.5, .5, 1.5, .5)
drawnorm tmpx1 tmpx2 tmpx3 tmpx4, means(`m2') sds(`sd2')
quietly replace x1 = tmpx1 if truegroup == 2
quietly replace x2 = tmpx2 if truegroup == 2
quietly replace x3 = tmpx3 if truegroup == 2
quietly replace x4 = tmpx4 if truegroup == 2
drop tmp*

tempname m3 sd3
matrix `m3' = (8, 15, 10, 15)
matrix `sd3' = (1.5, 2, 1.5, 2)
drawnorm tmpx1 tmpx2 tmpx3 tmpx4, means(`m3') sds(`sd3')
quietly replace x1 = tmpx1 if truegroup == 3
quietly replace x2 = tmpx2 if truegroup == 3
quietly replace x3 = tmpx3 if truegroup == 3
quietly replace x4 = tmpx4 if truegroup == 3
drop tmp*

capture drop random
quietly saveold "`path_data'/ch2_SimData.dta", version(12) replace

graph matrix x*, half scheme(sj)

local filename "`path_images'/fig2_13.pdf"
graph export "`filename'", replace

graph close


*** Figure 2.14 ***
local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

quietly use "`path_data'/ch2_SimData.dta", clear
capture keep x1-x4 truegroup

foreach var of varlist x1-x4 {
	egen `var'_std = std(`var')
}

cluster drop _all

cluster singlelinkage *_std, name(single_linkage)
cluster dendrogram, xlabel(, labsize(tiny)) scheme(sj) title("")

local filename "`path_images'/fig2_14a.pdf"
graph export "`filename'", replace

graph close

cluster completelinkage *_std, name(complete_linkage)
cluster dendrogram, xlabel(, labsize(tiny)) scheme(sj) title("")

local filename "`path_images'/fig2_14b.pdf"
graph export "`filename'", replace

graph close

cluster averagelinkage *_std, name(average_linkage)
cluster dendrogram, xlabel(, labsize(tiny)) scheme(sj) title("")

local filename "`path_images'/fig2_14c.pdf"
graph export "`filename'", replace

graph close

cluster wardslinkage *_std, name(ward_linkage)
cluster dendrogram, xlabel(, labsize(tiny)) scheme(sj) title("")

local filename "`path_images'/fig2_14d.pdf"
graph export "`filename'", replace

graph close

cluster stop single_linkage, rule(calinski) groups(2/8)
cluster stop complete_linkage, rule(calinski) groups(2/8)
cluster stop average_linkage, rule(calinski) groups(2/8)
cluster stop ward_linkage, rule(calinski) groups(2/8)

cluster stop single_linkage, rule(duda) groups(2/8)
cluster stop complete_linkage, rule(duda) groups(2/8)
cluster stop average_linkage, rule(duda) groups(2/8)
cluster stop ward_linkage, rule(duda) groups(2/8)

cluster generate single3 = groups(3), name(single_linkage)
cluster generate complete3 = groups(3), name(complete_linkage)
cluster generate average3 = groups(3), name(average_linkage)
cluster generate ward3 = groups(3), name(ward_linkage)

table single3 complete3, row col
table single3 average3, row col
table single3 ward3, row col

table single3 truegroup, row col

quietly saveold "`path_data'/ch2_SimData.dta", version(12) replace


*** Figure 2.17 ***
local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

quietly use "`path_data'/ch2_SimData.dta", clear

graph matrix x1-x4, half scheme(sj) msymbol(i) mlabel(average3) mlabpos(0) ///
	mlabsize(medsmall)

local filename "`path_images'/fig2_17.pdf"
graph export "`filename'", replace

graph close


*** Figure 2.19 ***
local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

quietly use "`path_data'/ch2_SimData.dta", clear
capture drop *_std

foreach var of varlist x1-x4 {
	egen `var'_std = std(`var')
}

cluster drop _all

capture drop km_*
forvalues g = 2/8 {
	cluster kmeans *_std, k(`g') start(krandom(301)) name(km`g') generate(km`g')
}

forvalues g = 2/8 {
	cluster stop km`g'
}

table single3 km3, row col

graph matrix x1-x4, half scheme(sj) msymbol(i) mlabel(km3) mlabpos(0) ///
	mlabsize(medsmall)

local filename "`path_images'/fig2_19.pdf"
graph export "`filename'", replace

graph close


*** Figure 2.20 ***
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter2/figures"

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
local filename "`path_images'/fig2_20a.pdf"
graph export "`filename'", replace

histogram x, bin(25) scheme(sj) color(gs13) xlabel(0(5)30) xscale(range(0 30)) addplot( ///
	function y = (1 - `pi')*normalden(x, `m1', `s1'), range(0 30) ///
		lwidth(medthick) lpattern(longdash) || ///
	function y = `pi'*normalden(x, `m2', `s2'), range(0 30) ///
		lwidth(medthick) lpattern(shortdash)) legend(off)
local filename "`path_images'/fig2_20b.pdf"
graph export "`filename'", replace

graph close

summarize x
bysort group: summarize x
ttest x, by(group) unequal


*** Figure 2.24 ***
local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

clear all
set obs 500
set seed 101
local cum_pi1 = .2
local cum_pi2 = .7
generate random  = uniform()
generate truegroup = cond(random < `cum_pi1', 1, cond(random < `cum_pi2', 2, 3)) 
generate x1 = .
generate x2 = .
generate x3 = .
generate x4 = .

tempname m1 sd1
matrix `m1' = (1, 1, 1, 1)
matrix `sd1' = (1, 1, 1, 1)
drawnorm tmpx1 tmpx2 tmpx3 tmpx4, means(`m1') sds(`sd1')
quietly replace x1 = tmpx1 if truegroup == 1
quietly replace x2 = tmpx2 if truegroup == 1
quietly replace x3 = tmpx3 if truegroup == 1
quietly replace x4 = tmpx4 if truegroup == 1
drop tmp*

tempname m2 sd2
matrix `m2' = (7, 5, 7, 5)
matrix `sd2' = (1.5, .5, 1.5, .5)
drawnorm tmpx1 tmpx2 tmpx3 tmpx4, means(`m2') sds(`sd2')
quietly replace x1 = tmpx1 if truegroup == 2
quietly replace x2 = tmpx2 if truegroup == 2
quietly replace x3 = tmpx3 if truegroup == 2
quietly replace x4 = tmpx4 if truegroup == 2
drop tmp*

tempname m3 sd3
matrix `m3' = (8, 15, 10, 15)
matrix `sd3' = (1.5, 2, 1.5, 2)
drawnorm tmpx1 tmpx2 tmpx3 tmpx4, means(`m3') sds(`sd3')
quietly replace x1 = tmpx1 if truegroup == 3
quietly replace x2 = tmpx2 if truegroup == 3
quietly replace x3 = tmpx3 if truegroup == 3
quietly replace x4 = tmpx4 if truegroup == 3
drop tmp*

capture drop random
quietly saveold "`path_data'/ch2_SimData2.dta", version(12) replace

forvalues K = 2/8 {
  quietly fmm `K': regress (x1-x4)
  estimates store fmm`K'
}
estimates stats fmm*

estimates restore fmm3

estat lcmean
estat lcprob

predict classpr*, classposteriorpr
generate fmm3 = .
mata: st_store(., "fmm3", ., which(st_data(., "classpr*"), "max"))

graph matrix x*, half scheme(sj) msymbol(i) mlabel(fmm3) mlabpos(0) ///
	mlabsize(small)

local filename "`path_images'/fig2_24.pdf"
graph export "`filename'", replace

graph close


*** LCA using gsem and fmm ***
use http://www.stata-press.com/data/r15/gsem_lca1, clear

quietly gsem (accident play insurance stock <- ), logit lclass(C 2)
estat lcprob

quietly fmm 2: logit (accident-stock)
estat lcprob


*** Figures 2.28-2.29 ***
clear all

local path_data ""   // place here the path to store data
local path_images "" // place here the path to store figures

use "`path_data'/ch2_hsb2.dta", clear

sem (math <- read write) (science <- math read write), method(ml)
// sem (math <- read write) (science <- math read write), method(ml) standardized
// estat eqgof
// estat gof, stats(all)
estat teffects

/*
pathreg (math read write) (science math read write)
*/
