clear all

local path_data "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/data"

use "`path_data'/ch3_MotivesActivity.dta", clear

rename spm1_6 energy
rename spm1_7 getaway
rename spm1_8 boredom
rename spm1_9 exciting
rename spm3_2 entertain
rename spm3_6 visittown
rename spm3_8 nature
rename spm3_12 fishing
rename spm15_3 recommend
rename spm15_7 satisf
rename spm15_8 expecta

*** Figure 3.6 ***
tabstat energy getaway boredom exciting entertain visittown ///
	nature fishing recommend satisf expecta, ///
	statistics(n mean median sd p25 p75) columns(statistics)
	
/*
correlate energy getaway boredom exciting
correlate entertain visittown nature fishing
correlate satisf expecta

graph matrix energy getaway boredom exciting entertain visittown nature ///
	fishing satisf expecta, half jitter(.75) jitterseed(101)
*/

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES)

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) tol(1e-9)

*** Table 3.3 ***
plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) wscheme(centroid)
estimates store centroid_scheme

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) wscheme(factorial)
estimates store factorial_scheme

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) wscheme(path)
estimates store path_scheme

estimates restore centroid_scheme
matlist e(loadings)
matlist e(struct_b)
estimates restore factorial_scheme
matlist e(loadings)
matlist e(struct_b)
estimates restore path_scheme
matlist e(loadings)
matlist e(struct_b)

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) ///
       boot(1000) seed(1406)
estimates store path_scheme_boot

estimates restore path_scheme
matlist e(loadings_se)
matlist e(struct_se)
estimates restore path_scheme_boot
matlist e(loadings_se)
matlist e(struct_se)

quietly plssem (ACTIVITY < entertain visittown nature fishing) ///
							 (MOTIVES > energy getaway boredom exciting) ///
							 (SATISFACTION > recommend satisf expecta), ///
							 structural(SATISFACTION ACTIVITY MOTIVES)

*** Figure 3.10 ***
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/figures"

plssemplot, loadings
local filename "`path_images'/fig3_10.pdf"
graph export "`filename'", replace
graph close

*** Figure 3.11 ***
plssemplot, outerweights
local filename "`path_images'/fig3_11.pdf"
graph export "`filename'", replace
graph close

*** Figure 3.12 ***
label variable ACTIVITY "Scores of ACTIVITY latent variable"
label variable MOTIVES  "Scores of MOTIVES latent variable"
label variable SATISFACTION "Scores of SATISFACTION latent variable"
plssemplot, scores
local filename "`path_images'/fig3_12.pdf"
graph export "`filename'", replace
graph close

misstable summarize energy-expecta
misstable pattern energy-expecta

drop ACT* MOT* SAT* _est*
set seed 1404

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES)
estimates store complete_case
label variable ACTIVITY "Scores of ACTIVITY (complete)"
label variable MOTIVES  "Scores of MOTIVES (complete)"
label variable SATISFACTION "Scores of SATISFACTION (complete)"
rename ACTIVITY ACTIVITY_cc
rename MOTIVES MOTIVES_cc
rename SATISFACTION SATISFACTION_cc

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) ///
			 missing(mean)
estimates store mean_imputation
label variable ACTIVITY "Scores of ACTIVITY (mean)"
label variable MOTIVES  "Scores of MOTIVES (mean)"
label variable SATISFACTION "Scores of SATISFACTION (mean)"
rename ACTIVITY ACTIVITY_mean
rename MOTIVES MOTIVES_mean
rename SATISFACTION SATISFACTION_mean

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) ///
			 missing(knn) k(1)
estimates store knn_imputation_1
label variable ACTIVITY "Scores of ACTIVITY (k-NN - k=1)"
label variable MOTIVES  "Scores of MOTIVES (k-NN - k=1)"
label variable SATISFACTION "Scores of SATISFACTION (k-NN - k=1)"
rename ACTIVITY ACTIVITY_knn1
rename MOTIVES MOTIVES_knn1
rename SATISFACTION SATISFACTION_knn1

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) ///
			 missing(knn) k(5)
estimates store knn_imputation_5
label variable ACTIVITY "Scores of ACTIVITY (k-NN - k=5)"
label variable MOTIVES  "Scores of MOTIVES (k-NN - k=5)"
label variable SATISFACTION "Scores of SATISFACTION (k-NN - k=5)"
rename ACTIVITY ACTIVITY_knn5
rename MOTIVES MOTIVES_knn5
rename SATISFACTION SATISFACTION_knn5

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES) ///
			 missing(knn) k(10)
estimates store knn_imputation_10
label variable ACTIVITY "Scores of ACTIVITY (k-NN - k=10)"
label variable MOTIVES  "Scores of MOTIVES (k-NN - k=10)"
label variable SATISFACTION "Scores of SATISFACTION (k-NN - k=10)"
rename ACTIVITY ACTIVITY_knn10
rename MOTIVES MOTIVES_knn10
rename SATISFACTION SATISFACTION_knn10

*** Table 3.4 ***
estimates restore complete_case
matlist e(loadings)
matlist e(struct_b)
estimates restore mean_imputation
matlist e(loadings)
matlist e(struct_b)
estimates restore knn_imputation_1
matlist e(loadings)
matlist e(struct_b)
estimates restore knn_imputation_5
matlist e(loadings)
matlist e(struct_b)
estimates restore knn_imputation_10
matlist e(loadings)
matlist e(struct_b)

*** Figure 3.13a, b, c ***
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/figures"

graph matrix ACTIVITY*, half scheme(sj)
local filename "`path_images'/fig3_13a.pdf"
graph export "`filename'", replace
graph close
graph matrix MOTIVES*, half scheme(sj)
local filename "`path_images'/fig3_13b.pdf"
graph export "`filename'", replace
graph close
graph matrix SATISFACTION*, half scheme(sj)
local filename "`path_images'/fig3_13c.pdf"
graph export "`filename'", replace
graph close

*** Figure 3.15 ***
clear all

local path_data "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/data"

use "`path_data'/ch3_MotivesActivity.dta", clear

rename spm1_6 energy
rename spm1_7 getaway
rename spm1_8 boredom
rename spm1_9 exciting
rename spm3_2 entertain
rename spm3_6 visittown
rename spm3_8 nature
rename spm3_12 fishing
rename spm15_3 recommend
rename spm15_7 satisf
rename spm15_8 expecta

plssem (ACTIVITY < entertain visittown nature fishing) ///
			 (MOTIVES > energy getaway boredom exciting) ///
       (SATISFACTION > recommend satisf expecta), ///
       structural(SATISFACTION ACTIVITY MOTIVES, MOTIVES ACTIVITY)
			 
estat total

*** Table 3.6 ***
clear all

local path_data "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/data"
local path_images "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/figures"

use "`path_data'/education.dta", clear

generate supappre = 8 - supunder
generate loypleas = 8 - loyasha
drop supunder loyasha

set seed 101

plssem (Support > sup*) (Advising > adv*) (Tutoring > tut*) (Value > val*) ///
	(Satisfaction > sat*) (Loyalty > loy*), ///
	structural(Value Support Advising Tutoring, ///
	Satisfaction Support Advising Tutoring Value, Loyalty Satisfaction) ///
	wscheme("centroid") digits(4) tol(1e-6) // boot(1000)

* plssemplot, inner

matrix b_PLS = e(struct_b)
matrix lambda_PLS = e(loadings)

plssemc (Support > sup*) (Advising > adv*) (Tutoring > tut*) (Value > val*) ///
	(Satisfaction > sat*) (Loyalty > loy*), ///
	structural(Value Support Advising Tutoring, ///
	Satisfaction Support Advising Tutoring Value, Loyalty Satisfaction) ///
	wscheme("centroid") digits(4) tol(1e-6) // boot(1000)

matrix b_PLSc = e(struct_b)
matrix lambda_PLSc = e(loadings)

/*
mat list lambda_PLS
mata: (rowsum(st_matrix("lambda_PLS")), rowsum(st_matrix("lambda_PLSc")))

mat list b_PLS
mata: (vec(st_matrix("b_PLS")), vec(st_matrix("b_PLSc")))
*/

*** Figures 3.20-3.21 ***
clear all

local path_data "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/data"

use "`path_data'/ch3_Curiosity.dta", clear

/* Repeated indicators approach */
plssem (PerceptCur > v2a v2b) ///
       (EpistemCur > v2e v2f) ///
       (Curiosity > v2a v2b v2e v2f) ///
			 (HolidayInt > v3a v3b), ///
       structural(PerceptCur Curiosity, ///  
                  EpistemCur Curiosity, ///
									HolidayInt Curiosity)

/* Two-step approach */
/* -- Step 1 -- */
quietly {
  pca v2a v2b, components(1)
  predict PerceptCur_s, score
	
  pca v2e v2f, components(1)
  predict EpistemCur_s, score
}

/* -- Step 2 -- */
plssem (Curiosity > PerceptCur_s EpistemCur_s) ///
       (HolidayInt > v3a v3b), ///
       structural(HolidayInt Curiosity)
