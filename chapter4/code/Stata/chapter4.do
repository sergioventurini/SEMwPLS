clear all

local path_data ""   // place here the path to store data

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

plssem (Escape > energy getaway) ///
       (Novelty > boredom exciting) ///
       (MOTIVES > energy getaway boredom exciting), ///
       structural(Escape MOTIVES, ///
                  Novelty MOTIVES)

plssem (Escape > energy getaway) ///
       (Novelty > boredom exciting) ///
       (MOTIVES > energy getaway boredom exciting) ///
       (ACTIVITY < entertain visittown nature fishing) ///
       (RECOMMENDATION < recommend) ///
       (SATISFACTION > satisf expecta), ///
       structural(Escape MOTIVES, ///
                  Novelty MOTIVES, ///
                  ACTIVITY MOTIVES, ///
                  SATISFACTION ACTIVITY MOTIVES, ///
                  RECOMMENDATION SATISFACTION ACTIVITY) 

plssem (Escape > energy getaway) ///
       (Novelty > boredom exciting) ///
       (MOTIVES > energy getaway boredom exciting) ///
       (ACTIVITY < entertain visittown nature fishing) ///
       (RECOMMENDATION < recommend) ///
       (SATISFACTION > satisf expecta), ///
       structural(Escape MOTIVES, ///
                  Novelty MOTIVES, ///
                  ACTIVITY MOTIVES, ///
                  SATISFACTION ACTIVITY MOTIVES, ///
                  RECOMMENDATION SATISFACTION ACTIVITY) ///
                  boot(1000) seed(123456)

*** Figure 4.7 ***
plssem (Escape > energy getaway) ///
       (Novelty > boredom exciting) ///
       (MOTIVES > energy getaway boredom exciting) ///
       (ACTIVITY < entertain visittown nature fishing) ///
       (RECOMMENDATION < recommend) ///
       (SATISFACTION > satisf expecta), ///
       structural(Escape MOTIVES, ///
                  Novelty MOTIVES, ///
                  ACTIVITY MOTIVES, ///
                  SATISFACTION ACTIVITY MOTIVES, ///
                  RECOMMENDATION SATISFACTION ACTIVITY) ///
                  boot(1000) seed(123456) ///
                  missing(mean) loadpval

factor energy getaway, pcf
factor boredom exciting, pcf
factor satisf expecta, pcf 

regress ACTIVITY entertain visittown nature fishing
estat vif

quietly {
	plssem (Escape > energy getaway) ///
				 (Novelty > boredom exciting) ///
				 (MOTIVES > energy getaway boredom exciting) ///
				 (ACTIVITY < entertain visittown nature fishing) ///
				 (RECOMMENDATION < recommend) ///
				 (SATISFACTION > satisf expecta), ///
				 structural(Escape MOTIVES, ///
										Novelty MOTIVES, ///
										ACTIVITY MOTIVES, ///
										SATISFACTION ACTIVITY MOTIVES, ///
										RECOMMENDATION SATISFACTION ACTIVITY) ///
										seed(123456) ///
										missing(mean)
}

estat vif
