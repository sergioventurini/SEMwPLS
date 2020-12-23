clear all

local path_data ""   // place here the path to store data

*** Figure 6.7 ***
use "`path_data'/ch6_CultureCuriosity.dta", clear

foreach var of varlist V1A V1B V1C V2A V2E V2F {
  egen `var'_std = std(`var')
}

plssem (CULTURE > V1A_std V1B_std V1C_std) /// 
       (CURIOSITY > V2A_std V2E_std V2F_std) ///
       (H_INTEREST > V3A V3B), /// 
       structural(H_INTEREST CULTURE*CURIOSITY) ///
       boot(1000) seed(123456)

*** Figure 6.8 ***
plssem (CULTURE2 > V1A V1B V1C) /// 
       (CURIOSITY2 > V2A V2E V2F) ///
			 (H_INTEREST2 > V3A V3B), /// 
			 structural(H_INTEREST2 CULTURE2 CURIOSITY2)

plssem (CULTURE > CULTURE2) /// 
       (CURIOSITY > CURIOSITY2) ///
       (H_INTEREST > H_INTEREST2), /// 
       structural(H_INTEREST CULTURE*CURIOSITY)

*** Figure 6.11 ***
plssem (CULTURE3 > V1A V1B V1C) /// 
       (CURIOSITY3 > CURIOSITY_D) ///
       (H_INTEREST3 > V3A V3B), /// 
       structural(H_INTEREST3 CULTURE3 CURIOSITY3)

plssem (CULTURE > CULTURE3) /// 
       (CURIOSITY > CURIOSITY3) ///
       (H_INTEREST > H_INTEREST3), /// 
       structural(H_INTEREST CULTURE*CURIOSITY)

*** Figure 6.13 ***
plssem (CULTURE > V1A V1B V1C) /// 
			 (H_INTEREST > V3A V3B), /// 
			 structural(H_INTEREST CULTURE) /// 
			 group(CURIOSITY_D, unequal reps(1000) /// 
			 groupseed(123456) method(bootstrap))

*** Measurement Model Invariance ***
factor V1A V1B V1C if CURIOSITY_D == 0, pcf  	
factor V1A V1B V1C if CURIOSITY_D == 1, pcf  	
factor V3A V3B if CURIOSITY_D == 0, pcf
factor V3A V3B if CURIOSITY_D == 1, pcf
