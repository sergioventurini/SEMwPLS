clear all

local path_data ""   // place here the path to store data

*** Figure 6.8 ***
use "`path_data'/ch6_CultureCuriosity.dta", clear

foreach var of varlist V1A V1B V1C V2A V2E V2F {
  egen `var'_std = std(`var')
}

plssemc (CULTURE > V1A_std V1B_std V1C_std) /// 
        (CURIOSITY > V2A_std V2E_std V2F_std) ///
        (H_INTEREST > V3A V3B), /// 
        structural(H_INTEREST CULTURE*CURIOSITY) ///
        boot(1000) seed(123456)

*** Figure 6.9 ***
plssemc (CULTURE2 > V1A V1B V1C) /// 
        (CURIOSITY2 > V2A V2E V2F) ///
			  (H_INTEREST2 > V3A V3B), /// 
			  structural(H_INTEREST2 CULTURE2 CURIOSITY2)

plssemc (CULTURE > CULTURE2) /// 
        (CURIOSITY > CURIOSITY2) ///
        (H_INTEREST > H_INTEREST2), /// 
        structural(H_INTEREST CULTURE*CURIOSITY)

*** Figure 6.12 ***
plssemc (CULTURE3 > V1A V1B V1C) /// 
        (CURIOSITY3 > CURIOSITY_D) ///
        (H_INTEREST3 > V3A V3B), /// 
        structural(H_INTEREST3 CULTURE3 CURIOSITY3)

plssemc (CULTURE > CULTURE3) /// 
        (CURIOSITY > CURIOSITY3) ///
        (H_INTEREST > H_INTEREST3), /// 
        structural(H_INTEREST CULTURE*CURIOSITY)

*** Figure 6.14 ***
* [01/09/2020: currently plssemc does not allow for the group() option]
plssem (CULTURE > V1A V1B V1C) /// 
			 (H_INTEREST > V3A V3B), /// 
			 structural(H_INTEREST CULTURE) /// 
			 group(CURIOSITY_D, reps(1000) /// 
			 groupseed(123456) method(bootstrap))

*** Measurement Model Invariance ***
factor V1A V1B V1C if CURIOSITY_D == 0, pcf  	
factor V1A V1B V1C if CURIOSITY_D == 1, pcf  	
factor V3A V3B if CURIOSITY_D == 0, pcf
factor V3A V3B if CURIOSITY_D == 1, pcf
