clear all

*** Figures 5.5-5.6 ***
use http://www.stata-press.com/data/r15/wageed.dta, clear
describe age tenure wage

plssemc (Age > age) ///
        (Tenure > tenure) ///
        (Wage > wage), ///
        structural(Tenure Age, ///
									 Wage Tenure Age) ///
        boot(1000) seed(12345)

estat mediate, indep(Age) med(Tenure) dep(Wage) ///
  seed(12345) breps(1000) zlc rit rid //bca

*** Figures 5.8-5.9 ***
local path_data ""   // place here the path to store data

use "`path_data'/ch5_envbehav.dta", clear

plssemc (EnvConcern > sp1e sp1m sp1o) ///
        (PersNorm > sp3a sp3b sp3c) ///
        (EnvBehavInt > sp2a sp2b sp2c sp2d), ///
        structural(PersNorm EnvConcern, ///
                   EnvBehavInt PersNorm EnvConcern) ///
        boot(1000) seed(12345)

estat mediate, indep(EnvConcern) med(PersNorm) dep(EnvBehavInt) ///
  seed(12345) breps(1000) zlc rit rid //bca

*** Figures 5.11-5.13 ***
plssemc (IndValues > sp8_6 sp8_7 sp8_9) ///
        (EnvConcern > sp1e sp1m sp1o) ///
        (PersNorm > sp3a sp3b sp3c) ///
        (EnvBehavInt > sp2a sp2b sp2c sp2d), ///
        structural(EnvConcern IndValues, ///
                   PersNorm IndValues EnvConcern, ///
	                 EnvBehavInt PersNorm EnvConcern IndValues) ///
        boot(1000) seed(12345)

estat mediate, indep(IndValues) med(PersNorm) dep(EnvBehavInt) ///
  seed(12345) breps(1000) zlc rit rid //bca

estat mediate, indep(IndValues) med(EnvConcern) dep(EnvBehavInt) ///
  seed(12345) breps(1000) zlc rit rid //bca
