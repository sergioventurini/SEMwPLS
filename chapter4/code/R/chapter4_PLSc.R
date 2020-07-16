path_data <- ""    # place here the path to store data
path_figures <- "" # place here the path to store figures
RNGversion("3.5.0")
if (getOption("scipen") <= 0) options(scipen = 5)

### Appendix: R Commands ###

tour_data <- read.csv(file.path(path_data,
  "ch3_MotivesActivity.csv"))

# mean imputation
if (!require(mice, quietly = TRUE)) {
  install.packages("mice")
}
library(mice)

tour_imp <- mice(tour_data, method = "mean")
tour_comp <- complete(tour_imp)

tour_comp$energy_tmp <- tour_comp$energy
tour_comp$getaway_tmp <- tour_comp$getaway
tour_comp$boredom_tmp <- tour_comp$boredom
tour_comp$exciting_tmp <- tour_comp$exciting

library(cSEM)

# repeated indicators appraoch
tour_mod <- "
  # measurement model
  Escape =~ energy + getaway
  Novelty =~ boredom + exciting
  ACTIVITY <~ entertain + visittown + nature + fishing
  SATISFACTION =~ satisf + expecta
  RECOMMENDATION <~ recommend

  # 2nd order construct
  MOTIVES =~ energy_tmp + getaway_tmp + boredom_tmp + exciting_tmp

  # structural model
  Escape ~ MOTIVES
  Novelty ~ MOTIVES
  ACTIVITY ~ MOTIVES
  SATISFACTION ~ ACTIVITY + MOTIVES
  RECOMMENDATION ~ SATISFACTION + ACTIVITY
"

tour_boot <- csem(.data = tour_comp,
  .model = tour_mod, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000,
  .seed = 1406, .handle_inadmissibles = "ignore")
summarize(tour_boot, .ci = "CI_percentile")
assess(tour_boot,
  .quality_criterion = c("ave", 
    "cronbachs_alpha", "cronbachs_alpha_weighted",
    "effects", "reliability", "r2",
    "r2_adj", "fl_criterion"))
calculateGoF(tour_boot)   # differ from Stata's plssem

# two-stage approach
tour_mod2 <- "
  # measurement model
  Escape =~ energy + getaway
  Novelty =~ boredom + exciting
  ACTIVITY <~ entertain + visittown + nature + fishing
  RECOMMENDATION <~ recommend
  SATISFACTION =~ satisf + expecta
  
  # 2nd order construct
  MOTIVES =~ Escape + Novelty

  # structural model
  Escape ~ MOTIVES
  Novelty ~ MOTIVES
  ACTIVITY ~ MOTIVES
  SATISFACTION ~ ACTIVITY + MOTIVES
  RECOMMENDATION ~ SATISFACTION + ACTIVITY
"

tour_boot2 <- csem(.data = tour_comp,
  .model = tour_mod2, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .approach_2ndorder = "2stage",
  .resample_method = "bootstrap", .R = 1000,
  .seed = 1406, .handle_inadmissibles = "ignore")
summarize(tour_boot2, .ci = "CI_percentile")
assess(tour_boot2)
