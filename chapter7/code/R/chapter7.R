path_data <- ""    # place here the path to store data
path_figures <- "" # place here the path to store figures
RNGversion("3.5.0")
if (getOption("scipen") <= 0) options(scipen = 5)

### Appendix: R Commands ###

if (!require(haven, quietly = TRUE)) {
  install.packages("haven")
}

cruise_data <- as.data.frame(read_stata(file =
  file.path(path_data, "ch7_Cruise.dta")))

# mean imputation
if (!require(mice, quietly = TRUE)) {
  install.packages("mice")
}
library(mice)

cruise_imp <- mice(cruise_data, method = "mean",
  print = FALSE)
cruise_comp <- complete(cruise_imp)

library(plspm)

TANGIBLE <- c(0, 0, 0)
ATMOSPHERIC <- c(0, 0, 0)
LOYALTY <- c(1, 1, 0)
cruise_path <- rbind(TANGIBLE, ATMOSPHERIC, LOYALTY)
colnames(cruise_path) <- rownames(cruise_path)
cruise_blocks <- list(c(2, 4, 5), c(1, 3), c(6, 7))
cruise_modes <- rep("A", 3)

cruise_plspm <- plspm(Data = cruise_comp,
  path_matrix = cruise_path,
  blocks = cruise_blocks,
  modes = cruise_modes,
  scheme = "path", tol = 1e-7)
# summary(cruise_plspm)

cruise_rebus <- rebus.pls(cruise_plspm, stop.crit = 0.005,
  iter.max = 100)
cruise_rebus

cruise_test <- rebus.test(cruise_plspm, cruise_rebus)
cruise_test$test_1_2
