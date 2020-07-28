path_figures <- "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/figures"
path_data <- "/Users/Sergio/Dropbox (Personal)/PLS-SEM_book/book/chapters/chapter3/data"
RNGversion("3.5.0")
if (getOption("scipen") <= 0) options(scipen = 5)

### Appendix: R Commands ###
## The matrixpls package ##
if (!require(matrixpls, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
  install_github("mronkko/matrixpls")
}
library(matrixpls)

tour_data <- read.csv(file.path(path_data,
  "ch3_MotivesActivity.csv"))

tour_mod <- "
  # measurement model
  ACTIVITY <~ entertain + visittown + nature + fishing
  MOTIVES =~ energy + getaway + boredom + exciting
  SATISFACTION =~ recommend + satisf + expecta
  # structural model
  SATISFACTION ~ ACTIVITY + MOTIVES
"
tour_res <- matrixpls(S = cov(tour_data,
  use = "complete.obs"), model = tour_mod)
tour_res

summary(tour_res)
# ave(tour_res)        # average variance extracted
# cr(tour_res)         # composite reliability indexes
effects(tour_res)     # direct, indirect and total effects
# fitted(tour_res)      # fitted correlation matrix
# gof(tour_res)         # goodness-of-fit index
# loadings(tour_res)    # estimated loadings
# print(tour_res)      # print output
# r2(tour_res)         # squared multiple correlations
# fitSummary(tour_res) # summary of fit indices
# predict(tour_res,    # model's predictions
#   tour_data,
#   predictionType = "composites")
# residuals(tour_res)  # model's residuals

tour_data_nomiss <- na.omit(tour_data)
set.seed(1406)   # for reproducibility
tour_boot <- matrixpls.boot(tour_data_nomiss, model = tour_mod, R = 1000)
summary(tour_boot)

educ_data <- read.csv(file.path(path_data, "education.csv"))
educ_data <- educ_data[2:24]
educ_data$sup.appre <- 8 - educ_data$sup.under
educ_data$loy.pleas <- 8 - educ_data$loy.asha
educ_data <- educ_data[,
  -match(c("sup.under", "loy.asha"), colnames(educ_data))]
educ_mod <- "
  Support =~ sup.help + sup.appre + sup.safe + sup.conc
  Advising =~ adv.comp + adv.acces + adv.comm + adv.qual
  Tutoring =~ tut.prof + tut.sched + tut.stud + tut.qual
  Value =~ val.devel + val.deci + val.meet + val.info
  Satisfaction =~ sat.glad + sat.expe + sat.over
  Loyalty =~ loy.proud + loy.recom + loy.pleas + loy.back
  Value ~ Support + Advising + Tutoring
  Satisfaction ~ Support + Advising + Tutoring + Value
  Loyalty ~ Satisfaction
"
set.seed(1406)   # for reproducibility
educ_boot <- matrixpls.boot(educ_data, model = educ_mod,
  R = 1000, disattenuate = TRUE,
  parametersReflective = estimator.plscLoadings,
  innerEstim = innerEstim.centroid, tol = 1e-06)
summary(educ_boot)

## The semPLS package ##
if (!require(semPLS, quietly = TRUE)) {
  install.packages("semPLS")
}
library(semPLS)

tour_mm <- c(
  "entertain", "ACTIVITY",
  "visittown", "ACTIVITY",
  "nature", "ACTIVITY",
  "fishing", "ACTIVITY",
  "MOTIVES", "energy",
  "MOTIVES", "getaway",
  "MOTIVES", "boredom",
  "MOTIVES", "exciting",
  "SATISFACTION", "recommend",
  "SATISFACTION", "satisf",
  "SATISFACTION", "expecta"
)
tour_mm <- matrix(tour_mm, nrow = 11, ncol = 2, byrow = TRUE)
colnames(tour_mm) <- c("source", "target")

tour_sm <- c(
  "ACTIVITY", "SATISFACTION",
  "MOTIVES", "SATISFACTION"
)
tour_sm <- matrix(tour_sm, nrow = 2, ncol = 2, byrow = TRUE)
colnames(tour_sm) <- c("source", "target")

tour_mod <- plsm(data = tour_data, strucmod = tour_sm, measuremod = tour_mm)
tour_mod
# mvpairs(model = tour_mod, data = tour_data, LVs = "ACTIVITY")
# mvplot(model = tour_mod, data = tour_data, LVs = "MOTIVES")

tour_res <- sempls(model = tour_mod, data = tour_data,
  wscheme = "pathWeighting")

print(pathCoeff(tour_res), digits = 5)
print(plsLoadings(tour_res), digits = 5)
print(totalEffects(tour_res), digits = 5)
# print(coef(tour_res), digits = 5)
# print(communality(tour_res), digits = 5)
# densityplot(tour_res, use = "fscores")
# print(dgrho(tour_res), digits = 5)
# print(gof(tour_res), digits = 5)
# pathDiagram(tour_res)
# plot(tour_res)
# print(plsWeights(tour_res), digits = 5)
# print(tour_res, digits = 5)
# print(qSquared(tour_res), digits = 5)
# print(redundancy(tour_res), digits = 5)
# print(rSquared(tour_res), digits = 5)
# print(rSquared2(tour_res), digits = 5)

set.sedd(1406)
tour_boot <- bootsempls(tour_res, nboot = 1000)
print(tour_boot, digits = 5)
summary(tour_boot, type = "perc", level = 0.95)
# densityplot(tour_boot, pattern = "beta")
# parallelplot(tour_boot, pattern = "beta", reflinesAt = c(0, 0.5),
  # alpha = 0.3, type = "perc",
  # main = "Path Coefficients\nof 1000 bootstrap samples")

## The plspm package ##
if (!require(plspm, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
  install_github("gastonstat/plspm")
}
library(plspm)

ACTIVITY <- c(0, 0, 0)
MOTIVES <- c(0, 0, 0)
SATISFACTION <- c(1, 1, 0)
tour_path <- rbind(ACTIVITY, MOTIVES, SATISFACTION)
colnames(tour_path) <- rownames(tour_path)
# tour_path

tour_blocks <- list(5:8, 1:4, 9:11)

tour_modes <- c("B", "A", "A")

tour_res <- plspm(Data = tour_data_nomiss,
  path_matrix = tour_path,
  blocks = tour_blocks,
  modes = tour_modes,
  scheme = "path", tol = 1e-7)
summary(tour_res)

# innerplot(tour_res)
# outerplot(tour_res)

set.seed(1406)
tour_boot <- plspm(Data = tour_data_nomiss,
  path_matrix = tour_path,
  blocks = tour_blocks,
  modes = tour_modes, scheme = "path",
  tol = 1e-7, boot.val = TRUE, br = 1000)
# summary(tour_boot)
tour_boot$boot

## The cSEM package ##
if (!require(cSEM, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
  install_github("M-E-Rademaker/cSEM")
}
library(cSEM)

tour_mod <- "
  # measurement model
  ACTIVITY <~ entertain + visittown + nature + fishing
  MOTIVES =~ energy + getaway + boredom + exciting
  SATISFACTION =~ recommend + satisf + expecta
  # structural model
  SATISFACTION ~ ACTIVITY + MOTIVES
"
tour_res <- csem(.data = tour_data_nomiss,
  .model = tour_mod, .PLS_weight_scheme_inner = "path",
  .disattenuate = FALSE, .tolerance = 1e-07)
summarize(tour_res)
assess(tour_res)

tour_boot <- csem(.data = tour_data_nomiss,
  .model = tour_mod, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000,
  .seed = 1406)
summarize(tour_boot, .ci = "CI_percentile")
# infer(tour_boot, .quantity = "CI_percentile")
