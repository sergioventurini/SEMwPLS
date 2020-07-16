### Appendix: R Commands ###

if (!require(systemfit, quietly = TRUE)) {
  install.packages("systemfit")
}
if (!require(bootstrap, quietly = TRUE)) {
  install.packages("bootstrap")
}
if (!require(numDeriv, quietly = TRUE)) {
  install.packages("numDeriv")
}
library(cSEM)

path_data <- ""    # place here the path to store data
path_figures <- "" # place here the path to store figures
path_code <- ""    # place here the path where to load code
RNGversion("3.5.0")
if (getOption("scipen") <= 0) options(scipen = 5)

source(file.path(path_code, "R", "mediate.R"))

# Example 1: a single observed mediator variable
wage_data <- read.csv(file.path(path_data,
  "wageed.csv"))

wage_mod <- "
  # measurement model
  Age =~ age
  Tenure =~ tenure
  Wage =~ wage

  # structural model
  Tenure ~ Age
  Wage ~ Tenure + Age
"

wage_res <- csem(.data = wage_data,
  .model = wage_mod, .PLS_weight_scheme_inner = "path",
  .disattenuate = FALSE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000)
# summarize(wage_res)

wage_hat <- as.data.frame(getConstructScores(wage_res)$Construct_scores)
tenure_frm <- as.formula(Tenure ~ Age)
wage_frm <- as.formula(Wage ~ Tenure + Age)
frmlist <- list(Tenure = tenure_frm, Wage = wage_frm)
dep <- "Wage"
med <- "Tenure"
indep <- "Age"

set.seed(1406)
wage_med <- mediate(frmlist, wage_hat, indep, med, dep,
  B = 1000, fit = wage_res)
mediate_print(wage_med, rit = TRUE, rid = TRUE, zlc = TRUE,
  digits = 4)

# Example 2: a single latent mediator variable
if (!require(haven, quietly = TRUE)) {
  install.packages("haven")
}

envbehav_data <- read_stata(file =
  file.path(path_data, "ch5_envbehav.dta"))
envbehav_data <- as.data.frame(envbehav_data)     # convert to data.frame
envbehav_data <- envbehav_data[, -c(11, 12, 13)]  # remove unneeded columns
envbehav_data <- na.omit(envbehav_data)           # remove missing values

envbehav_mod <- "
  # measurement model
  EnvConcern =~ sp1e + sp1m + sp1o
  PersNorm =~ sp3a + sp3b + sp3c
  EnvBehavInt =~ sp2a + sp2b + sp2c + sp2d

  # structural model
  PersNorm ~ EnvConcern
  EnvBehavInt ~ PersNorm + EnvConcern
"

envbehav_res <- csem(.data = envbehav_data,
  .model = envbehav_mod, .PLS_weight_scheme_inner = "path",
  .disattenuate = FALSE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000)
# summarize(envbehav_res)

envbehav_hat <- as.data.frame(getConstructScores(envbehav_res)$Construct_scores)
PersNorm_frm <- as.formula(PersNorm ~ EnvConcern)
EnvBehavInt_frm <- as.formula(EnvBehavInt ~ PersNorm + EnvConcern)
frmlist <- list(PersNorm = PersNorm_frm, EnvBehavInt = EnvBehavInt_frm)
dep <- "EnvBehavInt"
med <- "PersNorm"
indep <- "EnvConcern"

set.seed(1406)
envbehav_med <- mediate(frmlist, envbehav_hat, indep, med, dep,
  B = 1000, fit = envbehav_res)
mediate_print(envbehav_med, rit = TRUE, rid = TRUE, zlc = TRUE)

# Example 3: multiple latent mediator variables
envbehav_data <- read_stata(file =
  file.path(path_data, "ch5_envbehav.dta"))
envbehav_data <- as.data.frame(envbehav_data)     # convert to data.frame
envbehav_data <- na.omit(envbehav_data)           # remove missing values

envbehav_mod <- "
  # measurement model
  EnvConcern =~ sp1e + sp1m + sp1o
  PersNorm =~ sp3a + sp3b + sp3c
  EnvBehavInt =~ sp2a + sp2b + sp2c + sp2d
  IndValues =~ sp8_6 + sp8_7 + sp8_9

  # structural model
  EnvConcern ~ IndValues
  PersNorm ~ IndValues + EnvConcern
  EnvBehavInt ~ PersNorm + EnvConcern + IndValues
"

envbehav_res <- csem(.data = envbehav_data,
  .model = envbehav_mod, .PLS_weight_scheme_inner = "path",
  .disattenuate = FALSE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000)
# summarize(envbehav_res)

envbehav_hat <- as.data.frame(getConstructScores(envbehav_res)$Construct_scores)
EnvConcern_frm <- as.formula(EnvConcern ~ IndValues)
PersNorm_frm <- as.formula(PersNorm ~ IndValues + EnvConcern)
EnvBehavInt_frm <- as.formula(EnvBehavInt ~ PersNorm + EnvConcern + IndValues)
frmlist <- list(EnvConcern = EnvConcern_frm, PersNorm = PersNorm_frm,
  EnvBehavInt = EnvBehavInt_frm)
dep <- "EnvBehavInt"
med <- "PersNorm"
indep <- "IndValues"
set.seed(1406)
envbehav_med <- mediate(frmlist, envbehav_hat, indep, med, dep,
  B = 1000, fit = envbehav_res)
mediate_print(envbehav_med, rit = TRUE, rid = TRUE, zlc = TRUE)

dep <- "EnvBehavInt"
med <- "EnvConcern"
indep <- "IndValues"
set.seed(1406)
envbehav_med <- mediate(frmlist, envbehav_hat, indep, med, dep,
  B = 1000, fit = envbehav_res)
mediate_print(envbehav_med, rit = TRUE, rid = TRUE, zlc = TRUE)
