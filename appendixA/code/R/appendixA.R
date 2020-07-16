path_data <- ""    # place here the path to store data
path_figures <- "" # place here the path to store figures
RNGversion("3.5.0") # in April 2019, R 3.6.0 changed the way it generates random numbers
                    # this command is needed to get the same pictures as in the book

### R Appendix ###
## Covariance and Correlation
if (!require(haven, quietly = TRUE)) {
  install.packages("haven")
}
library(haven)
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

nlsw88 <- read_dta(file = file.path(path_data, "nlsw88.dta"))
vars <- c("age", "hours", "ttl_exp", "wage")
pairs(nlsw88[, vars], pch = 20, col = gray(.4))

nlsw88 <- mutate(nlsw88, logwage = log(wage))  # mutate() is part of the
                                               # dplyr package
vars <- c("age", "hours", "ttl_exp", "logwage")
pairs(nlsw88[, vars], pch = 20, col = gray(.4))

cor(nlsw88[, vars], use = "complete.obs")
cor(nlsw88[, vars], use = "pairwise.complete.obs")

cor.test(x = nlsw88$logwage, y = nlsw88$age)

## Linear Regression Analysis
nlsw88$smsa <- factor(nlsw88$smsa)
nlsw88$race <- factor(nlsw88$race)
nlsw88$married <- factor(nlsw88$married)
nlsw88$union <- factor(nlsw88$union)
nlsw88$south <- factor(nlsw88$south)
nlsw88$never_married <- factor(nlsw88$never_married)
nlsw88$occupation <- factor(nlsw88$occupation)

nlsw88_lm <- lm(formula = logwage ~ grade + smsa + age +
  hours + ttl_exp + tenure + race + married + union +
  south + never_married + occupation, data = nlsw88,
  subset = (occupation != 9 & occupation != 10 &
            occupation != 12))
summary(nlsw88_lm)

if (!require(car, quietly = TRUE)) {
  install.packages("car")
}
library(car)
if (!require(lmtest, quietly = TRUE)) {
  install.packages("lmtest")
}
library(lmtest)

vif(nlsw88_lm)
bptest(nlsw88_lm)
