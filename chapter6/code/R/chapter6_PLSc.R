path_data <- ""    # place here the path to store data
path_figures <- "" # place here the path to store figures
RNGversion("3.5.0")
if (getOption("scipen") <= 0) options(scipen = 5)

### Figure 6.3 ###
pdf(file = file.path(path_figures, "fig6_3.pdf"))
par(mar = c(2, 2, 1, 1))
x <- c(1, 5)
y <- c(3, 7)
plot(x, y, type = "n", xlim = c(0, 6), ylim = c(0, 8), axes = FALSE, xlab = "", ylab = "")
axis(side = 1)
axis(side = 2, at = seq(0, 8, by = 1), )
grid(ny = NA)
for (i in 0:8) abline(h = i, col = "lightgray", lty = "dotted", lwd = par("lwd"))
box()
for (i in 1:4) {
	segments(i, 2 + i, i + 1, 2 + i, lwd = 5, col = "red2")
	segments(i + 1, 2 + i, i + 1, 3 + i, lwd = 5, col = "red2")
}
segments(x[1], y[1], x[2], y[2], lwd = 5, col = "dodgerblue2")
dev.off()

### Appendix: R Commands ###

if (!require(haven, quietly = TRUE)) {
  install.packages("haven")
}

curios_data <- as.data.frame(read_stata(file =
  file.path(path_data, "ch6_CultureCuriosity.dta")))

## Application of the product-indicator approach ##
curios_data_std <- scale(curios_data[, 1:8])
colnames(curios_data_std) <- paste0(colnames(curios_data_std), "_std")
curios_data <- cbind(curios_data, curios_data_std)
curios_data <- na.omit(curios_data)

curios_data_pi <- model.matrix(
  object = ~ (V1A_std + V1B_std + V1C_std + V2A_std + V2E_std + V2F_std)^2
    + V3A + V3B - 1, data = curios_data)

curios_mod_pi_1 <- "
  # structural model
  H_INTEREST ~ CULTURE + CURIOSITY + CULTURE_CURIOSITY

  # measurement model
  CULTURE =~ V1A_std + V1B_std + V1C_std
  CURIOSITY =~ V2A_std + V2E_std + V2F_std
  H_INTEREST =~ V3A + V3B
"
curios_mod_pi_2 <- "
  CULTURE_CURIOSITY =~ V1A_std:V2A_std + V1A_std:V2E_std +
    V1A_std:V2F_std + V1B_std:V2A_std + V1B_std:V2E_std +
    V1B_std:V2F_std + V1C_std:V2A_std + V1C_std:V2E_std +
    V1C_std:V2F_std
"
curios_mod_pi <- paste0(curios_mod_pi_1, gsub("\n", "", curios_mod_pi_2))

library(cSEM)

curios_res_pi <- csem(.data = curios_data_pi,
  .model = curios_mod_pi, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000,
  .handle_inadmissibles = "ignore", .seed = 101)
# summarize(curios_res_pi)

## Application of the two-stage approach ##
## first stage
curios_mod_2s_step1 <- "
  # measurement model
  CULTURE2 =~ V1A + V1B + V1C
  CURIOSITY2 =~ V2A + V2E + V2F
  H_INTEREST2 =~ V3A + V3B

  # structural model
  H_INTEREST2 ~ CULTURE2 + CURIOSITY2
"

curios_res_2s_step1 <- csem(.data = curios_data,
  .model = curios_mod_2s_step1, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000, .seed = 101)
# summarize(curios_res_2s_step1)

## second stage
curios_data_2s <- as.data.frame(
  getConstructScores(curios_res_2s_step1)$Construct_scores)
curios_data_2s <- model.matrix(
  object = ~ (CULTURE2 + CURIOSITY2)^2 + H_INTEREST2 - 1,
  data = curios_data_2s)

curios_mod_2s_step2 <- "
  # measurement model
  CULTURE =~ CULTURE2
  CURIOSITY =~ CURIOSITY2
  H_INTEREST =~ H_INTEREST2
  CULTURE_CURIOSITY =~ CULTURE2:CURIOSITY2

  # structural model
  H_INTEREST ~ CULTURE + CURIOSITY + CULTURE_CURIOSITY
"

curios_res_2s_step2 <- csem(.data = curios_data_2s,
  .model = curios_mod_2s_step2, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000, .seed = 101)
# summarize(curios_res_2s_step2)

## direct use of csem() with an interaction
curios_data <- as.data.frame(read_stata(file =
  file.path(path_data, "ch6_CultureCuriosity.dta")))
curios_data <- na.omit(curios_data)

curios_mod_int <- "
  # measurement model
  CULTURE =~ V1A + V1B + V1C
  CURIOSITY =~ V2A + V2E + V2F
  H_INTEREST =~ V3A + V3B

  # structural model
  H_INTEREST ~ CULTURE + CURIOSITY + CULTURE.CURIOSITY
"
curios_res_int <- csem(.data = curios_data,
  .model = curios_mod_int, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000, .seed = 101)
summarize(curios_res_int)

neffects <- doNonlinearEffectsAnalysis(curios_res_int,
  .dependent = "H_INTEREST",
  .moderator = "CURIOSITY",
  .independent = "CULTURE")
# neffects
pdf(file = file.path(path_figures, "fig6_csem_nonlinear.pdf"), width = 7,
  height = 5)
plot(neffects, .plot_type = "simpleeffects")
dev.off()
pdf(file = file.path(path_figures, "fig6_csem_floodlight.pdf"), width = 7,
  height = 5)
plot(neffects, .plot_type = "floodlight")
dev.off()

## Two-stage with a categorical moderator ##
curios_data <- as.data.frame(read_stata(file =
  file.path(path_data, "ch6_CultureCuriosity.dta")))
curios_data_cat <- na.omit(curios_data[, c("V1A", "V1B", "V1C",
  "CURIOSITY_D", "V3A", "V3B")])

curios_mod_cat_step1 <- "
  # measurement model
  CULTURE3 =~ V1A + V1B + V1C
  CURIOSITY3 =~ CURIOSITY_D
  H_INTEREST3 =~ V3A + V3B

  # structural model
  H_INTEREST3 ~ CULTURE3 + CURIOSITY3
"

curios_res_cat_step1 <- csem(.data = curios_data_cat,
  .model = curios_mod_cat_step1, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .handle_inadmissibles = "ignore",
  .resample_method = "bootstrap", .R = 1000, .seed = 101)
# summarize(curios_res_cat_step1)

curios_data_cat_2s <-
  as.data.frame(getConstructScores(curios_res_cat_step1)$Construct_scores)
curios_data_cat_2s <- model.matrix(
  object = ~ (CULTURE3 + CURIOSITY3)^2 + H_INTEREST3 - 1,
  data = curios_data_cat_2s)

curios_mod_cat_step2 <- "
  # measurement model
  CULTURE =~ CULTURE3
  CURIOSITY =~ CURIOSITY3
  H_INTEREST =~ H_INTEREST3
  CULTURE_CURIOSITY =~ CULTURE3:CURIOSITY3

  # structural model
  H_INTEREST ~ CULTURE + CURIOSITY + CULTURE_CURIOSITY
"

curios_res_cat_step2 <- csem(.data = curios_data_cat_2s,
  .model = curios_mod_cat_step2, .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000, .seed = 101)
# summarize(curios_res_cat_step2)

## Application of the multi-sample approach ##
curios_data <- as.data.frame(read_stata(file =
  file.path(path_data, "ch6_CultureCuriosity.dta")))
curios_data <- na.omit(curios_data)

curios_mod_mga <- "
  # measurement model
  CULTURE =~ V1A + V1B + V1C
  H_INTEREST =~ V3A + V3B

  # structural model
  H_INTEREST ~ CULTURE
"

curios_res_mga <- csem(.data = curios_data,
  .id = "CURIOSITY_D", .model = curios_mod_mga,
  .PLS_weight_scheme_inner = "path",
  .disattenuate = TRUE, .tolerance = 1e-07,
  .resample_method = "bootstrap", .R = 1000, .seed = 101)
summarize(curios_res_mga)

curios_mga <- testMGD(curios_res_mga, .R_bootstrap = 1000,
  .R_permutation = 1000, .seed = 1406)
print(curios_mga, .approach_mgd = "Chin")
# print(curios_mga, .approach_mgd = "Klesel")
# print(curios_mga, .approach_mgd = "Sarstedt")
# print(curios_mga, .approach_mgd = "Keil")
# print(curios_mga, .approach_mgd = "Nitzl")
# print(curios_mga, .approach_mgd = "Henseler")
# print(curios_mga, .approach_mgd = "CI_para")  # [likely bug!]
# print(curios_mga, .approach_mgd = "CI_overlap")

## Measurement model invariance ##
testMICOM(curios_res_mga, .R = 1000, .seed = 1406)
