path_data <- ""    # place here the path to store data
path_figures <- "" # place here the path to store figures
RNGversion("3.5.0")
if (getOption("scipen") <= 0) options(scipen = 5)

### Appendix: R Commands ###

set.seed(123)

x <- c(7, 4, -5, 1, -2)

B <- 200
mean_boot <- numeric(B)
for (b in 1:B) {
  x_b <- x[sample(1:length(x), replace = TRUE)]
  mean_boot[b] <- mean(x_b)
}

hist(mean_boot, col = gray(0.5, 0.3))

###

if (!require(boot, quietly = TRUE)) install.packages("boot")
library(boot)

set.seed(123)

mean_w <- function(x, i) mean(x[i])
mean_boot <- boot(data = x, statistic = mean_w, R = 200)

plot(mean_boot)

boot.ci(mean_boot, conf = 0.95,
  type = c("norm", "basic", "perc", "bca"))

###

if (!require(alr4, quietly = TRUE)) install.packages("alr4")
data(Rateprof, package = "alr4")
vars <- c("quality", "helpfulness", "clarity", "easiness", "raterInterest")

# princomp_res <- princomp(x = Rateprof[, vars], cor = TRUE)
# print(princomp_res, digits = 5)
# summary(princomp_res)
# plot(princomp_res, type = "lines")
# # screeplot(princomp_res, type = "lines")
# print(loadings(princomp_res), cut-off = 0.0)
# princomp_res$scores
# # predict(princomp_res)
# # biplot(princomp_res, choices = c(1, 2))
# str(princomp_res)
# cor(Rateprof[, vars], princomp_res$scores)

prcomp_res <- prcomp(x = Rateprof[, vars], center = TRUE, scale. = TRUE)
print(prcomp_res, digits = 5)
# summary(prcomp_res)
plot(prcomp_res, type = "lines")   # not shown
# screeplot(prcomp_res, type = "lines")
# prcomp_res$rotation
# prcomp_res$x
pairs(predict(prcomp_res))         # not shown
# biplot(prcomp_res, choices = c(1, 2))
# str(prcomp_res)
prcomp_res$rotation %*% diag(prcomp_res$sdev)
# cor(Rateprof[, vars], prcomp_res$x)

# library(pls)
# library(mixOmics)

###

if (!require(foreign, quietly = TRUE)) install.packages("foreign")
library(foreign)

simdata <- read.dta(file.path(path_data, "ch2_SimData.dta"))
simdata <- simdata[, 1:5]
pairs(simdata[, -1], pch = 19, col = gray(.5, .3))  # not shown

simdata_std <- scale(simdata[, -1])      # data standardization

simdata_dist <- dist(simdata_std, method = "euclidean")
single_linkage <- hclust(simdata_dist, method = "single")
complete_linkage <- hclust(simdata_dist, method = "complete")
average_linkage <- hclust(simdata_dist, method = "average")
ward_linkage <- hclust(simdata_dist, method = "ward.D2")
par(mfrow = c(2, 2))                                # not shown
plot(single_linkage, main = "Single linkage", cex = .4)
plot(complete_linkage, main = "Complete linkage", cex = .4)
plot(average_linkage, main = "Average linkage", cex = .4)
plot(ward_linkage, main = "Ward method", cex = .4)

par(mfrow = c(2, 2))                                # not shown
plot(single_linkage, main = "Single linkage", cex = .4)
rect.hclust(single_linkage, k = 3)
plot(complete_linkage, main = "Complete linkage", cex = .4)
rect.hclust(complete_linkage, k = 3)
plot(average_linkage, main = "Average linkage", cex = .4)
rect.hclust(average_linkage, k = 3)
plot(ward_linkage, main = "Ward method", cex = .4)
rect.hclust(ward_linkage, k = 3)

ward_gm <- cutree(ward_linkage, k = 3)           # not reported
table(ward_gm, simdata[, 1])

if (!require(cluster, quietly = TRUE)) install.packages("cluster")
library(cluster)
# agglomerative nesting (hierarchical clustering)
res_agnes <- agnes(x = simdata[, 2:5],   # data matrix
                   stand = TRUE,         # standardize the data
                   metric = "euclidean", # distance measure
                   method = "ward"       # linkage method
)
plot(res_agnes, main = "Ward method", which.plots = 2) # not shown

if (!require(factoextra, quietly = TRUE))
  install.packages("factoextra")
library(factoextra)
fviz_dend(res_agnes,                                # not shown
          k = 3,                          # cut in three groups
          cex = 0.5,                               # label size
          color_labels_by_k = TRUE,    # color labels by groups
          rect = TRUE            # add rectangles around groups
)

set.seed(301)                            # for reproducibility
wss <- numeric(10)                       # allocate memory for TWSS
for (k in 1:10) {                        # loop over different K
  res_k <- kmeans(x = simdata[, 2:5],    # data matrix
                  centers = k,           # number of clusters
                  iter.max = 100,        # max num. of iterations
                  nstart = 10            # number of restarts
           )
  wss[k] <- res_k$tot.withinss           # stores TWSS for given K
}
plot(wss, type = "b", lwd = 2, pch = 20, # not shown
  xlab = "K", ylab = "Total within-cluster sum of squares")

# set.seed(101)                            # for reproducibility
# res_k3 <- kmeans(x = simdata[, 2:5],     # data matrix
                # centers = 3,             # number of clusters
                # iter.max = 100,          # max num. of iterations
                # nstart = 10              # number of restarts
# )
# table(res_k3$cluster, ward_gm)

if (!require(NbClust, quietly = TRUE)) install.packages("NbClust")
library(NBClust)
res <- NbClust(data = simdata_std,       # data matrix
               distance = "euclidean",   # distance measure
               min.nc = 2,               # minimum num. of clusters
               max.nc = 8,               # maximum num. of clusters
               method = "complete",      # clustering algorithm
               index = "alllong")        # compute all indexes

# if (!require(fpc, quietly = TRUE)) install.packages("fpc")
# library(fpc)
# complete_gm <- cutree(complete_linkage, k = 3)  # not reported
# clus_val <- cqcluster.stats(
  # d = dist(simdata_std),                        # distance matrix
  # clustering = simdata[, 1],                    # alternative
                                                # # cl. memberships
  # alt.clustering = complete_gm)                 # cl. memberships

###

if (!require(mclust, quietly = TRUE)) install.packages("mclust")
llibrary(mclust)
res_mclust <- Mclust(simdata_std, G = 1:8)
summary(res_mclust)
# summary(res_mclust$BIC)
# plot(res_mclust, what = "BIC")
# plot(res_mclust, what = "classification")
pdf(file = file.path(path_figures, "fig2_app_mclust.pdf"))
plot(res_mclust, what = "uncertainty")
dev.off()
# plot(res_mclust, what = "density")
# predict(res_mclust)
# table(simdata[, 1], res_mclust$classification)

if (!require(flexmix, quietly = TRUE)) install.packages("flexmix")
library(flexmix)

set.seed(301)   # for reproducibility
res_flexmix <- flexmix(formula = simdata_std ~ 1,
  data = data.frame(simdata_std), k = 3,
  model = FLXMCmvnorm())
# summary(res_flexmix)
# plot(res_flexmix)

pdf(file = file.path(path_figures, "fig2_app_flexmix.pdf"))
par(mfrow = c(3, 2), mar = c(4, 4, 1, 1) + .1)
for (i in 1:(ncol(simdata_std) - 1)) {
  for (j in (i + 1):ncol(simdata_std)) {
    plotEll(res_flexmix, data = data.frame(simdata_std),
      which = c(i, j))
  }
}
dev.off()

set.seed(301)   # for reproducibility
res_flexmix_all <- stepFlexmix(formula = simdata_std ~ 1,
  data = data.frame(simdata_std), k = 1:8,
  model = FLXMCmvnorm())
print(res_flexmix_all)
# plot(res_flexmix_all)
# res_flexmix_BIC <- getModel(res_flexmix_all, "BIC")
# parameters(res_flexmix_BIC)
# flexclust::groupBWplot(data.frame(simdata_std), clusters(res_flexmix_BIC),
  # alpha = 1, strip.prefix = "Segment ")

# if (!require(mixtools, quietly = TRUE)) install.packages("mixtools")
# library(mixtools)
# set.seed(301)
# res_mixtools <- mvnormalmixEM(x = simdata_std, lambda = NULL,
  # mu = NULL, sigma = NULL, k = 3, epsilon = 1e-2)
# summary(res_mixtools)
# # plot(res_mixtools, whichplots = 2)   # does not work

###

if (!require(foreign, quietly = TRUE)) install.packages("foreign")
library(foreign)

hsb2 <- read.dta(file.path(path_data, "ch2_hsb2.dta"))

if (!require(systemfit, quietly = TRUE))
  install.packages("systemfit")
library(systemfit)
eq_math <- math ~ read + write
eq_science <- science ~ math + read + write
eqs <- list(math = eq_math, science = eq_science)
res_systemfit <- systemfit(eqs, data = hsb2)
summary(res_systemfit)

if (!require(sem, quietly = TRUE)) install.packages("sem")
library(sem)
hsb2_mod <- matrix(c(
  "math <- read", "gamma_11", NA,
  "math <- write", "gamma_12", NA,
  "science <- math", "beta_21", NA,
  "science <- read", "gamma_21", NA,
  "science <- write", "gamma_22", NA,
  "math <-> math", "s2_math", NA,
  "science <-> science", "s2_science", NA),
  ncol = 3, byrow = TRUE)
class(hsb2_mod) <- "mod"
exo_var <- c("read", "write")
endo_var <- c("math", "science")
all_var <- c(exo_var, endo_var)
res_sem <- sem::sem(model = hsb2_mod, data = hsb2, fixed.x = exo_var)
summary(res_sem)
pathDiagram(res_sem, style = "ram")

hsb2_mod <- "
  math ~ alpha_1*1 + gamma_11*read + gamma_12*write
  science ~ alpha_2*1 + beta_21*math + gamma_21*read + gamma_22*write
  math ~~ s2_math*math
  science ~~ s2_science*science
"
if (!require(lavaan, quietly = TRUE))
  install.packages("lavaan")
library(lavaan)
res_lavann <- lavaan(model = hsb2_mod, data = hsb2)
summary(res_lavann, fit.measures = TRUE, ci = TRUE,
  rsquare = TRUE, nd = 3L)

if (!require(semPlot, quietly = TRUE)) install.packages("semPlot")
library(semPlot)
semPaths(res_lavann, 'path', layout = 'tree2', nCharNodes = 0, nCharEdges = 0)
semPaths(res_lavann, 'est', layout = 'tree2', nCharNodes = 0, nCharEdges = 0)

hsb2_ind <- "
  math ~ alpha_1*1 + gamma_11*read + gamma_12*write
  science ~ alpha_2*1 + beta_21*math + gamma_21*read + gamma_22*write
  math ~~ s2_math*math
  science ~~ s2_science*science
  sc_read_ind := beta_21*gamma_11
  sc_write_ind := beta_21*gamma_12
  sc_read_tot := sc_read_ind + gamma_21
  sc_write_tot := sc_write_ind + gamma_22
"
res_indirect <- lavaan(model = hsb2_ind, data = hsb2)
summary(res_indirect, fit.measures = TRUE, ci = TRUE,
  rsquare = TRUE, nd = 3L)
