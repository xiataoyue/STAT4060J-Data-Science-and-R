### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week06/week06_complex_data_resampling/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:82-85
###################################################
load("../../../../data/gamoran.rda")
gamoran$PH.AZ <- gamoran$PHOENIX == "(0) Non-Phoenix"
gamoran <- gamoran[!is.na(gamoran$READ_PCTZ) & !is.na(gamoran$MATH_PCTZ), ]


###################################################
### code chunk number 3: lecture-core.Rnw:89-90
###################################################
ggplot(gamoran, aes(x = READ_PCTZ, y = MATH_PCTZ)) + geom_point()


###################################################
### code chunk number 4: lecture-core.Rnw:95-101
###################################################
library(boot)
median_boot <- function(x, index) {
  xstar <- x[index, ] 
  cor(xstar$READ_PCTZ, xstar$MATH_PCTZ)
}
cor_boot <- boot(gamoran, statistic = median_boot, R = 100)


###################################################
### code chunk number 5: lecture-core.Rnw:106-107
###################################################
boot.ci(cor_boot, type = "basic")


###################################################
### code chunk number 6: lecture-core.Rnw:133-140
###################################################
rho <- 0.75
k <- 100
xs <- numeric(k)
xs[1] <- 0
for (i in 2:k) {
    xs[i] <- rho * xs[i - 1] + (rbeta(1, 2, 2) - 0.5)
}


###################################################
### code chunk number 7: lecture-core.Rnw:145-146
###################################################
plot(xs, type = 'l')


###################################################
### code chunk number 8: lecture-core.Rnw:152-153
###################################################
(est <- lm(xs[2:k] ~ xs[1:(k - 1)] - 1)) # no intercept term


###################################################
### code chunk number 9: lecture-core.Rnw:159-160
###################################################
library(boot)


###################################################
### code chunk number 10: lecture-core.Rnw:173-184
###################################################
rhohat <- coef(est)
et_hat <- xs[2:k] - predict(est) # estimate residuals
est_rho_stat <- function(x, index) {
  estar <- x[index]
  new_series <- numeric(k)
  new_series[1] <- xs[1] # starts at the same point
  for (i in 2:k) {
    new_series[i] <- rhohat * new_series[i - 1] + estar[i]
  }
  coef(lm(new_series[2:k] ~ new_series[1:(k - 1)] - 1))
}


###################################################
### code chunk number 11: lecture-core.Rnw:190-192
###################################################
boot_rho <- boot(et_hat, est_rho_stat, R = 1000)
boot.ci(boot_rho, type = "perc")


###################################################
### code chunk number 12: lecture-core.Rnw:226-235
###################################################
mean_diff <- function(x, index) { 
    xstar <- x[index, ] # boot will handle stratification for us
    mean(xstar$READ_PCTZ[xstar$PH.AZ], na.rm = TRUE) -
        mean(xstar$READ_PCTZ[!xstar$PH.AZ], na.rm = TRUE)
}
gam.boot <- boot(gamoran, 
                 statistic = mean_diff,
                 strata = gamoran$PH.AZ,
                 R = 1000)


###################################################
### code chunk number 13: lecture-core.Rnw:240-241
###################################################
(gbci <- boot.ci(gam.boot, type = "basic"))


###################################################
### code chunk number 14: lecture-core.Rnw:247-248
###################################################
gbci$basic[,4:5]


