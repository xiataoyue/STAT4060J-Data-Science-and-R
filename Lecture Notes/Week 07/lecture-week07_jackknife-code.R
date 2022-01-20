### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week07/week07_jackknife/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:88-98
###################################################
load("../../../../data/nhanes/nhanes.rda")
sys_dia <- na.omit(nhanes[, c('sys_mean', 'dia_mean')])
sys_dia <- sys_dia[sys_dia$sys_mean > 0 & sys_dia$dia_mean > 0, ]
sys_mean <- sys_dia$sys_mean
dia_mean <- sys_dia$dia_mean
trimmed_mean <- function(x, p) {
    xqs <- quantile(x, c(p/2, 1 - (p/2)))
    keep <- x > xqs[1] & x < xqs[2]
    mean(x[keep])
}


###################################################
### code chunk number 3: lecture-core.Rnw:102-108
###################################################
(observed_trim <- trimmed_mean(sys_mean, p = 0.2))
B <- 10000
n <- length(sys_mean)
bootstrap_samples <- rerun(B,
                           sample(sys_mean, size = n, replace = TRUE))
bootstrap_trims <- map_dbl(bootstrap_samples, trimmed_mean, p = 0.2)


###################################################
### code chunk number 4: lecture-core.Rnw:113-115
###################################################
df <- data.frame(statistic = "trimmed", x = bootstrap_trims)
ggplot(df, aes(x = x))   + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 5: lecture-core.Rnw:119-121
###################################################
observed_mean <- mean(sys_mean)
bootstrap_means <- map_dbl(bootstrap_samples, mean)


###################################################
### code chunk number 6: lecture-core.Rnw:125-129
###################################################
## bias
mean(bootstrap_trims - observed_trim)
## MSE
mean((bootstrap_trims - observed_trim)^2)


###################################################
### code chunk number 7: lecture-core.Rnw:133-134
###################################################
mean((bootstrap_means - observed_mean)^2)


