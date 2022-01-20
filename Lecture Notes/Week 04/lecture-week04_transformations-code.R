### R code from vignette source '/home/mark/Documents/Teaching/umich/stats406-fall2020/lectures/week04/week04_transformations/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:111-113
###################################################
par(mar = c(2, 0, 0, 0))
curve(1/2 * exp( -1 * abs(x - 2)), from = -1, to = 5)


###################################################
### code chunk number 3: lecture-core.Rnw:177-179
###################################################
rlaplace <- function(n, theta) { 
    sample(c(-1, 1), n, replace = T) * rexp(n) + theta }


###################################################
### code chunk number 4: lecture-core.Rnw:184-186
###################################################
ggplot(data.frame(y = rlaplace(10000, theta = 2)), aes(x = y)) + geom_density(fill = "blue", alpha = 0.5) + 
stat_function(fun = function(x) { 1/2 * exp(-1 * abs(x - 2))}, lwd = 2, color = "red")


