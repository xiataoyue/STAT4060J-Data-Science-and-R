### R code from vignette source '/home/mark/Documents/Teaching/sjtu/stats406-summer2021/lectures/week01/week01_statistical_review/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:174-176
###################################################
par(mar = c(4, 4, 2, 2))
curve(pnorm(x), from = -2, to = 2, ylab = "F(x)")


###################################################
### code chunk number 3: lecture-core.Rnw:206-208
###################################################
par(mar = c(4,4,2,2))
plot(stepfun(x = 0:10, c(0, pbinom(0:10, size = 10, p = 0.3))), ylab = "F(x)", main = NA)


###################################################
### code chunk number 4: lecture-core.Rnw:326-330
###################################################
terms <- map_dbl(0:10, function(i) {
  choose(10, i) * 0.3^i * 0.7^(10 - i) * i
})
sum(terms)


###################################################
### code chunk number 5: lecture-core.Rnw:339-343
###################################################
terms <- map_dbl(0:10, function(i) {
  choose(10, i) * 0.3^i * 0.7^(10 - i) * log(i + 1)
})
sum(terms)


###################################################
### code chunk number 6: lecture-core.Rnw:613-622
###################################################
par(mar = c(2, 2, 2, 0))
curve(dnorm(x), from = -2, 4, lwd = 2, col = "blue")
curve(dnorm(x, mean = 1), add = TRUE, col = "red", lwd = 2)
cut <- qnorm(0.95)
regionx <- seq(cut, 4, length.out = 200)
polygon(c(regionx, regionx[200], cut), c(dnorm(regionx, mean = 1), 0, 0), col = "#FF0000A0")
polygon(c(regionx, regionx[200], cut), c(dnorm(regionx), 0, 0), col = "#0000FFA0")
text("cutoff ->", x = cut - 0.5, y = 0.05)
legend("topright", fill = c("blue", "red"), legend = c("Type I Error", "Power"))


###################################################
### code chunk number 7: lecture-core.Rnw:629-631
###################################################
n <- 2 
(cutoff <- qnorm(0.95, mean = 0, sd = 1/n))


###################################################
### code chunk number 8: lecture-core.Rnw:635-636
###################################################
1 - pnorm(cutoff, mean = 1, sd = 1/n)


