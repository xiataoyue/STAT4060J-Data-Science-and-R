### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week10/week10_density_estimation/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:53-57
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:91-92
###################################################
x <- rnorm(1000, mean = 3 * rbinom(100, size = 1, 0.25))


###################################################
### code chunk number 3: lecture-core.Rnw:95-97
###################################################
par(mar = c(2,2,0,0))
curve(0.75 * dnorm(x) + 0.25 * dnorm(x, mean = 3), xlim = c(-3, 6), xlab = NA, ylab = NA)


###################################################
### code chunk number 4: lecture-core.Rnw:114-116
###################################################
par(mar = c(2, 4, 1, 1))
plot(hist(x, prob = TRUE, main = NA, xlab = NA))


###################################################
### code chunk number 5: lecture-core.Rnw:130-141
###################################################
par(mar = c(2, 3, 1, 1))
xrng <- range(x)
nbins <- 15
h <- diff(xrng) / nbins
k <- 3
breaks <- seq(xrng[1] - h, xrng[2] + h, by = h)
cols <- hcl.colors(k + 1)
hist(x, prob = TRUE, breaks = breaks, col = NA, main = NA,  ylim = c(0, 0.3), border = cols[1])
for (i in 1:k) {
    hist(x, prob = TRUE, breaks = breaks + i * h / k, border =  cols[i + 1],  add = TRUE, col = NA)
}


###################################################
### code chunk number 6: lecture-core.Rnw:152-154
###################################################
library(ash)
ash_x <- ash1(bin1(x), 5, kopt = c(0,0))


###################################################
### code chunk number 7: lecture-core.Rnw:160-162
###################################################
plot(stepfun(ash_x$x, c(0, ash_x$y)), do.points = FALSE, ylim = c(0, 0.3), main = NA)
curve(0.75 * dnorm(x) + 0.25 * dnorm(x, mean = 3), xlim = c(-3, 6), col = "red", add = TRUE)


###################################################
### code chunk number 8: lecture-core.Rnw:181-188
###################################################
par(mfrow = c(2, 3))
for (i in 1:6) {
    j <- 2 * i - 1
    xs <- seq(-1, 1, length.out = 2 * j - 1)
    ys <- c(seq(0, 1, length.out = j), seq(1, 0, length.out = j) )
    plot(stepfun(xs, ys), xlim = c(-1, 1), main = paste("m =", j))
}


###################################################
### code chunk number 9: lecture-core.Rnw:195-197
###################################################
par(mar = c(0,0,0,0))
curve(1 - abs(x), xlim = c(-1,1), main = NA)


###################################################
### code chunk number 10: lecture-core.Rnw:204-205
###################################################
fx <- density(x, kernel = "triangular")


###################################################
### code chunk number 11: lecture-core.Rnw:209-211
###################################################
fx$x[1:5]
fx$y[1:5]


###################################################
### code chunk number 12: lecture-core.Rnw:216-219
###################################################
plot(fx, ylim = c(0, 0.3))
truef <- function(x) { 0.75 * dnorm(x) + 0.25 * dnorm(x, mean = 3) }
curve(truef(x), add = TRUE, col = "red")


###################################################
### code chunk number 13: lecture-core.Rnw:224-226
###################################################
ggplot(data.frame(x), aes(x = x)) + geom_density(kernel = 'triangular') +
  stat_function(fun = truef, col = 'red')


###################################################
### code chunk number 14: lecture-core.Rnw:268-270
###################################################
load("../../../../data/nhanes/nhanes.rda")
nhanes <- filter(nhanes, !is.na(dia_mean), !is.na(sys_mean))


###################################################
### code chunk number 15: lecture-core.Rnw:274-275
###################################################
ggplot(nhanes, aes(x = sys_mean, y = dia_mean)) + geom_point() 


###################################################
### code chunk number 16: lecture-core.Rnw:280-282
###################################################
library(MASS)
contour(kde2d(nhanes$sys_mean, nhanes$dia_mean))  


###################################################
### code chunk number 17: lecture-core.Rnw:324-333
###################################################
par(mar = c(2, 0, 1, 0))
dmix <- function(x) {  dnorm(x, mean = 0, sd = 1/2) + 
                       dnorm(x, mean = 1, sd = 1/2) +
                       dnorm(x, mean = 3, sd = 1/2)
}
curve(2 * dmix(x), -2, 5)
curve(dnorm(x, mean = 0, sd = 1/2), add = T, lty = 2)
curve(dnorm(x, mean = 1, sd = 1/2), add = T, lty = 2)
curve(dnorm(x, mean = 3, sd = 1/2), add = T, lty = 2)


###################################################
### code chunk number 18: lecture-core.Rnw:380-381
###################################################
ggplot(nhanes, aes(x = dia_mean, y = sys_mean)) + geom_point(alpha = 0.5)


###################################################
### code chunk number 19: lecture-core.Rnw:428-433
###################################################
## default bandwidth is 0.5
bp_smooth <- ksmooth(nhanes$dia_mean, 
                     nhanes$sys_mean, 
                     kernel = "normal")
str(bp_smooth)


###################################################
### code chunk number 20: lecture-core.Rnw:438-440
###################################################
plot(nhanes$dia_mean, nhanes$sys_mean, xlab = "Diastolic", ylab = "Systolic", col = "#00000099", cex = 0.5)
lines(bp_smooth, col = "red")


###################################################
### code chunk number 21: lecture-core.Rnw:445-449
###################################################
bp_smooth_2 <- ksmooth(nhanes$dia_mean, 
                       nhanes$sys_mean, 
                       kernel = "normal", bandwidth = 10)
bp_linea <- lm(sys_mean ~ dia_mean, data = nhanes)


###################################################
### code chunk number 22: lecture-core.Rnw:455-458
###################################################
plot(nhanes$dia_mean, nhanes$sys_mean, xlab = "Diastolic", ylab = "Systolic", col = "#00000099", cex = 0.5)
lines(bp_smooth_2, col = "red")
abline(a= coef(bp_linea)[1], b = coef(bp_linea)[2], col = "blue")


###################################################
### code chunk number 23: lecture-core.Rnw:465-474
###################################################
par(mar = c(4,4, 2, 0))
plot(nhanes$dia_mean, nhanes$sys_mean, xlab = "Diastolic", ylab = "Systolic", col = "#AAAAAA")
cols <- rainbow(5)
hs <- c(0.5,  1, 2, 10, 20)
for (i in 1:5) {
sm <- ksmooth(nhanes$dia_mean, nhanes$sys_mean, kernel = "normal", bandwidth = hs[i])
lines(sm, col = cols[i], lwd = 2)
}
legend("bottomright", fill = cols, legend = hs)


###################################################
### code chunk number 24: lecture-core.Rnw:486-490
###################################################
range_d1 <- range(nhanes$dia_mean[nhanes$taking_aspirin])
range_d2 <- range(nhanes$dia_mean[!nhanes$taking_aspirin])
bounds <- c(max(range_d1[1], range_d2[1]), 
            min(range_d1[2], range_d2[2]))


###################################################
### code chunk number 25: lecture-core.Rnw:496-502
###################################################
compute_smoother <- function(y, x, z) {
    # we'll use the optimal bandwidth from before, 
    # but this could be computed within groups
    ksmooth(x[z], y[z], bandwidth = 7.6,
            n.points = 1000, range = bounds)
}


###################################################
### code chunk number 26: lecture-core.Rnw:504-508
###################################################
take_sm <- with(nhanes, 
                compute_smoother(sys_mean, dia_mean, taking_aspirin))
nott_sm <- with(nhanes, 
                compute_smoother(sys_mean, dia_mean, !taking_aspirin))


###################################################
### code chunk number 27: lecture-core.Rnw:514-520
###################################################
plot(take_sm, type = 'l',
     xlim = bounds,
     ylim = range(c(take_sm$y, nott_sm$y), na.rm = TRUE, finite = TRUE))

lines(nott_sm, lty = 2)
legend("bottomright", lty = 1:2, c("Taking", "Not taking"))


###################################################
### code chunk number 28: lecture-core.Rnw:528-529
###################################################
smoothed_differences <- take_sm$y - nott_sm$y


###################################################
### code chunk number 29: lecture-core.Rnw:532-535
###################################################
par(mar = c(4, 4, 1, 0))
plot(take_sm$x, smoothed_differences, type = 'l')
abline(h = 0, col = "red")


###################################################
### code chunk number 30: lecture-core.Rnw:540-541
###################################################
library(boot)


###################################################
### code chunk number 31: lecture-core.Rnw:544-555
###################################################
mu_diff <- function(data, index) {
  xstar <- data[index, ]

  s1 <- with(xstar, 
             compute_smoother(sys_mean, dia_mean, taking_aspirin))

  s2 <- with(xstar, 
             compute_smoother(sys_mean, dia_mean, !taking_aspirin))

  return(s1$y - s2$y)
}


###################################################
### code chunk number 32: lecture-core.Rnw:560-566
###################################################
mu_boot <- boot(nhanes, 
  mu_diff, 
  strata = nhanes$taking_aspirin, R = 100)

point_ci <- apply(mu_boot$t, 2, 
                  quantile, probs = c(0.025, 0.975), na.rm = TRUE)


###################################################
### code chunk number 33: lecture-core.Rnw:573-579
###################################################
par(mar = c(4, 4, 1, 0))
plot(take_sm$x, smoothed_differences, type = 'l',
     xlab = "Diastolic", ylab = "Difference of Systolic Mean")
lines(take_sm$x, point_ci[1, ], col = "blue")
lines(take_sm$x, point_ci[2, ], col = "blue")
abline(h = 0, col = "red")


ksmoothCV.Rnw
