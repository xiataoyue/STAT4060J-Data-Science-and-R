### R code from vignette source '/home/mark/Documents/Teaching/umich/stats406-fall2020/lectures/week05/week05_importance_sampling_variance_reduction/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:91-93
###################################################
k <- 100000
sum(rnorm(k) >= 4.5)


###################################################
### code chunk number 3: lecture-core.Rnw:103-107
###################################################
ys <- rexp(k) + 4.5
ratios <- dnorm(ys) / (dexp(ys - 4.5))
mean(ratios)
(truep <- pnorm(4.5, lower.tail = FALSE))


###################################################
### code chunk number 4: lecture-core.Rnw:115-116
###################################################
truep * (1 - truep) / k


###################################################
### code chunk number 5: lecture-core.Rnw:120-121
###################################################
var(ratios) / k


###################################################
### code chunk number 6: lecture-core.Rnw:151-154
###################################################
curve(dcauchy(x), -10, 10, ylim = c(0, dnorm(0)))
curve(dnorm(x), add = TRUE, col = "red", lty = 2)
legend("topright", c("Cauchy", "Standard Normal"), col = c("black", "red"), lty = 1:2)


###################################################
### code chunk number 7: lecture-core.Rnw:158-159
###################################################
set.seed(390393)


###################################################
### code chunk number 8: lecture-core.Rnw:164-168
###################################################
k <- 1000000 # one million samples
ys <- rnorm(k)
iweights <- dcauchy(ys) / dnorm(ys)
estimates <- cumsum(iweights * (ys >= 2)) / (1:k) 


###################################################
### code chunk number 9: lecture-core.Rnw:173-176
###################################################
ks <- seq(1, k, length.out = 1000) %>% round
ggplot(data.frame(estimate = estimates[ks], n = ks), aes(x = n, y = estimate)) + geom_line() +
    geom_abline(slope = 0, intercept = pcauchy(2, lower.tail = FALSE), lty = 2)


###################################################
### code chunk number 10: lecture-core.Rnw:270-283
###################################################
par(mar = c(0, 0, 0, 0))
mx <- dnorm(1)
curve(abs(x) * dnorm(x), from = 0, to = 1, lwd = 2)
betas <- c(1, 1.5)
for (i in seq_along(betas)) {
    b <- betas[i]
    bb <- dbeta(1, b, 1)
    curve(mx * dbeta(x, b, 1) / bb, add = TRUE, lty = i + 1, lwd = 2)
}
gx <- function(x) { (3/2) * (2 * x - x^2) }
gmx <- gx(1)
curve(mx / gmx * gx(x), add = TRUE, lty = 4, lwd = 2)
legend("bottomright", lty = 1:5, legend = c("TN", "U(0,1)", "B(1.5, 1)", "Quad."))


###################################################
### code chunk number 11: lecture-core.Rnw:287-291
###################################################
Fx <- function(x) { 3/2 * (x^2 - x^3/3)}
qx <- function(u) {
    map_dbl(u, ~uniroot(function(x) { Fx(x) - .x }, interval = c(0,1))$root)
}


###################################################
### code chunk number 12: lecture-core.Rnw:295-300
###################################################
tn <- function(x) { dnorm(x) / (pnorm(1) - pnorm(0))}
k <- 10000
yg <- qx(runif(k))
iwg <- tn(yg) / gx(yg) ## h(y) f(y) / g(y)
mean(yg * iwg)


###################################################
### code chunk number 13: lecture-core.Rnw:303-306
###################################################
yr <- rbeta(k, 1.5, 1)
iwr <- tn(yr) / dbeta(yr, 1.5, 1)
mean(yr * iwr)


###################################################
### code chunk number 14: lecture-core.Rnw:312-314
###################################################
varg <- var(yg * iwg) # variance of single h(Y) f(Y) / g(Y) term
varr <- var(yr * iwr)


###################################################
### code chunk number 15: lecture-core.Rnw:318-319
###################################################
(varr - varg) / varr 


###################################################
### code chunk number 16: lecture-core.Rnw:323-324
###################################################
diff(t.test(yg * iwg)$conf.int) / diff(t.test(yr * iwr)$conf.int)


###################################################
### code chunk number 17: lecture-core.Rnw:370-375
###################################################
ys <- rexp(k) + 4.5
imp_weights <- dnorm(ys) / (dexp(ys - 4.5))
omega <-  imp_weights / sum(imp_weights) ## the 1/m term gets canceled
xs <- sample(ys, replace = TRUE, prob = omega)
median(xs)


###################################################
### code chunk number 18: lecture-core.Rnw:416-420
###################################################
fstar <- function(x) { exp(-x^2) *
  (sin(6 * x)^2 + 3 * cos(x)^2 * sin(4 * x)^2 + 1) }
par(mar = c(3,3,1,1))
curve(fstar(x), -2, 2)


###################################################
### code chunk number 19: lecture-core.Rnw:429-435
###################################################
k <- 10000
ys <- rnorm(k)
as <- fstar(ys) / dnorm(ys)
omegas <- as / sum(as)
reweighted_ys2 <- ys^2 * (k * omegas)
mean(reweighted_ys2)


###################################################
### code chunk number 20: lecture-core.Rnw:444-446
###################################################
par(mar = c(2, 2, 0, 0))
curve(x^2 * fstar(x), 0, 5)


###################################################
### code chunk number 21: lecture-core.Rnw:452-465
###################################################
par(mar = c(1,1,1,1))
curve(x^2 * fstar(x), 0, 5)
curve(3 * dchisq(x, df = 3), add = TRUE, col = "blue")
dmix_star <- function(x) {
    0.15 * (x >= 0) * dnorm(x, mean = 2/5, sd = 1/8) +
    0.28 * (x >= 0) *  dnorm(x, mean = 4/5, sd = 3/16) +
    0.28 * (x >= 0) *  dnorm(x, mean = 5/4, sd = 3/16) +
    0.08 * (x >= 0) *  dnorm(x, mean = 9/5, sd = 5/32) +
    0.21 * dexp(x, 1/2)
}

curve(dmix_star(x), add = TRUE, col = "red")
legend("topright", legend = c("Target", "Chi^2", "Mixture"), fill = c("black", "blue", "red"))


###################################################
### code chunk number 22: lecture-core.Rnw:487-488
###################################################
dmix_star


###################################################
### code chunk number 23: lecture-core.Rnw:492-510
###################################################
rtn <- function(...) { abs(rnorm(...)) }

rmix <- function(n) {
    ps <-  c(0.15, 0.28, 0.28, 0.08, 0.21)
    rvs <- matrix(ncol = 5, nrow = n, c(
                                          rtn(n, mean = 2/5, sd = 1/8),
                                          rtn(n, mean = 4/5, sd = 3/16),
                                          rtn(n, mean = 5/4, sd = 3/16),
                                          rtn(n, mean = 9/5, sd = 5/32),
                                          rexp(n, 1/2)))
    picks <- sample(1:5, size = n, 
                    replace = TRUE, prob = ps)
    result <- numeric(n)
    for (i in 1:n) {
        result[i] <- rvs[i, picks[i]]
    }
    return(result)
}


###################################################
### code chunk number 24: lecture-core.Rnw:515-518
###################################################
par(mar = c(2,2, 0, 0))
h <- rmix(10000)
plot(density(h), main = NA)


###################################################
### code chunk number 25: lecture-core.Rnw:524-529
###################################################
chi3 <- rchisq(k, df = 3)
chi3_ratios <- fstar(chi3) / dchisq(chi3, df = 3)
chi3_omegas <- chi3_ratios / sum(chi3_ratios)
chi3_x2 <- chi3^2 *  (k * chi3_omegas) # 
(chi3_est <- mean(chi3_x2))


###################################################
### code chunk number 26: lecture-core.Rnw:534-539
###################################################
mixs <- rmix(k)
mixs_ratios <- fstar(mixs) / dmix_star(mixs)
mixs_omegas <- mixs_ratios / sum(mixs_ratios)
mixs_x2 <- mixs^2 * (k * mixs_omegas)
(mixs_est <- mean(mixs_x2))


###################################################
### code chunk number 27: lecture-core.Rnw:546-549
###################################################
var(reweighted_ys2)
var(chi3_x2)
var(mixs_x2)


###################################################
### code chunk number 28: lecture-core.Rnw:612-615
###################################################
iids <- runif(10000)^(1/3)
mean(iids)
(est_var_iid <- var(iids) / 10000)


###################################################
### code chunk number 29: lecture-core.Rnw:622-626
###################################################
tmp <- runif(5000)
antis <- (tmp^(1/3) + (1 - tmp)^(1/3)) / 2
mean(antis)
(est_var_anti <- var(antis) / 5000)


###################################################
### code chunk number 30: lecture-core.Rnw:633-634
###################################################
1 - (est_var_anti / est_var_iid)


