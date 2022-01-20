### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week02/week02_monte_carlo_integration/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:136-138
###################################################
g <- function(u) { log(1/u)^3 }
mean(g(runif(1000000)))
g(runif(1000000))


###################################################
### code chunk number 3: lecture-core.Rnw:178-179
###################################################
set.seed(393921292)


###################################################
### code chunk number 4: lecture-core.Rnw:183-187
###################################################
g <- function(x) {
  1/(2^x)
}
h <- function(x) { g(x) / dexp(x) } ## R's exp. density function


###################################################
### code chunk number 5: lecture-core.Rnw:190-193
###################################################
k <- 100000
hX <- h(rexp(k))
mean(hX)


###################################################
### code chunk number 6: lecture-core.Rnw:196-197
###################################################
1/log(2)


###################################################
### code chunk number 7: lecture-core.Rnw:232-234
###################################################
g <- function(u) { log(1/u)^3 }
ms <- map_dbl(2:20, ~ mean(g(runif(2^.x))))


###################################################
### code chunk number 8: lecture-core.Rnw:237-240
###################################################
par(mar = c(4,4,0,0))
plot(ms, xlab = "Sample Size (log_2)", ylab = "Estimate", type = 'b')
abline(h = 6, col ="red", lty = 2)


###################################################
### code chunk number 9: lecture-core.Rnw:280-281
###################################################
mean(hX)


###################################################
### code chunk number 10: lecture-core.Rnw:285-287
###################################################
## t.test will do other things, we only need the conf.int
t.test(hX, conf.level = 0.999)$conf.int 


###################################################
### code chunk number 11: lecture-core.Rnw:292-298
###################################################
js <- seq(100, length(hX), length.out = 100) %>% round
cis <- sapply(js, function(j) { t.test(hX[1:j], conf.level = 0.999)$conf.int})
tmp <- data.frame(t(cis))
colnames(tmp) <- c("lower", "upper")
tmp$size <- js
ggplot(tmp, aes(x = size)) + geom_line(aes(y = lower)) + geom_line(aes(y = upper))


###################################################
### code chunk number 12: lecture-core.Rnw:329-332
###################################################
k <- 100 ## intentionally small sample!
thetas <- seq(0, 5, length.out = 100)
estimated_means <- map_dbl(thetas, ~ mean(rbeta(k, .x, 1)))


###################################################
### code chunk number 13: lecture-core.Rnw:334-340
###################################################
df <- data.frame(x = thetas, y = estimated_means)
ggplot(df, aes(x = x, y = y)) + 
    geom_line(mapping = aes(color = "Monte Carlo")) +
    stat_function(fun = function (x) { x / (1 + x)}, mapping = aes(color = "Analytical")) +
    scale_color_manual(values = c("blue", "red")) +
    labs(x = "theta", y = "E(X)")


###################################################
### code chunk number 14: lecture-core.Rnw:351-354
###################################################
k <- 100000
x <- rbeta(k, 2, 2)
y <- x / (1 - x)


###################################################
### code chunk number 15: lecture-core.Rnw:361-362
###################################################
ggplot(data.frame(x = y), aes(x = x)) + geom_density(fill = "blue", alpha = 0.5) + lims(x = c(0, 20))


###################################################
### code chunk number 16: lecture-core.Rnw:367-370
###################################################

mean(y) ## should be about 2 / (2 - 1) = 2
t.test(y)$conf.int


###################################################
### code chunk number 17: lecture-core.Rnw:389-392
###################################################
f <- function(x) { 1 / (x * sqrt(2 * pi)) * exp(- log(x)^2 / 2)}
gW <- 1.25 * f(runif(10e6, min = 0, max = 1.25))
mean(gW)


###################################################
### code chunk number 18: lecture-core.Rnw:395-396
###################################################
plnorm(1.25)


###################################################
### code chunk number 19: lecture-core.Rnw:402-403
###################################################
mean(gW)


###################################################
### code chunk number 20: lecture-core.Rnw:406-407
###################################################
t.test(gW, conf.level = 0.99)$conf.int


###################################################
### code chunk number 21: lecture-core.Rnw:471-473
###################################################
xs <- rnorm(1e5)
mean(xs <= 1.96)


###################################################
### code chunk number 22: lecture-core.Rnw:480-484
###################################################
par(mar = c(4, 4, 2, 0))
grid <- seq(-2, 2, length.out = 200)
plot(grid, sapply(grid, function(g) { mean(xs <= g)}), type = 'l', xlab = "x", ylab = "F(x)")
curve(pnorm(x), add = TRUE, col = "red", lty = 2, lwd = 3)


###################################################
### code chunk number 23: lecture-core.Rnw:506-509
###################################################
xs <- exp(rnorm(10e6)) ## rnorm gives random N(0,1)
mean(xs <= 1.25)
binom.test(sum(xs <= 1.25), n = length(xs))$conf.int


###################################################
### code chunk number 24: lecture-core.Rnw:542-546
###################################################
par(mar = c(4,4,0,0))
curve(x * (1 - x), from = 0, to = 1)
abline(v = 0.5, lty = 3)
abline(h = 0.25, lty = 3)


###################################################
### code chunk number 25: lecture-core.Rnw:552-553
###################################################
(targetN <- 4 * qnorm(0.975)^2 * 0.25 / 0.001^2)


###################################################
### code chunk number 26: lecture-core.Rnw:556-559
###################################################
gxs <- rnorm(targetN) <= 1.96
(ci <- t.test(gxs, conf.level = 0.95)$conf.int)
diff(ci)


###################################################
### code chunk number 27: lecture-core.Rnw:570-571
###################################################
hY <- exp(rnorm(10e6)) <= 1.25


###################################################
### code chunk number 28: lecture-core.Rnw:573-575
###################################################
c(mean(gW), var(gW)) # based on f(runif(10e6, max = 1.25))
c(mean(hY), var(hY)) # based on exp(rnorm(10e6)) <= 1.25


###################################################
### code chunk number 29: lecture-core.Rnw:587-590
###################################################
x <- rpois(10e6, lambda = 2) %% 2 == 0 # Vectorized computation
mean(x)
t.test(x)$conf.int


