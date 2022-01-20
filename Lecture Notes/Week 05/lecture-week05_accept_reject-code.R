### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week05/week05_accept_reject/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:99-103
###################################################
rlaplace <- function(n) {
    u <- runif(n)
    ifelse(u < 1/2, log(2 * u), log(1 / (2 - 2 * u)))
}


###################################################
### code chunk number 3: lecture-core.Rnw:108-110
###################################################
d <- data.frame(sample = rlaplace(10000))
ggplot(d, aes(x = sample)) + geom_density(fill = "blue", alpha = 0.5) + stat_function(fun = function(x) { (1/2) * exp(- abs(x))}, color = "red", lwd = 2, lty = 2)


###################################################
### code chunk number 4: lecture-core.Rnw:116-120
###################################################
x <- rlaplace(1000)
x_positive <- keep(x, x > 0) 
mean(x_positive) # should be close to 1
t.test(x_positive, conf.level = 0.999)$conf.int


###################################################
### code chunk number 5: lecture-core.Rnw:127-128
###################################################
length(x_positive)


###################################################
### code chunk number 6: lecture-core.Rnw:153-157
###################################################
x <- rnorm(10000)
y <- keep(x, 0 < x & x < 1)
length(y) # number of samples kept
mean(y) # use the samples to estimate E(Y)


###################################################
### code chunk number 7: lecture-core.Rnw:163-164
###################################################
ggplot(data.frame(x = y), aes(x = x)) + geom_density(fill = "blue", alpha = 0.5) 


###################################################
### code chunk number 8: lecture-core.Rnw:182-188
###################################################
approx_pi <- function(k) {
  u1 <- runif(k)
  u2 <- runif(k)
  sqs <- sqrt(u1^2 + u2^2)
  return(4 * sum(sqs <= 1) / k)
}


###################################################
### code chunk number 9: lecture-core.Rnw:194-197
###################################################
xs <- seq(100, 100000, length.out = 50)
plot(xs, Vectorize(approx_pi)(xs), type = 'l', xlab = "k", "ylab" = "Approx Pi")
abline(h = pi, lty = 2, col = "red")


###################################################
### code chunk number 10: lecture-core.Rnw:214-219
###################################################
u1 <- runif(100)
u2 <- runif(100)
inquad <- 1 + (u1^2 + u2^2 <= 1)
plot(u1, u2, col = c( "red", "blue")[inquad], pch = c(15,16)[inquad], asp = 1, xlim = c(0,1), ylim = c(0,1))
curve(sqrt(1 - x^2), add = TRUE, lwd = 3, n = 501)


###################################################
### code chunk number 11: lecture-core.Rnw:237-243
###################################################
par(mar = c(2, 2, 0, 0))
u1 <- runif(100)
u2 <- runif(100)
inquad <- 1 + (u1^2 + u2^2 <= 1)
plot(u1, 4 / pi * u2, col = c( "red", "blue")[inquad], pch = c(15,16)[inquad], asp = 1, xlim = c(0,1), ylim = c(0, 4/pi))
curve(ifelse(x > 0 & x < 1, 4 / pi * sqrt(1 - x^2), 0), add = TRUE, lwd = 3, n = 501)


###################################################
### code chunk number 12: lecture-core.Rnw:252-259
###################################################
par(mar = c(2, 2, 0, 0))
k <- 10000
u1 <- runif(k)
u2 <- runif(k)
inquad <- 1 + (u1^2 + u2^2 <= 1)
plot(u1, 4 / pi * u2, col = c( "red", "blue")[inquad], pch = c(15,16)[inquad], asp = 1, xlim = c(0,1), ylim = c(0, 4/pi))
curve(ifelse(x > 0 & x < 1, 4 / pi * sqrt(1 - x^2), 0), add = TRUE, lwd = 3, n = 501)


###################################################
### code chunk number 13: lecture-core.Rnw:266-267
###################################################
ggplot(data.frame(x = u2[inquad == 2]), aes(x = x )) + geom_histogram(aes(y = ..density..), fill = "blue", alpha = 0.5) + stat_function(fun = function(x) { 4/pi*sqrt(1 - x^2)}, lty = 2, col = "red", lwd = 2)


###################################################
### code chunk number 14: lecture-core.Rnw:328-331
###################################################
curve(6 * x * (1 - x), ylim = c(0, 1.5), n = 501)
abline(h = 1, lty = 2, col = "blue")
abline(h = 6/4, lty = 2, col = "red")


###################################################
### code chunk number 15: lecture-core.Rnw:359-364
###################################################
par(mar = c(2,2,0,0))
curve(6 * x * (1 - x), ylim = c(0, 1.5), n = 501)
abline(h = 6/4, lty = 2, col = "blue")
lines(c(0.3, 0.3), c(0, 1.26), lwd = 3, col = "green")
lines(c(0.3, 0.3), c(1.26, 6/4), lwd = 3, col = "red")


###################################################
### code chunk number 16: lecture-core.Rnw:370-375
###################################################
k <- 10000
ys <- runif(k)
fys <- 6 * ys * (1 - ys)
gys <- 1
ratios <- fys / (gys * (6/4))


###################################################
### code chunk number 17: lecture-core.Rnw:378-382
###################################################
us <- runif(k)
accept <- us < ratios
accepted <- ys[accept]
rejected <- ys[!accept] ; mean(!accept)


###################################################
### code chunk number 18: lecture-core.Rnw:388-392
###################################################
par(mar = c(2,2,0,0))
curve(6 * x * (1 - x), ylim = c(0, 1.5), n = 501)
abline(h = 6/4, lty = 2, col = "blue")
points(ys, us * (6/4), col = c("red", "blue")[1 + accept], pch = c(18,2)[1 + accept])


###################################################
### code chunk number 19: lecture-core.Rnw:399-401
###################################################
ggplot(data.frame(x = ys[accept]), aes(x = x)) + geom_density(fill = "blue", alpha = 0.5) + 
    stat_function(fun = function(x) {6 * x * (1 - x)},  col = "red", lty = 2, lwd = 2)


###################################################
### code chunk number 20: lecture-core.Rnw:496-497
###################################################
(scaling_const <- pnorm(1) - pnorm(0))


###################################################
### code chunk number 21: lecture-core.Rnw:500-505
###################################################
truncated <- function(x) {
  ifelse(x >= 0 & x <= 1,
         dnorm(x) / scaling_const,
         0)
}


###################################################
### code chunk number 22: lecture-core.Rnw:511-515
###################################################

par(mar = c(3,3,0,0))
curve(truncated(x), from = -0.5, to = 1.5, n = 501, lwd = 3)
lines(c(0,0, 1, 1), c(0, truncated(0), truncated(0), 0), lty = 2, lwd = 3, col = "blue")


###################################################
### code chunk number 23: lecture-core.Rnw:522-525
###################################################
ys <- runif(1000)
const <- truncated(0)
ratios <- truncated(ys) / (const * 1) # g(y) = 1


###################################################
### code chunk number 24: lecture-core.Rnw:528-532
###################################################
us <- runif(1000)
accept_uniform <- us < ratios
accepted_uniform <- ys[accept_uniform]
rejected_uniform <- ys[!accept_uniform] 


###################################################
### code chunk number 25: lecture-core.Rnw:536-537
###################################################
mean(accepted_uniform)


###################################################
### code chunk number 26: lecture-core.Rnw:543-547
###################################################
par(mar = c(2,2,0,0))
curve(truncated(x), from = -0.5, to = 1.5, n = 501, lwd = 3)
lines(c(0,0, 1, 1), c(0, truncated(0), truncated(0), 0), lty = 2, lwd = 3, col = "blue")
points(ys, us * truncated(0), col = c("red", "blue")[1 + accept_uniform], pch = c(18,2)[1 + accept_uniform])


###################################################
### code chunk number 27: lecture-core.Rnw:566-569
###################################################
par(mar = c(4,4, 1, 0))
g <- function(y) { (2/3) * (2 - y)}
curve(dnorm(x)/g(x), xlab = "x", ylab = "f(x)/g(x)")


###################################################
### code chunk number 28: lecture-core.Rnw:577-582
###################################################
g <- function(y) { (2/3) * (2 - y)}
qg <- function(p) { 2 - sqrt(4 - 3 * p)}
ys <- qg(runif(1000))
const <- truncated(1) / g(1)
ratios <- truncated(ys) / (const * g(ys))


###################################################
### code chunk number 29: lecture-core.Rnw:585-589
###################################################
us <- runif(1000)
accept_g <- us < ratios
accepted_g <- ys[accept_g]
rejected_g <- ys[!accept_g] 


###################################################
### code chunk number 30: lecture-core.Rnw:595-599
###################################################
par(mar = c(2,3,0,0))
curve(truncated(x), from = -0.5, to = 1.5, ylim = c(0, const * g(0)), n = 501)
lines(c(0,0, 1, 1), c(0, 2 * truncated(1), truncated(1), 0), lty = 2, lwd = 2, col = "blue")
points(ys, us * const * g(ys), col = c("red", "blue")[1 + accept_g], pch = c(18,2)[1 + accept_g])


###################################################
### code chunk number 31: lecture-core.Rnw:613-614
###################################################
sum(accept_g) / sum(accept_uniform)


###################################################
### code chunk number 32: lecture-core.Rnw:628-629
###################################################
f <- function(x) { 0.25 * dnorm(x + 2) + 0.75 * dnorm(x - 1)}


###################################################
### code chunk number 33: lecture-core.Rnw:636-638
###################################################
par(mar = c(4,4,1,0))
curve(f(x), -5, 4)


###################################################
### code chunk number 34: lecture-core.Rnw:656-658
###################################################
par(mar = c(4,4,1,0))
curve(f(x)/dnorm(x), -10, 18)


###################################################
### code chunk number 35: lecture-core.Rnw:663-665
###################################################
par(mar = c(4,4,1,0))
curve(f(x)/dcauchy(x), -10, 18)


###################################################
### code chunk number 36: lecture-core.Rnw:670-673
###################################################
h <- function(x) { f(x) / dcauchy(x)}
xs <- seq(-4, 4, length.out = 1000)
(const <- max(h(xs)))


###################################################
### code chunk number 37: lecture-core.Rnw:678-683
###################################################
par(mar = c(4,4,1,0))
curve(const * dcauchy(x) , -5, 5)
curve(f(x), add = TRUE, lty = 2)
legend("topleft", lty = 1:2, legend = c("Cauchy", 
                                        "f(x)"))


###################################################
### code chunk number 38: lecture-core.Rnw:689-695
###################################################
k <- 10000
ys <- rcauchy(k)
ratios <- f(ys) / (const * dcauchy(ys))
us <- runif(k)
accept <- us < ratios
x <- ys[accept] ; mean(accept)


###################################################
### code chunk number 39: lecture-core.Rnw:700-707
###################################################
df <- data.frame(y = ys, us = us, ratios, accept = accept, scaleu = const * dcauchy(ys) * us )
ggplot(df, aes(x = ys, y = scaleu, color = accept)) + 
    xlim(-5,5) +
    geom_point() +
    stat_function(fun = f, lty = 2, col = "black") +
    stat_function(fun = ~ const * dcauchy(.x), lty = 1, col = "red") +
    labs(x = "Y", y = "c * g(Y) * U")


###################################################
### code chunk number 40: lecture-core.Rnw:711-712
###################################################
rm(ratios) # just to avoid bugs in some copy/pasted code later


###################################################
### code chunk number 41: lecture-core.Rnw:718-724
###################################################
wrong_consts <- c(2 *  const, 
                  const, 
                  0.75 * const, 
                  0.25 * const, 
                  0.1 * const, 
                  0)


###################################################
### code chunk number 42: lecture-core.Rnw:732-738
###################################################
accept_reject <- function(n, f, g, rg, const) {
    ys <- rg(n)
    us <- runif(k)
    accept <- us * const * g(ys) <= f(ys)
    data.frame(y = ys, accept = accept)
}


###################################################
### code chunk number 43: lecture-core.Rnw:743-748
###################################################
bimodal <- function(const) {
    accept_reject(1000, f, dcauchy, rcauchy, const)
}
wrong <- map_dfr(wrong_consts, .id = "constant", bimodal)
wrong$constant <- wrong_consts[as.numeric(wrong$constant)] # get the original labels back


###################################################
### code chunk number 44: lecture-core.Rnw:754-762
###################################################
accept_rates <- group_by(wrong, constant) %>% summarize(rate = mean(accept)) %>% mutate(label = factor(paste0(round(constant, 2), " (", round(rate, 2), ")")))

wrong_accepted <- wrong[wrong$accept, ] %>% left_join(accept_rates, by = "constant")
ggplot(wrong_accepted, aes(x = y)) + geom_density(fill = "blue", alpha = 0.25, lty = 0) + 
    stat_function(fun = f, lty = 2, lwd = 1, color = "red") + 
    stat_function(fun = dcauchy, lty = 1, lwd = 1, color = "#008800") +
    xlim(-5, 5) + 
    facet_wrap(~ label)


