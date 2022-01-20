### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week09/week09_bayesian_stats/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:134-137
###################################################
test_theta <- 0.8
test_score <- rbinom(1, size = 30, test_theta)
print(test_score)


###################################################
### code chunk number 3: lecture-core.Rnw:168-173
###################################################
    par(mar = c(3,3,1,1))
curve(dbeta(x, 1, 1), xlim = c(0,1), ylim = c(0, 2.8))
curve(dbeta(x, 2, 1), xlim = c(0,1), add = TRUE, lty = 2, col = "red")
curve(dbeta(x, 5, 2), xlim = c(0,1), add = TRUE, lty = 3, col = "blue")
legend("topleft", lty = 1:3, col = c("black", "red", "blue"), legend = c("1,1", "2,1", "5,2"))


###################################################
### code chunk number 4: lecture-core.Rnw:216-220
###################################################
par(mar = c(3,3,1,1))
curve(dbeta(x, 5 + test_score, 32 - test_score))
curve(dbeta(x, 5, 2), add =TRUE, lty = 2)
legend(x = "topleft", legend = c("Posterior given test scores", "Prior a = 5, b = 2"), lty = 1:2)


###################################################
### code chunk number 5: lecture-core.Rnw:226-227
###################################################
1 - pbeta(0.75, 5 + test_score, 32 - test_score)


###################################################
### code chunk number 6: lecture-core.Rnw:231-233
###################################################
1 - var(rbeta(1000, 5 + test_score, 32 - test_score)) /
  var(rbeta(1000, 5, 2)) ## MC estimates of variance


###################################################
### code chunk number 7: lecture-core.Rnw:282-286
###################################################
rv1 <- cumsum(c(1, rnorm(9, mean = 0)))
curve(dnorm(x), xlim = range(c(-1, 1, rv1)))
lines(density(rv1), lty = 2)
text(y = 0.3, x = rv1, labels =  1:10)


###################################################
### code chunk number 8: lecture-core.Rnw:293-299
###################################################
par(mar = c(3, 3, 1, 1))
rv2 <- c(0, rnorm(200, mean = 0))
rvs <- c(rv1, rv2)
curve(dnorm(x), xlim = range(c(-1, 1, rvs)))
lines(density(rvs), lty = 2)
# text(y = 0.3, x = rv1, labels =  1:10)


###################################################
### code chunk number 9: lecture-core.Rnw:306-310
###################################################
par(mar = c(3, 3, 1, 1))
curve(dnorm(x), xlim = range(c(-1, 1, rv2)))
lines(density(rv2), lty = 2)
# text(y = 0.3, x = rv1, labels =  1:10)


###################################################
### code chunk number 10: lecture-core.Rnw:385-396
###################################################
# we'll fix sigma at 4 
f <- function(x) {
  (x / 16) * exp(-x^2 / 32)
}
B <- 10000
xs <- numeric(B)
xs[1] <- 2 # arbitrary starting point

# we'll log rejects
rejected <- logical(B)
rejected[1] <- FALSE


###################################################
### code chunk number 11: lecture-core.Rnw:401-402
###################################################
curve(f(x), xlim = c(0, 10))


###################################################
### code chunk number 12: lecture-core.Rnw:407-422
###################################################
# starting i = 2, apply MH 
for (i in 2:B) {
  x <- xs[i - 1]
  xstar <- rchisq(1, df = x)
  ratio <- f(xstar) * dchisq(x, df = xstar) /
           (f(x) * dchisq(xstar, df = x))
  u <- runif(1)
  if (u <= ratio) {
    xs[i] <- xstar
    rejected[i] <- FALSE
  } else {
    xs[i] <- x
    rejected[i] <- TRUE
  }
}


###################################################
### code chunk number 13: lecture-core.Rnw:428-431
###################################################
par(mar = c(4,4,0,0))
k <- 20
plot(xs[1:k], type = 'b', col = c("black", "red")[1 + rejected], ylab = "X(t)", xlab = "t")


###################################################
### code chunk number 14: lecture-core.Rnw:438-440
###################################################
par(mar = c(4,4,0,0))
plot(xs, type = 'l', ylab = "X(t)", xlab = "t")


###################################################
### code chunk number 15: lecture-core.Rnw:454-456
###################################################
df <- data.frame(x_t = xs, burnin = c(rep("Yes", 200), rep("No", B - 200)))
ggplot(df, aes(x = x_t, fill = burnin)) + geom_density(alpha = 0.5) + stat_function(fun = f, lwd = 2)


###################################################
### code chunk number 16: lecture-core.Rnw:512-516
###################################################
pi_star <- function(theta) {
  theta^(5 + test_score - 1) *
  (1 - theta)^(32 - test_score - 1)
}


###################################################
### code chunk number 17: lecture-core.Rnw:521-536
###################################################
B <- 20000; chain <- numeric(B) ; chain[1] <- 0.5; rejects <- 0
for (i in 2:B) {
    candidate <- ifelse(chain[i - 1] < 0.5, 
                        runif(1, 0, 0.6),
                        runif(1, 0.4, 1))
    ratio <- pi_star(candidate) * (5/3) /
        (pi_star(chain[i - 1]) * (5/3))
    if (runif(1) <= ratio) {
        chain[i] <- candidate
    } else {
        chain[i] <- chain[i - 1]
        rejects <- rejects + 1
    }
}
reject_rate <- rejects / B


###################################################
### code chunk number 18: lecture-core.Rnw:541-542
###################################################
plot(chain, type = 'l')


###################################################
### code chunk number 19: lecture-core.Rnw:547-549
###################################################
plot(density(chain[2000:5000]))
curve(dbeta(x, 5 + test_score, 32 - test_score), add = TRUE, col = "red")


###################################################
### code chunk number 20: lecture-core.Rnw:578-590
###################################################
chain_ind <- numeric(B) ; chain_ind[1] <- 0.1 ; rejects_ind <- 0
for (i in 2:B) {
    candidate <- runif(1)
    ratio <- pi_star(candidate) / pi_star(chain_ind[i - 1])
    if (runif(1) <= ratio) {
        chain_ind[i] <- candidate
    } else {
        chain_ind[i] <- chain_ind[i - 1]
        rejects_ind <- rejects_ind + 1
    }
}
reject_rate_ind <- rejects_ind / B


###################################################
### code chunk number 21: lecture-core.Rnw:595-596
###################################################
plot(chain_ind, type = 'l')


###################################################
### code chunk number 22: lecture-core.Rnw:602-604
###################################################
plot(density(chain_ind[2000:B]))
curve(dbeta(x, 5 + test_score, 32 - test_score), add = TRUE, col = "red")


###################################################
### code chunk number 23: lecture-core.Rnw:623-625
###################################################
reject_rate
reject_rate_ind


###################################################
### code chunk number 24: lecture-core.Rnw:658-672
###################################################
unif_chain <- function(delta, B = 5000) {
  chain <- numeric(B); chain[1] <- 0 ; rejects <- 0
  for (i in 2:B) {
    candidate <- chain[i - 1] + runif(1, -delta, delta)
    ratio <- dnorm(candidate) / dnorm(chain[i - 1])
    if (runif(1) <= ratio) {
        chain[i] <- candidate
    } else {
        chain[i] <- chain[i - 1]
        rejects <- rejects + 1
    }
  }
  list(reject_rate = rejects / B, chain = chain)
}


###################################################
### code chunk number 25: lecture-core.Rnw:677-680
###################################################
n01_chain_0.1 <- unif_chain(0.1)
n01_chain_1 <- unif_chain(1)
n01_chain_10 <- unif_chain(10)


###################################################
### code chunk number 26: lecture-core.Rnw:685-693
###################################################
par(mar = c(4, 4, 2,2))
k <-1000 
plot(NULL, xlim = c(1, k), ylim = 1.5 * range(c(n01_chain_10$chain, n01_chain_1$chain, n01_chain_0.1$chain)))
rs <- rainbow(3)
lines(1:k, n01_chain_0.1$chain[1:k], col = rs[1])
lines(1:k, n01_chain_1$chain[1:k], col = rs[2])
lines(1:k, n01_chain_10$chain[1:k], col = rs[3])
legend("bottomright", legend = c(0.1, 1, 10), fill = rs)


###################################################
### code chunk number 27: lecture-core.Rnw:698-701
###################################################
n01_chain_0.1$reject_rate
n01_chain_1$reject_rate
n01_chain_10$reject_rate


