### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week09/week09_gibbs_sampling/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:110-120
###################################################
par(mar = c(2, 1, 1, 1))
d_p_trunc <- function(x, mean, sd) { 
    dnorm(x, mean = mean, sd = sd) / 
        (pnorm(1, mean = mean, sd = sd) - pnorm(0, mean = mean, sd = sd)) 
}
# the mode is higher for the beta distribution and it will be at 4 / 5 
maxh <- dbeta(4/5, 5, 2)
curve(d_p_trunc(x, 0.75, 0.25), from = 0, to = 1, ylim = c(0, maxh))
curve(dbeta(x, 5, 2), add = TRUE, lty = 2)
legend("topleft", c("TN(0.75, 0.5)", "B(5, 2)"), lty = 1:2)


###################################################
### code chunk number 3: lecture-core.Rnw:130-133
###################################################
posterior_star <- function(theta) { 
  dbinom(24, 30, prob = theta) * dnorm(theta, 0.75, 0.25)
}


###################################################
### code chunk number 4: lecture-core.Rnw:138-139
###################################################
curve(posterior_star)


###################################################
### code chunk number 5: lecture-core.Rnw:162-171
###################################################
findDeltas <- function(point, delta) { 
    c(max(0, point - delta), # must be positive 
      min(1, point + delta)) # must be less than 1
}

drawCandidate <- function(previous, delta) {
    ab <- findDeltas(previous, delta)
    runif(1, ab[1], ab[2])
}


###################################################
### code chunk number 6: lecture-core.Rnw:176-190
###################################################
nextDraw <- function(previous, delta = 0.1) {
    candidate <- drawCandidate(previous, delta)

    post_ratio <- posterior_star(candidate) / posterior_star(previous)
    cand_ratio <- diff(findDeltas(previous, delta)) /
                  diff(findDeltas(candidate, delta))

    u <- runif(1)
    if (u <= post_ratio * cand_ratio) {
        return(candidate)
    } else {
        return(previous)
    }
}


###################################################
### code chunk number 7: lecture-core.Rnw:195-201
###################################################
B <- 10000 ; chain <- numeric(B) ; chain[1] <- 0.75
for(i in 2:B) {
    chain[i] <- nextDraw(chain[i - 1], 0.1)
}
half_chain <- chain[5001:B] # drop burn in
mean(half_chain > 0.75) # probability of successful teaching


###################################################
### code chunk number 8: lecture-core.Rnw:206-208
###################################################
par(mar = c(2, 2, 1, 1))
plot(chain[5000:10000], type = 'l')


###################################################
### code chunk number 9: lecture-core.Rnw:213-215
###################################################
ggplot(data.frame(x = chain[5000:10000]), aes(x = x)) + geom_density(fill = "blue", alpha = 0.5) +
stat_function(fun = function(x) { 20 * posterior_star(x) }, lty = 2, lwd = 2, col = "red")


###################################################
### code chunk number 10: echo
###################################################
library(mvtnorm) # includes the multivariate normal density
B <- 5000
chain <- matrix(0, nrow = 2, ncol = B)
S1 <- matrix(c(1, 0.8, 0.8, 1), ncol = 2)
S2 <- matrix(c(0.25, 0, 0, 0.25), ncol = 2)


###################################################
### code chunk number 11: lecture-core.Rnw:257-269
###################################################
ratio <- function(candidate, previous) {

    ## evaluate pi(theta*) / pi(theta(t - 1))
    a <- dmvnorm(candidate, mean = c(0, 0), sigma = S1) /
         dmvnorm(previous, mean = c(0, 0), sigma = S1)

    ## evaluate the candidate density
    b <- dmvnorm(previous, mean = candidate, sigma = S2) /
         dmvnorm(candidate, mean = previous, sigma = S2)

    return(a * b)
}


###################################################
### code chunk number 12: lecture-core.Rnw:274-286
###################################################
for (i in 2:B) {
  ## generate 2, independent Normals at the previous point
  candidate <- rnorm(2, mean = chain[, i - 1],
                        sd = sqrt(0.25))

  r <- ratio(candidate, chain[, i - 1])
  if (runif(1) <= r) {
      chain[, i] <- candidate
  } else {
      chain[, i] <- chain[, i - 1]
  }
}


###################################################
### code chunk number 13: plot_chain5
###################################################
par(mar = c(2, 2, 1, 1))
xx <- range(chain[1, ])
yy <- range(chain[2, ])

tmp <- chain[, 1:10]
plot(NULL, xlim = xx, ylim = yy, xlab = NA, ylab = NA)
arrows(tmp[1, -10], tmp[2, -10], tmp[1, -1], tmp[2, -1], code = 2, length = 0.1)
points(0, 0, col = "red", pch = 16)
points(tmp[1, 10], tmp[2, 10], col = "blue", pch = 16)


###################################################
### code chunk number 14: plot_chain100
###################################################
par(mar = c(2, 2, 1, 1))
xx <- range(chain[1, ])
yy <- range(chain[2, ])

tmp <- chain[, 1:100]
plot(NULL, xlim = xx, ylim = yy, xlab = NA, ylab = NA)
arrows(tmp[1, -100], tmp[2, -100], tmp[1, -1], tmp[2, -1], code = 2, length = 0.1)
points(0, 0, col = "red", pch = 16)
points(tmp[1, 100], tmp[2, 100], col = "blue", pch = 16)


###################################################
### code chunk number 15: plot2
###################################################
par(mar = c(2, 2, 1, 1))

tmp <- jitter(chain[, 2001:5000])
plot(tmp[1, ], tmp[2, ], xlim = xx, ylim = yy, xlab = NA, ylab = NA, cex = 0.25)
text(-3, 2.75, paste("Cor:", round(cor(tmp[1, ], tmp[2, ]), 4)), col = "red", pos = 4)


###################################################
### code chunk number 16: lecture-core.Rnw:365-373
###################################################
pi_i <- function(t) {
  rnorm(1, mean = 0.8 * t, sd = sqrt(1 - 0.8^2))
}
chain_gibbs <- matrix(0, ncol = B, nrow = 2)
for (i in 2:B) {
    chain_gibbs[1, i] <- pi_i(chain_gibbs[2, i - 1])
    chain_gibbs[2, i] <- pi_i(chain_gibbs[1, i])
}


###################################################
### code chunk number 17: plot3
###################################################
par(mar = c(2, 2, 1, 1))
xx <- range(chain_gibbs[1, ])
yy <- range(chain_gibbs[2, ])

tmp <- chain_gibbs[, 1:100]
plot(NULL, xlim = xx, ylim = yy, xlab = NA, ylab = NA)
arrows(tmp[1, -100], tmp[2, -100], tmp[1, -1], tmp[2, -1], code = 2, length = 0.1)
points(0, 0, col = "red", pch = 16)
points(tmp[1, 100], tmp[2, 100], col = "blue", pch = 16)


###################################################
### code chunk number 18: plot4
###################################################
par(mar = c(2, 2, 1, 1))

tmp <- jitter(chain_gibbs[, 2001:5000])
plot(tmp[1, ], tmp[2, ], xlim = xx, ylim = yy, xlab = NA, ylab = NA, cex = 0.25)
text(-3, 3, paste("Cor:", round(cor(tmp[1, ], tmp[2, ]), 4)), col = "red", pos = 4)


###################################################
### code chunk number 19: lecture-core.Rnw:477-479
###################################################
par(mar = c(4,2,1,1))
curve(dnorm(x, mean = 120, sd = 20), xlim = c(80, 160), xlab = "mu", ylab = NA)


###################################################
### code chunk number 20: plot5
###################################################
inv.gamma <- function(x, a, b) {
b^a / x^(a + 1) * exp(-b / x) / gamma(a)
}
as <- c(1, 1, 2, 3, 5)
bs <- c(1, 2, 2, 5, 10)
par(mar = c(3, 3, 1, 1))
plot(NULL, xlim = c(0, 4), ylim = c(0,0.9), xlab = "sigma^2", ylab = NA)
for (i in 1:5) {
    curve(inv.gamma(x, as[i], bs[i]), lty = i, add = TRUE)
}
legend("topright", legend = c(paste("a = ", as, "b = ", bs)), lty = 1:5)


###################################################
### code chunk number 21: lecture-core.Rnw:502-504
###################################################
load("../../../../data/nhanes/nhanes.rda")
combined <- filter(nhanes, !is.na(sys_mean), !is.na(dia_mean), !is.na(taking_aspirin))


###################################################
### code chunk number 22: lecture-core.Rnw:510-515
###################################################
param_a <- 3
param_b <- 5
param_theta = 120
param_tau2 = 20^2
n <- dim(combined)[1] # the BP data


###################################################
### code chunk number 23: lecture-core.Rnw:524-530
###################################################
xsum <- sum(combined$sys_mean)
gibbs_mu <- function(s2) {
  denom <- s2 + n * param_tau2
  rnorm(1, mean = (s2 * param_theta + param_tau2 * xsum) / denom,
        sd = sqrt(s2 * param_tau2 / denom))
}


###################################################
### code chunk number 24: lecture-core.Rnw:537-543
###################################################
shape_1 <- n/2 + param_a # doesn't depend on mu
gibbs_s2 <- function(mu) {
  shape_2 <- (0.5) * sum((combined$sys_mean - mu)^2) + param_b
  g <- rgamma(1, shape_1, shape_2)
  1 / g # recall: inverse gamma
}


###################################################
### code chunk number 25: lecture-core.Rnw:548-552
###################################################
gibbs_mu_sigma2 <- matrix(0, nrow = 2, ncol = B)
## draw from the prior to set up the chain
gibbs_mu_sigma2[1, 1] <- rnorm(1, param_theta, sqrt(param_tau2))
gibbs_mu_sigma2[2, 1] <- 1 / rgamma(1, param_a, param_b)


###################################################
### code chunk number 26: lecture-core.Rnw:555-559
###################################################
for (i in 2:B) {
  chain[1, i] <- gibbs_mu(chain[2, i - 1])
  chain[2, i] <- gibbs_s2(chain[1, i])
}


###################################################
### code chunk number 27: plot6
###################################################
par(mar = c(4, 4, 1, 1))
tmp <- as.data.frame(t(chain[, -(1:B/2)]))
colnames(tmp) <- c("mu", "s2")
ggplot(tmp, aes(x = mu, y = s2)) + geom_point(alpha = 0.5)
# plot(tmp[1, ], tmp[2, ], xlab = "mu", ylab = "sigma2")


###################################################
### code chunk number 28: plot7
###################################################
# plot(density(tmp[1, ]))
ggplot(tmp, aes(x = mu)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 29: plot8
###################################################
#plot(density(tmp[2, ]))
ggplot(tmp, aes(x = s2)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 30: lecture-core.Rnw:595-596
###################################################
1 - var(chain[1, -(1:500)]) / 20^2 # recall, tau^2  = 20^2


###################################################
### code chunk number 31: lecture-core.Rnw:602-604
###################################################
prior_var <- param_b^2  / ((param_a - 1)^2  * (param_a - 2))
1 - var(chain[2, -(1:500)]) / prior_var 


###################################################
### code chunk number 32: lecture-core.Rnw:610-612
###################################################
# discard the first 500 draws
stationary <- chain[, -(1:500)]


###################################################
### code chunk number 33: lecture-core.Rnw:616-620
###################################################
# mu
mean(stationary[1,])
# sigma^2
mean(stationary[2,])


###################################################
### code chunk number 34: lecture-core.Rnw:626-630
###################################################
# mu
quantile(stationary[1, ], c(0.025, 0.975))
# sigma^2
quantile(stationary[2, ], c(0.025, 0.975))


###################################################
### code chunk number 35: rstran (eval = FALSE)
###################################################
## library(rstan)
## bp_stan <- stan("bp.stan", 
##                 data = list(n = dim(combined)[1], 
##                 y = combined$sys_mean))


###################################################
### code chunk number 36: lecture-core.Rnw:717-719
###################################################
library("rstan")
load("bp_stan.rda")


###################################################
### code chunk number 37: lecture-core.Rnw:723-724
###################################################
print(bp_stan)


###################################################
### code chunk number 38: lecture-core.Rnw:729-730
###################################################
traceplot(bp_stan, pars = "mu", inc_warmup = TRUE, window = c(0, 200))


###################################################
### code chunk number 39: lecture-core.Rnw:735-736
###################################################
traceplot(bp_stan, "mu")


###################################################
### code chunk number 40: lecture-core.Rnw:741-742
###################################################
pairs(bp_stan, pars = c("mu", "sigmasq"))


###################################################
### code chunk number 41: lecture-core.Rnw:791-798 (eval = FALSE)
###################################################
## aspirin_effect <- stan("aspirin_effect.stan",
##   data = with(combined,
##               list(
##                   n = sum(taking_aspirin),
##                   m = sum(!taking_aspirin),
##                   takers = sys_mean[taking_aspirin],
##                   nontakers = sys_mean[!taking_aspirin])))


###################################################
### code chunk number 42: lecture-core.Rnw:802-803
###################################################
load("aspirin_effect.rda")


###################################################
### code chunk number 43: lecture-core.Rnw:807-808
###################################################
print(aspirin_effect)


###################################################
### code chunk number 44: lecture-core.Rnw:813-814
###################################################
pairs(aspirin_effect, pars = c("mu_1", "mu_2", "delta"))


###################################################
### code chunk number 45: lecture-core.Rnw:819-821
###################################################
samples <- as.array(aspirin_effect)
dimnames(samples)


###################################################
### code chunk number 46: lecture-core.Rnw:826-828
###################################################
## probability that the effect size is at least 5
mean(abs(samples[,, "delta"]) >= 5)


###################################################
### code chunk number 47: lecture-core.Rnw:856-870
###################################################
unif_chain <- function(delta, B = 5000) {
  chain <- numeric(B); chain[1] <- runif(1, -delta, delta)  ; rejects <- 0
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
### code chunk number 48: lecture-core.Rnw:875-878
###################################################
n01_chain_0.1 <- unif_chain(0.1)
n01_chain_1 <- unif_chain(1)
n01_chain_10 <- unif_chain(10)


###################################################
### code chunk number 49: plot9
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
### code chunk number 50: lecture-core.Rnw:904-907
###################################################
library(parallel)
RNGkind("L'Ecuyer-CMRG") # parallel safe PRNG
chains_0.1 <- mclapply(rep(0.1, 3), unif_chain, mc.cores = 3, B = 1000)


###################################################
### code chunk number 51: plot10
###################################################
plot(NULL, xlim = c(1, 1000), ylim = range(sapply(chains_0.1, function(x) { x$chain })), xlab = NA, ylab = NA)
lines(1:1000, chains_0.1[[1]]$chain)
lines(1:1000, chains_0.1[[2]]$chain)
lines(1:1000, chains_0.1[[3]]$chain)


###################################################
### code chunk number 52: lecture-core.Rnw:942-946
###################################################
psi <- function(chain) {
  n <- length(chain)
  cumsum(chain) / (1:n) # cumulative mean
}


###################################################
### code chunk number 53: lecture-core.Rnw:949-952
###################################################
psi_1 <- psi(chains_0.1[[1]]$chain)
psi_2 <- psi(chains_0.1[[2]]$chain)
psi_3 <- psi(chains_0.1[[3]]$chain)


###################################################
### code chunk number 54: plot11
###################################################
plot(NULL, xlim = c(1, 1000), ylim = range(c(psi_1, psi_2, psi_3)), xlab = NA, ylab = NA)
lines(1:1000, psi_1)
lines(1:1000, psi_2)
lines(1:1000, psi_3)


###################################################
### code chunk number 55: lecture-core.Rnw:995-1007
###################################################
Rs <- numeric(1000)
for (t in 1:1000) {
    p1 <- psi_1[1:t] ; p2 <- psi_2[1:t] ; p3 <- psi_3[1:t]
    psi_bars <- c(mean(p1), mean(p2), mean(p3))
    psi_bar_bar <- mean(c(p1, p2, p3))

    B <- (t / 2) * sum((psi_bars - psi_bar_bar)^2)
    W = (1 / (2 * t))*(sum((p1 - psi_bars[1])^2) + 
                       sum((p2 - psi_bars[2])^2) +
                       sum((p2 - psi_bars[2])^2))
   Rs[t] <- sqrt(((t - 1) / t * W + 1/ t * B) / W)
}


###################################################
### code chunk number 56: plot12
###################################################
plot(Rs, type = 'l', ylim = c(0.5, max(Rs[-1])))
abline(h = 1, lty = 2)


###################################################
### code chunk number 57: lecture-core.Rnw:1019-1020
###################################################
chains_1 <- mclapply(rep(1, 3), unif_chain, mc.cores = 3, B = 1000)


###################################################
### code chunk number 58: lecture-core.Rnw:1022-1027
###################################################
par(mar = c(0, 0, 0, 0))
plot(NULL, xlim = c(1, 1000), ylim = range(sapply(chains_1, function(x) { x$chain })), xlab = NA, ylab = NA)
lines(1:1000, chains_1[[1]]$chain)
lines(1:1000, chains_1[[2]]$chain)
lines(1:1000, chains_1[[3]]$chain)


###################################################
### code chunk number 59: lecture-core.Rnw:1031-1047
###################################################
psi_1 <- psi(chains_1[[1]]$chain)
psi_2 <- psi(chains_1[[2]]$chain)
psi_3 <- psi(chains_1[[3]]$chain)

Rs <- numeric(1000)
for (t in 1:1000) {
p1 <- psi_1[1:t] ; p2 <- psi_2[1:t] ; p3 <- psi_3[1:t]
psi_bars <- c(mean(p1), mean(p2), mean(p3))
psi_bar_bar <- mean(c(p1, p2, p3))

B <- (t / 2) * sum((psi_bars - psi_bar_bar)^2)
W = (1 / (2 * t))*(sum((p1 - psi_bars[1])^2) + 
sum((p2 - psi_bars[2])^2) +
sum((p2 - psi_bars[2])^2))
Rs[t] <- sqrt(((t - 1) / t * W + 1/ t * B) / W)
}


###################################################
### code chunk number 60: lecture-core.Rnw:1051-1053
###################################################
plot(Rs, type = 'l', ylim = c(0.5, max(Rs[-1])))
abline(h = 1, lty = 2)


###################################################
### code chunk number 61: lecture-core.Rnw:1059-1060
###################################################
summary(bp_stan)$summary[, c("mean", "sd", "n_eff", "Rhat")]


