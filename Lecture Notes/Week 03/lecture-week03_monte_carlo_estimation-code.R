### R code from vignette source '/home/mark/Documents/Teaching/umich/stats406-fall2020/lectures/week03/week03_monte_carlo_estimation/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:83-87
###################################################
par(mar = c(2, 2, 1, 1))
curve(dnorm(x), from = -10, to = 10, xlab = NA, ylab = NA, lwd = 2)
curve(dcauchy(x), add = TRUE, col = "red", lty = 2, lwd = 2)
legend("topright", lty = 1:2, col = c("black", "red"), legend = c("Std. Norm", "Cauchy"))


###################################################
### code chunk number 3: lecture-core.Rnw:94-98
###################################################
par(mar = c(2, 2, 1, 1))
curve(dcauchy(x), from = 3, to = 5, xlab = NA, ylab = NA, lwd = 2, col = "red", lty = 2, ylim = c(0, dcauchy(3)) )
curve(dnorm(x), add = TRUE, col = "black", lty = 1, lwd = 2)
legend("topright", lty = 1:2, col = c("black", "red"), legend = c("Std. Norm", "Cauchy"))


###################################################
### code chunk number 4: lecture-core.Rnw:141-144
###################################################
n <- 10
true_theta <- 10
samples <- rerun(10000, rcauchy(10, location = true_theta))


###################################################
### code chunk number 5: lecture-core.Rnw:147-149
###################################################
median_sampling_dist <- map_dbl(samples, median)
mean_sampling_dist <- map_dbl(samples, mean)


###################################################
### code chunk number 6: lecture-core.Rnw:155-166
###################################################

## par(mar = c(3,3,2,2))
## plot(density(median_dist), main = NA, xlim = c(0, 20))
## md2 <- mean_dist[mean_dist >= -100 & mean_dist <= 100]
## points(density(md2), type = 'l', col = "blue", lty = 2)
## abline(v = true_theta, col = "red")
## legend("topleft", fill = c("black", "blue"), legend = c("Median", "Mean"))
df <- data.frame(median = median_sampling_dist, 
                 mean = mean_sampling_dist) %>% gather %>% mutate(key = factor(key))

ggplot(df, aes(x = value, fill = key)) + geom_density(alpha = 0.75) + lims(x = c(0,20)) 


###################################################
### code chunk number 7: lecture-core.Rnw:180-182
###################################################
mean(median_sampling_dist) - true_theta
mean(mean_sampling_dist) - true_theta


###################################################
### code chunk number 8: lecture-core.Rnw:187-189
###################################################
summary(median_sampling_dist)
summary(mean_sampling_dist)


###################################################
### code chunk number 9: lecture-core.Rnw:197-199
###################################################
var(median_sampling_dist)
var(mean_sampling_dist)


###################################################
### code chunk number 10: lecture-core.Rnw:211-216
###################################################
trimmed_mean <- function(x, k) {
  n <- length(x)
  orderstats <- sort(x)
  mean(orderstats[k:(n - k)])
}


###################################################
### code chunk number 11: lecture-core.Rnw:222-229
###################################################
ks <- 1:4
map(ks, function(k) {
  ts <- map_dbl(samples, ~ trimmed_mean(.x,  k = k))
  t_bias <- mean(ts) - true_theta
  t_var  <- var(ts)
  c(t_bias, t_var)
}) %>% bind_cols


###################################################
### code chunk number 12: lecture-core.Rnw:234-239
###################################################
n <- 500 
k <- round(0.38 * n / 2)
samples_500 <- data.frame(replicate(1000, rcauchy(n, true_theta)))
median_500 <- map_dbl(samples_500, median)
trimmed_500 <- map_dbl(samples_500, trimmed_mean, k = k) 


###################################################
### code chunk number 13: lecture-core.Rnw:242-244
###################################################
c(mean(median_500), var(median_500))
c(mean(trimmed_500), var(trimmed_500))


###################################################
### code chunk number 14: lecture-core.Rnw:269-271
###################################################
mean((median_500 - true_theta)^2)
mean((trimmed_500 - true_theta)^2)


###################################################
### code chunk number 15: lecture-core.Rnw:281-283
###################################################
t.test((median_500 - true_theta)^2)$conf.int
t.test((trimmed_500 - true_theta)^2)$conf.int


###################################################
### code chunk number 16: lecture-core.Rnw:337-340
###################################################
k <- 100000
n <- 20
theta <- 2


###################################################
### code chunk number 17: lecture-core.Rnw:343-346
###################################################
samples <- rerun(k, runif(n, min = 0, max = theta))
mle_sampling_dist <- map_dbl(samples, max)
mom_sampling_dist <- map_dbl(samples, ~ 2 * mean(.x))


###################################################
### code chunk number 18: lecture-core.Rnw:351-353
###################################################
df <- data.frame(mle = mle_sampling_dist, mom = mom_sampling_dist) %>% gather %>% mutate(key = factor(key))
ggplot(df, aes(x = value, fill = key)) + geom_density(alpha = 0.75)


###################################################
### code chunk number 19: lecture-core.Rnw:359-361
###################################################
t.test(mle_sampling_dist - theta)$conf.int[1:2] # 1:2 silence attr
t.test(mom_sampling_dist - theta)$conf.int[1:2]


###################################################
### code chunk number 20: lecture-core.Rnw:365-367
###################################################
t.test((mle_sampling_dist - theta)^2)$conf.int[1:2] # 1:2 silence attr
t.test((mom_sampling_dist - theta)^2)$conf.int[1:2]


###################################################
### code chunk number 21: lecture-core.Rnw:476-481
###################################################
alpha <- 0.05
k <- 10000
n <- 20
mu <- 2 # unknown truth
sigma2 <- 10 ; sigma <- sqrt(sigma2) # known variance


###################################################
### code chunk number 22: lecture-core.Rnw:484-489
###################################################
samples_norm <- rerun(k, rnorm(n, mean = mu, sd = sigma))
cis_norm <- map(samples_norm,
                ~ mean(.x) + c(-1, 1) * qnorm(1 - alpha/2) * (sigma / sqrt(n)))
cis_t <- map(samples_norm,
             ~ mean(.x) + c(-1, 1) * qt(1 - alpha/2, df = n - 1) * (sd(.x) / sqrt(n)))


###################################################
### code chunk number 23: lecture-core.Rnw:495-497
###################################################
map_dbl(cis_norm, ~ .x[1] <= mu && mu <= .x[2]) %>% mean
map_dbl(cis_t, ~ .x[1] <= mu && mu <= .x[2]) %>% mean


###################################################
### code chunk number 24: lecture-core.Rnw:502-504
###################################################
map_dbl(cis_norm, ~ .x[2] - .x[1]) %>% mean
map_dbl(cis_t, ~ .x[2] - .x[1]) %>% mean


###################################################
### code chunk number 25: lecture-core.Rnw:509-530
###################################################
cis_norm_100 <- cis_norm[1:100]
cis_t_100 <- cis_t[1:100]
cis_norm_100_covers <- map_lgl(cis_norm_100, ~ .x[1] <= mu && mu <= .x[2])
cis_t_100_covers <- map_lgl(cis_t_100, ~ .x[1] <= mu && mu <= .x[2])

df1 <- data.frame(low = map_dbl(cis_norm_100, ~ .x[1]),
                  high = map_dbl(cis_norm_100, ~ .x[2]),
                  order = 1:100,
                  type = "normal",
                  covers = cis_norm_100_covers)
df2 <- data.frame(low = map_dbl(cis_t_100, ~ .x[1]),
                  high = map_dbl(cis_t_100, ~ .x[2]),
                  order = 1:100,
                  type = "t",
                  covers = cis_t_100_covers)

df <- rbind(df1, df2)

ggplot(df, aes(x = order, color = covers)) + geom_segment(aes(y = low, yend = high, xend = order)) +
    geom_abline(intercept = mu, slope = 0) + facet_wrap(~ type) + labs(x = NULL, y = "Confidence Interval")



###################################################
### code chunk number 26: lecture-core.Rnw:561-565
###################################################
n <- 10000
x <- rcauchy(n)
y <- x > 1
ybar <- mean(y)


###################################################
### code chunk number 27: lecture-core.Rnw:568-569
###################################################
(norm_ci <- ybar + c(-1,1) * qnorm(0.975) * sqrt(ybar * (1 - ybar) / n))


###################################################
### code chunk number 28: lecture-core.Rnw:589-590
###################################################
theta_0 <- seq(0, 1, length.out = 1000)


###################################################
### code chunk number 29: lecture-core.Rnw:594-597
###################################################
a0 <- qbinom(0.025, size = n, prob = theta_0)
b0 <- qbinom(0.025, size = n, prob = theta_0,
             lower.tail = FALSE) - 1


###################################################
### code chunk number 30: lecture-core.Rnw:603-605
###################################################
w <- sum(y)
range(theta_0[a0 < w & w < b0])


###################################################
### code chunk number 31: lecture-core.Rnw:609-610
###################################################
norm_ci


###################################################
### code chunk number 32: lecture-core.Rnw:627-631
###################################################
theta <- 0.25
n <- 20
k <- 1000
xs <- rbinom(k, size = n, prob =  theta)


###################################################
### code chunk number 33: lecture-core.Rnw:635-642
###################################################
tints <- map(xs, function(x) {
    if (x == 0 || x == n) {
        return(c(0, 1)) # can't estimate
    } else {
       return(t.test(c(rep(1, x), rep(0, n - x)))$conf.int)
    }
})


###################################################
### code chunk number 34: lecture-core.Rnw:645-647
###################################################
bints <- map(xs, ~ binom.test(.x, n)$conf.int)
pints <- map(xs, ~ prop.test(.x, n)$conf.int)


###################################################
### code chunk number 35: lecture-core.Rnw:654-658
###################################################
cover <- function(x) { x[1] <= theta && theta <= x[2] }
(tcover <- map_dbl(tints, cover) %>% mean)
(bcover <- map_dbl(bints, cover) %>% mean)
(pcover <- map_dbl(pints, cover) %>% mean)


###################################################
### code chunk number 36: lecture-core.Rnw:663-666
###################################################
(twidth <- map_dbl(tints, diff) %>% mean)
(bwidth <- map_dbl(bints, diff) %>% mean)
(pwidth <- map_dbl(pints, diff) %>% mean)


