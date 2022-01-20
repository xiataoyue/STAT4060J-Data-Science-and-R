### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week03/week03_monte_carlo_hypothesis_testing/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:160-169
###################################################
par(mar = c(3, 3, 2, 0))
curve(dnorm(x, sd = 1/sqrt(2)), from = -2, 4, lwd = 2, col = "blue")
curve(dnorm(x, sd = 1/sqrt(2), mean = 1), add = TRUE, col = "red", lwd = 2)
# cut <- qnorm(0.95)
# regionx <- seq(cut, 4, length.out = 200)
# polygon(c(regionx, regionx[200], cut), c(dnorm(regionx, mean = 1), 0, 0), col = "#FF0000A0")
# polygon(c(regionx, regionx[200], cut), c(dnorm(regionx), 0, 0), col = "#0000FFA0")
# text("cutoff ->", x = cut - 0.5, y = 0.05)
 legend("topright", fill = c("blue", "red"), legend = c("H0", "H1"))


###################################################
### code chunk number 3: lecture-core.Rnw:192-194
###################################################
n <- 2; a <- 0; b <- 0.5
pnorm(b, sd = 1/sqrt(n)) - pnorm(a, sd = 1/sqrt(n))


###################################################
### code chunk number 4: lecture-core.Rnw:198-199
###################################################
pnorm(b, mean = 1, sd = 1/sqrt(n)) - pnorm(a, mean = 1, sd = 1/sqrt(n))


###################################################
### code chunk number 5: lecture-core.Rnw:210-213
###################################################
n <- 2
pnorm(-0.1, sd = 1/sqrt(n)) - pnorm(-0.1905725, sd = 1/sqrt(2))
pnorm(2, sd = 1/sqrt(n)) - pnorm(1.147342, sd = 1/sqrt(2))


###################################################
### code chunk number 6: lecture-core.Rnw:221-235
###################################################
par(mar = c(3, 3, 2, 0))

r1 <- c(-0.1905725, -0.1)
r2 <- c(1.147342, 2)

curve(dnorm(x, sd = 1/sqrt(2)), from = -2, 4, lwd = 2, col = "blue")
curve(dnorm(x, sd = 1/sqrt(2), mean = 1), add = TRUE, col = "red", lwd = 2)
region1 <- seq(r1[1], r1[2], length.out = 200)
region2 <- seq(r2[1], r2[2], length.out = 200)
polygon(c(region1, region1[200], r1[1]), c(dnorm(region1, mean = 0, sd = 1/sqrt(2)), 0, 0), col = "blue")
polygon(c(region2, region2[200], r2[1]), c(dnorm(region2, mean = 0, sd = 1/sqrt(2)), 0, 0), col = "blue")
# polygon(c(regionx, regionx[200], cut), c(dnorm(regionx), 0, 0), col = "#0000FFA0")
# text("cutoff ->", x = cut - 0.5, y = 0.05)
 legend("topright", fill = c("blue", "red"), legend = c("H0", "H1"))


###################################################
### code chunk number 7: lecture-core.Rnw:243-257
###################################################
par(mar = c(3, 3, 2, 0))

r1 <- c(-0.1905725, -0.1)
r2 <- c(1.147342, 2)

curve(dnorm(x, sd = 1/sqrt(2)), from = -2, 4, lwd = 2, col = "blue")
curve(dnorm(x, sd = 1/sqrt(2), mean = 1), add = TRUE, col = "red", lwd = 2)
region1 <- seq(r1[1], r1[2], length.out = 200)
region2 <- seq(r2[1], r2[2], length.out = 200)
polygon(c(region1, region1[200], r1[1]), c(dnorm(region1, mean = 1, sd = 1/sqrt(2)), 0, 0), col = "red")
polygon(c(region2, region2[200], r2[1]), c(dnorm(region2, mean = 1, sd = 1/sqrt(2)), 0, 0), col = "red")
# polygon(c(regionx, regionx[200], cut), c(dnorm(regionx), 0, 0), col = "#0000FFA0")
# text("cutoff ->", x = cut - 0.5, y = 0.05)
 legend("topright", fill = c("blue", "red"), legend = c("H0", "H1"))


###################################################
### code chunk number 8: lecture-core.Rnw:267-268
###################################################
(cutoff <- qnorm(0.95, mean = 0, sd = sqrt(1/sqrt(2))))


###################################################
### code chunk number 9: lecture-core.Rnw:272-273
###################################################
1 - pnorm(cutoff, mean = 1, sd = sqrt(1/2))


###################################################
### code chunk number 10: lecture-core.Rnw:279-284
###################################################
sample_sizes <- 2:20
power_n <- map_dbl(sample_sizes, function(x) {
    cutoff <- qnorm(0.95, mean = 0, sd = sqrt(1 / x))
    1 - pnorm(cutoff, mean = 1, sd = sqrt(1 / x))
})


###################################################
### code chunk number 11: lecture-core.Rnw:287-291
###################################################
pc <- ggplot(data.frame(n = sample_sizes, y = power_n),
             aes(x = n, y = y))  +
      geom_point(size = 2) + geom_line() 



###################################################
### code chunk number 12: lecture-core.Rnw:296-299
###################################################
    pc + labs(y = "Power for mu = 1") +geom_abline(slope = 0, intercept = 0.9, lty = 2) +
    annotate("text", x = 6, y = 0.6, label = "Power at n", size = 5) +
    annotate("text", x = 15, y = 0.85, label = "90% Power", size = 5)


###################################################
### code chunk number 13: lecture-core.Rnw:332-333
###################################################
hidden_p <- 0.25


###################################################
### code chunk number 14: lecture-core.Rnw:336-337
###################################################
(xs <- rbinom(10, size = 5, p = hidden_p))


###################################################
### code chunk number 15: lecture-core.Rnw:362-365
###################################################
k <- 1000
T_statistic <- function(sample) { mean(sample) / 5 }
ts_0 <- replicate(k, rbinom(10, size = 5, p = 0.5) %>% T_statistic)


###################################################
### code chunk number 16: lecture-core.Rnw:372-373
###################################################
ts_1 <- replicate(k, rbinom(10, size = 5, p = 0.6) %>% T_statistic)


###################################################
### code chunk number 17: lecture-core.Rnw:378-380
###################################################
ggplot(data.frame(T = c(ts_0, ts_1), Hypothesis = c(rep('Null', k),
rep('Alternative', k))), aes(x = T, fill = Hypothesis, color = Hypothesis)) + geom_density( alpha = 0.75)


###################################################
### code chunk number 18: lecture-core.Rnw:388-389
###################################################
(rr <- c(quantile(ts_0, 0.95), 1))


###################################################
### code chunk number 19: lecture-core.Rnw:392-393
###################################################
T_statistic(xs) # the observed sample


###################################################
### code chunk number 20: lecture-core.Rnw:399-403
###################################################

ts_2 <- replicate(k, rbinom(10, size = 5, p = 0.7) %>% T_statistic)
ggplot(data.frame(T = c(ts_0, ts_1, ts_2), Hypothesis = c(rep('Null', k),
rep('Alternative: 0.6', k), rep('Alternative: 0.7', k))), aes(x = T, fill = Hypothesis, color = Hypothesis)) + geom_density( alpha = 0.75)


###################################################
### code chunk number 21: lecture-core.Rnw:436-440
###################################################

ts_3 <- replicate(k, rbinom(10, size = 5, p = 0.4) %>% T_statistic)
ggplot(data.frame(T = c(ts_0, ts_1, ts_2, ts_3), Hypothesis = c(rep('Null: 0.5', k),
rep('Alternative: 0.6', k), rep('Alternative: 0.7', k), rep('Null: 0.4',k))), aes(x = T, fill = Hypothesis, color = Hypothesis)) + geom_density( alpha = 0.75)


###################################################
### code chunk number 22: lecture-core.Rnw:447-448
###################################################
rr


###################################################
### code chunk number 23: lecture-core.Rnw:482-485
###################################################
rr_upper <- quantile(ts_0, 0.975) # cut the alpha level in half
rr_lower <- quantile(ts_0, 0.025)
T_statistic(xs) < rr_lower || T_statistic(xs) > rr_upper # do we reject?


###################################################
### code chunk number 24: lecture-core.Rnw:530-531
###################################################
(observed_t <- T_statistic(xs))


###################################################
### code chunk number 25: lecture-core.Rnw:535-538
###################################################
p_less    <- mean(ts_0 <= observed_t)
p_greater <- mean(ts_0 >= observed_t)
(pvalue <- 2 * min(p_less, p_greater))


###################################################
### code chunk number 26: lecture-core.Rnw:544-547
###################################################
ts_0.25 <- replicate(10000, T_statistic(rbinom(10, size = 5, p = 0.25)))
(p_0.25 <- 2 * min(mean(ts_0.25 <= observed_t),
                   mean(ts_0.25 >= observed_t)))


###################################################
### code chunk number 27: lecture-core.Rnw:571-574
###################################################
dbenford <- function(x) {
    ifelse(x >= 1 & x <= 9, log((x + 1)/ x, base = 10), 0)
}


###################################################
### code chunk number 28: lecture-core.Rnw:579-581
###################################################
ggplot(data.frame(d = 1:9, P = dbenford(1:9)), aes(x = d, y = P)) + geom_col() + geom_text(aes(label = d, y = P + 0.01))
barplot(dbenford(1:9), names.arg = 1:9)


###################################################
### code chunk number 29: lecture-core.Rnw:588-589
###################################################
pol_digits <- c(23.3, 21.1, 8.5, 11.7, 9.5, 4.2, 3.7, 4.0, 14.1) / 100


###################################################
### code chunk number 30: lecture-core.Rnw:593-594
###################################################
distance <- function(v)  { sqrt(sum((v - dbenford(1:9))^2)) }


###################################################
### code chunk number 31: lecture-core.Rnw:602-603
###################################################
(observed_dist <- distance(pol_digits))


###################################################
### code chunk number 32: lecture-core.Rnw:610-620
###################################################
rbenford <- function(n) {
  sample(1:9, size = n, prob = dbenford(1:9), replace = TRUE)
}
n <- 8396
compute_test_statistic <- function(ds) {
    probs <- hist(ds, breaks = 0:9, plot = FALSE)$density
    distance(probs)
}
null_distances <- replicate(1000,
                             compute_test_statistic(rbenford(n)))


###################################################
### code chunk number 33: lecture-core.Rnw:627-628
###################################################
ggplot(data.frame(distance = null_distances), aes(x = distance)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 34: lecture-core.Rnw:671-675
###################################################
alt_dist <- function(theta) { 
  a <- (10 * (1 + theta) / (10 + theta))^(1/9)
  log10(a * (1:9 + theta + 1) / (1:9 + theta))
}


###################################################
### code chunk number 35: lecture-core.Rnw:680-685
###################################################
plot(1:9, alt_dist(0), type = 'b', xlab = "digit", ylab = "P(d)", pch = 15)
points(alt_dist(0.5), type = 'b', lty = 2, pch = 16)
points(alt_dist(1), type = 'b', lty = 3, pch = 17)
points(alt_dist(10), type = 'b', lty = 4, pch = 18)
legend("topright", legend = c(0, 0.5, 1, 10), lty = 1:4, pch = 15:19)


###################################################
### code chunk number 36: lecture-core.Rnw:690-696
###################################################
alt_0.1 <- replicate(1000, {
    a_sample <- sample(1:9, size = n, replace = TRUE, 
                       prob = alt_dist(0.1))
    compute_test_statistic(a_sample)
})



###################################################
### code chunk number 37: lecture-core.Rnw:700-706
###################################################

ggplot(data.frame(distance = c(null_distances, alt_0.1),
                  hypothesis = c(rep("Null (theta = 0)", length(null_distances)),
                                 rep("Alternative (theta = 0.1)", length(alt_0.1)))),
       aes(x = distance, fill = hypothesis)) + geom_density(alpha = 0.75)



###################################################
### code chunk number 38: lecture-core.Rnw:711-712
###################################################
(p_value <- mean(null_distances >= observed_dist)) # P(T > t)


###################################################
### code chunk number 39: lecture-core.Rnw:721-723
###################################################
(rejection_cutoff <- quantile(null_distances, 0.999))
mean(alt_0.1 >= rejection_cutoff)


###################################################
### code chunk number 40: lecture-core.Rnw:732-741
###################################################
thetas <- seq(0, 0.4, length.out = 20)
power_curve <- map_dbl(thetas, function(theta) {
    alt <- replicate(1000, {
        a_sample <- sample(1:9, size = n, 
                           replace = TRUE, prob = alt_dist(theta))
        compute_test_statistic(a_sample)
    })
    mean(alt >= rejection_cutoff)
})


###################################################
### code chunk number 41: lecture-core.Rnw:747-748
###################################################
plot(thetas, power_curve, type = 'l', xlab = "theta", ylab = "power")


