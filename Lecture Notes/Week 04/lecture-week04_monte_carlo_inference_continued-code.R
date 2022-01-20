### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week04/week04_monte_carlo_inference_continued/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:178-182
###################################################
alpha <- 0.05 ; k <- 10000 ; n <- 20
mu <- 2 # unknown truth
sigma2 <- 10 ; sigma <- sqrt(sigma2) # known variance
samples_norm <- rerun(k, rnorm(n, mean = mu, sd = sigma))


###################################################
### code chunk number 3: lecture-core.Rnw:185-188
###################################################
const_norm <- qnorm(1 - alpha/2) * (sigma / sqrt(n))
cis_norm <- map(samples_norm,
                ~ mean(.x) + c(-1, 1) * const_norm)


###################################################
### code chunk number 4: lecture-core.Rnw:191-194
###################################################
const_t <- qt(1 - alpha/2, df = n - 1) / sqrt(n)
cis_t <- map(samples_norm,
~ mean(.x) + c(-1, 1) * const_t * sd(.x))


###################################################
### code chunk number 5: lecture-core.Rnw:200-202
###################################################
map_dbl(cis_norm, ~ .x[1] <= mu && mu <= .x[2]) %>% mean
map_dbl(cis_t, ~ .x[1] <= mu && mu <= .x[2]) %>% mean


###################################################
### code chunk number 6: lecture-core.Rnw:207-209
###################################################
map_dbl(cis_norm, ~ .x[2] - .x[1]) %>% mean
map_dbl(cis_t, ~ .x[2] - .x[1]) %>% mean


###################################################
### code chunk number 7: lecture-core.Rnw:214-235
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
### code chunk number 8: lecture-core.Rnw:266-270
###################################################
n <- 10000
x <- rcauchy(n)
y <- x > 1
ybar <- mean(y)


###################################################
### code chunk number 9: lecture-core.Rnw:273-274
###################################################
(norm_ci <- ybar + c(-1,1) * qnorm(0.975) * sqrt(ybar * (1 - ybar) / n))


###################################################
### code chunk number 10: lecture-core.Rnw:294-295
###################################################
theta_0 <- seq(0, 1, length.out = 1000)


###################################################
### code chunk number 11: lecture-core.Rnw:299-302
###################################################
a0 <- qbinom(0.025, size = n, prob = theta_0)
b0 <- qbinom(0.025, size = n, prob = theta_0,
             lower.tail = FALSE) - 1


###################################################
### code chunk number 12: lecture-core.Rnw:308-310
###################################################
w <- sum(y)
range(theta_0[a0 < w & w < b0])


###################################################
### code chunk number 13: lecture-core.Rnw:314-315
###################################################
norm_ci


###################################################
### code chunk number 14: lecture-core.Rnw:332-336
###################################################
theta <- 0.25
n <- 20
k <- 1000
xs <- rbinom(k, size = n, prob =  theta)


###################################################
### code chunk number 15: lecture-core.Rnw:340-347
###################################################
tints <- map(xs, function(x) {
    if (x == 0 || x == n) {
        return(c(0, 1)) # can't estimate
    } else {
       return(t.test(c(rep(1, x), rep(0, n - x)))$conf.int)
    }
})


###################################################
### code chunk number 16: lecture-core.Rnw:350-352
###################################################
bints <- map(xs, ~ binom.test(.x, n)$conf.int)
pints <- map(xs, ~ prop.test(.x, n)$conf.int)


###################################################
### code chunk number 17: lecture-core.Rnw:359-363
###################################################
cover <- function(x) { x[1] <= theta && theta <= x[2] }
(tcover <- map_dbl(tints, cover) %>% mean)
(bcover <- map_dbl(bints, cover) %>% mean)
(pcover <- map_dbl(pints, cover) %>% mean)


###################################################
### code chunk number 18: lecture-core.Rnw:368-371
###################################################
(twidth <- map_dbl(tints, diff) %>% mean)
(bwidth <- map_dbl(bints, diff) %>% mean)
(pwidth <- map_dbl(pints, diff) %>% mean)


###################################################
### code chunk number 19: lecture-core.Rnw:402-405
###################################################
dbenford <- function(x) {
    ifelse(x >= 1 & x <= 9, log((x + 1)/ x, base = 10), 0)
}


###################################################
### code chunk number 20: lecture-core.Rnw:410-412
###################################################
ggplot(data.frame(d = 1:9, P = dbenford(1:9)), aes(x = d, y = P)) + geom_col() + geom_text(aes(label = d, y = P + 0.01))
barplot(dbenford(1:9), names.arg = 1:9)


###################################################
### code chunk number 21: lecture-core.Rnw:419-420
###################################################
pol_digits <- c(23.3, 21.1, 8.5, 11.7, 9.5, 4.2, 3.7, 4.0, 14.1) / 100


###################################################
### code chunk number 22: lecture-core.Rnw:424-425
###################################################
distance <- function(v)  { sqrt(sum((v - dbenford(1:9))^2)) }


###################################################
### code chunk number 23: lecture-core.Rnw:433-434
###################################################
(observed_dist <- distance(pol_digits))


###################################################
### code chunk number 24: lecture-core.Rnw:441-451
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
### code chunk number 25: lecture-core.Rnw:458-459
###################################################
ggplot(data.frame(distance = null_distances), aes(x = distance)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 26: lecture-core.Rnw:502-506
###################################################
alt_dist <- function(theta) { 
  a <- (10 * (1 + theta) / (10 + theta))^(1/9)
  log10(a * (1:9 + theta + 1) / (1:9 + theta))
}


###################################################
### code chunk number 27: lecture-core.Rnw:511-516
###################################################
plot(1:9, alt_dist(0), type = 'b', xlab = "digit", ylab = "P(d)", pch = 15)
points(alt_dist(0.5), type = 'b', lty = 2, pch = 16)
points(alt_dist(1), type = 'b', lty = 3, pch = 17)
points(alt_dist(10), type = 'b', lty = 4, pch = 18)
legend("topright", legend = c(0, 0.5, 1, 10), lty = 1:4, pch = 15:19)


###################################################
### code chunk number 28: lecture-core.Rnw:521-527
###################################################
alt_0.1 <- replicate(1000, {
    a_sample <- sample(1:9, size = n, replace = TRUE, 
                       prob = alt_dist(0.1))
    compute_test_statistic(a_sample)
})



###################################################
### code chunk number 29: lecture-core.Rnw:531-537
###################################################

ggplot(data.frame(distance = c(null_distances, alt_0.1),
                  hypothesis = c(rep("Null (theta = 0)", length(null_distances)),
                                 rep("Alternative (theta = 0.1)", length(alt_0.1)))),
       aes(x = distance, fill = hypothesis)) + geom_density(alpha = 0.75)



###################################################
### code chunk number 30: lecture-core.Rnw:542-543
###################################################
(p_value <- mean(null_distances >= observed_dist)) # P(T > t)


###################################################
### code chunk number 31: lecture-core.Rnw:552-554
###################################################
(rejection_cutoff <- quantile(null_distances, 0.999))
mean(alt_0.1 >= rejection_cutoff)


###################################################
### code chunk number 32: lecture-core.Rnw:563-572
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
### code chunk number 33: lecture-core.Rnw:578-579
###################################################
plot(thetas, power_curve, type = 'l', xlab = "theta", ylab = "power")


