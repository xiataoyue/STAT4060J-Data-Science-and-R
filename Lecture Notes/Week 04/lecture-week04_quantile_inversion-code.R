### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week04/week04_quantile_inversion/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:53-57
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:96-99
###################################################
x <- rexp(100)
ggplot(data.frame(sample = x), aes(sample = sample)) +
geom_qq(distribution = qexp) + geom_qq_line(distribution = qexp)


###################################################
### code chunk number 3: lecture-core.Rnw:104-106
###################################################
ggplot(data.frame(sample = x), aes(sample = sample)) +
geom_qq(distribution = qnorm) + geom_qq_line(distribution = qnorm)


###################################################
### code chunk number 4: lecture-core.Rnw:123-131
###################################################
par(mar = c(2, 0, 0, 0))
f <- function(x) {
    (0 <= x & x <= 2) *
        ifelse(x < 1,
               x,
               2 - x)
}
curve(f, 0, 2)


###################################################
### code chunk number 5: lecture-core.Rnw:154-162
###################################################
F <- function(x) {
    ifelse(x < 1,
           x^2 / 2,
           2 * x - x^2/2 - 1
           )
}
curve(F(x), 0, 2)
abline(v = 1, lty = 2, col = "grey")


###################################################
### code chunk number 6: lecture-core.Rnw:255-261
###################################################
Q_theta <- function(u, theta) {
    u * theta
}

k <- 10e5
u_0_3 <- Q_theta(runif(k), 3)


###################################################
### code chunk number 7: lecture-core.Rnw:266-267
###################################################
ggplot(data.frame(u = u_0_3), aes(x = u)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 8: lecture-core.Rnw:281-288
###################################################
Q_tri <- function(u) {
    ifelse(u <= 1/2,
           sqrt(2 * u),
           2 - sqrt(2 - 2 * u))
}
Q_tri(c(0.25, 0.5, 0.75, 1))
triangulars <- Q_tri(runif(100000))


###################################################
### code chunk number 9: lecture-core.Rnw:294-295
###################################################
ggplot(data.frame(x = triangulars), aes(x = x)) + geom_density(fill = "blue", alpha = 0.5) + stat_function(fun = f, color = "red", lwd = 2)


###################################################
### code chunk number 10: lecture-core.Rnw:308-310
###################################################
x <- Q_tri(runif(10000))
mean(x^2) - 1^2


###################################################
### code chunk number 11: lecture-core.Rnw:325-329
###################################################
rx <- function(n, theta) {
    runif(n)^(1/theta)
}
xs_theta3<- rx(10000, 3)


###################################################
### code chunk number 12: lecture-core.Rnw:334-340
###################################################
# par(mar = c(4,2, 1, 0))
# plot(density(xs_theta3, from = 0, to = 1, cut = 0), main = NA, xlab = NA)
# curve(3 * x^2, add = TRUE, lty = 2, col = "red", lwd = 2)
# legend("topleft", fill = c("black", "red"), legend = c("Estimated", "True"))
qx <- function(p, theta = 3) { p^(1/theta) } 
ggplot(data.frame(x = xs_theta3), aes(sample = x)) + geom_qq(distribution = qx) + geom_qq_line(distribution = qx)


###################################################
### code chunk number 13: lecture-core.Rnw:359-361
###################################################
mom <- function(x) { mean(x) / (1 - mean(x)) }
mle <- function(x) { - length(x) / sum(log(x)) }


###################################################
### code chunk number 14: lecture-core.Rnw:364-370
###################################################
theta <- 3
n <- 50
k <- 1000
samples <- rerun(k, rx(n, theta = 3))
moms <- map_dbl(samples, mom)
mles <- map_dbl(samples, mle)


###################################################
### code chunk number 15: lecture-core.Rnw:376-385
###################################################
par(mar = c(3, 3, 4, 2))
dmom <- density(moms)
dmle <- density(mles)
xrng <- range(c(dmom$x, dmle$x))
yrng <- range(c(dmom$y, dmle$y))
plot(dmom, xlim = xrng, ylim = yrng, main = "Dist. of Estimators")
lines(dmle, col = "red", lty = 2, lwd = 2)
abline(v = 3, col = "blue")
legend("topleft", fill = c("black", "red"), legend = c("MoM", "MLE"))


###################################################
### code chunk number 16: lecture-core.Rnw:391-393
###################################################
mean((moms - theta)^2)
mean((mles - theta)^2)


###################################################
### code chunk number 17: lecture-core.Rnw:422-432
###################################################
Q_bin <- function(t, n, p) {
  so_far <- 0
  for (i in 0:n) {
      so_far <- so_far + dbinom(i, n, p)
      if (so_far >= t) {
          return(i)
      }
  }
  return(n) # this shouldn't happen, but be safe!
}


###################################################
### code chunk number 18: lecture-core.Rnw:434-439
###################################################
par(mfrow = c(1,3))
barplot(dbinom(0:5, size = 5, p = 0.25), names.arg = 0:5, main = "f(x)")
plot(stepfun(0:5, c(0, pbinom(0:5, size = 5, p = 0.25))), pch = 20, main = "F(x)")
qqrange <- pbinom(0:5, size = 5, p = 0.25)
plot(stepfun(qqrange, c(0, Vectorize(Q_bin)(qqrange + .Machine$double.eps, n = 5, p = 0.25))), pch = 20, main = "Q(u)")


###################################################
### code chunk number 19: lecture-core.Rnw:476-477
###################################################
Q_bin


###################################################
### code chunk number 20: lecture-core.Rnw:482-483
###################################################
xs <- map_dbl(runif(10000), ~ Q_bin(.x, 5, 0.2)) # non-vectorized Q_bin


###################################################
### code chunk number 21: lecture-core.Rnw:485-486
###################################################
ggplot(data.frame(x = xs), aes(x = xs)) + geom_histogram(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 22: lecture-core.Rnw:523-524
###################################################
options(digits = 4)


###################################################
### code chunk number 23: lecture-core.Rnw:526-527
###################################################
rbenford <- function(n) { ceiling(10^runif(n) - 1) }


###################################################
### code chunk number 24: lecture-core.Rnw:530-533
###################################################
k <- 10000
rbind(log10((2:10) / (1:9)), # analytical P(D = d)
      table(rbenford(k)) / k) # empirical P(D = d)


###################################################
### code chunk number 25: lecture-core.Rnw:554-568
###################################################
makeCDFTable <- function(u, lambda) {
    maxu <- max(u)
    # base case: x= 0
    fx <- exp(-lambda)
    cdf <- c(fx)
    x <- 0
    # build table until F(k) >= maxu
    while (last(cdf) < maxu) {
        x <- x + 1
        fx <- fx * lambda / x
        cdf <- c(cdf, last(cdf) + fx)
    }
    return(cdf)
}


###################################################
### code chunk number 26: lecture-core.Rnw:573-575
###################################################
makeCDFTable(c(0, 0.95), 3)
ppois(0:6, lambda = 3)


###################################################
### code chunk number 27: lecture-core.Rnw:580-587
###################################################
rpoisson <- function(n, lambda) {
    u <- runif(n)
    tbl <- makeCDFTable(u, lambda)
    map_dbl(u, function(u_i) {
        min(which(tbl >= u_i)) - 1 ## table is defined on 0, 1, 2, ...
    })
}


###################################################
### code chunk number 28: lecture-core.Rnw:592-599
###################################################
df <- data.frame(
    type = c(rep("Sample", 16), rep("Actual", 16)),
    x = c(0:15, 0:15),
    y = c(ecdf(rpoisson(10000, 7))(0:15),
          ppois(0:15, lambda = 7)))
plt <- ggplot(df, aes(x = x, y = y, color = type)) +
    geom_step() + labs(y = "F(x)")


###################################################
### code chunk number 29: lecture-core.Rnw:605-606
###################################################
print(plt)


###################################################
### code chunk number 30: lecture-core.Rnw:623-625
###################################################
true_theta <- 1/2
fail_times <- rx(n = 20, theta = true_theta)


###################################################
### code chunk number 31: lecture-core.Rnw:641-642
###################################################
fail_times[1:5] # just the first 5 of 20


###################################################
### code chunk number 32: lecture-core.Rnw:647-651
###################################################
n <- length(fail_times) # 20
k <- 1000
null_samples <- rerun(k, rx(n, theta = 1/2))
alt_samples  <- rerun(k, rx(n, theta = 1))


###################################################
### code chunk number 33: lecture-core.Rnw:657-662
###################################################
null_mle <- map_dbl(null_samples, mle)
null_mom <- map_dbl(null_samples, mom)

alt_mle <- map_dbl(alt_samples, mle)
alt_mom <- map_dbl(alt_samples, mom)


###################################################
### code chunk number 34: lecture-core.Rnw:668-678
###################################################

par(mar = c(3, 3, 0, 2))
dnull_mle <- density(null_mle)
dalt_mle <- density(alt_mle)
xrng <- range(c(dnull_mle$x, dalt_mle$x))
yrng <- range(c(dnull_mle$y, dalt_mle$y))
plot(dnull_mle, xlim = xrng, ylim = yrng, main = NA)
lines(dalt_mle, col = "red", lty = 2, lwd = 2)
abline(v = 3, col = "blue")
legend("topright", fill = c("black", "red"), legend = c("Null", "Alt."))


###################################################
### code chunk number 35: lecture-core.Rnw:685-695
###################################################

par(mar = c(3, 3, 0, 2))
dnull_mom <- density(null_mom)
dalt_mom <- density(alt_mom)
xrng <- range(c(dnull_mom$x, dalt_mom$x))
yrng <- range(c(dnull_mom$y, dalt_mom$y))
plot(dnull_mom, xlim = xrng, ylim = yrng, main = NA)
lines(dalt_mom, add = TRUE, col = "red", lty = 2, lwd = 2)
abline(v = 3, col = "blue")
legend("topright", fill = c("black", "red"), legend = c("Null", "Alt."))


###################################################
### code chunk number 36: lecture-core.Rnw:702-704
###################################################
cutoff_mle <- quantile(null_mle, 0.9)
cutoff_mom <- quantile(null_mom, 0.9)


###################################################
### code chunk number 37: lecture-core.Rnw:707-709
###################################################
mean(alt_mle > cutoff_mle)
mean(alt_mom > cutoff_mom)


###################################################
### code chunk number 38: lecture-core.Rnw:714-717
###################################################
(observed_mle <- mle(fail_times))
## accept if true
observed_mle <= cutoff_mle


###################################################
### code chunk number 39: lecture-core.Rnw:724-731
###################################################
thetas <- seq(0.001, 1, length.out = 1000) 
test_theta <- function(theta) {
    samples <- rerun(k, rx(n, theta))
    null_mles <- map_dbl(samples, mle)
    cutoff <- quantile(null_mles, c(0.025, 0.975))
    cutoff[1] <= observed_mle & observed_mle <= cutoff[2]
}


###################################################
### code chunk number 40: lecture-core.Rnw:736-739
###################################################
accepted <- map_lgl(thetas, test_theta)
min(thetas[accepted]) 
max(thetas[accepted])


###################################################
### code chunk number 41: lecture-core.Rnw:754-755
###################################################
curve(cos(x), 0, pi / 2, xlab = "x", ylab = "f(x)", main = "Density")


###################################################
### code chunk number 42: lecture-core.Rnw:763-764
###################################################
curve(sin(x), 0, pi / 2, xlab = "x", ylab = "F(x)", main = "Distribution")


###################################################
### code chunk number 43: lecture-core.Rnw:771-772
###################################################
curve(asin(x), 0, 1, xlab = "u", ylab = "Q(u)", main = "Quantile")


###################################################
### code chunk number 44: lecture-core.Rnw:779-782
###################################################
ggplot(data.frame(x = asin(runif(1000))), aes(x = x)) + geom_density(fill = "blue", alpha = 0.3) +
    stat_function(fun = cos, color= "red")



###################################################
### code chunk number 45: lecture-core.Rnw:810-813
###################################################
rgeo <- function(n, theta) {
   ceiling(log(1 - runif(n)) / log(1 - theta))
}


###################################################
### code chunk number 46: lecture-core.Rnw:816-817
###################################################
hist(rgeo(10000, 0.25))


