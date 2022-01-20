### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week08/week08_permutation_randomization_basics/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:114-116
###################################################
load("../../../../data/spotify_tracks.rda")
tracks10 <- mutate(tracks, duration_sec = track.duration_ms / 1000) %>% filter(duration_sec <= 10 * 60)


###################################################
### code chunk number 3: lecture-core.Rnw:119-120
###################################################
ggplot(tracks10, aes(x = duration_sec)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 4: lecture-core.Rnw:125-130
###################################################
testMedian <- function(median0) {
    y <- tracks10$duration_sec - median0 > 0
    binom.test(sum(y), length(y), p = 0.5)$p.value
}
testMedian(200)


###################################################
### code chunk number 5: lecture-core.Rnw:136-142
###################################################
medians <- unique(sort(c(
             median(tracks10$duration_sec),
             seq(min(tracks10$duration_sec), 
               max(tracks10$duration_sec), 
               length.out = 1000))))
pvalues <- map_dbl(medians, testMedian)


###################################################
### code chunk number 6: lecture-core.Rnw:145-147
###################################################
medians[which.max(pvalues)] # point estimate
range(medians[0.001 <= pvalues]) # 99.9% CI


###################################################
### code chunk number 7: lecture-core.Rnw:152-154
###################################################
xrng <- range(medians[0.0001 < pvalues])
ggplot(data.frame(h_0 = medians, p = pvalues), aes(x = h_0, y = p, color = p <= 0.1)) + geom_point() + geom_abline(intercept = 0.1, slope = 0, lty = 2) + lims(x = xrng)


###################################################
### code chunk number 8: lecture-core.Rnw:183-192
###################################################
testSymmetry <- function(x, theta, k = 1000) {
    n <- length(x)
    s_0 <- sum(x - theta)
    y <- abs(x - theta)
    dist <- map_dbl(rerun(k, 2 * rbinom(n, size = 1, p = 0.5) - 1), 
            ~ sum(y * .x))
    2 * min(dist >= s_0, dist <= s_0) / k
} 
testSymmetry(tracks10$duration_sec, 201)


###################################################
### code chunk number 9: lecture-core.Rnw:258-260
###################################################
load("../../../../data/gamoran.rda")
gamoran$PH.AZ <- gamoran$PHOENIX == "(0) Non-Phoenix"


###################################################
### code chunk number 10: lecture-core.Rnw:262-264
###################################################
mean_diff <- function(w, z) {mean(w[z], na.rm = TRUE) - 
                                 mean(w[!z], na.rm = TRUE)}


###################################################
### code chunk number 11: lecture-core.Rnw:267-274
###################################################
n <- nrow(gamoran)
dist.t <- replicate(1000, {
    ## shuffle the "Z_i" values
    permuted_label <- sample(gamoran$PH.AZ)
    ## compute the test statistic
    mean_diff(gamoran$READ_PCTZ, permuted_label)
})


###################################################
### code chunk number 12: lecture-core.Rnw:281-282
###################################################
plot(density(dist.t), main = NA)


###################################################
### code chunk number 13: lecture-core.Rnw:296-298
###################################################
(t_observed <- mean_diff(gamoran$READ_PCTZ, gamoran$PH.AZ))
2 * min(mean(dist.t <= t_observed), mean(dist.t >= t_observed))


###################################################
### code chunk number 14: lecture-core.Rnw:328-337
###################################################
par(mar = c(2,4,1,1))
curve(pnorm(x), -2, 2, ylab = "P(X <= x)")
curve(pnorm(x, mean = 0.5), add = TRUE, lty = 2)
legend("topleft", lty = 1:2, legend = c("F", "G"))
x0 <- c(-1, -0, 0.25, 1)
y0 <- pnorm(x0, mean = 0.5)
y1 <- pnorm(x0)
segments(x0, y0, x0, y1, lwd = 2)
text(x = x0, y1 + 0.1, labels = round(y1 - y0, 3), col = c("black", "black", "red", "black"))


###################################################
### code chunk number 15: lecture-core.Rnw:343-346
###################################################
load("../../../../data/nhanes/nhanes.rda")
nhanes$ratio <- nhanes$sys_mean / nhanes$dia_mean
nhanes <- nhanes[!is.na(nhanes$taking_aspirin) & !is.na(nhanes$ratio), ]


###################################################
### code chunk number 16: lecture-core.Rnw:355-360
###################################################
xx <- c(1, 3) 
par(mar = c(2,0,0,0))
curve(ecdf(nhanes$ratio[nhanes$taking_aspirin])(x), from = xx[1], to = xx[2], xlab = NA, ylab = NA)
curve(ecdf(nhanes$ratio[!nhanes$taking_aspirin])(x), from = xx[1], to = xx[2], add =TRUE, col = "red", lty = 2)
legend("topleft", fill = c("black", "red"), legend = c("Taking", "Not taking"))


###################################################
### code chunk number 17: lecture-core.Rnw:367-372
###################################################
ks <- function(w, z) {
  f <- ecdf(w[z == 1])
  g <- ecdf(w[z == 0])
  max(abs(f(w) - g(w)))
}


###################################################
### code chunk number 18: lecture-core.Rnw:375-382
###################################################
perms <- replicate(1000, sample(nhanes$taking_aspirin))
ts <- apply(perms, 2, function(zstar) { 
    ks(nhanes$ratio, zstar) 
})

observed_ks <- ks(nhanes$ratio, nhanes$taking_aspirin)
(ksp <- 2 * min(mean(ts >= observed_ks), mean(ts <= observed_ks)))


###################################################
### code chunk number 19: lecture-core.Rnw:388-391
###################################################
par(mar = c(3, 3, 1,1))
plot(density(ts), xlim = range(c(observed_ks, ts)), main = NA)
abline(v = observed_ks, col = "red")


###################################################
### code chunk number 20: lecture-core.Rnw:489-492
###################################################
with(nhanes, ## creates variables ratio, taking_apsirin
     ks.test(x = ratio[taking_aspirin],
             y = ratio[!taking_aspirin]))


###################################################
### code chunk number 21: lecture-core.Rnw:496-499
###################################################
with(nhanes,
     wilcox.test(x = ratio[taking_aspirin],
                 y = ratio[!taking_aspirin]))


