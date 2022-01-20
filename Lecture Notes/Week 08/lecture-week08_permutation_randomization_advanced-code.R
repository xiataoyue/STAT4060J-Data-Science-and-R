### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week08/week08_permutation_randomization_advanced/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:114-115
###################################################
load("../../../../data/nhanes/nhanes.rda")


###################################################
### code chunk number 3: lecture-core.Rnw:122-125
###################################################
nhanes$healthy <- nhanes$sys_mean <= 120
library(xtable)
print(xtable(table(nhanes$healthy, nhanes$taking_aspirin)))


###################################################
### code chunk number 4: lecture-core.Rnw:132-138
###################################################
observed_a00 <- with(nhanes, table(healthy, taking_aspirin))[1,1]
a00s <- replicate(1000, {
    newz <- sample(nhanes$taking_aspirin) # permutes labels
    table(nhanes$healthy, newz)[1, 1]
})
2 * min(mean(a00s >= observed_a00), mean(a00s <= observed_a00))


###################################################
### code chunk number 5: lecture-core.Rnw:144-146
###################################################
hist(a00s, xlim = range(c(a00s, observed_a00)))
abline(v = observed_a00, col = "red", lwd = 2)


###################################################
### code chunk number 6: lecture-core.Rnw:155-156
###################################################
fisher.test(nhanes$healthy, nhanes$taking_aspirin)$p.value


###################################################
### code chunk number 7: lecture-core.Rnw:160-162
###################################################
load("../../../../data/gamoran.rda")
gamoran$PH.AZ <- gamoran$PHOENIX == "(0) Non-Phoenix"


###################################################
### code chunk number 8: lecture-core.Rnw:179-181
###################################################
hwtab <- table(c("Phoenix, AZ", "San Antonio, TX")[gamoran$PH.AZ + 1], gamoran$B4Y)
print(xtable(hwtab))


###################################################
### code chunk number 9: lecture-core.Rnw:211-213
###################################################
col_totals <- colSums(hwtab)
(midranks <-  col_totals / 2  + cumsum(c(0, col_totals))[1:3]) 


###################################################
### code chunk number 10: lecture-core.Rnw:221-224
###################################################
w <- midranks[gamoran$B4Y]
(obs_stat <- mean(w[gamoran$PH.AZ], na.rm = TRUE) 
    - mean(w[!gamoran$PH.AZ], na.rm = TRUE))


###################################################
### code chunk number 11: lecture-core.Rnw:227-231
###################################################
null_distribution <- replicate(1000, {
    newZ <- sample(gamoran$PH.AZ)
    mean(w[newZ], na.rm = TRUE) - mean(w[!newZ], na.rm = TRUE)
})


###################################################
### code chunk number 12: lecture-core.Rnw:237-240
###################################################
par(mar = c(2, 1, 0, 0))
hist(null_distribution, main = NA)
abline(v = obs_stat, col = "red", lwd = 2)


###################################################
### code chunk number 13: lecture-core.Rnw:243-245
###################################################
2 * min(mean(obs_stat <= null_distribution),
        mean(obs_stat >= null_distribution))


###################################################
### code chunk number 14: lecture-core.Rnw:276-277
###################################################
ggplot(nhanes, aes(x = dia_mean, y = sys_mean)) + geom_point(alpha = 0.5)


###################################################
### code chunk number 15: lecture-core.Rnw:282-283
###################################################
nhanes <- filter(nhanes, !is.na(dia_mean), !is.na(sys_mean))


###################################################
### code chunk number 16: lecture-core.Rnw:288-294
###################################################
observed_cor <- with(nhanes, cor(sys_mean, dia_mean))
cors <- replicate(1000, {
  shuffled_dia <- sample(nhanes$dia_mean)
  cor(nhanes$sys_mean, shuffled_dia)
})
2 * min(mean(cors >= observed_cor), mean(cors <= observed_cor))


###################################################
### code chunk number 17: lecture-core.Rnw:300-303
###################################################
ggplot(data.frame(x = cors), aes(x = x)) + geom_density(fill = "blue", alpha =0.75) + geom_vline(xintercept = observed_cor, col = 'red')
# hist(cors, xlim = range(c(cors, observed_cor)))
# abline(v = observed_cor, col = "red", lwd = 2)


###################################################
### code chunk number 18: lecture-core.Rnw:351-354
###################################################
school_assignments <- group_by(gamoran, Y1SCHOOLID) %>% 
    summarize(z = first(z))
table(school_assignments$z)


###################################################
### code chunk number 19: lecture-core.Rnw:361-368
###################################################
(w_clus <- mean(w[gamoran$z], na.rm = TRUE) - mean(w[!gamoran$z], na.rm = TRUE))
null_distribution_cluster <- replicate(1000, {
    newZ <- sample(school_assignments$z)
    names(newZ) <- school_assignments$Y1SCHOOLID
    studentZ <- newZ[as.character(gamoran$Y1SCHOOLID)]
    mean(w[studentZ], na.rm = TRUE) - mean(w[!studentZ], na.rm = TRUE)
})


###################################################
### code chunk number 20: lecture-core.Rnw:374-377
###################################################
par(mar = c(2, 1, 0, 0))
hist(null_distribution_cluster, main = NA)
abline(v = w_clus, col = "red", lwd = 2)


###################################################
### code chunk number 21: lecture-core.Rnw:380-382
###################################################
2 * min(mean(w_clus <= null_distribution_cluster),
        mean(w_clus >= null_distribution_cluster))


###################################################
### code chunk number 22: lecture-core.Rnw:388-394
###################################################
null_distribution_wrong <- replicate(1000, {
    newZ <- sample(gamoran$z)
    mean(w[newZ], na.rm = TRUE) - mean(w[!newZ], na.rm = TRUE)
})
2 * min(mean(w_clus <= null_distribution_wrong),
        mean(w_clus >= null_distribution_wrong))


###################################################
### code chunk number 23: lecture-core.Rnw:440-441
###################################################
ggplot(nhanes, aes(x = dia_mean, y = sys_mean, color = taking_aspirin)) + geom_point(alpha = 0.5)


###################################################
### code chunk number 24: lecture-core.Rnw:459-461
###################################################
sys_dia_dist <- as.matrix(dist(nhanes[, c("sys_mean", "dia_mean")]))
sys_dia_dist[1:5, 1:5]


###################################################
### code chunk number 25: lecture-core.Rnw:465-468
###################################################
diag(sys_dia_dist) <- Inf # can't be closest to ourselves
nearest_neighbors <- apply(sys_dia_dist, 1, which.min)
nearest_neighbors[1:5]


###################################################
### code chunk number 26: lecture-core.Rnw:473-482
###################################################
teststat <- function(z, nn) {
    sum(z == z[nn])
}
observed_t <- teststat(nhanes$taking_aspirin, nearest_neighbors)
ts <- replicate(1000, {
    z <- sample(nhanes$taking_aspirin)
    teststat(z, nearest_neighbors)
})
2 * min(mean(observed_t <= ts), mean(observed_t >= ts))


###################################################
### code chunk number 27: lecture-core.Rnw:488-496
###################################################

plot(nhanes$sys_mean, nhanes$dia_mean, 
     col = c("red", "blue")[nhanes$taking_aspirin + 1],
     cex = 0.25)

segments(nhanes$sys_mean, nhanes$dia_mean,
         nhanes$sys_mean[nearest_neighbors], nhanes$dia_mean[nearest_neighbors],
         col = c("green", "purple")[1 + (nhanes$taking_aspirin == nhanes$taking_aspirin[nearest_neighbors])])


###################################################
### code chunk number 28: lecture-core.Rnw:501-502
###################################################
gam_mathread <- na.omit(gamoran[, c("z", "Y1SCHOOLID", "MATH_PCTZ", "READ_PCTZ")])


###################################################
### code chunk number 29: lecture-core.Rnw:505-508
###################################################
plot(gam_mathread$READ_PCTZ ~ gam_mathread$MATH_PCTZ,
     col = c("red", "blue")[gam_mathread$z + 1],
     cex = 0.8)


###################################################
### code chunk number 30: lecture-core.Rnw:513-518
###################################################
math_read_dist <- as.matrix(
    dist(gam_mathread[, c("READ_PCTZ", "MATH_PCTZ")]))
diag(math_read_dist) <- Inf # can't be closest to ourselves
mrnn <- apply(math_read_dist, 1, which.min) # row wise apply
mrnn_obs_t <- teststat(gam_mathread$z, mrnn)


###################################################
### code chunk number 31: lecture-core.Rnw:523-531
###################################################
nn_cluster <- replicate(1000, {
    newZ <- sample(school_assignments$z)
    names(newZ) <- school_assignments$Y1SCHOOLID
    studentZ <- newZ[as.character(gam_mathread$Y1SCHOOLID)]
    teststat(studentZ, mrnn)
})
2 * min(mean(mrnn_obs_t <= nn_cluster),
        mean(mrnn_obs_t >= nn_cluster))


### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week08/week08_permutation_randomization_advanced/models.Rnw'

###################################################
### code chunk number 1: models.Rnw:1-6
###################################################
load("../../../../data/nhanes/nhanes.rda")
nhanes$ratio <- nhanes$sys_mean / nhanes$dia_mean
nhanes <- nhanes[!is.na(nhanes$taking_aspirin) & !is.na(nhanes$ratio), ]
library(ggplot2)
library(tidyverse)


###################################################
### code chunk number 2: models.Rnw:34-35
###################################################
ggplot(nhanes, aes(x = sys_mean, fill = taking_aspirin)) + geom_density(alpha = 0.5)


###################################################
### code chunk number 3: models.Rnw:42-43
###################################################
thetas <- seq(-8, 0, length.out = 100)


###################################################
### code chunk number 4: models.Rnw:46-51
###################################################
ps <- sapply(thetas, function(theta) {
  with(nhanes, ## creates variables sys_mean, taking_apsirin
     ks.test(x = sys_mean[taking_aspirin],
             y = sys_mean[!taking_aspirin] - theta)$p.value)
})


###################################################
### code chunk number 5: models.Rnw:54-56
###################################################
(ci90 <- range(thetas[ps >= 0.1])) ## 90% CI
(thetahat <- thetas[which.max(ps)])


###################################################
### code chunk number 6: models.Rnw:62-70
###################################################
par(mar = c(4, 4, 1, 1))
qqplot(nhanes$sys_mean[nhanes$taking_aspirin],
       nhanes$sys_mean[!nhanes$taking_aspirin] - thetahat, type = 'l',
       xlab = "Taking", ylab = "Not Taking (transformed)")
abline(a = 0, b = 1, col = "red")
## plot(ecdf(combined$sys_mean[combined$taking_aspirin]), lty = 1, pch = NA, main = NA)
## plot(ecdf(combined$sys_mean[!combined$taking_aspirin] - thetahat), add = TRUE, col = "red", pch = NA)
## legend("right", fill = c("black", "red"), legend = c("Taking Aspirin", "Not Taking ASA"))


###################################################
### code chunk number 7: models.Rnw:78-79
###################################################
ggplot(nhanes, aes(x = ratio, fill = taking_aspirin)) + geom_density(alpha = 0.5)


###################################################
### code chunk number 8: models.Rnw:86-93
###################################################
thetas <- seq(-1, 1, length.out = 100)
ps <- map_dbl(thetas, function(theta) {
    with(nhanes, 
         ks.test(x = ratio[taking_aspirin],
                 y = ratio[!taking_aspirin] - theta)$p.value)
})
any(ps >= 0.01)


###################################################
### code chunk number 9: models.Rnw:111-119
###################################################
deltas <- seq(0.1, 0.2,length.out = 100)
h <- function(x, delta) { x^(1 + delta * (x > 1.2) )}
ps <- map_dbl(deltas, function(delta) {
with(nhanes, ## creates variables sys_mean, taking_apsirin
     ks.test(x = ratio[taking_aspirin],
             y = h(ratio[!taking_aspirin], delta))$p.value)
})
any(ps >= 0.01)


###################################################
### code chunk number 10: models.Rnw:124-127
###################################################
par(mar = c(4, 4, 1, 1))
plot(deltas, ps, type = 'l', xlab = "delta", ylab = "p-value")
abline(h = 0.10)


###################################################
### code chunk number 11: models.Rnw:132-138
###################################################
hatdelta <- deltas[which.max(ps)]
qqplot(nhanes$ratio[nhanes$taking_aspirin],
       h(nhanes$ratio[!nhanes$taking_aspirin], hatdelta), type = 'l',
       xlab = "Taking", ylab = "Not Taking (transformed)",
       xlim = c(0,15), ylim = c(0,15))
abline(a = 0, b = 1, col = "red")


