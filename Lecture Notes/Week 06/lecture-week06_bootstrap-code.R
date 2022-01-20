### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week06/week06_bootstrap/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:53-57
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:64-65
###################################################
load("C:/Users/James Xia/Desktop/STAT4060J/Lecture Notes/Week 06/nhanes.rda")


###################################################
### code chunk number 3: lecture-core.Rnw:86-87
###################################################
ggplot(nhanes, aes(x = dia_mean, y = sys_mean, color = taking_aspirin)) + geom_point(alpha = 0.8, size = 0.5)


###################################################
### code chunk number 4: lecture-core.Rnw:176-180
###################################################
sys_dia <- na.omit(nhanes[, c('sys_mean', 'dia_mean')])
sys_dia <- sys_dia[sys_dia$sys_mean > 0 & sys_dia$dia_mean > 0, ]
sys_mean <- sys_dia$sys_mean
dia_mean <- sys_dia$dia_mean


###################################################
### code chunk number 5: lecture-core.Rnw:186-187
###################################################
ggplot(sys_dia, aes(x = sys_mean)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 6: lecture-core.Rnw:210-217
###################################################
trimmed_mean <- function(x, p) {
    xqs <- quantile(x, c(p/2, 1 - (p/2)))
    keep <- x > xqs[1] & x < xqs[2]
    mean(x[keep])
}
(observed_mean <- mean(sys_mean))
(observed_trim <- trimmed_mean(sys_mean, p = 0.2))


###################################################
### code chunk number 7: lecture-core.Rnw:224-228
###################################################
B <- 10000
n <- length(sys_mean)
bootstrap_samples <- rerun(B,
    sample(sys_mean, size = n, replace = TRUE))


###################################################
### code chunk number 8: lecture-core.Rnw:231-233
###################################################
bootstrap_means <- map_dbl(bootstrap_samples, mean)
bootstrap_trims <- map_dbl(bootstrap_samples, trimmed_mean, p = 0.2)


###################################################
### code chunk number 9: lecture-core.Rnw:238-241
###################################################
df <- rbind(data.frame(statistic = "mean", x = bootstrap_means),
            data.frame(statistic = "trimmed", x = bootstrap_trims))
ggplot(df, aes(x = x, fill = statistic)) + geom_density(alpha = 0.5)


###################################################
### code chunk number 10: lecture-core.Rnw:293-294
###################################################
observed_trim + c(1, -1) * qnorm(0.025) * sd(bootstrap_trims)


###################################################
### code chunk number 11: lecture-core.Rnw:345-349
###################################################
ba_trim <- quantile(bootstrap_trims, c(0.975, 0.025))
basic_trim <- 2 * observed_trim - ba_trim
names(basic_trim) <- NULL # quantile adds some names, we don't need them
basic_trim


###################################################
### code chunk number 12: lecture-core.Rnw:368-369
###################################################
quantile(bootstrap_trims, c(0.025, 0.975))


###################################################
### code chunk number 13: lecture-core.Rnw:398-403
###################################################
trimmed_mean_boot <- function(x, index, p = 0.1) {
    trimmed_mean(x[index], p = p)
}
library(boot)
boot_tm <- boot(sys_mean, statistic = trimmed_mean_boot, p = 0.2, R = B)


###################################################
### code chunk number 14: lecture-core.Rnw:408-409
###################################################
boot.ci(boot_tm, type = c("norm", "basic", "perc"))


### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week06/week06_bootstrap/bootstrap.Rnw'

###################################################
### code chunk number 1: bootstrap.Rnw:1-6
###################################################
library(boot)
library(tidyverse)
library(ggplot2)

nhanes <- nhanes[!is.na(nhanes$sys_mean) & !is.na(nhanes$dia_mean), ]


###################################################
### code chunk number 2: bootstrap.Rnw:11-12
###################################################
ggplot(nhanes,  aes(x = dia_mean, y = sys_mean)) + geom_point(alpha = 0.25) + labs(x = "Diastolic", y = "Systolic")


###################################################
### code chunk number 3: bootstrap.Rnw:19-26
###################################################
cor_boot_stat <- function(x, index) {
    cor(x[index, 1], x[index, 2], use = "complete")
}
library(boot)
boot_cor <- boot(nhanes[, c("sys_mean", "dia_mean")],
                 statistic = cor_boot_stat,
                 R = 1000)


###################################################
### code chunk number 4: bootstrap.Rnw:32-33
###################################################
ggplot(data.frame(x = boot_cor$t), aes(x = x)) + geom_density(fill = "blue", alpha = 0.25)


###################################################
### code chunk number 5: bootstrap.Rnw:40-41
###################################################
boot.ci(boot_cor, type = c("norm", "basic", "perc"))


###################################################
### code chunk number 6: bootstrap.Rnw:81-83
###################################################
sysdia_ratio <- with(nhanes, sys_mean / dia_mean)
sysdia_ratio <- sysdia_ratio[is.finite(log(sysdia_ratio))]


###################################################
### code chunk number 7: bootstrap.Rnw:87-88
###################################################
ggplot(data.frame(x = log(sysdia_ratio)), aes(x = x)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 8: bootstrap.Rnw:94-97
###################################################
library(boot)
mean_boot <- function(x, index) { mean(x[index]) }
boot_mean <- boot(log(sysdia_ratio), statistic = mean_boot, R = 1000)


###################################################
### code chunk number 9: bootstrap.Rnw:102-103
###################################################
boot.ci(boot_mean, type = c("norm", "basic", "perc"))


###################################################
### code chunk number 10: bootstrap.Rnw:108-113
###################################################
B <- 1000
lsdr <- log(sysdia_ratio)
n <- length(lsdr)
est_t <- mean(lsdr)
est_var_t <- var(lsdr) / n


###################################################
### code chunk number 11: bootstrap.Rnw:117-123
###################################################
boot_sv <- replicate(B, {
    xstar <- sample(lsdr, replace = TRUE)
    (mean(xstar) - est_t) / sqrt(var(xstar) / n)
})
(boot_ci_svar <- est_t - sqrt(est_var_t) *
    quantile(boot_sv, c(0.975, 0.025)))


###################################################
### code chunk number 12: bootstrap.Rnw:128-133
###################################################
boot_for_var <- replicate(100, {
    xstar <- sample(lsdr, replace = TRUE)
    mean(xstar)
})
boot_var_est <- var(boot_for_var)


###################################################
### code chunk number 13: bootstrap.Rnw:139-150
###################################################
boot_boot <- replicate(100, {
    xstar <- sample(lsdr, replace = TRUE)
    xstar_boot <- replicate(100, {
        xstarstar <- sample(xstar, replace = TRUE)
        mean(xstarstar)
    })
    (mean(xstar) - est_t) / sd(xstar_boot)
})

(boot_ci_boot <- est_t - sqrt(boot_var_est) *
     quantile(boot_boot, c(0.975, 0.025)))


###################################################
### code chunk number 14: bootstrap.Rnw:156-162
###################################################
mean_var <- function(x, index) { 
  xstar <- x[index]
  n <- length(xstar)
  c(mean(xstar), var(xstar) / n)
}
boot_both <- boot(lsdr, mean_var, R = 1000)


###################################################
### code chunk number 15: bootstrap.Rnw:167-168
###################################################
boot.ci(boot_both, type = 'stud')


###################################################
### code chunk number 16: bootstrap.Rnw:173-187
###################################################
tmp <- boot.ci(boot_both, type = c("basic", "perc", "stud"))
tbl <- rbind(tmp$basic[4:5],
tmp$percent[4:5],
tmp$student[4:5])
aa <- tmp$basic[5] - tmp$basic[4]
bb <- tmp$percent[5] - tmp$percent[4]
cc <- tmp$student[5] - tmp$student[4]

tbl <- cbind(tbl, c(1, bb / aa, cc / aa))
colnames(tbl) <- c("Low", "High", "Rel. Width")
rownames(tbl) <- c("Basic", "Percentile", "Studentized")

library(xtable)
print(xtable(tbl, digits = 9))


###################################################
### code chunk number 17: bootstrap.Rnw:197-206
###################################################
median_idx <- function(data, idx) {
  median(data[idx])
}

median_nested <- function(data, idx) {
    xstar <- data[idx]
    meds <- boot(xstar, median_idx, R = 100)$t # just the T values
    return(c(median(xstar), var(meds)))
}


###################################################
### code chunk number 18: bootstrap.Rnw:211-221
###################################################
library(parallel)
cl <- makeCluster(detectCores())

## load the nested bootstrap components on the cluster
ignore <- clusterEvalQ(cl, library(boot))
clusterExport(cl, c("median_idx", "median_nested"))

boot_median <- boot(lsdr, median_nested, R = 1000,
  parallel = "snow", cl = cl, ncpus = detectCores())
stopCluster(cl)


###################################################
### code chunk number 19: bootstrap.Rnw:226-227
###################################################
boot.ci(boot_median, type = "stud")


### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week06/week06_bootstrap/confcoef.Rnw'

###################################################
### code chunk number 1: confcoef.Rnw:1-4
###################################################
library(tidyverse)
library(ggplot2)
set.seed(30320392)


###################################################
### code chunk number 2: confcoef.Rnw:42-49
###################################################

rlaplace <- function(n, mean) {
    s <- 2 * rbinom(n, size = 1, p = 0.5) - 1
    m <- rexp(n) 
    s * m + mean
}



###################################################
### code chunk number 3: confcoef.Rnw:53-59
###################################################
k <- 10000
laplace_intervals <- rerun(k, {
  t.test(rlaplace(20, mean = 1/2), conf.level = 0.95)$conf.int 
})
a1 <- map_dbl(laplace_intervals, ~ .x[1] <= 1/2 & .x[2] >= 1/2)
binom.test(sum(a1), k, conf.level = 0.99)$conf.int


###################################################
### code chunk number 4: confcoef.Rnw:64-69
###################################################
exponential_intervals <- rerun(k, { 
  t.test(rexp(20, rate = 2), conf.level = 0.95)$conf.int 
})
a2 <- map_dbl(exponential_intervals, ~ .x[1] <= 1/2 & .x[2] >= 1/2)
binom.test(sum(a2), k, conf.level = 0.99)$conf.int


###################################################
### code chunk number 5: confcoef.Rnw:87-98
###################################################
library(boot)
library(parallel)
cl <- makeCluster(detectCores() - 1)

## Example usage:
mean_boot <- function(x, idx) { mean(x[idx]) }
a <- boot(rlaplace(20, 1/2), mean_boot, R = 1000,
          parallel = "snow",
          cl = cl,
          ncpus = 2) %>% # bug: must be greater than 1
       boot.ci(type = c("norm", "basic", "perc"))


###################################################
### code chunk number 6: confcoef.Rnw:103-104
###################################################
print(a)


###################################################
### code chunk number 7: confcoef.Rnw:109-111
###################################################
names(a)
a$normal ; a$basic ; a$percent


###################################################
### code chunk number 8: confcoef.Rnw:116-123
###################################################
getCIs <- function(boot_ci_result) {
    with(boot_ci_result,
         matrix(c(normal[2:3],
                  basic[4:5],
                  percent[4:5]), nrow = 2))
}
getCIs(a)


###################################################
### code chunk number 9: confcoef.Rnw:128-135
###################################################
k <- 1000 ; R <- 1000
laplace_bootstrap_cis <- rerun(k, {
   boot(rlaplace(20, 1/2), mean_boot, R = R,
        parallel = "snow", cl = cl, ncpus = 2) %>%
       boot.ci(type = c("norm", "basic", "perc")) %>%
       getCIs
})


###################################################
### code chunk number 10: confcoef.Rnw:140-146
###################################################
exp_bootstrap_cis <- rerun(k, {
   boot(rexp(20, 2), mean_boot, R = R,
        parallel = "snow", cl = cl, ncpus = 2) %>%
       boot.ci(type = c("norm", "basic", "perc")) %>%
       getCIs
})


###################################################
### code chunk number 11: confcoef.Rnw:151-157
###################################################
covers <- function(x) { x[1, ] <= 1/2 & x[2, ] >= 1/2 }
laplace_bootstrap_covers <- map(laplace_bootstrap_cis, covers) %>% 
    simplify2array  %>% rowSums

exp_bootstrap_covers <- map(exp_bootstrap_cis, covers) %>% 
    simplify2array %>% rowSums


###################################################
### code chunk number 12: confcoef.Rnw:161-161
###################################################



###################################################
### code chunk number 13: confcoef.Rnw:164-166
###################################################
coverage_ci <- function(x) { binom.test(x, k, conf.level = 0.99)$conf.int[1:2] }
map(laplace_bootstrap_covers, coverage_ci)


###################################################
### code chunk number 14: confcoef.Rnw:171-172
###################################################
map(exp_bootstrap_covers, coverage_ci)


###################################################
### code chunk number 15: confcoef.Rnw:185-210
###################################################
sample_sizes <- 4:9
tmp <- map(2^sample_sizes, function(n) {
    exp_bootstrap_cis <- rerun(k, {
        samplex <- rexp(n, 2)
        tmp <- boot(samplex, mean_boot, R = R,
             parallel = "snow", cl = cl, ncpus = 2) %>%
            boot.ci(type = c("norm", "basic", "perc")) %>%
            getCIs %>% covers
        tci <- t.test(samplex)$conf.int
        c(tmp, tci[1] <= 1/2 && tci[2] >= 1/2)
    }) %>% simplify2array
    cover_count <- rowSums(exp_bootstrap_cis)
    map(cover_count, coverage_ci) %>% simplify2array
})  %>% simplify2array

dimnames(tmp) <- list(c("Low", "High"), c("Normal (Boot)", "Basic", "Percentile", "t-Test"), sample_sizes)

mkTbl <- function(type) {
    tmp2 <- as.data.frame(t(tmp[type,,]))
    tmp2$n <- 2^sample_sizes
    tmp3 <- gather(tmp2, key = "Type", value = "Value", -n)
    tmp3$Bound <- type
    tmp3
}
df <- rbind(mkTbl('Low'), mkTbl('High'))


###################################################
### code chunk number 16: confcoef.Rnw:214-219
###################################################
ggplot(df, aes(x = n, y = Value, color = Bound)) + 
    geom_point() + 
    geom_line() + 
    facet_wrap(~ Type) +
    geom_abline(intercept = 0.95, slope = 0, lty = 2)


