### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week11/week11_glms/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:250-253
###################################################
load("../../../../data/edu/edu.rda")
edu_analysis <- na.omit(edu[, c("name", "gradrate_total", "tuition", "avg_faculty_salary")])
edu_analysis <- mutate(edu_analysis, tuition1k = tuition/1000, salary10k = avg_faculty_salary / 10000)


###################################################
### code chunk number 3: lecture-core.Rnw:257-259
###################################################
par(mar = c(0, 0, 0, 0))
plot(edu_analysis[, c(2, 5,6)])


###################################################
### code chunk number 4: lecture-core.Rnw:264-265
###################################################
edu_analysis$grad50 <- edu_analysis$gradrate_total >= 50


###################################################
### code chunk number 5: lecture-core.Rnw:278-282
###################################################
mod50 <- glm(grad50 ~ tuition1k + salary10k,
             data = edu_analysis,
             family = binomial())
coef(mod50)


###################################################
### code chunk number 6: lecture-core.Rnw:321-322
###################################################
coef(mod50)


###################################################
### code chunk number 7: lecture-core.Rnw:344-350
###################################################
mean_sd <- summarize(edu_analysis, 
  tuition1k_sd = sd(tuition1k),
  tuition1k_mean = mean(tuition1k),
  salary10k_sd = sd(salary10k),
  salary10k_mean = mean(salary10k),
)


###################################################
### code chunk number 8: lecture-core.Rnw:355-365
###################################################
sds <- seq(-2, 2, length.out = 1000)
predict_tuition1k <- with(mean_sd,
predict(mod50, type = "response",
  newdata = data.frame(tuition1k = tuition1k_mean + sds * tuition1k_sd,
                       salary10k = salary10k_mean)))

predict_salary10k <- with(mean_sd,
predict(mod50, type = "response",
  newdata = data.frame(tuition1k = tuition1k_mean,
                       salary10k = salary10k_mean + sds * salary10k_sd)))


###################################################
### code chunk number 9: lecture-core.Rnw:372-375
###################################################
df <- rbind(data.frame(SD = sds, Predict = predict_tuition1k, Variable = "Tuition"),
            data.frame(SD = sds, Predict = predict_salary10k, Variable = "Salary"))
ggplot(df, aes(x = SD, y = Predict, color = Variable)) + geom_line()


###################################################
### code chunk number 10: lecture-core.Rnw:418-423
###################################################
b0 <- 2 
b1 <- 0.8
mu <- function(x) {  1 / (b0 + b1 * x) } # inv. link
x <- runif(100, 0, 5)
y <- rexp(100, rate =  1 / mu(x)) # rate is 1/mean


###################################################
### code chunk number 11: lecture-core.Rnw:430-432
###################################################
xy <- data.frame(x = x, y = y)
ggplot(xy, aes(x = y)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 12: lecture-core.Rnw:439-440
###################################################
ggplot(xy, aes(x = x, y = y)) + geom_point() + stat_function(fun = mu, color = "red", lty = 2)


###################################################
### code chunk number 13: lecture-core.Rnw:451-452
###################################################
exp_mod <- glm(y ~ x, family = Gamma(link = "inverse"))


###################################################
### code chunk number 14: lecture-core.Rnw:457-458
###################################################
exp_mod


###################################################
### code chunk number 15: lecture-core.Rnw:464-471
###################################################
## plot(x, y)
## curve(mu(x), add  = TRUE, col = "red")
## curve(predict(exp_mod, newdata = data.frame(x = x), type = "response"), add  = TRUE, col = "blue")
ggplot(xy, aes(x = x, y = y)) + geom_point() +
           stat_function(fun = mu, color = "red", lwd = 2) +
           stat_function(fun = function(x) { predict(exp_mod, newdata = data.frame(x = x), type = "response") },
                         color = "blue", lwd = 2)


###################################################
### code chunk number 16: lecture-core.Rnw:477-481
###################################################
medx <- median(x)
sdx <- sd(x)
predict(exp_mod, newdata = data.frame(x = medx + sdx), type = "response") -
    predict(exp_mod, newdata = data.frame(x = medx), type = "response")


###################################################
### code chunk number 17: lecture-core.Rnw:489-492
###################################################
plot(x, y, col = "grey")
curve(predict(exp_mod, newdata = data.frame(x = x), type = "response"), add = TRUE, col = "red")
points(c(medx, medx + sdx), predict(exp_mod, newdata = data.frame(x = c(medx, medx + sdx)), type = "response"), col = c('blue'), pch = 20, cex = 2)


###################################################
### code chunk number 18: lecture-core.Rnw:500-501
###################################################
summary(exp_mod)$coefficients


###################################################
### code chunk number 19: lecture-core.Rnw:504-505
###################################################
confint(exp_mod)


###################################################
### code chunk number 20: boot_np
###################################################
library(boot)
boot_glm_np <- function(y, idx, x) {
    ystar <- y[idx]
    xstar <- x[idx]
    coef(glm(ystar ~ xstar, family = Gamma(link = "inverse")))
}
boot_exp_mod <- boot(y, boot_glm_np, R = 1000, x = x)  


###################################################
### code chunk number 21: lecture-core.Rnw:536-538
###################################################
boot.ci(boot_exp_mod, index = 1, type = "basic")$basic[, 4:5]
boot.ci(boot_exp_mod, index = 2, type = "basic")$basic[, 4:5]


###################################################
### code chunk number 22: lecture-core.Rnw:542-543
###################################################
set.seed(239393)


###################################################
### code chunk number 23: boot_para
###################################################
newy <- simulate(exp_mod, 1000)
bscoefs <- apply(newy, 2, function(newy) {
  coef(glm(newy ~ x, family = Gamma())) })
quantile(bscoefs[1, ], c(0.025, 0.975)) ## percentile interval
quantile(bscoefs[2, ], c(0.025, 0.975)) ## percentile interval


###################################################
### code chunk number 24: lecture-core.Rnw:568-570
###################################################
## xy is the exponential data
(modlog <- glm(y ~ x, data = xy, family = Gamma(link = "log")))


###################################################
### code chunk number 25: lecture-core.Rnw:575-581
###################################################
f <- function(x) { predict(modlog, type = "response", newdata = data.frame(x = x))}
g <- function(x) { predict(exp_mod, type = "response", newdata = data.frame(x = x))}
ggplot(xy, aes(x = x, y = y)) + geom_point() + 
    stat_function(fun = f, color = "red", lwd = 2) + 
    stat_function(fun = g, color = "blue", lwd = 2) +
    scale_colour_manual("Link Function", values = c("Log" ="red","Inverse" = "blue"))


