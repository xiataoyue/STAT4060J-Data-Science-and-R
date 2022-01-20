### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week07/week07_crossval/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week07/week07_crossval/gamoran_cv.Rnw'

###################################################
### code chunk number 1: gamoran_cv.Rnw:1-3
###################################################
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: gamoran_cv.Rnw:14-16
###################################################
load("C:/Users/James Xia/Desktop/STAT4060J/Data/gamoran.rda")
ggplot(gamoran, aes(x = READ_PCTZ)) + geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 3: gamoran_cv.Rnw:22-23
###################################################
reading <- na.omit(gamoran$READ_PCTZ)



###################################################
### code chunk number 4: gamoran_cv.Rnw:28-29
###################################################
mean(reading, na.rm = TRUE)


###################################################
### code chunk number 5: gamoran_cv.Rnw:32-34
###################################################
d <- density(reading)
d$x[which.max(d$y)]


###################################################
### code chunk number 6: gamoran_cv.Rnw:42-44
###################################################
n <- length(reading)
barxj <- 1 / (n - 1) * (sum(reading) - reading)


###################################################
### code chunk number 7: gamoran_cv.Rnw:48-50
###################################################
err_mean <- barxj - reading
(err2_mean <- mean(err_mean^2))


###################################################
### code chunk number 8: gamoran_cv.Rnw:55-62
###################################################
modej <- sapply(1:n, function(i) {
    d <- density(reading[-i]) 
    d$x[which.max(d$y)]
})

err_mode <- modej - reading
(err2_mode <- mean(err_mode^2))


###################################################
### code chunk number 9: gamoran_cv.Rnw:65-66
###################################################
err2_mean / err2_mode


###################################################
### code chunk number 10: gamoran_cv.Rnw:73-75
###################################################
par(mar = c(4, 4, 1, 1))
curve(x^2, from = -2, to = 2, xlab = "predicted y - true y", ylab = "loss")


###################################################
### code chunk number 11: gamoran_cv.Rnw:83-85
###################################################
par(mar = c(4, 4, 1, 1))
curve(abs(x), from = -2, to = 2, xlab = "predicted y - true y")


###################################################
### code chunk number 12: gamoran_cv.Rnw:91-92
###################################################
closs <- function(err) {  2 * (err > 0) + 1 * (err < 0)}


###################################################
### code chunk number 13: gamoran_cv.Rnw:95-98
###################################################

par(mar = c(2, 4, 1, 1))
curve(closs(x), from = -2, to = 2, xlab = "predicted y - true y")


###################################################
### code chunk number 14: gamoran_cv.Rnw:104-105
###################################################
hloss <- function(err) { pmax(0, err) }


###################################################
### code chunk number 15: gamoran_cv.Rnw:108-111
###################################################

par(mar = c(3, 4, 1, 1))
curve(hloss(x), from = -2, to = 2, xlab = "predicted y - true y")


###################################################
### code chunk number 16: gamoran_cv.Rnw:117-121
###################################################
mean(err_mean^2) / mean(err_mode^2) # squared error
mean(abs(err_mean)) / mean(abs(err_mode)) # absolute error
mean(closs(err_mean)) / mean(closs(err_mode)) # constant loss
mean(hloss(err_mean)) / mean(hloss(err_mode)) # hinge loss


###################################################
### code chunk number 17: gamoran_cv.Rnw:136-140
###################################################
read_math <- na.omit(gamoran[, c("READ_PCTZ", "MATH_PCTZ")])
colnames(read_math) <- c("read", "math")
# plot(read_math$read, read_math$math, cex = 0.5)
ggplot(gamoran, aes(x = READ_PCTZ, y = MATH_PCTZ)) + geom_point()


###################################################
### code chunk number 18: gamoran_cv.Rnw:172-174
###################################################
mod_lm <- lm(math ~ read, data = read_math)
coef(mod_lm)


###################################################
### code chunk number 19: gamoran_cv.Rnw:179-192
###################################################
k <- 1000
n <- dim(read_math)[1]
half <- round(n/2)

err_lm <- replicate(k, {
    rand.order <- sample.int(n)
    train.idx <- rand.order[1:half]
    test.idx <- rand.order[(half + 1):n]
    
    mod <- lm(math ~ read, read_math[train.idx, ])
    preds <- predict(mod, newdata = read_math[test.idx, ])
    read_math$math[test.idx] - preds
})


###################################################
### code chunk number 20: gamoran_cv.Rnw:201-204
###################################################
library(quantreg)
mod_rq <- rq(math ~ read, data = read_math)
coef(mod_rq)


###################################################
### code chunk number 21: gamoran_cv.Rnw:210-216
###################################################
ggplot(gamoran, aes(x = READ_PCTZ, y = MATH_PCTZ)) + geom_point(alpha = 0.6) + 
    geom_abline(aes(intercept = coef(mod_lm)[1], slope = coef(mod_lm)[2], col = "red"), lwd = 2) +
    geom_abline(aes(intercept = coef(mod_rq)[1], slope = coef(mod_rq)[2], col = "blue"), lwd = 2) +
    scale_colour_manual(name='Fit',
                      labels = c("OLS", "Median"), 
                      values=c("red", "blue"))


###################################################
### code chunk number 22: gamoran_cv.Rnw:222-231
###################################################
err_abs <- replicate(k, {
    rand.order <- sample.int(n)
    train.idx <- rand.order[1:half]
    test.idx <- rand.order[(half + 1):n]
    
    mod <- rq(math ~ read, data = read_math[train.idx, ]) # only difference
    preds <- predict(mod, newdata = read_math[test.idx, ])
    read_math$math[test.idx] - preds
})


###################################################
### code chunk number 23: gamoran_cv.Rnw:236-237
###################################################
mean(err_lm^2) / mean(err_abs^2)


###################################################
### code chunk number 24: gamoran_cv.Rnw:240-241
###################################################
mean(abs(err_lm)) / mean(abs(err_abs))


