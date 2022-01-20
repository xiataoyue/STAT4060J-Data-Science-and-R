### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week10/week10_ols/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:269-272
###################################################
ds <- 1:10
plot(ds, pi^(ds / 2) / (ds * 2^(ds - 1) * gamma(ds / 2)), type = 'b', lwd = 2,
     xlab = "p", ylab = "Proportion with distance of 0.5")


###################################################
### code chunk number 3: lecture-core.Rnw:401-407
###################################################
n <- 100
x <- runif(n, 0, 100)
epsilon <- rnorm(n, 
                 mean = 1 - 4/3 * rbinom(n, size = 1, prob = 0.75),
                 sd = sqrt(2 * abs(x - 50)^2 + 10))
y <- 3 * x + epsilon


###################################################
### code chunk number 4: lecture-core.Rnw:413-416
###################################################
par(mar = c(4, 4, 2, 2))
plot(x, y)
abline(a = 0, b = 3, col = "red")


###################################################
### code chunk number 5: lecture-core.Rnw:423-431
###################################################

loss_abs <- function(beta) {
    sum(abs(y - beta * x))
}

loss_sqd <- function(beta) {
    sum((y - beta * x)^2)
}


###################################################
### code chunk number 6: lecture-core.Rnw:436-439
###################################################
betas <- seq(-10, 10, length.out = 1000)
beta_loss_abs <- map_dbl(betas, loss_abs)
beta_loss_sqd <- map_dbl(betas, loss_sqd)


###################################################
### code chunk number 7: lecture-core.Rnw:442-444
###################################################
best_abs <- betas[which.min(beta_loss_abs)]
best_sqd <- betas[which.min(beta_loss_sqd)]


###################################################
### code chunk number 8: lecture-core.Rnw:450-453
###################################################
par(mar = c(4,4,2,2))
plot(betas, beta_loss_abs, xlab = "beta", ylab = "loss", type = 'l' )
text(x = best_abs, sum(range(beta_loss_abs)) / 1.5, labels = paste("Minimized at", round(best_abs, 3) ))


###################################################
### code chunk number 9: lecture-core.Rnw:459-462
###################################################
par(mar = c(4,4,2,2))
plot(betas, beta_loss_sqd, xlab = "beta", ylab = "loss", type = 'l' )
text(x = best_sqd, sum(range(beta_loss_sqd)) / 1.5, labels = paste("Minimized at", round(best_sqd, 3) ))


###################################################
### code chunk number 10: lecture-core.Rnw:468-471
###################################################
betas_coarse <- seq(-10, 10, length.out = 10)
betas_coarse[which.min(map_dbl(betas_coarse, loss_abs))] # Absolute Loss
betas_coarse[which.min(map_dbl(betas_coarse, loss_sqd))] # Squared Loss


###################################################
### code chunk number 11: lecture-core.Rnw:638-639 (eval = FALSE)
###################################################
## solve(A, b) ## solves Ax = b for x


###################################################
### code chunk number 12: lecture-core.Rnw:650-653
###################################################
load("../../../../data/edu/edu.rda")
edu_analysis <- na.omit(edu[, c("name", "gradrate_total", "tuition", "avg_faculty_salary")])
pairs(edu_analysis[, c("gradrate_total", "tuition", "avg_faculty_salary")])


###################################################
### code chunk number 13: eval
###################################################
edu_analysis[edu_analysis$name == "University of Michigan-Ann Arbor", 2:4]
design_matrix <- as.matrix(
    cbind(1, 
          edu_analysis[, c("tuition", "avg_faculty_salary")]))


###################################################
### code chunk number 14: eval
###################################################
XtX <- t(design_matrix) %*% design_matrix
XtY <- t(design_matrix) %*% edu_analysis$gradrate_total
(beta_hat <- as.vector(solve(XtX, XtY))) 


###################################################
### code chunk number 15: eval
###################################################
(mod <- lm(gradrate_total ~ tuition + avg_faculty_salary, 
           data = edu_analysis))


###################################################
### code chunk number 16: lecture-core.Rnw:693-694
###################################################
1000 * beta_hat[2]


###################################################
### code chunk number 17: lecture-core.Rnw:700-701
###################################################
10000 * beta_hat[3]


###################################################
### code chunk number 18: lecture-core.Rnw:766-767
###################################################
options(width = 100)


###################################################
### code chunk number 19: lecture-core.Rnw:769-770
###################################################
summary(mod)


###################################################
### code chunk number 20: lecture-core.Rnw:830-832
###################################################
library(quadprog)
(qps <- solve.QP(XtX, XtY, diag(3), rep(0, 3))$solution)


