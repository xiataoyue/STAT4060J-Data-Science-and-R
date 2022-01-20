### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week02/week02_working_with_data/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:111-114
###################################################
k <- 100000
myrands <- rnorm(k)
save(file = "example.rda", k, myrands)


###################################################
### code chunk number 3: lecture-core.Rnw:119-122
###################################################
rm(k) ; rm(myrands)
load("example.rda")
print(k)


###################################################
### code chunk number 4: lecture-core.Rnw:127-130
###################################################
somedata <- data.frame(x1 = rnorm(100), x2 = sample(letters, size = 100, replace = TRUE))
somedata$x1[1:5] <- NA
somedata$x2[8] <- "-1"


###################################################
### code chunk number 5: lecture-core.Rnw:135-136
###################################################
summary(somedata)


###################################################
### code chunk number 6: lecture-core.Rnw:141-143
###################################################
somedata$x1[is.na(somedata$x1)] <- mean(somedata$x1, na.rm = TRUE)
summary(somedata)


###################################################
### code chunk number 7: lecture-core.Rnw:158-163
###################################################
library(haven)
aspirin <- read_xpt("./RXQASA_H.XPT")
dim(aspirin)
bp <- read_xpt("./BPX_H.XPT")
dim(bp)


###################################################
### code chunk number 8: lecture-core.Rnw:168-179
###################################################
### Questions:
# RXQ515 - Followed (doctor's) advice, took low-dose aspirin?
# RXQ520 - Taking low-dose aspirin on your own?

eq1 <- function(x) {
    tmp <- x == 1 
    tmp[is.na(tmp)] <- FALSE
    return(tmp)
}

aspirin$taking_aspirin <- eq1(aspirin$RXQ515) | eq1(aspirin$RXQ520)


###################################################
### code chunk number 9: lecture-core.Rnw:184-186
###################################################
bp$sys_mean <- rowMeans(bp[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)
bp$dia_mean <- rowMeans(bp[, c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")], na.rm = TRUE)


###################################################
### code chunk number 10: lecture-core.Rnw:197-198
###################################################
combined <- inner_join(aspirin, bp, "SEQN") # common "sequence number" ID


###################################################
### code chunk number 11: lecture-core.Rnw:203-204
###################################################
ggplot(combined, aes(x = dia_mean, y = sys_mean, color = taking_aspirin)) + geom_point(alpha = 0.8, size = 0.9)


