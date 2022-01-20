### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week06/week06_research/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:220-232
###################################################
run_pop_process <- function(cities, steps, start = 1) {
  pops <- matrix(1, nrow = steps, ncol = cities)
  for (i in 2:steps) {
    prev <- pops[i - 1, ]
    ps <- prev^2 / sum(prev^2)
    which_city <- sample(1:cities, size = 1, prob = ps)
    prev[which_city] <- prev[which_city] + 1
    pops[i, ] <- prev
  }
  return(pops)
}
pops <- run_pop_process(10, 1000)


###################################################
### code chunk number 3: lecture-core.Rnw:237-241
###################################################
df <- as.data.frame(pops)
df$Step <- 1:1000
dfg <- gather(df, key = "City", value = "Population", - Step)
ggplot(dfg, aes(x = Step, y = Population)) + geom_line() + facet_wrap(~ City)


###################################################
### code chunk number 4: lecture-core.Rnw:264-267
###################################################
# just get the final value
big_pops <- run_pop_process(1000, 10000)[10000, ]
range(big_pops)


###################################################
### code chunk number 5: lecture-core.Rnw:272-274
###################################################
library(ggplot2)
ggplot(data.frame(pops = big_pops), aes(pops)) + geom_histogram()


###################################################
### code chunk number 6: lecture-core.Rnw:279-280
###################################################
ggplot(data.frame(pops = big_pops), aes(pops)) + geom_histogram() + scale_y_log10()


###################################################
### code chunk number 7: lecture-core.Rnw:285-286
###################################################
ggplot(data.frame(pops = big_pops), aes(pops)) + geom_histogram() + scale_y_log10() + scale_x_log10()


###################################################
### code chunk number 8: lecture-core.Rnw:329-333
###################################################
rpareto <- function(n, r) {
    ls <- rgamma(n, r, 1)
    rexp(n, ls)
}


###################################################
### code chunk number 9: lecture-core.Rnw:338-340
###################################################
ggplot(data.frame(x = rpareto(1000, 10)), aes(x = x)) + 
    geom_density(fill = "blue", alpha = 0.5)


###################################################
### code chunk number 10: lecture-core.Rnw:356-361
###################################################
r_mle <- function(x) { 1 + 1 / (mean(log(x)))}
r_mom <- function(x) { mean(x) / (mean(x) - 1)}
samples <- rerun(1000, rpareto(10, r = 10))
mean(map_dbl(samples, ~ (r_mle(.x) - 10)^2))
mean(map_dbl(samples, ~ (r_mom(.x) - 10)^2 ))


###################################################
### code chunk number 11: lecture-core.Rnw:366-372
###################################################
# read the data and drop the first (garbage) row
michigan_complete <- read.csv("sub-est2017_26.csv", 
                              stringsAsFactors = FALSE)[-1, ] %>%
   mutate(CENSUS2010POP = as.numeric(CENSUS2010POP)) %>%
   filter(CENSUS2010POP > 0, FUNCSTAT == "A") 



###################################################
### code chunk number 12: lecture-core.Rnw:377-378
###################################################
ggplot(data.frame(pops = michigan_complete$CENSUS2010POP), aes(pops)) + geom_histogram() + scale_y_log10() + scale_x_log10()


###################################################
### code chunk number 13: lecture-core.Rnw:383-384
###################################################
r_mle(na.omit(michigan_complete$CENSUS2010POP))


###################################################
### code chunk number 14: lecture-core.Rnw:456-459
###################################################
k <- 100000
myrands <- rnorm(k)
save(file = "example.rda", k, myrands)


###################################################
### code chunk number 15: lecture-core.Rnw:464-467
###################################################
rm(k) ; rm(myrands)
load("example.rda")
print(k)


###################################################
### code chunk number 16: lecture-core.Rnw:472-475
###################################################
somedata <- data.frame(x1 = rnorm(100), x2 = sample(letters, size = 100, replace = TRUE))
somedata$x1[1:5] <- NA
somedata$x2[8] <- "-1"


###################################################
### code chunk number 17: lecture-core.Rnw:480-481
###################################################
summary(somedata)


###################################################
### code chunk number 18: lecture-core.Rnw:486-488
###################################################
somedata$x1[is.na(somedata$x1)] <- mean(somedata$x1, na.rm = TRUE)
summary(somedata)


###################################################
### code chunk number 19: lecture-core.Rnw:503-508
###################################################
library(haven)
aspirin <- read_xpt("./RXQASA_H.XPT")
dim(aspirin)
bp <- read_xpt("./BPX_H.XPT")
dim(bp)


###################################################
### code chunk number 20: lecture-core.Rnw:513-524
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
### code chunk number 21: lecture-core.Rnw:529-531
###################################################
bp$sys_mean <- rowMeans(bp[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")], na.rm = TRUE)
bp$dia_mean <- rowMeans(bp[, c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")], na.rm = TRUE)


###################################################
### code chunk number 22: lecture-core.Rnw:542-543
###################################################
combined <- inner_join(aspirin, bp, "SEQN") # common "sequence number" ID


###################################################
### code chunk number 23: lecture-core.Rnw:548-549
###################################################
ggplot(combined, aes(x = dia_mean, y = sys_mean, color = taking_aspirin)) + geom_point(alpha = 0.8, size = 0.9)


