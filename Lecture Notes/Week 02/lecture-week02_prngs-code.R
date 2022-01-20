### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week02/week02_prngs/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:53-57
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:74-79
###################################################
sample_max_mag <- function(n) {
    x <- rnorm(n)
    max(abs(x))
}
empirical_distrib <- replicate(100000, sample_max_mag(3))


###################################################
### code chunk number 3: lecture-core.Rnw:85-87
###################################################
# par(mar = c(4,4,2,2))
ggplot(data = data.frame(x = empirical_distrib), aes(x = x)) + geom_histogram() + labs(x = "Max Mag")# (empirical_distrib, breaks = 100, main = NA, xlab = "Harmonic Mean")


###################################################
### code chunk number 4: lecture-core.Rnw:164-168 (eval = FALSE)
###################################################
## prng <- function(seed) {
##    # do something
##    c(random_number, new_seed)
## }


###################################################
### code chunk number 5: lecture-core.Rnw:174-180
###################################################
lcg <- function(seed) {
  a <- 5 ; c <- 1 ; m <- 8
  new_number <- (a * seed + c) %% m
  c(new_number, new_number) # using the new number as the seed
}
lcg(7)


###################################################
### code chunk number 6: lecture-core.Rnw:184-191
###################################################
rngs <- numeric(length = 16)
seed <- 7
for (i in 1:16) {
    rs <- lcg(seed)
    rngs[i] <- rs[1] # pull out the first item in the vector
    seed <- rs[2]
}


###################################################
### code chunk number 7: lecture-core.Rnw:194-195
###################################################
print(rngs)


###################################################
### code chunk number 8: lecture-core.Rnw:198-199
###################################################
rngs / 7 # scale to [0, 1]


###################################################
### code chunk number 9: lecture-core.Rnw:220-221
###################################################
set.seed(3949392)


###################################################
### code chunk number 10: lecture-core.Rnw:228-230
###################################################
length(.Random.seed) # first two items are book keeping 
.Random.seed[3:6] # just a few elements


