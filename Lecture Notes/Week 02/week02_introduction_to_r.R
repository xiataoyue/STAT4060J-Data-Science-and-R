## Lecturer Notes: Introduction to R

# Note, things staring with "#" are comments ignored by the R interpreter
################# You can have as many as you want

## Getting help

# You can search the help in the left hand pane
# or interactively using
?t.test
??"Student"

## Tidyverse

# In addition to base R, we'll make heavy use of packages from the "tidyverse"
library(tidyverse)

## Variables

# variable names must start with a letter, but can also contain ".", "_", and numbers

a1 <- 3
b_2 <- 4
C.3 <- a1 * b_2 + a1 / b_2

# R will tell you when you are using a variable that doesn't exist

d <- a7 + 1 # uhoh

# now d won't exist either

d^2

# both <- and = can be used for assignment, == expresses equality
x1 <- 1
x2 = 1
x1 == x2

# basic math operations
2 * 3
9 / 3
# order of operations generally preserved
2 + 9 / 3
# but grouping with parens is a safe idea when mixing with other functions
2 + (9 / 3)

?Arithmetic

## TASK: 
## 1. transform 30 degrees into radians and store in a variable called rad30 (hint: pi is already defined)
## 2. Confirm that the angles in a 30-60-90 triangle sum to pi

## Functions

# functions are just objects in R

x2 <- function(x) {
  return(x^2)
}

x2(4)
x2(8)

# the last line is automatically returned

x2_2 <- function(x) { x^2 }
x2_2(1:5)

## TASK: write a function deg_to_rad that takes an angle in degrees and converts to radians

# functions are locally scoped

x <- "hello"
f <- function(x) {
  x <- x + 1
  return(x^2)
}

f(7)
print(x)

# beware of writing functions that reference variables defined outside the function
y <- 127
f <- function(x) {
  x + y # refers to y defined outside
}
f(1)
y <- -1
f(1)

# named and default arguments
f <- function(name, opening = "hello", closing = "goodbye") {
  paste(opening, ",", name, ".", closing, ",", name)
}

f("Ernie")

f()

f("Earnie", "Bonjour", "Au revoir")

f(closing = "Good night", name = "Ernie", opening = "Good morning")

# improving our grammar

f <- function(name, opening = "hello", closing = "goodbye") {
  paste(sep = "", opening, ", ", name, ". ", closing, ", ", name)
}

f(closing = "Good night", name = "Ernie", opening = "Good morning")

## TASK: Write a function that computes the Poisson probabiilty mass function with a default argument for lambda of 1.
## Hint: https://en.wikipedia.org/wiki/Poisson_distribution#Probability_mass_function
## Hint: exp() is the exponential function, factorial() is the factorial function

## For lamba = 2, what is the probability that Poisson RV equals 1

# R uses "..." to mean extra arguments, for example
?paste

paste. <- function(...) {
  paste(sep = ".", ...)
}

paste.("A", "B", "C")


## R's distribution functions and random number generators
?dnorm
?rpois

## TASK: use the built in function dpois to compute the probability that a Poisson RV equals 1 when lambda is 2

?Distributions


## Types of variables

# Boolean/Logical

print(T) # predefined variable: T <- TRUE
print(F) # likewise, F <- FALSE

# negation
!TRUE

# compare them with "double" operators

# "and"
TRUE && TRUE
TRUE && FALSE

# "or"
TRUE || FALSE
FALSE || FALSE

## TASK: write a function to compute the truth table for "A => B"
##         A
##       T | F
##      --------
##   T | T | T |
## B    -------
##   F | F | T |
##      -------
##
## Test on all for possible inputs.

# usually generate logicals/booleans with comparisons
3 > 4
B <- 7
B == 9

# we use logicals in "if" statments
if (B == 9) {
  print("is 9")
} else if (B == 8) {
  print("is 8")
} else {
  print("not 9 or 8")
}

## Numeric holds both integers and real numbers
integer1 <-1L
real1 <- 1.0

# technically different under the hood, compare the same
identical(integer1, real1)
integer1 == real1

# we can also use "scientific notation"
4.3e34

# beware that real numbers are "really reals", see Gentle chapter 2
identical(4.3e34, 4.3e34 + 1)

# we have some special numerical variables
1/0
-1/0
0/0

## More specials

# NA mean "we don't have data"

is.na(NA)
is.na(T)

# missing values propagate
1 + NA

# NULL means "not set yet" (usually)
is.null(NULL)
is.null(T)

## Characters/strings
"hello"
"hello" + 7

# can convert
7 + as.numeric("8")
paste("my favorite number is", as.character(42)) # paste joins strings together

## Scalars vs. vectors

# most basic data in are inherently "vectors" with a length
# in fact "scalars" are just lengh-1 vectors
length(99)

# the c() function "concatenates"
some_odds <- c(7,9,11,13)

# most math can operate over vectors
some_odds + some_odds
2 * some_odds

# values will be "recycled" as needed, sometimes...
c(2, 0) * some_odds
c(2, 0, 1) * some_odds

# we can quickly create sequences using ":" or the seq() function
1:5
seq(1, 10, by = 2)

# many functions take vectors 

sum(1:10)

## TASK: What is the mean of the squares of the odd numbers between 20 and 50?

# we can perform element-wise logical operations with "single" operators
c(TRUE, TRUE) & c(TRUE, FALSE)

# vectors can have names
(named_vec <- c("A" = 1, B = 2)) # () both assigns and prints
names(named_vec)
names(named_vec) <- c("C", "D")
named_vec

# we acess vectors with [] notation

LETTERS[1:10]
letters[c(1, 5, 9, 15, 21)]

## Task: create a vector that represents a date, with day, month, and year entries.

# lists are similar but can contain mixed data types

(ll <- list("A", 7, TRUE))

# single brackets return a new list 
ll[c(1, 3)]

# this can be confusing if you want just one thing
ll[3]
class(ll[3])

# we use double brackets to get things out of lists
ll[[3]]
class(ll[[3]])

# for named lists we use the "$" notation to access elements
family <- list("father" = "David", "mother" = "Jana")

family$father

## Iteration

# The basic way to do the same operation many times is the "for" loop
for (animal in c("tiger", "llama", "cat")) {
  print(paste("my favorite animal is", animal))
}

# Often we want to collect the results
colors <- c("red", "purple", "green")
animals <- c("whale", "alligator", "ostrich")

what_we_saw <- character(3) # set up a vector to hold the results
for (i in 1:3) {
  what_we_saw[i] <- paste(colors[i], animals[i])
}

print(what_we_saw)

# We will emphasize the use of helper functions that handle iteration for us in many cases.

# The basic function `map` performs a function to each item of a vector or list and returns a list:
map(1:6, function(x) { x^2 })
map(1:6, ~ .x^2) # special "anonymous function" syntax replaces . the argument

# map2 works on multiple vectors:
map2(colors, animals, paste)

# TASK: For x = 0 to 10, compute the probability of observing a Binomial random variable with success probability of 0.3 on 10 trials
# Hint: dbinom is the probability mass function of the binomial distribution

# If we know the result should be a vector of given type, we can specialize it:
map_dbl(1:6, ~ .x^2)
map2_chr(colors, animals, paste)

# built up a single result
reduce(1:10, ~ .x + .y, .init = 0)
sum(1:10)

# built up the result, showing the iterations
accumulate(1:5, ~ .x + .y, .init = 0)
cumsum(1:5) # (cum)ulative sum

# Another common pattern is limiting a list those items having a specific quality
keep(1:10, ~ .x %% 2 == 0) ## %% is the modulus operator
discard(1:10, ~ .x %% 2 == 0)

## Tables

# often have variables measured on the same units
n <- 20
var1 <- rnorm(n, mean = 7, sd = 2) # random Normal/Gaussian variables
var2 <- rnorm(n, mean = var1, sd = 3) # each RV has a different mean

mydata <- data.frame(x = var1, y = var2)

head(mydata)

# tables are "lists of vectors" (with name legth)

mydata$x # list notation
mydata[1]

# we can also fetch out particular entries:

mydata[3, 2] # third row, y value
mydata[3,  ] # all of the third row

# TASK: What is the average value of the "x" variable in the mydata data.frame?


# the `dplyr` package contains tools for manipulating data. They usually return new tables.

mydata2 <- mutate(mydata, z = x / y)
colnames(mydata)
colnames(mydata2)

# another frequent task is grouping data and summarizing:

summarize(mydata, mean_of_x = mean(x))
summarize_all(mydata, mean)

mydata_above_below_median <- group_by(mydata, x > median(x))
summarize_all(mydata_above_below_median, mean)

# "pipe" operator:
group_by(mydata, x > median(x)) %>% summarize_all(mean) # notice: no first data argument!

# cut function divides vector into ranges.
mutate(mydata, z = x + y) %>% group_by(cut(z, 3)) %>% summarize_all(mean)

# TASK: compute the correlation of x and y using the summarize function and the cor function

## Plotting

library(ggplot2)
ggplot(data = mydata, aes(x = x, y = y)) + geom_point()

## Visual Variables: https://www.axismaps.com/guide/general/visual-variables/

## ggplot() maps variables in the `data` argument to visual variables

mydata <- mutate(mydata, 
             z = rpois(n, lambda = 3),
             u = runif(n, -1, 1))

ggplot(data = mydata, aes(x = x, y = y, size = z, color = u)) + 
  geom_point()

## what aesthetics matter is controlled by the "geometry" function:
?geom_point

## when looking at single variables, we often want to use density plots (smoothed histograms)
## Notice I'm passing aes() features directly to geom_density
ggplot(data = mydata, aes(x = x)) + geom_density(fill = "blue", alpha = 0.25)

## sometimes we want to break up data into smaller groups
## ggplot calls these facets

mutate(mydata, pos_u = u > 0) %>%
  ggplot(aes(x = x, y = y)) + geom_point() + 
  facet_wrap(~ pos_u)


