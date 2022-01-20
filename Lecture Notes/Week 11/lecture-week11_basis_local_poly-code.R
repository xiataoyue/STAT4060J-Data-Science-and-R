### R code from vignette source '/Users/mark/Documents/Professional/Teaching/sjtu/stats406-summer2021/lectures/week11/week11_basis_local_poly/lecture-core.Rnw'

###################################################
### code chunk number 1: lecture-core.Rnw:52-56
###################################################
options(width = 60, digits = 4)
set.seed(49482)
library(tidyverse)
library(ggplot2)


###################################################
### code chunk number 2: lecture-core.Rnw:99-102
###################################################
xs <- runif(100, -1, 1)
ys <- 3 * xs^2 - xs + 3 + 0.5 * rt(100, df = 4)
d <- data.frame(x = xs, y = ys)


###################################################
### code chunk number 3: lecture-core.Rnw:105-106
###################################################
ggplot(d, aes(x = xs, y = ys)) + geom_point() + stat_function(fun = ~ 3 * .x^2 - .x + 3, lty = 2, col = "red" )


###################################################
### code chunk number 4: lecture-core.Rnw:113-115
###################################################
l1 <- lm(ys ~ xs)
ggplot(d, aes(x = xs, y = ys)) + geom_point() + geom_abline(intercept = coef(l1)[1], slope = coef(l1)[2],  lty = 2, col = "red" )


###################################################
### code chunk number 5: lecture-core.Rnw:124-126
###################################################
quadmod <- lm(ys ~ xs + I(xs^2)) # intercept included automatically
coef(quadmod)


###################################################
### code chunk number 6: lecture-core.Rnw:131-132
###################################################
estmean <- predict(quadmod)


###################################################
### code chunk number 7: lecture-core.Rnw:135-137
###################################################
d$eta <- estmean
ggplot(d, aes(x = xs, y = ys)) + geom_point() + geom_line(aes(y = eta), col = "blue") +  stat_function(fun = ~ 3 * .x^2 - .x + 3, lty = 2, col = "red" )


###################################################
### code chunk number 8: lecture-core.Rnw:144-147
###################################################
ggplot(d, aes(x = xs, y = ys)) + geom_point() +  
    stat_function(fun = ~ 3 * .x^2 - .x + 3, lty = 2, col = "red" ) + 
    geom_segment(aes(xend = xs, yend = eta))


###################################################
### code chunk number 9: lecture-core.Rnw:165-169
###################################################
par(mar = c(4, 4, 1, 1))
weather <- read.csv("ann_arbor_weather.csv")
temp_ts <- ts(weather$TOBS, frequency = 365, start = c(2014, 03, 29))
plot(temp_ts, ylab = "Temp Deg F", xlab = "Date")


###################################################
### code chunk number 10: lecture-core.Rnw:177-183
###################################################
day_id    <- cycle(temp_ts)
## days start on 1 for 2014-03-29
is_spring <- as.numeric(day_id > 3 & day_id <= 65) # April  and May
is_summer <- as.numeric(day_id > 65 & day_id <= 157) # June to Aug
is_fall   <- as.numeric(day_id > 157 & day_id <= 218 ) # Sept and Oct
is_winter <- as.numeric(day_id > 218 | day_id <= 3) # Nov - Mar


###################################################
### code chunk number 11: lecture-core.Rnw:189-192
###################################################
temp_pwc <- lm(weather$TOBS ~ is_winter + is_spring 
                + is_summer + is_fall - 1) # no intercept term
coef(temp_pwc)


###################################################
### code chunk number 12: lecture-core.Rnw:196-197
###################################################
mean(weather$TOBS[is_winter == 1])


###################################################
### code chunk number 13: lecture-core.Rnw:203-207
###################################################
par(mar = c(4, 4, 0,0))
preds_pwc <- ts(predict(temp_pwc), frequency = 365, start = c(2014, 03, 29))
plot(temp_ts, ylab = "Temp Deg F", xlab = "Date", col = "#999999")
lines(preds_pwc, col = "red", lwd = 3)


###################################################
### code chunk number 14: lecture-core.Rnw:219-220
###################################################
temp_sin <- lm(weather$TOBS ~ I(sin(2 * pi * (day_id - 30) / 365)))


###################################################
### code chunk number 15: lecture-core.Rnw:227-231
###################################################
par(mar = c(4, 4, 0,0))
preds_sin <- ts(predict(temp_sin), frequency = 365, start = c(2014, 03, 29))
plot(temp_ts, ylab = "Temp Deg F", xlab = "Date", col = "#999999")
lines(preds_sin, col = "red", lwd = 3)


###################################################
### code chunk number 16: lecture-core.Rnw:264-266
###################################################
temp_2014 <- weather$TOBS[1:365]
plot(temp_2014, type = 'l')


###################################################
### code chunk number 17: lecture-core.Rnw:272-275
###################################################
x <- 0:364 / 364
monoMod <- lm(temp_2014 ~ 1 + x + I(x^2) + I(x^3))
coef(monoMod)


###################################################
### code chunk number 18: lecture-core.Rnw:281-284
###################################################
temp_2014 <- weather$TOBS[1:365]
plot(temp_2014, type = 'l', col = "#999999")
points(predict(monoMod), col = "red", lwd = 2, type = 'l')


###################################################
### code chunk number 19: lecture-core.Rnw:312-320
###################################################
lsp <- function(x, v1, v2, v3) {
    slopes <- ifelse(x < v2, 
    (x >= v1) * (x - v1) / (v2 - v1),
    (x <= v3) * (v3 - x) / (v3 - v2))
}
b1 <- lsp(x, 0, 0.33, 0.66)
b2 <- lsp(x, 0.33, 0.66, 1)
lspmod <- lm(temp_2014 ~ b1 + b2)


###################################################
### code chunk number 20: lecture-core.Rnw:326-328
###################################################
curve(lsp(x, 0, 0.33, 0.66))
curve(lsp(x, 0.33, 0.66, 1), add = TRUE, lty = 2)


###################################################
### code chunk number 21: lecture-core.Rnw:335-337
###################################################
plot(x, temp_2014, type = 'l', col = "#999999")
points(x, predict(lspmod), type = 'l', col = "red", lwd = 2)


###################################################
### code chunk number 22: lecture-core.Rnw:343-344
###################################################
summary(lspmod)


###################################################
### code chunk number 23: lecture-core.Rnw:363-368
###################################################
library(splines)
x01 <- seq(0, 1, length.out = 100)
par(mar = c(4, 2, 0, 0))
sd <- splineDesign(x = x01, knots = c(0, 0, 0.25, 0.5, 0.75, 1, 1), ord = 3, outer.ok = TRUE)
matplot(x01, sd)


###################################################
### code chunk number 24: lecture-core.Rnw:374-377
###################################################
library(splines)
temp_ns <- lm(temp_2014 ~ ns(x, 6)) # "natrual" (goes to zero)
(temp_bs <- lm(temp_2014 ~ bs(x, 6))) # less constrained


###################################################
### code chunk number 25: lecture-core.Rnw:383-387
###################################################
plot(x, temp_2014, col = "#999999", type ='l')
points(x, predict(temp_ns), col = "red", lwd = 2, type = 'l')
points(x, predict(temp_ns), col = "blue", lwd = 2, type = 'l')
legend("topright", fill = c("blue", "red"), legend = c("Nat Spline", "B-Spline"))


###################################################
### code chunk number 26: lecture-core.Rnw:523-536
###################################################
par(mar = c(2, 2, 1, 0))

plot(xs, ys, col = c("red", "blue")[(xs < 0) + 1])
l1 <- lm(ys[xs < 0] ~ xs[xs < 0])
l2 <- lm(ys[xs > 0] ~ xs[xs > 0])
segments(x0 = -1, x1 = 0, 
         y0 = coef(l1)[1] - coef(l1)[2],
         y1 = coef(l1)[1] ,
         col = "blue", lwd = 2)
segments(x0 = 0, x1 = 1, 
         y0 = coef(l2)[1],
         y1 = coef(l2)[1] + coef(l2)[2] ,
         col = "red", lwd = 2)


###################################################
### code chunk number 27: lecture-core.Rnw:543-565
###################################################
par(mar = c(2, 2, 0, 0))
plot(xs, ys, cex = 0.25)
g <- quantile(xs, 0.25)
abline(v = g, lwd = 2, col = "#aaaaaa")
ws <- dnorm((xs - g) / 0.25)
fit <- lm(ys ~ I(xs - g), weights = ws)
points(g, coef(fit)[1], col = 'red', pch = 19)
abline(a = coef(fit)[1] - coef(fit)[2] * g, b = coef(fit)[2], col = 'red')

g <- quantile(xs, 0.5)
abline(v = g, lwd = 2, col = "#aaaaaa")
ws <- dnorm((xs - g) / 0.25)
fit <- lm(ys ~ I(xs - g), weights = ws)
points(g, coef(fit)[1], col = 'blue', pch = 19)
abline(a = coef(fit)[1] - coef(fit)[2] * g, b = coef(fit)[2], col = 'blue')

g <- quantile(xs, 0.9)
abline(v = g, lwd = 2, col = "#aaaaaa")
ws <- dnorm((xs - g) / 0.25)
fit <- lm(ys ~ I(xs - g), weights = ws)
points(g, coef(fit)[1], col = 'green', pch = 19)
abline(a = coef(fit)[1] - coef(fit)[2] * g, b = coef(fit)[2], col = 'green')


###################################################
### code chunk number 28: lecture-core.Rnw:585-591
###################################################
grid <- seq(-1, 1, length.out = 100)
a0s <- sapply(grid, function (g) {
  ws <- dnorm((xs - g) / 0.25)
  fit <- lm(ys ~ I(xs - g), weights = ws)
  return(coef(fit)[1])
})


###################################################
### code chunk number 29: lecture-core.Rnw:598-603
###################################################
par(mar = c(2, 2, 0, 0))
plot(xs, ys)
lines(grid, a0s, col = "red", lwd = 2)
lines(ksmooth(xs, ys, "normal", bandwidth = 0.5), col = "blue", lwd = 2)
legend("top", fill = c("red", "blue"), legend = c("Local Linear", "Kernel"))


###################################################
### code chunk number 30: lecture-core.Rnw:609-610
###################################################
ll <- loess(ys ~ xs, degree = 1, span = 0.75)


###################################################
### code chunk number 31: lecture-core.Rnw:613-615
###################################################
par(mar = c(2, 2, 0, 0)) ; plot(xs, ys)
lines(grid, predict(ll, newdata = grid), col = "red", lwd = 2)


###################################################
### code chunk number 32: lecture-core.Rnw:622-625
###################################################
par(mar = c(2, 2, 0, 0))
scatter.smooth(ys ~ xs, degree = 1, span = 0.75, 
               lpars = list(col = "red", lwd = 2))


###################################################
### code chunk number 33: lecture-core.Rnw:637-638
###################################################
ll2 <- loess(ys ~ xs, degree = 2, span = 0.75)


###################################################
### code chunk number 34: lecture-core.Rnw:644-649
###################################################
par(mar = c(2,2,1,0))
scatter.smooth(ys ~ xs, degree = 2, span = 0.75, 
               lpars = list(col = "blue", lwd = 2))
lines(grid, predict(ll, newdata = grid), col = "red", lwd = 2)
legend("top", c("Linear", "Quadratic"), fill = c("red", "blue"))


###################################################
### code chunk number 35: lecture-core.Rnw:658-667
###################################################
spans <- seq(0.5, 5, length.out = 100)
train_ids <- sample.int(length(xs), size = round(length(xs) / 2))
training <- data.frame(y = ys[train_ids], x = xs[train_ids] )
testing  <- data.frame(y = ys[-train_ids], x = xs[-train_ids] )
sse.test.train <- sapply(spans, function(s) {
    mod <- loess(y ~ x, training, span = s)
    preds <- predict(mod, newdata = testing)
    sum((preds - testing$y)^2, na.rm = TRUE)
})


###################################################
### code chunk number 36: lecture-core.Rnw:673-675
###################################################
plot(spans, sse.test.train, type = 'l')
text(0.5, 10, paste("Minimum at", round(spans[which.min(sse.test.train)], 3)))


###################################################
### code chunk number 37: lecture-core.Rnw:681-683
###################################################
d$pred <- predict(loess(y ~ x, d,
  span = spans[which.min(sse.test.train)]))


###################################################
### code chunk number 38: lecture-core.Rnw:686-688
###################################################
d$pred <- predict(loess(y ~ x, d, span = spans[which.min(sse.test.train)]))
ggplot(d, aes(x = x, y = y)) + geom_point() + geom_line(aes(y = pred   ), col = 'red')


###################################################
### code chunk number 39: lecture-core.Rnw:695-697
###################################################
u <- smooth.spline(d$x, d$y, cv = TRUE) # does leave one out
d$ss <- u$y[match(d$x, u$x)]


###################################################
### code chunk number 40: lecture-core.Rnw:700-701
###################################################
ggplot(d, aes(x = x, y = y)) + geom_point() + geom_line(aes(y = ss), col = 'red')


