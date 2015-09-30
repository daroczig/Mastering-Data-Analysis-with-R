## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #8: Polishing Data. pp. 169-192.
##
##
##  This file includes the code chunks from the above mentioned
##  chapter except for the leading ">" and "+" characters, which
##  stand for the prompt in the R console. The prompt was
##  intentionally removed here along with arbitrary line-breaks,
##  so that you copy and paste the R expressions to the R console
##  in a more convenient and seamless way.
##
##  Code chunks are grouped here by the printed pages of the book.
##  Two hash signs at the beginning of a line stands for a page
##  break, while an extra empty line between the code chunks
##  represents one or more paragraphs in the original book between
##  the examples for easier navigation.
##
##  Sometimes extra instructions starting with a double hash are
##  also provided on how to run the below expressions.
##
##
##  Find more information on the book at http://bit.ly/mastering-R
##  and you can contact me on Twitter and GitHub by the @daroczig
##  handle, or mail me at daroczig@rapporter.net
##

library(hflights)
table(complete.cases(hflights))

##

prop.table(table(complete.cases(hflights))) * 100

sort(sapply(hflights, function(x) sum(is.na(x))))

mean(cor(apply(hflights, 2, function(x) as.numeric(is.na(x)))), na.rm = TRUE)

##

Funs <- Filter(is.function, sapply(ls(baseenv()), get, baseenv()))
names(Filter(function(x) any(names(formals(args(x))) %in% 'na.rm'), Funs))

##

names(Filter(function(x) any(names(formals(args(x))) %in% 'na.rm'), Filter(is.function, sapply(ls('package:stats'), get, 'package:stats'))))

myMean <- function(...) mean(..., na.rm = TRUE)
mean(c(1:5, NA))
myMean(c(1:5, NA))

##

library(rapportools)
mean(c(1:5, NA))

detach('package:rapportools')
mean(c(1:5, NA))

library(Defaults)
setDefaults(mean.default, na.rm = TRUE)
mean(c(1:5, NA))

setDefaults(mean, na.rm = TRUE)

##

mean
formals(mean)

unDefaults(ls)

##

na.omit(c(1:5, NA))
na.exclude(c(1:5, NA))

x <- rnorm(10); y <- rnorm(10)
x[1] <- NA; y[2] <- NA
exclude <- lm(y ~ x, na.action = 'na.exclude')
omit <- lm(y ~ x, na.action = 'na.omit')
round(residuals(exclude), 2)
round(residuals(omit), 2)

##

m <- matrix(1:9, 3)
m[which(m %% 4 == 0, arr.ind = TRUE)] <- NA
m
na.omit(m)

mean(hflights$ActualElapsedTime)

mean(hflights$ActualElapsedTime, na.rm = TRUE)
mean(na.omit(hflights$ActualElapsedTime))

##

library(microbenchmark)
NA.RM   <- function() mean(hflights$ActualElapsedTime, na.rm = TRUE)
NA.OMIT <- function() mean(na.omit(hflights$ActualElapsedTime))
microbenchmark(NA.RM(), NA.OMIT())

##

m[which(is.na(m), arr.ind = TRUE)] <- 0
m

ActualElapsedTime <- hflights$ActualElapsedTime
mean(ActualElapsedTime, na.rm = TRUE)
ActualElapsedTime[which(is.na(ActualElapsedTime))] <- mean(ActualElapsedTime, na.rm = TRUE)
mean(ActualElapsedTime)

library(Hmisc)
mean(impute(hflights$ActualElapsedTime, mean))

##

sd(hflights$ActualElapsedTime, na.rm = TRUE)
sd(ActualElapsedTime)

##

summary(iris)

library(missForest)
set.seed(81)
miris <- prodNA(iris, noNA = 0.2)
summary(miris)

##

iiris <- missForest(miris, xtrue = iris, verbose = TRUE)

##

str(iiris)

##

miris <- miris[, 1:4]

##

iris_mean <- impute(miris, fun = mean)
iris_forest <- missForest(miris)

diag(cor(iris[, -5], iris_mean))
diag(cor(iris[, -5], iris_forest$ximp))

##

detach('package:missForest')
detach('package:randomForest')

##

library(outliers)
outlier(hflights$DepDelay)

summary(hflights$DepDelay)

library(lattice)
bwplot(hflights$DepDelay)

IQR(hflights$DepDelay, na.rm = TRUE)

##

set.seed(83)
dixon.test(c(runif(10), pi))

model <- lm(hflights$DepDelay ~ 1)

model$coefficients
mean(hflights$DepDelay, na.rm = TRUE)

##

a <- 0.1
(n <- length(hflights$DepDelay))
(F <- qf(1 - (a/n), 1, n-2, lower.tail = TRUE))
(L <- ((n - 1) * F / (n - 2 + F))^0.5)

sum(abs(rstandard(model)) > L)

summary(lm(Sepal.Length ~ Petal.Length, data = miris))

##

lm(Sepal.Length ~ Petal.Length, data = iris)$coefficients

library(MASS)
summary(rlm(Sepal.Length ~ Petal.Length, data = miris))

##

f <- formula(Sepal.Length ~ Petal.Length)
cbind(
    orig = lm(f, data = iris)$coefficients,
    lm   = lm(f, data = miris)$coefficients,
    rlm  = rlm(f, data = miris)$coefficients)

miris$Sepal.Length[1] <- 14
cbind(
    orig = lm(f, data = iris)$coefficients,
    lm   = lm(f, data = miris)$coefficients,
    rlm  = rlm(f, data = miris)$coefficients)
