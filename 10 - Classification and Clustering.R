## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #10: Classification and Clustering. pp. 235-268.
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

d <- dist(mtcars)
h <- hclust(d)
h

##

plot(h)

##

plot(h)
rect.hclust(h, k = 3, border = 'red')

(cn <- cutree(h, k = 3))

##

table(cn)

round(aggregate(mtcars, FUN = mean, by = list(cn)), 1)

round(aggregate(mtcars, FUN = sd, by = list(cn)), 1)

##

round(sapply(mtcars, sd), 1)

round(apply(aggregate(mtcars, FUN = mean, by = list(cn)), 2, sd), 1)

##

library(NbClust)
NbClust(mtcars, method = 'complete', index = 'dindex')

NbClust(mtcars, method = 'complete', index = 'hartigan')$Best.nc

##

NbClust(mtcars, method = 'complete', index = 'kl')$Best.nc

NbClust(iris[, -5], method = 'complete', index = 'all')$Best.nc[1, ]

##

(k <- kmeans(mtcars, 3))

##

all(cn == k$cluster)

cbind(cn, k$cluster)

##

library(cluster)
clusplot(mtcars, k$cluster, color = TRUE, shade = TRUE, labels = 2, cex = 0.7)

##

factors <- c('cyl', 'vs', 'am', 'carb', 'gear')
mtcars[, factors] <- lapply(mtcars[, factors], factor)

##

library(poLCA)
p <- poLCA(cbind(cyl, vs, am, carb, gear) ~ 1, data = mtcars, graphs = TRUE, nclass = 3)

p$probs

##

plot(p)

p$P

##

rm(mtcars)
mtcars$gear <- factor(mtcars$gear)

library(MASS)
d <- lda(gear ~ ., data = mtcars, CV = TRUE)

(tab <- table(mtcars$gear, d$class))

tab / rowSums(tab)

sum(diag(tab)) / sum(tab)

##

round(d$posterior, 4)

##

d <- lda(gear ~ ., data = mtcars)
plot(d)

##

plot(d, dimen = 1, type = 'both' )

##

lr <- glm(am ~ hp + wt, data = mtcars, family = binomial)
summary(lr)

##

table(mtcars$am, round(predict(lr, type = 'response')))

library(nnet)
(mlr <- multinom(factor(gear) ~ ., data = mtcars))

##

table(mtcars$gear, predict(mlr))

rm(mtcars)

##

set.seed(42)
n     <- nrow(mtcars)
train <- mtcars[sample(n, n/2), ]

library(dplyr)
train <- sample_n(mtcars, n / 2)

##

test <- mtcars[setdiff(row.names(mtcars), row.names(train)), ]

library(class)
(cm <- knn(
    train = subset(train, select = -gear),
    test  = subset(test, select = -gear),
    cl    = train$gear,
    k     = 5))

cor(test$gear, as.numeric(as.character(cm)))

table(test$gear, as.numeric(as.character(cm)))

##

table(train$gear)

##

library(rpart)
ct <- rpart(factor(gear) ~ ., data = train, minsplit = 3)
summary(ct)

##

plot(ct); text(ct)

table(test$gear, predict(ct, newdata = test, type = 'class'))

##

library(rpart.plot)
rpart.plot(ct)

library(partykit)
plot(as.party(ct))

library(party)
ct <- ctree(factor(gear) ~ drat, data = train, controls = ctree_control(minsplit = 3))
plot(ct, main = "Conditional Inference Tree")

table(test$gear, predict(ct, newdata = test, type = 'node'))

##

library(randomForest)
(rf <- randomForest(factor(gear) ~ ., data = train, ntree = 250))

table(test$gear, predict(rf, test))

##

plot(rf)
legend('topright', colnames(rf$err.rate), col = 1:4, fill = 1:4, bty = 'n')

library(caret)

##

library(C50)
C50 <- train(factor(gear) ~ ., data = train, method = 'C5.0')
summary(C50)

##

table(test$gear, predict(C50, test))
