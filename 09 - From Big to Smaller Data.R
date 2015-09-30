## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #9: From Big to Small Data. pp. 192-234.
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

JFK <- hflights[which(hflights$Dest == 'JFK'), c('TaxiIn', 'TaxiOut')]
JFK <- subset(hflights, Dest == 'JFK', select = c(TaxiIn, TaxiOut))

##

par(mfrow = c(1, 2))
qqnorm(JFK$TaxiIn, ylab = 'TaxiIn'); qqline(JFK$TaxiIn)
qqnorm(JFK$TaxiOut, ylab = 'TaxiOut'); qqline(JFK$TaxiOut)

shapiro.test(JFK$TaxiIn)

##

JFK <- na.omit(JFK)
library(MVN)
mardiaTest(JFK)

##

hzTest(JFK)

roystonTest(JFK)

##

mvt <- roystonTest(JFK, qqplot = TRUE)

par(mfrow = c(1, 2))
mvnPlot(mvt, type = 'contour', default = TRUE)
mvnPlot(mvt, type = 'persp', default = TRUE)

##

set.seed(42)
mvt <- roystonTest(MASS::mvrnorm(100, mu = c(0, 0), Sigma = matrix(c(10, 3, 3, 2), 2)))
par(mfrow = c(1, 2), mar = c(0,0,0,0))
mvnPlot(mvt, type = 'contour', default = TRUE)
mvnPlot(mvt, type = 'persp', default = TRUE)

##

hflights_numeric <- hflights[, which(sapply(hflights, is.numeric))]
cor(hflights_numeric, use = 'pairwise.complete.obs')

str(cor(hflights_numeric, use = 'pairwise.complete.obs'))

hflights_numeric <- hflights[,which(
    sapply(hflights, function(x)
        is.numeric(x) && var(x, na.rm = TRUE) != 0))]

table(is.na(cor(hflights_numeric, use = 'pairwise.complete.obs')))

##

library(ellipse)

plotcorr(cor(hflights_numeric, use = 'pairwise.complete.obs'))

##

plotcorr(cor(data.frame(
    1:10,
    1:10+runif(10),
    1:10+runif(10)*5,
    runif(10),
    10:1,
    check.names = FALSE)))

cor(hflights$FlightNum, hflights$Month)

##

##

library(psych)
KMO(cor(hflights_numeric, use = 'pairwise.complete.obs'))

##

cor(hflights_numeric[, c('Cancelled', 'AirTime')])

table(hflights_numeric$AirTime[which(hflights_numeric$Cancelled == 1)], exclude = NULL)

table(hflights_numeric$Cancelled)

hflights_numeric <- subset(hflights_numeric, select = -Cancelled)

which(is.na(cor(hflights_numeric, use = 'pairwise.complete.obs')), arr.ind = TRUE)

##

hflights_numeric <- subset(hflights_numeric, select = -Diverted)
KMO(cor(hflights_numeric[, -c(14)], use = 'pairwise.complete.obs'))

##

KMO(mtcars)

bartlett.test(mtcars)
cortest.bartlett(cor(mtcars))

##

prcomp(mtcars, scale = TRUE)

##

summary(prcomp(mtcars, scale = TRUE))

##

sum(prcomp(scale(mtcars))$sdev^2)

prcomp(scale(mtcars))$sdev^2

(6.6 + 2.65) / 11

##

VSS.scree(cor(mtcars))

##

scree(cor(mtcars))

##

fa.parallel(mtcars)

###

pc <- prcomp(mtcars, scale = TRUE)
head(pc$x[, 1:2])

head(scale(mtcars) %*% pc$rotation[, 1:2])

summary(pc$x[, 1:2])

##

apply(pc$x[, 1:2], 2, sd)
pc$sdev[1:2]

round(cor(pc$x))

pc$rotation[, 1:2]

##

biplot(pc, cex = c(0.8, 1.2))
abline(h = 0, v = 0, lty = 'dashed')

##

cor(mtcars, pc$x[, 1:2])

##

varimax(pc$rotation[, 1:2])

##

pcv <- varimax(pc$rotation[, 1:2])$loadings
plot(scale(mtcars) %*% pcv, type = 'n', xlab = 'Transmission', ylab = 'Power')
text(scale(mtcars) %*% pcv, labels = rownames(mtcars))

##

library(GPArotation)
promax(pc$rotation[, 1:2])
cor(promax(pc$rotation[, 1:2])$loadings)

##

library(jpeg)
t <- tempfile()
download.file('http://bit.ly/nasa-img', t)
img <- readJPEG(t)
str(img)

h <- dim(img)[1]
w <- dim(img)[2]
m <- matrix(img, h*w)
str(m)

pca <- prcomp(m)

##

summary(pca)
pca$rotation

extractColors <- function(x) rgb(x[1], x[2], x[3])

(colors <- apply(abs(pca$rotation), 2, extractColors))

##

pie(pca$sdev, col = colors, labels = colors)

par(mfrow = c(1, 2), mar = rep(0, 4))
image(matrix(pca$x[, 1], h), col = gray.colors(100))
image(matrix(pca$x[, 2], h), col = gray.colors(100), yaxt = 'n')

##

m <- subset(mtcars, select = c(mpg, cyl, hp, carb))

(f <- fa(m))

##

fa.diagram(f)

cor(f$scores, mtcars$disp)

##

as.matrix(eurodist)[1:5, 1:5]

(mds <- cmdscale(eurodist))

##

plot(mds)

##

plot(mds, type = 'n')
text(mds[, 1], mds[, 2], labels(eurodist), cex = 0.7)

##

mds <- cmdscale(dist(mtcars))
plot(mds, type = 'n')
text(mds[, 1], mds[, 2], rownames(mds), cex = 0.7)
