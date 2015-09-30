## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #5: Building Models. pp. 107-126.
##      -- written by Renata Nemeth and Gergely Toth
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

library(gamlss.data)
data(usair)

model.0 <- lm(y ~ x3, data = usair)
summary(model.0)

##

plot(y ~ x3, data = usair, cex.lab = 1.5)
abline(model.0, col = 'red', lwd = 2.5)
legend('bottomright', legend = 'y ~ x3', lty = 1, col = 'red', lwd = 2.5, title = 'Regression line')

##

usair$prediction <- predict(model.0)
usair$residual<- resid(model.0)
plot(y ~ x3, data = usair, cex.lab = 1.5)
abline(model.0, col = 'red', lwd = 2.5)
segments(usair$x3, usair$y, usair$x3, usair$prediction, col = 'blue', lty = 2)
legend('bottomright', legend = c('y ~ x3', 'residuals'), lty = c(1, 2), col = c('red', 'blue'), lwd = 2.5, title = 'Regression line')

##

model.1 <- update(model.0, . ~ . + x2)
summary(model.1)

##

as.numeric(predict(model.1, data.frame(x2 = 150, x3 = 400)))

-0.05661 * 400 + 0.08243 * 150 + 26.32508

library(scatterplot3d)
plot3d <- scatterplot3d(usair$x3, usair$x2, usair$y, pch = 19, type = 'h', highlight.3d = TRUE, main = '3-D Scatterplot')

##

plot3d$plane3d(model.1, lty = 'solid', col = 'red')

par(mfrow = c(1, 2))
plot(y ~ x3, data = usair, cex.lab = 1.5, main = '2D projection for x3')
abline(model.1, col = 'red', lwd = 2.5)
plot(y ~ x2, data = usair, cex.lab = 1.5, main = '2D projection for x2')
abline(lm(y ~ x2 + x3, data = usair), col = 'red', lwd = 2.5)

##

library(Hmisc)
library(ggplot2)
library(gridExtra)
set.seed(7)
x  <- sort(rnorm(1000, 10, 100))[26:975]
y  <- x * 500 + rnorm(950, 5000, 20000)
df <- data.frame(x = x, y = y, cuts = factor(cut2(x, g = 5)), resid = resid(lm(y ~ x)))
scatterPl <- ggplot(df, aes(x = x, y = y)) + geom_point(aes(colour = cuts, fill = cuts), shape = 1, show_guide = FALSE) + geom_smooth(method = lm, level = 0.99) + theme_bw() + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), text = element_text(size=20))

##

plot_left <- ggplot(df,  aes(x = y, fill = cuts)) + geom_density(alpha = .5) + coord_flip() + scale_y_reverse() + xlab('Y values by groups') + theme_bw() + theme(legend.position = 'none') + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), text = element_text(size=20))
plot_right <- ggplot(data = df, aes(x = resid, fill = cuts)) + geom_density(alpha = .5) + coord_flip() + theme_bw() + xlab('Residuals by groups') + theme(legend.position = 'none') + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), text = element_text(size=20))
grid.arrange(plot_left, scatterPl, plot_right, ncol=3, nrow=1, widths=c(1, 3, 1))

library(gvlma)
gvlma(model.1)

##

model.2 <- update(model.1, data = usair[-31, ])
gvlma(model.2)

##

model.0 <- update(model.0, data = usair[-31, ])
summary(model.0)[c('r.squared', 'adj.r.squared')]

summary(model.2)[c('r.squared', 'adj.r.squared')]

##

summary(model.3 <- update(model.2, .~. -x2 + x1))$coefficients
summary(model.4 <- update(model.2, .~. -x3 + x1))$coefficients
AIC(model.3, model.4)

##

plot(y ~ x5, data = usair, cex.lab = 1.5)
abline(lm(y ~ x5, data = usair), col = 'red', lwd = 2.5, lty = 1)
abline(lm(y ~ x5, data = usair[usair$x5<=45,]), col = 'red', lwd = 2.5, lty = 3)
abline(lm(y ~ x5, data = usair[usair$x5>=30,]), col = 'red', lwd = 2.5, lty = 2)
abline(v = c(30, 45),col = 'blue', lwd = 2.5)
legend('topleft', legend = c('y ~ x5', 'y ~ x5 | x5<=45','y ~ x5 | x5>=30', 'Critical zone'), lty = c(1, 3, 2, 1) , col = c('red', 'red', 'red', 'blue'), lwd = rep(2.5, 4), title = NULL)

##

library(partykit)
library(rpart)
plot(as.party(rpart(y ~ x5, data = usair)))

##

usair$x5_3 <- cut2(usair$x5, c(30, 45))
plot(y ~ as.numeric(x5_3), data = usair, cex.lab = 1.5, xlab = 'Categorized annual rainfall(x5)', xaxt = 'n')
axis(1, at = 1:3, labels = levels(usair$x5_3))
lines(tapply(usair$y, usair$x5_3, mean), col = 'red', lwd = 2.5, lty = 1)
legend('topright', legend = 'Linear prediction', lty = 1, col = 'red', lwd = 2.5, title = NULL)

summary(glmmodel.1 <- glm(y ~ x2 + x3 + x5_3, data = usair[-31, ]))
