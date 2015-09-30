## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #6: Beyond the Linear Trend Line. pp. 127-152.
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

library(catdata)
data(deathpenalty)
library(vcdExtra)
deathpenalty.expand <- expand.dft(deathpenalty)
binom.model.0 <- glm(DeathPenalty ~ DefendantRace, data = deathpenalty.expand, family = binomial)
summary(binom.model.0)

##

exp(cbind(OR = coef(binom.model.0), confint(binom.model.0)))

##

binom.model.1 <- update(binom.model.0, .~.+VictimRace)
summary(binom.model.1)
exp(cbind(OR = coef(binom.model.1), confint(binom.model.1)))

##

prop.table(table(factor(deathpenalty.expand$VictimRace, labels = c('VictimRace=0', 'VictimRace=1')), factor(deathpenalty.expand$DefendantRace, labels = c('DefendantRace=0', 'DefendantRace=1'))), 1)

##

library(lmtest)
lrtest(binom.model.1)

##

library(BaylorEdPsych)
PseudoR2(binom.model.1)

##

lrtest(binom.model.0, binom.model.1)

##

dfa <- readRDS('SMART_2013.RData')

(ct <- xtabs(~model + failure, data = dfa))

##

dfa <- dfa[dfa$model %in% names(which(rowSums(ct) - ct[, 1] > 0)), ]

library(ggplot2)
ggplot(rbind(dfa, data.frame(model = 'All', dfa[, -1])), aes(failure)) + geom_histogram(binwidth = 1, drop = TRUE, origin = -0.5)  + scale_y_log10() + scale_x_continuous(breaks=c(0:10)) + ylab('log(count)') + facet_wrap( ~ model, ncol = 3) + ggtitle('Histograms by manufacturer') + theme_bw()

##

poiss.base <- glm(failure ~ model, offset(log(freq)), family = 'poisson', data = dfa)
summary(poiss.base)

##

contrasts(dfa$model, sparse = TRUE)

##

exp(1.7666)

lrtest(poiss.base)

##

library(MASS)
model.negbin.0 <- glm.nb(failure ~ model, offset(log(freq)), data = dfa)

lrtest(poiss.base,model.negbin.0)

##

model.negbin.1 <- update(model.negbin.0, .~. + capacity_bytes + age_month + temperature)
model.negbin.2 <- update(model.negbin.1, .~. + PendingSector)
lrtest(model.negbin.0, model.negbin.1, model.negbin.2)

summary(model.negbin.2)

##

exp(data.frame(exp_coef = coef(model.negbin.2)))

##

dfa$model <- relevel(dfa$model, 'WDC')

model.negbin.3 <- update(model.negbin.2, data = dfa)
library(broom)
format(tidy(model.negbin.3), digits = 4)

##

library(data.table)
dfa <- data.table(dfa)
dfa[, temp6 := cut2(temperature, g = 6)]
temperature.weighted.mean <- dfa[, .(wfailure = weighted.mean(failure, freq)), by = temp6]
ggplot(temperature.weighted.mean, aes(x = temp6, y = wfailure)) + geom_bar(stat = 'identity') + xlab('Categorized temperature') + ylab('Weighted mean of disk faults') + theme_bw()

##

model.negbin.4 <- update(model.negbin.0, .~. + capacity_bytes + age_month + temp6 + PendingSector, data = dfa)
AIC(model.negbin.3, model.negbin.4)

weighted.means <- rbind(
    dfa[, .(var = 'capacity', wfailure = weighted.mean(failure, freq)), by = .(value = capacity_bytes)],
    dfa[, .(var = 'age', wfailure = weighted.mean(failure, freq)), by = .(value = age_month)])

ggplot(weighted.means, aes(x = value, y = wfailure)) + geom_step() + facet_grid(. ~ var, scales = 'free_x') + ylab('Weighted mean of disk faults') + xlab('') + theme_bw()

##

dfa[, capacity_bytes := as.factor(capacity_bytes)]
dfa[, age8 := cut2(age_month, g = 8)]
model.negbin.5 <- update(model.negbin.0, .~. + capacity_bytes + age8 + temp6 + PendingSector, data = dfa)

AIC(model.negbin.5, model.negbin.4)

format(tidy(model.negbin.5), digits = 3)

##

tmnb5 <- tidy(model.negbin.5)
str(terms <- tmnb5$term[tmnb5$p.value < 0.05][-1])

library(plyr)
ci <- ldply(terms, function(t) confint(model.negbin.5, t))

##

names(ci) <- c('min', 'max')
ci$term <- terms
ci$variable <- sub('[A-Z0-9\\]\\[,() ]*$', '', terms, perl = TRUE)

ggplot(ci, aes(x = factor(term), color = variable, ymin = min, ymax = max)) + geom_errorbar(size = 1.3) + ylab('Coefficients (95% conf.int)') + xlab('') + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3), legend.position = 'top')

##

PseudoR2(model.negbin.5)
