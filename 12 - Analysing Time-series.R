## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #12: Analyzing Time-series. pp. 281-296.
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
library(data.table)

dt <- data.table(hflights)
dt[, date := ISOdate(Year, Month, DayofMonth)]

daily <- dt[, list(
    N         = .N,
    Delays    = sum(ArrDelay, na.rm = TRUE),
    Cancelled = sum(Cancelled),
    Distance  = mean(Distance)
), by = date]

str(daily)

##

plot(ts(daily))

##

setorder(daily, date)
plot(ts(daily))

plot(ts(daily$N, start = 2011, frequency = 365), main = 'Number of flights from Houston in 2011')

##

plot(decompose(ts(daily$N, frequency = 7)))

##

setNames(decompose(ts(daily$N, frequency = 7))$figure, weekdays(daily$date[1:7]))

decompose(ts(daily$N, frequency = 365))

nts <- ts(daily$N, frequency = 7)
fit <- HoltWinters(nts, beta = FALSE, gamma = FALSE)
plot(fit)

##

fit <- HoltWinters(nts)
plot(fit)

library(forecast)
forecast(fit)

##

plot(forecast(HoltWinters(nts), 31))

##

auto.arima(nts)

##

auto.arima(nts, approximation = FALSE)

plot(forecast(auto.arima(nts, D = 1, approximation = FALSE), 31))

##

cts <- ts(daily$Cancelled)
auto.arima(cts)

library(tsoutliers)
outliers <- tso(cts, tsmethod = 'arima', args.tsmethod  = list(order = c(1, 1, 2)))

##

plot(outliers)

plot(tso(ts(daily$Cancelled)))

##

dfc <- as.data.frame(daily[, c('date', 'Cancelled'), with = FALSE])

library(AnomalyDetection)
AnomalyDetectionTs(dfc, plot = TRUE)$plot

library(zoo)
zd <- zoo(daily[, -1, with = FALSE], daily[[1]])

##

plot(zd)

##

plot(cumsum(zd))
