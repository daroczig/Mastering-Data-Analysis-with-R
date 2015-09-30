## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #3: Filtering and Summarizing Data. pp. 65-84.
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

library(sqldf)
sqldf('SELECT * FROM mtcars WHERE am=1 AND vs=1')

##

subset(mtcars, am == 1 & vs == 1)
sqldf('SELECT * FROM mtcars WHERE am=1 AND vs=1', row.names = TRUE)

identical(
    sqldf('SELECT * FROM mtcars WHERE am=1 AND vs=1', row.names = TRUE),
    subset(mtcars, am == 1 & vs == 1)
    )

subset(mtcars, am == 1 & vs == 1, select = hp:wt)

##

library(hflights)
system.time(sqldf("SELECT * FROM hflights WHERE Dest == 'BNA'", row.names = TRUE))
system.time(subset(hflights, Dest == 'BNA'))

##

library(dplyr)
system.time(filter(hflights, Dest == 'BNA'))

str(select(filter(hflights, Dest == 'BNA'), DepTime:ArrTime))

mtcars$rownames <- rownames(mtcars)
select(filter(mtcars, hp > 300), c(rownames, hp))

##

library(data.table)
hflights_dt <- data.table(hflights)
hflights_dt[, rownames := rownames(hflights)]
system.time(hflights_dt[Dest == 'BNA'])

##

str(hflights_dt[Dest == 'BNA', list(DepTime, ArrTime)])

str(hflights_dt[Dest == 'BNA', c('DepTime', 'ArrTime'), with = FALSE])

aggregate(hflights$Diverted, by = list(hflights$DayOfWeek), FUN = mean)

##

with(hflights, aggregate(Diverted, by = list(DayOfWeek), FUN = mean))

aggregate(Diverted ~ DayOfWeek, data = hflights, FUN = mean)

##

tapply(hflights$Diverted, hflights$DayOfWeek, mean)

##

library(plyr)
ddply(hflights, .(DayOfWeek), function(x) mean(x$Diverted))

ddply(hflights, .(DayOfWeek), summarise, Diverted = mean(Diverted))

##

hflights_DayOfWeek <- group_by(hflights, DayOfWeek)

str(attributes(hflights_DayOfWeek))

##

dplyr::summarise(hflights_DayOfWeek, mean(Diverted))

hflights_dt[, mean(Diverted), by = DayOfWeek]

##

setkey(hflights_dt, 'DayOfWeek')
hflights_dt[, mean(Diverted), by = DayOfWeek]

hflights_dt[, list('mean(Diverted)' = mean(Diverted)), by = DayOfWeek]

##

AGGR1     <- function() aggregate(hflights$Diverted, by = list(hflights$DayOfWeek), FUN = mean)
AGGR2     <- function() with(hflights, aggregate(Diverted, by = list(DayOfWeek), FUN = mean))
AGGR3     <- function() aggregate(Diverted ~ DayOfWeek, data = hflights, FUN = mean)
TAPPLY    <- function() tapply(hflights$Diverted, hflights$DayOfWeek, mean)
PLYR1     <- function() ddply(hflights, .(DayOfWeek), function(x) mean(x$Diverted))
PLYR2     <- function() ddply(hflights, .(DayOfWeek), summarise, Diverted = mean(Diverted))
DPLYR     <- function() dplyr::summarise(hflights_DayOfWeek, mean(Diverted))

DPLYR_ALL <- function() {
    hflights_DayOfWeek <- group_by(hflights, DayOfWeek)
    dplyr::summarise(hflights_DayOfWeek, mean(Diverted))
}

hflights_dt_nokey <- data.table(hflights)

key(hflights_dt_nokey)

##

DT        <- function() hflights_dt_nokey[, mean(FlightNum), by = DayOfWeek]
DT_KEY    <- function() hflights_dt[, mean(FlightNum), by = key(hflights_dt)]
DT_ALL    <- function() {
    hflights_dt_nokey <- data.table(hflights)
    setkey(hflights_dt_nokey, 'DayOfWeek')
    hflights_dt_nokey[, mean(FlightNum), by = DayOfWeek]
}

library(microbenchmark)
res <- microbenchmark(AGGR1(), AGGR2(), AGGR3(), TAPPLY(), PLYR1(), PLYR2(), DPLYR(), DPLYR_ALL(), DT(), DT_KEY(), DT_ALL(), times = 10)
print(res, digits = 3)

##

autoplot(res)

dplyr::summarise(group_by(hflights_dt, DayOfWeek), mean(Diverted))

##

ddply(hflights, .(DayOfWeek), summarise, n = length(Diverted))

##

ddply(hflights, .(DayOfWeek), nrow)

table(hflights$DayOfWeek)

count(hflights, 'DayOfWeek')

##

dplyr::summarise(hflights_DayOfWeek, n())

attr(hflights_DayOfWeek, 'group_sizes')

hflights_dt[, .N, by = list(DayOfWeek)]
