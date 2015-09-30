## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #4: Restructuring Data. pp. 85-106.
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

(m <- matrix(1:9, 3))

##

t(m)

##

library(dplyr)
library(hflights)
str(select(hflights, ends_with('delay')))

str(select(hflights, contains('T', ignore.case = FALSE)))

##

str(select(hflights, matches('^[[:alpha:]]{5,6}$')))

table(nchar(names(hflights)))

names(select(hflights, -matches('^[[:alpha:]]{7,8}$')))

##

str(arrange(hflights, ActualElapsedTime))

##

hflights %>% arrange(ActualElapsedTime) %>% str

hflights %>%
    arrange(ActualElapsedTime) %>%
    select(ActualElapsedTime, Dest) %>%
    subset(Dest != 'AUS') %>%
    head %>%
    str

##

library(data.table)
str(head(data.table(hflights, key = 'ActualElapsedTime')[Dest != 'AUS', c('ActualElapsedTime', 'Dest'), with = FALSE]))

str(head(na.omit(data.table(hflights, key = 'ActualElapsedTime'))[Dest != 'AUS', .(ActualElapsedTime, Dest)]))

system.time(str(head(data.table(na.omit(hflights), key = 'ActualElapsedTime')[Dest != 'AUS', c('ActualElapsedTime', 'Dest'), with = FALSE])))
system.time(str(head(na.omit(data.table(hflights, key = 'ActualElapsedTime'))[Dest != 'AUS', c('ActualElapsedTime', 'Dest'), with = FALSE])))

##

hflights_dt <- data.table(hflights)
hflights_dt[, DistanceKMs := Distance / 0.62137]

system.time(hflights_dt$DistanceKMs <- hflights_dt$Distance / 0.62137)
system.time(hflights_dt[, DistanceKMs := Distance / 0.62137])

##

library(pryr)
hflights_dt <- data.table(hflights)
address(hflights_dt)

hflights_dt$DistanceKMs <- hflights_dt$Distance / 0.62137
address(hflights_dt)

##

hflights_dt <- data.table(hflights)
address(hflights_dt)
hflights_dt[, DistanceKMs := Distance / 0.62137]
address(hflights_dt)

system.time(within(hflights_dt, DistanceKMs <- Distance / 0.62137))

hflights_dt[, c('DistanceKMs', 'DiastanceFeets') := list(Distance / 0.62137, Distance * 5280)]

##

carriers <- unique(hflights_dt$UniqueCarrier)
hflights_dt[, paste('carrier', carriers, sep = '_') := lapply(carriers, function(x) as.numeric(UniqueCarrier == x))]
str(hflights_dt[, grep('^carrier', names(hflights_dt)), with = FALSE])

###

hflights <- hflights %>%
    mutate(DistanceKMs = Distance / 0.62137)

hflights <- mutate(hflights, DistanceKMs = Distance / 0.62137)

##

(wdays <- data.frame(
    DayOfWeek       = 1:7,
    DayOfWeekString = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
    ))

system.time(merge(hflights, wdays))
system.time(merge(hflights_dt, wdays, by = 'DayOfWeek'))

##

weekdays(as.Date(with(hflights, paste(Year, Month, DayofMonth, sep = '-'))))

##

library(reshape2)

##

head(melt(hflights))

hflights_melted <- melt(hflights, id.vars = 0, measure.vars = c('ActualElapsedTime', 'AirTime'))
str(hflights_melted)

##

library(ggplot2)
ggplot(hflights_melted, aes(x = variable, y = value)) + geom_boxplot()

##

hflights_melted <- melt(hflights, id.vars = 'Month', measure.vars = c('ActualElapsedTime', 'AirTime'))
(df <- dcast(hflights_melted, Month ~ variable, fun.aggregate = mean, na.rm = TRUE))

ggplot(melt(df, id.vars = 'Month')) + geom_line(aes(x = Month, y = value, color = variable)) + scale_x_continuous(breaks = 1:12) + theme_bw() + theme(legend.position = 'top')

##

hflights_melted <- melt(add_margins(hflights, 'Month'), id.vars = 'Month', measure.vars = c('ActualElapsedTime', 'AirTime'))
(df <- dcast(hflights_melted, Month ~ variable, fun.aggregate = mean, na.rm = TRUE))

##

library(tidyr)
str(gather(hflights[, c('Month', 'ActualElapsedTime', 'AirTime')], variable, value, -Month))
