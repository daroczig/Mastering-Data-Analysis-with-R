## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #2: Getting Data from the Web. pp. 37-64.
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

str(read.csv('http://opengeocode.org/download/CCurls.txt'))

##

library(RCurl)
df <- read.csv(text = getURL('https://data.consumerfinance.gov/api/views/x94z-ydhh/rows.csv?accessType=DOWNLOAD'))
str(df)
sort(table(df$Product))

##

library(rjson)
u <- 'http://data.consumerfinance.gov/api/views'
fromJSON(file = u)

##

res <- fromJSON(file = paste0(u, '/25ei-6bcr/rows.json?max_rows=5'))
names(res)

res <- res$data
class(res)

df <- as.data.frame(t(sapply(res, function(x) unlist(x[-13]))))
str(df)

##

library(plyr)
df <- ldply(res, function(x) unlist(x[-13]))

names(df) <- sapply(res$meta$view$columns, `[`, 'name')[-13]

##

library(XML)
doc <- xmlParse(paste0(u, '/25ei-6bcr/rows.xml?max_rows=5'))
df <- xmlToDataFrame(nodes = getNodeSet(doc,'//response/row/row'))
str(df)

is.number <- function(x)
    all(!is.na(suppressWarnings(as.numeric(as.character(x)))))
for (n in names(df))
    if (is.number(df[, n]))
        df[, n] <- as.numeric(as.character(df[, n]))

##

doc <- getURL(paste0(u, '/25ei-6bcr/rows?max_rows=5'), httpheader = c(Accept = 'text/html'))

res <- readHTMLTable(doc)

df <- res[[1]]
df <- readHTMLTable(doc, which = 1)

res <- readHTMLTable('http://cran.r-project.org/web/packages/available_packages_by_name.html')
str(res)

##

library(wordcloud)
wordcloud(res[[1]][, 2])

##

page <- htmlParse('http://cran.r-project.org/web/views/WebTechnologies.html')
res <- unlist(xpathApply(page, "//h3[text()='CRAN packages:']/following-sibling::ul[1]/li", xmlValue))
str(res)

##

res <- xpathSApply(page, "//h3[text()='CRAN packages:']/following-sibling::ul[1]/li", xmlValue)

xpathSApply(page, "//h3[text()='CRAN packages:']/following-sibling::ul[1]/li/a", xmlAttrs, 'href')

##

detach('package:rjson')
library(RSocrata)

##

df <- read.socrata(paste0(u, '/25ei-6bcr'))
str(df)

##

library(quantmod)
tail(getSymbols('A', env = NULL))

getFX('USD/EUR')

tail(USDEUR)

##

methods(getSymbols)

str(stockSymbols())

##

library(Quandl)
Quandl('SEC/DIV_A')

##

attr(Quandl('SEC/DIV_A', meta = TRUE), 'meta')$frequency

##

library(devtools)
install_bitbucket('GTrendsR', 'persican')

##

library(GTrendsR)

conn <- gconnect('username', 'password')
df   <- gtrends(conn, query = 'how to install R')
tail(df)
plot(df)

##

library(weatherData)
getWeatherForDate('London', start_date = Sys.Date()-7, end_date = Sys.Date())
