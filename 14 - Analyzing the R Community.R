## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #14: Analyzing the R Community. pp. 323-348.
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

library(XML)
page <- htmlParse('http://www.r-project.org/foundation/donors.html')

##

list <- unlist(xpathApply(page, "//h3[@id='supporting-members']/following-sibling::ul[1]/li", xmlValue))
str(list)

supporterlist <- sub(' \\([a-zA-Z ]*\\)$', '', list)
countrylist   <- substr(list, nchar(supporterlist) + 3, nchar(list) - 1)

tail(sort(prop.table(table(countrylist)) * 100), 5)

countries <- as.data.frame(table(countrylist))

##

library(rworldmap)
joinCountryData2Map(countries, joinCode = 'NAME', nameJoinColumn = 'countrylist', verbose = TRUE)

library(ggmap)
for (fix in c('Brasil', 'CZ', 'Danmark', 'NL')) {
    countrylist[which(countrylist == fix)] <- geocode(fix, output = 'more')$country
}

countries <- as.data.frame(table(countrylist))
countries <- joinCountryData2Map(countries, joinCode = 'NAME', nameJoinColumn = 'countrylist')

##

mapCountryData(countries, 'Freq', catMethod = 'logFixedWidth', mapTitle = 'Number of R Foundation supporting members')

##

packages <- readHTMLTable('http://cran.r-project.org/web/checks/check_summary.html', which = 2)

maintainers <- sub('(.*) <(.*)>', '\\1', packages$' Maintainer')
maintainers <- gsub(' ', ' ', maintainers)
str(maintainers)

tail(sort(table(maintainers)), 8)

##

N <- as.numeric(table(maintainers))
library(fitdistrplus)
plotdist(N)

descdist(N, boot = 1e3)

##

(gparams <- fitdist(N, 'gamma'))

##

gshape  <- gparams$estimate[['shape']]
grate   <- gparams$estimate[['rate']]
sum(rgamma(1e5, shape = gshape, rate = grate))
hist(rgamma(1e5, shape = gshape, rate = grate))

pgamma(2, shape = gshape, rate = grate)

prop.table(table(N <= 2))

##

ploc <- min(N)
pshp <- length(N) / sum(log(N) - log(ploc))

library(actuar)
ppareto(2, pshp, ploc)

fg <- fitdist(N, 'gamma')
fw <- fitdist(N, 'weibull')
fl <- fitdist(N, 'lnorm')
fp <- fitdist(N, 'pareto', start = list(shape = 1, scale = 1))
par(mfrow = c(1, 2))
denscomp(list(fg, fw, fl, fp), addlegend = FALSE)
qqcomp(list(fg, fw, fl, fp), legendtext = c('gamma', 'Weibull', 'Lognormal', 'Pareto'))

##

length(unique(maintainers))

library(RCurl)
url <- getURL('https://stat.ethz.ch/pipermail/r-help/')

R.help.toc <- htmlParse(url)
R.help.archives <- unlist(xpathApply(R.help.toc, '//table//td[3]/a', xmlAttrs), use.names = FALSE)

dir.create('r-help')
for (f in R.help.archives)
    download.file(url = paste0(url, f), file.path('help-r', f), method = 'curl')

##

lines <- system("zgrep -E '^From: .* at .*' ./help-r/*.txt.gz", intern = TRUE)
length(lines)
length(unique(lines))

lines    <- sub('.*From: ', '', lines)
Rhelpers <- sub('.*\\((.*)\\)', '\\1', lines)

tail(sort(table(Rhelpers)), 6)

##

grep('Brian( D)? Ripley', names(table(Rhelpers)), value = TRUE)

sum(grepl('Brian( D)? Ripley', Rhelpers))

##

lines <- system("zgrep -E '^Date: [A-Za-z]{3}, [0-9]{1,2} [A-Za-z]{3} [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} [-+]{1}[0-9]{4}' ./help-r/*.txt.gz", intern = TRUE)
length(lines)

head(sub('.*Date: ', '', lines[1]))

##

times <- strptime(sub('.*Date: ', '', lines), format = '%a, %d %b %Y %H:%M:%S %z')

plot(table(format(times, '%Y')), type = 'l')

library(data.table)
Rhelp <- data.table(time = times)
Rhelp[, H := hour(time)]
Rhelp[, D := wday(time)]

##

library(ggplot2)
ggplot(na.omit(Rhelp[, .N, by = .(H, D)]),
       aes(x = factor(H), y = factor(D), size = N)) + geom_point() +
       ylab('Day of the week') + xlab('Hour of the day') +
       ggtitle('Number of mails posted on [R-help]') +
       theme_bw() + theme('legend.position' = 'top')

tail(sort(prop.table(table(sub('.*([+-][0-9]{4}).*', '\\1', lines)))), 22)

##

Rhelp[, date := as.Date(time)]
Rdaily <- Rhelp[, .N, by = date]

Rdaily <- zoo(Rdaily$N, Rdaily$date)

plot(Rdaily)

library(forecast)
fit <- ets(Rdaily)

##

forecast(fit, 1)

plot(forecast(fit, 30), include = 365)

##

lists <- rbindlist(list(
    data.frame(name = unique(supporterlist), list = 'supporter'),
    data.frame(name = unique(maintainers), list = 'maintainer'),
    data.frame(name = unique(Rhelpers), list = 'R-help')))

t <- table(lists$name, lists$list)
table(rowSums(t))

library(Rcapture)
descriptive(t)

##

closedp(t)

##

library(fbRads)

## register an application at
##   https://developers.facebook.com/apps/
## and get a token e.g. via
## library(httr)
## app <- oauth_app('facebook', 'your_app_id', 'your_app_secret')
## tkn <- oauth2.0_token(
##   oauth_endpoints('facebook'), app, scope = 'ads_management',
##   type  = 'application/x-www-form-urlencoded')
## tkn <- tkn$credentials$access_token

fbad_init(..., ...)
fbad_get_search(fbacc = fbacc, q = 'rstats', type = 'adinterest')

fbad_get_search(fbacc = fbacc, q = 'SPSS', type = 'adinterest')

res <- fbad_get_search(fbacc = fbacc, q = 'programming language', type = 'adinterest')
res <- res[order(res$audience_size, decreasing = TRUE), ]
res[1:10, 1:3]

##

library(twitteR)
## register an application and make a note of your consumer key & secret,
## also your access token and secret at
##   https://apps.twitter.com
setup_twitter_oauth(...)

str(searchTwitter('#rstats', n = 1, resultType = 'recent'))

##

tweets <- Rtweets(n = 500)

##

length(strip_retweets(tweets))

tweets <- twListToDF(tweets)

library(tm)
corpus <- Corpus(VectorSource(tweets$text))

corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, 'rstats')

library(wordcloud)
wordcloud(corpus)


