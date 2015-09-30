## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #7: Unstructured Data. pp. 153-168.
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

library(tm)
getSources()

##

getReaders()

res <- XML::readHTMLTable('http://cran.r-project.org/web/packages/available_packages_by_name.html', which = 1)

v <- Corpus(VectorSource(res$V2))

##

v

inspect(head(v, 3))

getTransformations()

##

stopwords('english')

##

removeWords('to be or not to be', stopwords('english'))

v <- tm_map(v, removeWords, stopwords('english'))

inspect(head(v, 3))

##

removeWords('To be or not to be.', stopwords('english'))

v <- tm_map(v, content_transformer(tolower))
v <- tm_map(v, removePunctuation)
v <- tm_map(v, stripWhitespace)
inspect(head(v, 3))

##

library(wordcloud)
wordcloud::wordcloud(v)

##

v <- tm_map(v, removeNumbers)

tdm <- TermDocumentMatrix(v)

inspect(tdm[1:5, 1:20])

##

findFreqTerms(tdm, lowfreq = 100)

myStopwords <- c('package', 'based', 'using')
v <- tm_map(v, removeWords, myStopwords)

library(SnowballC)
wordStem(c('cats', 'mastering', 'modelling', 'models', 'model'))

wordStem(c('are', 'analyst', 'analyze', 'analysis'))

##

d <- v

v <- tm_map(v, stemDocument, language = 'english')

v <- tm_map(v, content_transformer(function(x, d) paste(stemCompletion(strsplit(stemDocument(x), ' ')[[1]], d), collapse = ' ')), d)

tdm <- TermDocumentMatrix(v)
findFreqTerms(tdm, lowfreq = 100)

##

tdm

##

findAssocs(tdm, 'data', 0.1)

findAssocs(tdm, 'big', 0.1)

##

vnchar <- sapply(v, function(x) nchar(x$content))
summary(vnchar)

(vm <- which.min(vnchar))

v[[vm]]
res[vm, ]

##

hist(vnchar, main = 'Length of R package descriptions', xlab = 'Number of characters')

##

hadleyverse <- c('ggplot2', 'dplyr', 'reshape2', 'lubridate', 'stringr', 'devtools', 'roxygen2', 'tidyr')

(w <- which(res$V1 %in% hadleyverse))

plot(hclust(dist(DocumentTermMatrix(v[w]))), xlab = 'Hadleyverse packages')

##

sapply(v[w], function(x) structure(content(x), .Names = meta(x, 'id')))


library(topicmodels)
fit <- LDA(DocumentTermMatrix(v[w]), k = 3)
topics(fit)
terms(fit, 10)

fit <- CTM(DocumentTermMatrix(v[w]), k = 3)
