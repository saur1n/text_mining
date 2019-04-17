##### TEXT_MINING
##### Text mining from interesting papers to generate a word cloud
##### Author  : Saurin Parikh (dr.saurin.parikh@gmail.com)
##### Date    : 21/11/2018

##### INITIALIZE
# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

##### LOAD DATA
text <- readLines('data/text.txt')
# Load the data as a corpus
docs <- Corpus(VectorSource(text))
inspect(docs)

##### TEXT TRANSFORMATION
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

##### TEXT CLEANUP
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("figure", "data","can","-","spotsizer","gitter","sgatools")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

##### GENERATE TERM-DOCUMENT MATRIX
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

##### WORD CLOUD
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##### MORE
findAssocs(dtm, terms = "genes", corlimit = 0.3)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most Frequent Words",
        ylab = "Word Frequencies")
