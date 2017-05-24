Consumer.C <- read.csv("CC copy.csv", stringsAsFactors = FALSE)

## checking out the structure of data frame
str(Consumer.C)

##number of rows and columns
nrow(Consumer.C)
ncol(Consumer.C)

## libraries to load in this enviornment

library(plyr)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(ggplot2)
library(stringr)
library(syuzhet)
library(psych)
library(lubridate)
library(jsonlite)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(Amelia)
library(proxy)
library(sentimentr)
library(SnowballC)
library(tcltk)
library(arules)
library(arulesViz)

install.packages("Rcpp",dependencies = T)
install.packages("RcppArmadillo",dependencies = T)
install.packages("BH",dependencies = T)
install.packages("tcltk", dependencies = T)
## since there is no value in sub-product for consumer protection bureau will delete
Consumer.C$Sub.product <- NULL
Consumer.C$Sub.issue <- NULL
## let check the missing values
missmap(Consumer.C)

## Code for Text Analysis

glimpse(Consumer.C)

comments <- Consumer.C$Consumer.complaint.narrative

writeLines(as.character(Consumer.C[[4]]))

##nohandles <- tolower(comments)
#nohandles <- tolower(comments)

nohandles <- str_replace_all(comments, "XXXX","")
#nohandles <- str_replace_all(comments, "{\\","")
nohandles <- str_replace_all(comments,"[^[:graph:]]", " ")
nohandles <- sapply(comments,function(row) iconv(row, "latin1", "ASCII", sub=""))

s_v <- get_sentences(nohandles)

wordCorpus <- Corpus(VectorSource(s_v))
#Remove URLs
#removeURL <- function(x) gsub("https[[:alnum:]]*","",x)
#wordCorpus <- tm_map(wordCorpus, removeURL)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, removeWords, c("one","get","will","just","make","even","also","got","now", "to","is", "the","in","and","for","a", "of","at", "and", "by","told","many","however","still","every","credit","card","instead","yet","try","say","tried","per","much","take","tell","etc","within","used","use","please","believe","took","like"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
#wordCorpus <- tm_map(wordCorpus, PlainTextDocument)

#df <- as.data.frame(as.matrix(wordCorpus))

m <-as.matrix(wordCorpus)
v <- sort(colSums(m),decreasing=TRUE)
WordCorpusCopy <- wordCorpus

library(ggplot2)
ggplot()
### Word Clouds###################################################
wordcloud(wordCorpus, min.freq = 100, scale = c(4,0.5),
          max.word=500, random.order=F, colors=brewer.pal(8, "Dark2"))


df <-write.csv(wordCorpus, file = "CleanCCSentiment.csv")
##DOcument term Matrix#########################################

Consumer.C.TDM <-TermDocumentMatrix(wordCorpus)

#we need to sum up all the values in each term column so that we can drive the frequency of each term occurrence. We also want to sort those values from highest to lowest. ##
m <-as.matrix(Consumer.C.TDM)
v <- sort(colSums(m),decreasing=TRUE)

words <- names(v)
d <- data.frame(word=words, freq=v)

wordcloud(d$word,d$freq,min.freq = 45)

WordCorpusCopy <- wordCorpus


# inspect frequent words

library(arules)
library(arulesViz)

freq = colSums(as.matrix(Consumer.C.TDM))
ord = order(-freq)

## exmain head and tail of the object
freq[head(ord)]

findFreqTerms(Consumer.C.TDM, 50)


findAssocs(Consumer.C.TDM, "charged", .20) 
findAssocs(Consumer.C.TDM, "payment", .30)
findAssocs(Consumer.C.TDM, "capital", .60)
findAssocs(Consumer.C.TDM, "dispute", .70)
findAssocs(Consumer.C.TDM, "business", .10)
findAssocs(Consumer.C.TDM, "fraudulent", .40)
findAssocs(Consumer.C.TDM, "balance", .10)
findAssocs(Consumer.C.TDM, "fee", .40)
findAssocs(Consumer.C.TDM, "pay", .20)

itemFrequencyplot(voc, topN=15, type = "relative")


Consumer.CHighReq <- findFreqTerms(Consumer.C.TDM, 10, )
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = lapply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('(?![<>+])[[:punct:]]', '', sentence, perl=TRUE)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    
    #Error handling for tolower conversion
    tryTolower = function(x){
      y = NA
      try_error <- tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
        y = tolower(x)
      return(y)
    }
    sentence = sapply(sentence, tryTolower)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
pos.words <- scan("positive-words.txt",what='character', comment.char=';', fileEncoding="utf-8")
neg.words <- scan("negative-words.txt",what='character', comment.char=';', fileEncoding="utf-8")

scores <- score.sentiment(s_v, pos.words, neg.words, .progress='text')

mean(scores$score)
hist(scores$score, col = 'light blue', main = "Histogram of Sentiment Score",
     xlab = "Sentiment Score")

comments_sentiment <- get_nrc_sentiment(s_v)


sentimentTotals <- data.frame(colSums(comments_sentiment))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

ggplot(data = sentimentTotals[1:8,], aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for All Comments")

###SIMPLE  CLUSTERING###

###hierarchial clustering

docsdissim <- dis(Consumer.C.TDM, method = "cosine")



dtm_tfxidf <- weightTfIdf(Consumer.C.TDM)

inspect(dtm_tfxidf[1:10, 5001:5010])

### do document clustering


econ.tdm2 <- removeSparseTerms(wordCorpus, sparse= 0.95)
consumer.km <- as.matrix(Consumer.C.TDM)
titles.dataframe<-as.data.frame(consumer.km, stringAsFactors=FALSE)

ws <- c(wordCorpus)
dissimilarity(wordCorpus[[4]],wordCorpus[[5]], "cosine")


write.csv(m,file=”dtmEight2Late.csv”)
#shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                     +                      substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m)

kPostText <- GetPositiveText()


library(graph)
library(Rgraphviz)
plot(Consumer.C.TDM, corThreshold = 0.1, weighting = T)

