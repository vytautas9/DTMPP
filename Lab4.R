library(twitteR)
library(ROAuth)

library(NLP)
library(Rcpp)
library(tm)



library(SnowballC)
library(fpc)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)

library(tidyverse)
library(rtweet)


# RETRIEVING tweets from Twitter
# consumer_key <- "tmrCmRkUIxnmL3lpD76dbXnVV" #API Key
# consumer_secret <- "nDPRBbaUiR1aTK2NXqO7I44HGr55Zd03VY1jSlpL9yLz3uK6sU" # API Key secret
# access_token <- "738658682653790209-TcBVcp8BHbNQ05sahnMMCaqiW3Asmp5"
# access_token_secret <- "PDD3hiIeXSlmlSApzGty9IO9Q99rwTpeUYAEznnn88eCj"
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
# rdmTweets <- userTimeline("tesla", includeRts=TRUE, n = 300)  
# saveRDS(rdmTweets, file="rdmTweets.RData")

# RETRIEVING tweets from file
rdmTweets <- readRDS("rdmTweets.RData")
(length(rdmTweets))


# convert tweets to a data frame
df <- twListToDF(rdmTweets)
dim(df)

# Sudaromas  dokumentu rinkinys (CORPUS) --> 
myCorpus <- Corpus(VectorSource(df$text))

# Check the tweets
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=73))
}


# We need to remove:
# unicode emojis (<U+0000>), 
# mentions (@??????),
# URLS (https://...)
# retweet start (RT @?????)
# symbols??? (&amp;) - 89 row
# escape characters (\n)
# hashtags (#)


clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove <U+???> instances
    str_replace_all("[<].*[>]", "") %>%
    # Remove non-ASCII letters (including emojis)
    str_replace_all("[^\x01-\x7F]", "") %>% 
    # Remove non-alphanumeric characters
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # Remove numbers
    str_replace_all("[[:digit:]]+", " ") %>% 
    # Remove 1-2 letter words
    str_replace_all(" *\\b[[:alpha:]]{1,2}\\b *", " ") %>% 
      # Replace solar roof with solar panel
      str_replace_all("solar roof", "solar panel") %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

myCorpus <- tm_map(myCorpus, content_transformer(clean_tweets))

# Remove mystopwords from corpus
myStopwords <- c(stopwords("en"), "k", "b", "m", "tesla", "just", "one", "every", "deliveries", "production")

# Remove mystopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


myCorpusCopy <- myCorpus
myCorpus2<-tm_map(myCorpus, stemDocument)
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus3 <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus4<-myCorpus
for (i in 1:length(myCorpus)) {
  myCorpus4[[i]] <-(myCorpus3[[i]]$content)
}

# Check the orginial tweets and steemed tweets
for (i in 1:300) {
  cat(paste("ORIGINAL", "[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=73))

  cat(paste("STEMMED", "[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus4[[i]], width=73))
}



df.tweets<-data.frame(text = sapply(myCorpus4, as.character), stringsAsFactors = FALSE)
# remove duplicated
tweets.out.unique <- df.tweets[!duplicated(df.tweets),]
myCorpus5 <- Corpus(VectorSource(tweets.out.unique))

rdmTweets_removed <- rdmTweets[!duplicated(df.tweets)]















tdm <- TermDocumentMatrix(myCorpus5, control=list(wordLengths=c(1,Inf)))
tdm
m <- as.matrix(tdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# word cloud
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=5, 
          random.order=F, colors=pal)





# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse=0.975) #0.975
# Kuo sparse arciau vieneto tuo daugiau zodziu lieka matricoje (didesne dimensija).

tdm2
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
# cut tree into 4 clusters
rect.hclust(fit, k=4)
(groups <- cutree(fit, k=4))


# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k<- 4
kmeansResult <- kmeans(m3, k)
# cluster centers
round(kmeansResult$centers, digits=3)
# 
# To make it easy to fnd what the clusters are about,
# we then check the top three words in every
# cluster.

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:5], "\n")
  # print the tweets of every cluster
  # print(rdmTweets[which(kmeansResult$cluster==i)])
}



# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)

pamResult <- pamk(m3, metric="manhattan")
# number of clusters identified
(k <- pamResult$nc)

pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat(paste("cluster", i, ": "))
  cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
  # print tweets in cluster i
  # print(rdmTweets[pamResult$clustering==i])
}

# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, krange=4, metric="manhattan")
# number of clusters identified
(k<-pamResult$nc)


pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat(paste("cluster", i, ": "))
  cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
  # print tweets in cluster i
   print(rdmTweets_removed[pamResult$clustering==i])
}

# set layout to two graphs per page matrix 2x1
# layout(matrix(c(1,2),2,1)) # set to two graphs per page
# plot clustering result
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,
     col.p=pamResult$clustering)
# change back to one graph per page
layout(matrix(1)) 
