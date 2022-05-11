

####################################################################
# TEXT MINING  
# 
#######################################################################
##  TEXT MINING praktika. Vytautas Janilionis  2022-05-04
##
##  1. Prisijungimas prie TWITTER 
##  2. TWITER ?inu?iu importas
##  3. Twiter Text Mining
##
##  Modifikuotas  R programos kodas is e-knygos: 
##  "R Data examples and Case Studies, 2012 (2015)". 
##  e-knyga yra KTU bibliotekoje
##  Programa pritaikytas R tm paketui, versija 0.7-5
#######################################################################

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


#################################
# Option 1.
# RETRIEVING tweets from Twitter
# 
######################################################################
# 
# consumer_key <- "tmrCmRkUIxnmL3lpD76dbXnVV" #API Key
# consumer_secret <- "nDPRBbaUiR1aTK2NXqO7I44HGr55Zd03VY1jSlpL9yLz3uK6sU" # API Key secret
# access_token <- "738658682653790209-TcBVcp8BHbNQ05sahnMMCaqiW3Asmp5"
# access_token_secret <- "PDD3hiIeXSlmlSApzGty9IO9Q99rwTpeUYAEznnn88eCj"
 
# 
# ## Twitter authentication
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
# ## 3200 is the maximum to retrieve
# ## tweetsVJ <- userTimeline("user_name", n = 300)
# 
# ##  Twiter zinuiu  skaitymas i? Twiter temos #sciam  - Scientific American
# rdmTweets <- userTimeline("tesla", includeRts=TRUE, n = 300)  
# ##   includeRts=TRUE  nurodo, kad siusti visas zinutes, o ne tik paskutines savaites  
# saveRDS(rdmTweets, file="rdmTweets.RData")

# RETRIEVING tweets from file
#################################

## Jeigu neturite  Twiter paskyros naudokite i Moodle ikelta  fail?
## rdmTweets.RData  !!!

## load tweets into R
##  load(file = "D:/SAS VJ/Text_mining/data/rdmTweets.RData")
##  load(file = "E:/OneDrive - Kaunas University of Technology/MODULIAI/DDRTM/10 Text analytics/Paskaita/DemoTA/rdmTweets.RData")

rdmTweets <- readRDS("rdmTweets.RData")
(length(rdmTweets))

## 154

###################################
# # Option 3
# # RETRIEVING tweets from http    
###################################

# url <- "http://www.rdatamining.com/data/rdmTweets-201306.RData"
# download.file(url, destfile = "D:/SAS VJ/Text_mining/data/rdmTweets_201306.RData")
# load(file = "D:/SAS VJ/Text_mining/data/rdmTweets_201306.RData")
# rdmTweets<-tweets
# (length(rdmTweets))
# ## 320 

##  BEGIN TEXT ANALYSIS ##

### Zinute spausdina vienoje ilgoje eiluteje  (neptogu skaityti). 
rdmTweets[11:15]

### Patogesnis skaitymui  zinuciu isvedimas, eilutes  ilgis <=73 .
for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(rdmTweets[[i]]$getText(), width=73))
}  

###################################################
###  Transforming Text
###################################################

# convert tweets to a data frame
#df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
#dim(df)

df <- twListToDF(rdmTweets)
dim(df)

# Sudaromas  dokumentu rinkinys (CORPUS) --> 
# Twiter zinuciu rinkinys , kuris toliau redaguojamas naudojant R paketa tm. 

# Build a corpus, and specify the source to be character vectors

myCorpus <- Corpus(VectorSource(df$text))


###################################################
# TEXT CLEANING
###################################################

# Transformations on Corpus:

# m_map {tm paketas} -interface to apply transformation functions 
#             (also denoted as mappings) to corpora.
# ## S3 method for class 'PCorpus' or 'SimpleCorpus'

# tm_map(x, FUN, ...)
#        x     a corpus.
#        FUN	   a transformation function taking a text document
#               (a character vector when x is a SimpleCorpus) as input and 
#               returning a text document (a character vector of the same length
#               as the input vector for SimpleCorpus). 
#               The function content_transformer can be used to create a wrapper 
#               to get and set the content of text documents
#        ...	  arguments to FUN.

# A list of available transformations can be 
# obtained with getTransformations(), and the mostly used are: 
#   removeNumbers(),removePunctuation(),removeWords(),
#   stemDocument() and stripWhitespace().
getTransformations()


# content_transformer (FUN) - create content transformers, i.e., 
# functions which modify the content of an R object.


# Function tolower and toupper convert upper-case characters in a character vector 
# to lower-case, or vice versa. Non-alphabetic characters are left unchanged.

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# Vartotojas gali pats kurti teksto redagavimo funkcijas 
# Generate a custom transformation function.
# Custom transformation: remove URLs  

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)


myCorpus <- tm_map(myCorpus, content_transformer(removeURL))


# A function removeURL() is defined above to remove
# hypelinks, where pattern "http[^[:space:]]*" matches strings starting
# with \http" and then followed by any number of alphabetic characters
# and digits. Strings matching this pattern are
# removed with gsub(). 
# The above pattern is specied as an regular expression, and detail about
# that can be found by running  ?regex in R.
?regex
# Explanation:
# gsub perform replacement of the first and all matches respectively.
# gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
#   fixed = FALSE, useBytes = FALSE)
# pattern       "http[^[:space:]]*"  - R regular expression
# replacement   ""  .


##############################################################################
# INTARPAS. Kaip ra?yti R reguliarias i?rai?kas (R regular expression)? 
# Reguliarios israiskos naudojamos teksto fragmentu paie?kai.
# Pavyzdziui,  reguliari i?rai?ka
# "ab*c" re?kia aib?  {"ac" , "abc", "abbc", "abbbc", ....}, 
# cia * reiskia, kad prie? tai einantis simbolis "b"
# gali pasikartoti 0 arba daugiau kart?.  
###############################################################################
# A regular expression may be followed by one of several repetition quantifiers:
#   
# ?     item is optional and will be matched at most once.
# *     item will be matched zero or more times.
# +     item will be matched one or more times.
# {n}   item is matched exactly n times.
# {n,}  item is matched n or more times.
# {n,m} item is matched at least n times, but not more than m times.
(strings <- c("a", "ab", "a cb", "acc:b", "acbcc b", "acb cccb"))
# # Elements of character vectors x which are
# # not substituted will be returned unchanged
# #  gsub("ac+b", "", strings)
# #   "a"  "ab" ""   ""   ""   ""
gsub("ac+b", "", strings)
gsub("ac{2,}b", "", strings)
gsub("ac*b", "", strings)
# gsub("ac?b", "", strings)
# gsub("ac+b", "", strings)
# gsub("ac{2}b", "", strings)
# gsub("ac{2,}b", "", strings)
# gsub("ac{2,3}b", "", strings)
# gsub("ac[^[:space:]]*", "", strings)

# A character class is a list of characters enclosed between [ and ]
# which matches any single character in that list; 
# unless the first character of the list is the caret ^, 
# when it matches any character not in the list. 

# A range of characters may be specified by giving the first and 
# last characters, separated by a hyphen.
# (Because their interpretation is locale- and implementation-dependent,
# character ranges are best avoided.) 
# The only portable way to specify all ASCII letters is to list them all 
# as the character class
# [ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz].
# (The current implementation uses numerical order of the encoding.)
# Certain named classes of characters are predefined. 
# Their interpretation depends on the locale (see locales);
# the interpretation below is that of the POSIX locale.

# [:alnum:] Alphanumeric characters: [:alpha:] and [:digit:].
# [:alpha:] Alphabetic characters: [:lower:] and [:upper:].
# [:blank:] Blank characters: space and tab, and possibly other locale-dependent characters such as non-breaking space.
# [:cntrl:] Control characters. In ASCII, these characters have
#           octal codes 000 through 037, and 177 (DEL). 
#           In another character set, these are the equivalent characters, 
#           if any.
# [:digit:] Digits: 0 1 2 3 4 5 6 7 8 9.
# [:graph:] Graphical characters: [:alnum:] and [:punct:].
# [:punct:] Punctuation characters:   ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.
# [:space:] Space characters: tab, newline, vertical tab, form feed, carriage return, space and possibly other locale-dependent characters.

# Custom transformation: remove anything other than English letters or space

removeNoEngLettersANDspaces <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNoEngLettersANDspaces))

# Remove Punctutation
# myCorpus <- tm_map(myCorpus, removePunctuation)
# Remove numbers
# myCorpus <- tm_map(myCorpus, removeNumbers)




######################################################
#  STOPWORDS
######################################################
# stopwords("en")  -  R paketo tm stopords zodynas. 
# Operacijos su stopwords zodynu (zodziu vektoriumi).

# Add two extra stop words: "r" , available" and "via"  

myStopwords <- c(stopwords("en"), "r", "available", "via")
stopwords("en")
myStopwords

# Remove stop words: "r" , "big" 
myStopwords <- setdiff(myStopwords, c("r", "big" ))
myStopwords

# Remove mystopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#################
# WHITESPACE
#################

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

myCorpus[11:15]$content
for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=73))
                }  



##########################
###  Steemming words  
##########################

# Kamieno iskyrimo tekste DEMO INTARPO PRADZIA.

# Dokumentu rinkinyje "crude" yra 20 tekstinu dokumentu  

data("crude")
# Saknio isskyrimas  dokumentu rinkinio "crude" treciame sakinyje.
inspect(crude[[2]])
crude2<-stemDocument(crude[[2]])
inspect(stemDocument(crude[[2]]))

        # DEMO INTARPO PABAIGA.

myCorpus
for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=73))
}  

# keep a copy of corpus to use later as a dictionary for stem completion

myCorpusCopy <- myCorpus

# myCorpus2 - stemed corpus
myCorpus2<-tm_map(myCorpus, stemDocument)

# Spausdinam myCorpus2 11-15 ir 1-5 dokumentus.

for (i in 11:15) {
cat(paste("[[", i, "]] ", sep=""))
writeLines(strwrap(myCorpus2[[i]], width=73))
}

for (i in 1:5) {
    cat(paste0("[", i, "] "))
    writeLines(strwrap(as.character(myCorpus2[[i]]), 73))
    }

# After that, we use stemCompletion() to complete the stems with
# the unstemmed corpus myCorpusCopy as a dictionary. 
# With the default setting, it takes the most frequent match in
# dictionary as completion.
########################################################################
# tm paketo versij? skirtumai:
# tm v0.6 ir tm 0.7-8
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

# Keiciam myCcorpus3 struktura i myCorpus  struktura ir kopijuojam zinutes. 

## tik versijai tm v0.6  
#  myCorpus4 <-Corpus(VectorSource(myCorpus3))
## end tm v0.6

## tik versija tm  v0.7-8
# Sukuriam myCorpus4 tokios pat strukturos kaip  myCorpus

 myCorpus4<-myCorpus

# Kopijuojame tekstus is myCorpus3 i myCorpus4 

 for (i in 1:length(myCorpus)) {
  myCorpus4[[i]] <-(myCorpus3[[i]]$content)
 }
# myCorpus4  yra duomenu rinkinys tokios pat strukt?ros kaip ir myCorpus
# po saknu iskyrimo ir zodziu atstaymo pagal sakni is zodyno
# (jeigu zodyne yra keli zodziai su ta pacia saknimi atstatomas tas, kuris 
# dazniausiai pasikartoja dokumentu rinkinyje)
 
## end tm v0.7-5



## Spausdinam myCorpus4

for (i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus4[[i]], width=73))
}

## Kiti budai peziureti myCorpus4 elementus:

inspect(myCorpus4[11:15])
myCorpus4[11:15]
myCorpus4[11]$content[1]
myCorpus4[11]$meta[1]
myCorpus4[[11]] [[1]] [1]
myCorpus4[[11]] [[2]] [1]
myCorpus4[[11]] [[2]] [2]

#################################
# Dokumentu rinkinio redagavimas 
#################################

################
# REPLACE WORD
################


# Below we focus  word \mining" 
# is first stemmed to \mine" and then completed
# to \miners", instead of \mining", although there are many instances
# of \mining" in the tweets,
# compared to only two instances of \miners". There might be a 
# solution for the above problem by changing the parameters
# and/or dictionaries for stemming and completion, but we failed to find
# one due to limitation of time and eforts. Instead, we chose a simple
# way to get around of that by replacing \miners" with \mining",
# since the latter has many more cases than the former in the
# corpus.


# count frequency of "mining"
# count word frequence

wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x),
                                  pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}

n.miner <- wordFreq(myCorpus4, "miner")
n.mining <- wordFreq(myCorpus4, "mining")
cat(n.miner, n.mining)

# replace oldword with newword

replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}
myCorpus4 <- replaceWord(myCorpus4, "miner", "mining")







###################################################
### Building a Term-Document Matrix  (TDM) 
###################################################

tdm <- TermDocumentMatrix(myCorpus4, control=list(wordLengths=c(1,Inf)))

# tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
tdm

# kitas budas tm 0.7-5 versijai , bet tik matricos formavimas 
# s <- SimpleCorpus(VectorSource(unlist(lapply(myCorpus, as.character))))
# tdm <- TermDocumentMatrix(s,
#                         control = list(
#                                        removeNumbers = TRUE,
#                                        stopwords = TRUE,
#                                        stemming = TRUE,
#                                    #   dicionary=myCorpusCopy,
#                                        wordLengths=c(1,Inf)))



# It is very sparse, with 98% of the entries being zero.
# We then have a look at the rst six terms starting 
# with \r" and tweets numbered 101 to 110.

idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx+(0:5),21:30])
idx <- which(dimnames(tdm)$Terms %in% c("r", "data", "mining"))
as.matrix(tdm[idx, 21:30])


inspect(tdm[idx+(0:5),101:110])



# The list of terms can be retrieved with
rownames(tdm)
# Based on the above matrix, many
# data mining tasks can be done, for example, clustering,
# classifcation and association analysis.


###################################################
###  Frequent Terms and Associations 
###################################################

# inspect frequent words
findFreqTerms(tdm, lowfreq=10)

# To show the top frequent words visually, we next make a 
# barplot for them

termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=10)

 library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
   xlab("Terms") + ylab("Count") + coord_flip()

# Alternatively, the above plot can also be drawn with barplot() 
# as below, where las sets the direction of x-axis labels
# to be vertical

barplot(termFrequency, las=2)

# We can also find what are highly associated with a word with 
# function findAssocs()

# which words are associated with "r"?

findAssocs(tdm,"r", 0.25)

# which words are associated with "mine"?
findAssocs(tdm,"data", 0.25)

###################################################
### World Cloud    
###################################################

# After building a term-document matrix, we can show the importance
# of words with a word cloud (also known as a tag cloud), which 
# can be easily produced with package wordcloud (2014)


# In the code below, we first convert the term-document matrix 
# to a normal matrix, and then calculate word frequencies.
# After that, we set gray levels based on word frequency and use
# wordcloud() to make a plot for it.



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



###################################################
###  Clustering Words 
###################################################

# We then try to find clusters of words with hierarchical clustering.
# Sparse terms are removed, so that the plot of clustering will
# not be crowded with words. Then the distances between terms are
# calculated with dist() after scaling. After that, the terms are
# clustered with hclust() and the dendrogram. The agglomeration method
# is set to ward, which denotes the increase in variance when 
# two clusters are merged. Some other options are single linkage, 
# complete linkage, average linkage, median and centroid.

# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse=0.95)
# Kuo sparse arciau vieneto tuo daugiau zodziu lieka matricoje (didesne dimensija).

tdm2
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
# cut tree into 10 clusters
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))

# Insigts:
# In the above dendrogram, we can see the topics in the tweets. 
# Words \analysis", \network" and \social" are clustered
# into one group, because there are a couple of tweets on 
# social network analysis.
# The rightmost three clusters consists of \r", \data"and\mining",
# which are the keywords of tweets on data mining



###################################################
### Clustering Tweets         
###################################################
# k- means Algorithm
# We transpose the term-document matrix to a document-term one. 
# The tweets are then clustered with kmeans()
# After that, we check the popular words in every cluster
# and also the cluster centers. Note that a fxed random seed is set
# with set.seed() before running kmeans(), so that the clustering
# result can be reproduced. 

# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k<- 8
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

# From the above top words and centers of clusters, 
# we can see that the clusters are of diferent topics.
# For instance, cluster 1 focuses on R codes and examples, 
# cluster 4 on data mining
# cluster 6 on R packages
# cluster 3 on social network analysis


# Clustering Tweets with the k-medoids Algorithm

# It is more robust to noise and outliers than k-means clustering,
# and provides a display of the silhouette plot to show
# the quality of clustering. In the example below, 
# we use function pamk() from package fpc (2015),
# which calls the function pam() with the number
# of clusters estimated by optimum average silhouette.



# partitioning around medoids with estimation of number of clusters

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

# set layout to two graphs per page matrix 2x1
# layout(matrix(c(1,2),2,1)) # set to two graphs per page
# plot clustering result
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,
       col.p=pamResult$clustering)
# change back to one graph per page
layout(matrix(1)) 

# the frst chart is a 2D "clusplot" (clustering plot) of the k clusters,
# and the second one shows their silhouettes. With the silhouette, 
# a large s_i (almost 1) suggests that
# the corresponding observations are very well clustered, 
# a small si (around 0) means that the 
# observation lies between two clusters, and observations 
# with a negative si are probably placed in the wrong cluster.
# The average silhouette width is 0.12, which suggests that 
# the clusters are not well separated from one another.

# To improve the clustering quality, we have also tried to set
# the range of cluster numbers
# krange=2:9 when calling pamk()

# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, krange=2:9, metric="manhattan")
# number of clusters identified
 (k<-pamResult$nc)


pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat(paste("cluster", i, ": "))
  cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
  # print tweets in cluster i
  # print(rdmTweets[pamResult$clustering==i])
}

# set layout to two graphs per page matrix 2x1
# layout(matrix(c(1,2),2,1)) # set to two graphs per page
# plot clustering result
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,
     col.p=pamResult$clustering)
# change back to one graph per page
layout(matrix(1)) 


