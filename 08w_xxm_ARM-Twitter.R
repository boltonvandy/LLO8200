####### Twitter in R
#  Consumer API keys
#  Access token & access token secret

## I have created a text file that contains the
## consumerKey, the comsumerSecret, the access_Token, and the access_Secret
## They are comma seperated. 
## The name of my file is TwitterConKey_ConSec_AccTok_AccSec.txt
#Insert your consumerKey and consumerSecret below

consumerKey='SiMslBfTdWEimvLweRDTTrZVH'
consumerSecret='FoPYqK3uwpzutwE6G1RmQvPbRJ8RChFSLfIlgRAcFHjymKDzHh'
access_Token='1084502204038479872-v2czQaDlMt9ikoLnxhiQYk8Yb3f0RT'
access_Secret='U9ktzvd5rEwcK13mttsgwAujS0VxNPtJstxXcEE5znnid'

#filename="TwitterConKey_ConSec_AccTok_AccSec.txt"
#(tokens<-read.csv(filename, header=TRUE, sep=","))


#(consumerKey=as.character(tokens$consumerKey))
#consumerSecret=as.character(tokens$consumerSecret)
#access_Token=as.character(tokens$access_Token)
#access_Secret=as.character(tokens$access_Secret)


requestURL='https://api.twitter.com/oauth/request_token'
accessURL='https://api.twitter.com/oauth/access_token'
authURL='https://api.twitter.com/oauth/authorize'

### NOTES: rtweet is another excellent option
## https://mkearney.github.io/blog/2017/06/01/intro-to-rtweet/
### https://rtweet.info/

### Install the needed packages...
install.packages("twitteR")
#install.packages("ROAuth")
# install.packages("rtweet")
library(arules)
library(rtweet)
library(twitteR)
library(ROAuth)
library(jsonlite)
#install.packages("streamR")
#library(streamR)
#install.packages("rjson")
library(rjson)
#install.packages("tokenizers")
library(tokenizers)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
#install.packages("syuzhet")  ## sentiment analysis
library(syuzhet)
library(stringr)
#install.packages("arulesViz")
library(arulesViz)

##############  Using twittR ##########################################################
setup_twitter_oauth(consumerKey,consumerSecret,access_Token,access_Secret)

Search<-twitteR::searchTwitter("#Trump",n=100,since="2018-09-09")
(Search_DF <- twListToDF(Search))
TransactionTweetsFile = "Choc.csv"
(Search_DF$text[1])

#make data frame
## do.call is a list that holds all arguments in a function
## https://www.stat.berkeley.edu/~s133/Docall.html
##(Search2_DF <- do.call("rbind", lapply(Search2, as.data.frame)))
## OR
#tokenize_tweets(x, lowercase = TRUE, stopwords = NULL, strip_punct = TRUE, 
#                 strip_url = FALSE, simplify = FALSE)

#tokenize_tweets(Search2_DF$text[1],stopwords = stopwords::stopwords("en"), 
#               lowercase = TRUE,  strip_punct = TRUE, 
#               strip_url = TRUE, simplify = TRUE)

## Start the file
Trans <- file(TransactionTweetsFile)
## Tokenize to words 
Tokens<-tokenizers::tokenize_words(Search_DF$text[1],stopwords = stopwords::stopwords("en"), 
          lowercase = TRUE,  strip_punct = TRUE, strip_numeric = TRUE,simplify = TRUE)
## Write squished tokens
cat(unlist(str_squish(Tokens)), "\n", file=Trans, sep=",")
close(Trans)

## Append remaining lists of tokens into file
## Recall - a list of tokens is the set of words from a Tweet
Trans <- file(TransactionTweetsFile, open = "a")
for(i in 2:nrow(Search_DF)){
  Tokens<-tokenize_words(Search_DF$text[i],stopwords = stopwords::stopwords("en"), 
            lowercase = TRUE,  strip_punct = TRUE, simplify = TRUE)
  cat(unlist(str_squish(Tokens)), "\n", file=Trans, sep=",")
}
close(Trans)


######### Read in the tweet transactions
TweetTrans <- read.transactions(TransactionTweetsFile,
                                rm.duplicates = FALSE, 
                                format = "basket",
                                sep=","
                                ## cols = 
                                )
inspect(TweetTrans)
## See the words that occur the most
Sample_Trans <- sample(TweetTrans, 50)
summary(Sample_Trans)

## Read the transactions data into a dataframe
TweetDF <- read.csv(TransactionTweetsFile, header = FALSE, sep = ",")
head(TweetDF)
(str(TweetDF))

## Convert all columns to char 
TweetDF<-TweetDF %>%
  mutate_all(as.character)
(str(TweetDF))
# We can now remove certain words
TweetDF[TweetDF == "t.co"] <- ""
TweetDF[TweetDF == "rt"] <- ""
TweetDF[TweetDF == "http"] <- ""
TweetDF[TweetDF == "https"] <- ""

## Clean with grepl - every row in each column
MyDF<-NULL
for (i in 1:ncol(TweetDF)){
  MyList=c() # each list is a column of logicals ...
  MyList=c(MyList,grepl("[[:digit:]]", TweetDF[[i]]))
  MyDF<-cbind(MyDF,MyList)  ## create a logical DF
  ## TRUE is when a cell has a word that contains digits
}
## For all TRUE, replace with blank
TweetDF[MyDF] <- ""
(head(TweetDF,10))

## does not work## TweetDF[grepl("\\d", TweetDF[[1]])] <- ""
## does not work ## (grepl("[[:digit:]]", TweetDF[[i]]))
#does not work ## TweetDF[grepl("[0-9]+",TweetDF)] <- ""
#(TweetDF)
#grepl("[0-9]+",TweetDF)
#gsub('[[:digit:]]+', '', x)


# Now we save the dataframe using the write table command 
write.table(TweetDF, file = "UpdatedChocolate.csv", col.names = FALSE, 
            row.names = FALSE, sep = ",")
TweetTrans <- read.transactions("UpdatedChocolate.csv", sep =",", 
            format("basket"),  rm.duplicates = TRUE)
inspect(TweetTrans)

### Clean up the datafile
#setwd("C:/Users/profa/Documents/R/RStudioFolder_1/DrGExamples")


## For this ERror
## Error in length(obj) : Method length not implemented for class rules 
## DO THIS: (1) detach("package:arulesViz", unload=TRUE)
## (2) detach("package:arules", unload=TRUE)
## (3) library(arules)



TweetTrans_rules = arules::apriori(TweetTrans, 
            parameter = list(support=.1, confidence=.5, minlen=3))
inspect(TweetTrans_rules[1:10])
## sorted
SortedRules_conf <- sort(TweetTrans_rules, by="confidence", decreasing=TRUE)
inspect(SortedRules_conf[1:20])

SortedRules_sup <- sort(TweetTrans_rules, by="support", decreasing=TRUE)
inspect(SortedRules_sup[1:20])


plot (SortedRules_sup[1:50],method="graph",interactive=TRUE,shading="confidence") 
plot (SortedRules_conf[1:50],method="graph",interactive=TRUE,shading="confidence") 

# feel free to expand and move around the objects in this plot

#plot (SortedRules_conf[1:15], measure=c("support", "lift"), shading="confidence")
