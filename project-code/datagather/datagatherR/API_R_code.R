library(selectr)
library(rvest)
library(xml2)
library(rtweet) # for scraping tweets
library(wordcloud2) # for generating really cool looking wordclouds
library(tm) # for text minning
library(dplyr) # loads of fun stuff including piping
library(ROAuth)
library(jsonlite)
library(httpuv)
library(twitteR)
### Setting API parameters

setwd('C:\\Users\\84160\\Desktop')
api = read.csv('Twitter_API.txt')


consumer_key = api[api["Type"]=="API Key"][2]
consumer_secret = api[api["Type"]=="API Key Secret"][2]
access_token = api[api["Type"]=="Access Token"][2]
access_token_secret = api[api["Type"]=="Access Token Secret"][2]
bearer_token = api[api["Type"]=="Bearer Token"][2]

### Extracting tweets

library(twitteR)

requestURL='https://api.twitter.com/oauth/request_token'
accessURL='https://api.twitter.com/oauth/access_token'
authURL='https://api.twitter.com/oauth/authorize'

s_key  = 'online seller'
n_tweets = 250 

twitteR:::setup_twitter_oauth(consumer_key, consumer_secret,access_token,access_token_secret)
Search1<-twitteR::searchTwitter(s_key,n=n_tweets, since="2022-04-01")
# Search2<-twitteR::searchTwitter("#datascience",n=50, since="2022-01-01")
TweetsDF<- twListToDF(Search1)

########## Place Tweets in a new file ###################
FName = "./MyFileExample.txt"
## Start the file
MyFile <- file(FName)
## Write Tweets to file
cat(unlist(TweetsDF), " ", file=MyFile, sep="\n")
close(MyFile)



