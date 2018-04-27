#devtools::install_github("beanumber/imdb")
#devtools::install_github("hrbrmstr/omdbapi")
#install.packages("etl")
#install.packages("ggplot2movies")
#install.packages("boxoffice")
#install.packages("pbapply")
#install.packages("wordcloud")
#install.packages('tm')
#install.packages('SnowballC')
#install.packages('RWeka')
#install.packages('DT')
#install.packages('RSentiment')
#install.packages('slam')

library(omdbapi)
library(dplyr)
library(pbapply)
library(readr)
library(stringr)
library(readr)
library(wordcloud)
library(tm)
library(SnowballC)
library(RWeka)
library(RSentiment)
library(DT)

#Loading the data
t = read_csv("tweets.csv")

#extracting relevant data
r1 = as.character(t$text)

#removing Retweets
r1_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",r1)

#let's clean html links
r1_txt<-gsub("http[^[:blank:]]+","",r1_txt)

#let's remove people names
r1_txt<-gsub("@\\w+","",r1_txt)

r1_txt<- gsub("^\\s+|\\s+$", "", as.character(r1_txt))


#Data Preprocessing
set.seed(100)
sample = sample(r1, (length(r1_txt)))
corpus = Corpus(VectorSource(list(sample)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, c(stopwords('english'),'tweets2','tweets'))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, stemDocument)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9] ","",x)
corpus = tm_map(corpus, removeSpecialChars)

corpus <- tm_map(corpus, PlainTextDocument)

write.csv(corpus, file = "tweets_clean.csv")
