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
t = read_csv("tweets_long.csv")

#extracting relevant data
iw = as.character(t$InfinityWar)
aqp= as.character(t$AQuitePlace)
ifp= as.character(t$IFeelPretty)
rm= as.character(t$Rampage)

#removing Retweets
iwtxt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",iw)
aqptxt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",aqp)
ifptxt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",ifp)
rmtxt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",rm)
# r1_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",r1)

#let's clean html links
iwtxt<-gsub("http[^[:blank:]]+","",iwtxt)
aqptxt<-gsub("http[^[:blank:]]+","",aqptxt)
ifptxt<-gsub("http[^[:blank:]]+","",ifptxt)
rmtxt<-gsub("http[^[:blank:]]+","",rmtxt)
# r1_txt<-gsub("http[^[:blank:]]+","",r1_txt)

#let's remove people names
iwtxt<-gsub("@\\w+","",iwtxt)
aqptxt<-gsub("@\\w+","",aqptxt)
ifptxt<-gsub("@\\w+","",ifptxt)
rmtxt<-gsub("@\\w+","",rmtxt)
# r1_txt<-gsub("@\\w+","",r1_txt)

iwtxt<-gsub("^\\s+|\\s+$","",as.character(iwtxt))
aqptxt<-gsub("^\\s+|\\s+$","",as.character(aqptxt))
ifptxt<-gsub("^\\s+|\\s+$","",as.character(ifptxt))
rmtxt<-gsub("^\\s+|\\s+$","",as.character(rmtxt))
# r1_txt<- gsub("^\\s+|\\s+$", "", as.character(r1_txt))

#remove punctuations
iwtxt<-gsub("[[:punct:]]"," ",iwtxt)
ifptxt<-gsub("[[:punct:]]","",as.character(ifptxt))
iwtxt<-gsub("[[:punct:]]","",as.character(iwtxt))
aqptxt<-gsub("[[:punct:]]","",as.character(aqptxt))

##let's remove number (alphanumeric)
iwtxt<-gsub("[^[:alnum:]]"," ",iwtxt)
aqptxt<-gsub("[^[:alnum:]]","",as.character(aqptxt))
ifptxt<-gsub("[^[:alnum:]]","",as.character(ifptxt))
rmtxt<-gsub("[^[:alnum:]]","",as.character(rmtxt))


#Data Preprocessing Infinity War
set.seed(100)
sampleIW = sample(iw, (length(iwtxt)))
corIW = Corpus(VectorSource(list(sampleIW)))
corIW = tm_map(corIW, removePunctuation)
corIW = tm_map(corIW, content_transformer(tolower))
corIW = tm_map(corIW, removeNumbers)
corIW = tm_map(corIW, removeWords, c(stopwords('english'),'tweets2','tweets'))
corIW = tm_map(corIW, stripWhitespace)
corIW = tm_map(corIW, stemDocument)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9] ","",x)
corIW = tm_map(corIW, removeSpecialChars)

corIW <- tm_map(corIW, PlainTextDocument)

#Data Preprocessing A Quiet Place 
set.seed(100)
sampleAQP = sample(aqp, (length(aqptxt)))
corAQP = Corpus(VectorSource(list(sampleAQP)))
corAQP = tm_map(corAQP, removePunctuation)
corAQP = tm_map(corAQP, content_transformer(tolower))
corAQP = tm_map(corAQP, removeNumbers)
corAQP = tm_map(corAQP, removeWords, c(stopwords('english'),'tweets2','tweets'))
corAQP = tm_map(corAQP, stripWhitespace)
corAQP = tm_map(corAQP, stemDocument)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9] ","",x)
corAQP = tm_map(corAQP, removeSpecialChars)

corAQP <- tm_map(corAQP, PlainTextDocument)


#Data Preprocessing I Feel Pretty
set.seed(100)
sampleIFP = sample(ifp, (length(ifptxt)))
corIFP = Corpus(VectorSource(list(sampleIFP)))
corIFP = tm_map(corIFP, removePunctuation)
corIFP = tm_map(corIFP, content_transformer(tolower))
corIFP = tm_map(corIFP, removeNumbers)
corIFP = tm_map(corIFP, removeWords, c(stopwords('english'),'tweets2','tweets'))
corIFP = tm_map(corIFP, stripWhitespace)
corIFP = tm_map(corIFP, stemDocument)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9] ","",x)
corIFP = tm_map(corIFP, removeSpecialChars)

corIFP <- tm_map(corIFP, PlainTextDocument)


#Data Preprocessing Rampage
set.seed(100)
sampleRM = sample(rm, (length(rmptxt)))
corRM = Corpus(VectorSource(list(sampleRM)))
corRM = tm_map(corRM, removePunctuation)
corRM = tm_map(corRM, content_transformer(tolower))
corRM = tm_map(corRM, removeNumbers)
corRM = tm_map(corRM, removeWords, c(stopwords('english'),'tweets2','tweets'))
corRM = tm_map(corRM, stripWhitespace)
corRM = tm_map(corRM, stemDocument)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9] ","",x)
corRM = tm_map(corRM, removeSpecialChars)

corRM <- tm_map(corRM, PlainTextDocument)

corpus <- cbind(corIW, corAQP, corIFP,corRM)

colnames(corpus) <- c("IW", "AQP", "IFP","RM")

write.csv(corpus, file = "tweets_clean.csv")


# set.seed(100)
# sample = sample(r1, (length(r1_txt)))
# corpus = Corpus(VectorSource(list(sample)))
# corpus = tm_map(corpus, removePunctuation)
# corpus = tm_map(corpus, content_transformer(tolower))
# corpus = tm_map(corpus, removeNumbers)
# corpus = tm_map(corpus, removeWords, c(stopwords('english'),'tweets2','tweets'))
# corpus = tm_map(corpus, stripWhitespace)
# corpus = tm_map(corpus, stemDocument)
# 
# removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9] ","",x)
# corpus = tm_map(corpus, removeSpecialChars)
# 
# corpus <- tm_map(corpus, PlainTextDocument)


