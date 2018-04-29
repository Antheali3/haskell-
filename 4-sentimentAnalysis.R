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
library(ggplot2) 
library(syuzhet)

tw.df <- read_csv("tweets_clean.csv")

#Creating 1 and 2 grams and creating a sparse matrix
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
dtm_up_iw = DocumentTermMatrix(tw.df$IW, control = list(tokenize = BigramTokenizer))
dtm_up_aqp = DocumentTermMatrix(tw.df$AQP, control = list(tokenize = BigramTokenizer))
dtm_up_ifp = DocumentTermMatrix(tw.df$IFP, control = list(tokenize = BigramTokenizer))
dtm_up_rm = DocumentTermMatrix(tw.df$RM, control = list(tokenize = BigramTokenizer))

freq_up_iw <- colSums(as.matrix(dtm_up_iw))
freq_up_aqp <- colSums(as.matrix(dtm_up_aqp))
freq_up_ifp <- colSums(as.matrix(dtm_up_ifp))
freq_up_rm <- colSums(as.matrix(dtm_up_rm))


#Calculating Sentiments
sentiments_up = calculate_sentiment(names(freq_up))
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

DT::datatable(sent_pos_up)

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(sent_pos_up$text,sent_pos_up$freq,min.freq=10,colors=brewer.pal(6,"Dark2"))

DT::datatable(sent_neg_up)

plot.new()
set.seed(100)
wordcloud(sent_neg_up$text,sent_neg_up$freq, min.freq=10,colors=brewer.pal(6,"Dark2"))


#Approach 2 - using the 'syuzhet' package
iw = as.character(tw.iw.df$IW) 

##removing Retw.iweets
tw.iw<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",iw)
##let's clean html links
tw.iw<-gsub("http[^[:blank:]]+","",tw.iw)
##let's remove people names
tw.iw<-gsub("@\\w+","",tw.iw)
##let's remove punctuations
tw.iw<-gsub("[[:punct:]]"," ",tw.iw)
##let's remove number (alphanumeric)
tw.iw<-gsub("[^[:alnum:]]"," ",tw.iw)


sent.iw<-get_nrc_sentiment((tw.iw))

# Get the sentiment score for each emotion
sent.iw.positive =sum(sent.iw$positive)
sent.iw.anger =sum(sent.iw$anger)
sent.iw.anticipation =sum(sent.iw$anticipation)
sent.iw.disgust =sum(sent.iw$disgust)
sent.iw.fear =sum(sent.iw$fear)
sent.iw.joy =sum(sent.iw$joy)
sent.iw.sadness =sum(sent.iw$sadness)
sent.iw.surprise =sum(sent.iw$surprise)
sent.iw.trust =sum(sent.iw$trust)
sent.iw.negative =sum(sent.iw$negative)

# Create the bar chart
yAxis <- c(sent.iw.positive,
           + sent.iw.anger,
           + sent.iw.anticipation,
           + sent.iw.disgust,
           + sent.iw.fear,
           + sent.iw.joy,
           + sent.iw.sadness,
           + sent.iw.surprise,
           + sent.iw.trust,
           + sent.iw.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis) + 1000
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional violence", ylab = "Score", main = "Twitter sentiment for tw.iweets 2", sub = "", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")
colSums(sent.iw)

 
#Approach 2 - using the 'syuzhet' package
text = as.character(tweets$text)

##removing Retweets
tw<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",text)
##let's clean html links
tw<-gsub("http[^[:blank:]]+","",tw)
##let's remove people names
tw<-gsub("@\\w+","",tw)
##let's remove punctuations
tw<-gsub("[[:punct:]]"," ",tw)
##let's remove number (alphanumeric)
tw<-gsub("[^[:alnum:]]"," ",tw)


mysentiment<-get_nrc_sentiment((some_txt))

# Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis) + 1000
barplot(yAxis, names.arg = xAxis,
        xlab = "Emotional violence", ylab = "Score", main = "Twitter sentiment for tweets 2", sub = "", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")
colSums(mysentiment)


