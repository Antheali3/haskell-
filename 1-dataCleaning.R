#devtools::install_github("beanumber/imdb")
#devtools::install_github("hrbrmstr/omdbapi")
#slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
#devtools::install_url(slam_url)

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

#Initiating library RWeka
library(RWeka)

#Creating 1 and 2 grams and creating a sparse matrix
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
dtm_up = DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))

freq_up <- colSums(as.matrix(dtm_up))


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
text = as.character(tweets$text) 

##removing Retweets
some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",text)
##let's clean html links
some_txt<-gsub("http[^[:blank:]]+","",some_txt)
##let's remove people names
some_txt<-gsub("@\\w+","",some_txt)
##let's remove punctuations
some_txt<-gsub("[[:punct:]]"," ",some_txt)
##let's remove number (alphanumeric)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)

#visual
library(ggplot2) # Data visualization
library(syuzhet)
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


