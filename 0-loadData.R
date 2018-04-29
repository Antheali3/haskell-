#install.packages("rtweet")
#install.packages("ROAuth")
#install.packages("twitteR")
library(twitteR)
library(rtweet)
library(ROAuth)

# Set API Keys
#Setup with the personal secret keys from your Twitter account
api_key <- 	"5cQq18oLwAyMcYeTnJXNf1Xs7"
api_secret <- "Wi8FDcmtxtQrxOiOwy7hOP5tmCOxfh2jmpx1D5NVrayG2BPtSN"
access_token <- "974718269830647810-5aF4ccRqLaMP7yMv1ME2MrTaBxC5sQJ"
access_token_secret <- "scb6S8QFCDiiiXuts1FJm8i36hKRsNnq3teGRTXVJKId7"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#AvengersInfinityWar
#InfinityWar 
tweets_iw <- searchTwitter('#InfinityWar', n = 5000, lang="en")
#tweets_aqp <- searchTwitter('#AQuitePlace', n=500, lang="en")
#AQuitePlace
tweets_aqp <- searchTwitter('#AQuitePlace', n = 5000, lang="en")
#tweets_aqp <- searchTwitter('#AQuitePlace', n=500, lang="en")
#IFeelPretty
tweets_ifp <- searchTwitter('#IFeelPretty', n = 5000, lang="en")
#tweets_iw <- searchTwitter('#IFeelPretty', n=500, lang="en")
#RampageMovie 
tweets_rm <- searchTwitter('#RampageMovie', n = 5000, lang="en")
#tweets_rm <- searchTwitter('#RampageMovie', n=500, lang="en")

# convert tweets to a data frame

tweets.iw <- twListToDF(tweets_iw)
tweets.aqp <- twListToDF(tweets_aqp)
tweets.ifp <- twListToDF(tweets_iw)
tweets.rm <- twListToDF(tweets_rm)

# presreves original value
# tweets.df.iw <- twListToDF(tweets_iw)
# tweets.df.aqp <- twListToDF(tweets_aqp)
# tweets.df.ifp <- twListToDF(tweets_iw)
# tweets.df.rm <- twListToDF(tweets_rm)

tweets.df <- cbind(tweets.iw$text)
tweets.df <- cbind(tweets.df, tweets.aqp$text)
tweets.df <- cbind(tweets.df, tweets.ifp$text)
tweets.df <- cbind(tweets.df, tweets.rm$text)

colnames(tweets.df) <- c("InfinityWar", "AQuitePlace", "IFeelPretty","Rampage")

write.csv(tweets.df, file = "tweets_long.csv")
write.csv(tweets.df, file = "tweets.csv")
