#install.packages("janitor")
# install.packages(dplyr)
# install.packages(tidyr)
# install.packages(janitor)
# install.packages(ggplot2)
# install.packages(ggthemes)
# install.packages("plotly")
#install.packages("rowr")
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rowr)

# load data 
IMDB = read.csv("data/movie-sun.csv")
IMDBg <- IMDB

# data cleaning 
filterFirst <- function(x){
  if (genres.df[x,1] %like% "Action") "Action"
  else if (genres.df[x,1] %like% "Adventure") "Adventure" 
  else if (genres.df[x,1] %like% "Adventure") "Adventure"
  if (genres.df[x,1] %like% "Animation") "Animation" else 
    if (genres.df[x,1] %like% "Biography") "Biography" else 
      if (genres.df[x,1] %like% "Comedy") "Comedy" else 
        if (genres.df[x,1] %like% "Crime") "Crime" else 
          if (genres.df[x,1] %like% "Documentary") "Documentary" else 
            if (genres.df[x,1] %like% "Drama") "Drama" else 
              if (genres.df[x,1] %like% "Family") "Family" else 
                if (genres.df[x,1] %like% "Fantasy") "Fantasy" else 
                  if (genres.df[x,1] %like% "Film-Noir") "Film-Noir" else 
                    if (genres.df[x,1] %like% "History") "History" else 
                      if (genres.df[x,1] %like% "Horror") "Horror" else 
                        if (genres.df[x,1] %like% "Musical") "Musical" else 
                          if (genres.df[x,1] %like% "Mystery") "Mystery" else 
                            if (genres.df[x,1] %like% "News") "News" else 
                              if (genres.df[x,1] %like% "Romance") "Romance" else 
                                if (genres.df[x,1] %like% "Sci-Fi") "Sci-Fi" else 
                                  if (genres.df[x,1] %like% "Short") "Short" else 
                                    if (genres.df[x,1] %like% "Sport") "Sport" else 
                                      if (genres.df[x,1] %like% "Thriller") "Thriller" else 
                                        if (genres.df[x,1] %like% "War") "War" else 
                                          if (genres.df[x,1] %like% "Western") "Western" else "None" 
  
}

IMDBg$genres <- sapply(1:length(IMDBg$genres), filterFirst)

# data cleaning split genre column into factor levels of genres with binary 1 or 0 value 
genres.df <- as.data.frame(IMDB[,c("genres", "imdb_score", "title_year")])
# separate different genres into new columns
genres.df$Action <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Action") 1 else 0)
genres.df$Adventure <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Adventure") 1 else 0)
genres.df$Animation <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Animation") 1 else 0)
genres.df$Biography <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Biography") 1 else 0)
genres.df$Comedy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Comedy") 1 else 0)
genres.df$Crime <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Crime") 1 else 0)
genres.df$Documentary <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Documentary") 1 else 0)
genres.df$Drama <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Drama") 1 else 0)
genres.df$Family <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Family") 1 else 0)
genres.df$Fantasy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Fantasy") 1 else 0)
genres.df$`Film-Noir` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Film-Noir") 1 else 0)
genres.df$History <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "History") 1 else 0)
genres.df$Horror <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Horror") 1 else 0)
genres.df$Musical <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Musical") 1 else 0)
genres.df$Mystery <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Mystery") 1 else 0)
genres.df$News <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "News") 1 else 0)
genres.df$Romance <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Romance") 1 else 0)
genres.df$`Sci-Fi` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sci-Fi") 1 else 0)
genres.df$Short <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Short") 1 else 0)
genres.df$Sport <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sport") 1 else 0)
genres.df$Thriller <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Thriller") 1 else 0)
genres.df$War <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "War") 1 else 0)
genres.df$Western <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Western") 1 else 0)

# tidy up the data set  
genres.df <- genres.df[order(genres.df$title_year),]
genres.df <- genres.df[complete.cases(genres.df), ]
genres.df <- genres.df %>% drop_na()

# get the mean imdb rating for each genre
meanscore <- rep(0,23)
for (i in 1:23) {
  meanscore[i] <- mean(genres.df$imdb_score[genres.df[i+2]==1])
}

nmovies.df <- as.data.frame(table(genres.df$title_year))

nmovies.df <- cbind.fill(nmovies.df, genres.df[c(1:117),c(4:23)], fill = 0)

for (i in 3:22) {
  nmovies.df[i] = 0
}
r <- nrow(genres.df)-1
for (j in 1:91) {
  for (i in j:r){
    i
    j
    genres.df$title_year[i]
    nmovies.df$Var1[j]
    if(genres.df$title_year[i] == nmovies.df$Var1[j]){
      for (k in 3:22) {
        if (genres.df[i,k+1]==1){
          nmovies.df[j, k] = nmovies.df[j, k] + 1
        }
      }
    }
  }
}

nmovies2.df<- nmovies.df
temp <- adorn_totals(nmovies.df, where = "row", fill = "-", na.rm = TRUE)
temp2 <- adorn_totals(nmovies.df, where = "col", fill = "-", na.rm = TRUE)

# grab specific year for year on year comparison
t2016 <- melt(temp[91,])
t2016 <- t2016[,c("variable", "value")]
t2000 <- melt(temp[(91-16),])
t1985 <- melt(temp[(91-16-15),])
t1 <- melt(temp[118,])

# genre vs ratings
barplot(meanscore, main = "Average imdb scores for different genres")

#time series number of movies produced per year
ggplot(genres.df, aes(title_year)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  theme(plot.title = element_text(hjust = 0.5))

IMDB %>%
  plot_ly(x = ~genres, y = ~imdb_score, color = ~content_rating , mode = "markers", text = ~content_rating, alpha = 0.7, type = "scatter")

# different genres produced in total 
ggplot(data=t1, aes(x=variable, y=value)) +
  geom_bar(stat="identity", color="red", fill="red", alpha=0.1)

# different genres produced in 2016
ggplot(data=t2016, aes(x=variable, y=value)) +
  geom_bar(stat="identity", color="blue", fill="white")+
  labs(x = "number of movies released", y = "genre", title = "genre vs released")

# only get the first genre as defined for each title
ggplot(data=IMDB, aes(x=title_year, y=,imdb_score, color = genres)) +
  geom_line()
  # labs(x = "number of movies released", y = "genre", title = "genre vs released")

ggplot(data=t1, aes(x=variable, y= value, fill = variable)) + geom_boxplot()+theme_wsj()

