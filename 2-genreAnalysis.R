#install.packages("janitor")
# install.packages(dplyr)
# install.packages(tidyr)
# install.packages(janitor)
# install.packages(ggplot2)
# install.packages(ggthemes)
# install.packages("plotly")
#install.packages("rowr")
library(dplyr)
library(data.table)
library(stringr)
library(tidyr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rowr)
library(dplyr)

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

write_csv(IMDBg,path="Final_Project_Documentation/data/imdb_fistGenre.csv")

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

# save progress

write_csv(genres.df,path="Final_Project_Documentation/data/genresdf.csv")
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

nmovies2.df <- nmovies.df
nmovies2.df <- nmovies2.df %>% dplyr::filter(Var1 != 0)
temp <- adorn_totals(nmovies.df, where = "row", fill = "-", na.rm = TRUE)
temp2 <- adorn_totals(nmovies.df, where = "col", fill = "-", na.rm = TRUE)

# save progress
write_csv(nmovies2.df,path="Final_Project_Documentation/data/imdb_allGenre.csv")

# grab specific year for year on year comparison
t2016 <- melt(temp[91,])
t2016 <- t2016[,c("variable", "value")]
colnames(t2016) <- c("genre", "total count")
t2009 <- melt(temp[91-7,])
t2009 <- t2009[,c("variable", "value")]
colnames(t2009) <- c("genre", "total count")
t2000 <- melt(temp[(91-16),])
t2000 <- t2000[,c("variable", "value")]
colnames(t2000) <- c("genre", "total count")
t1985 <- melt(temp[(91-16-15),])
t1985 <- t1985[,c("variable", "value")]
colnames(t1985) <- c("genre", "total count")
t1970 <- melt(temp[(91-16-15-15),])
t1970 <- t1970[,c("variable", "value")]
colnames(t1970) <- c("genre", "total count")
t1955 <- melt(temp[(91-16-15-15-15),])
t1955 <- t1955[,c("variable", "value")]
colnames(t1970) <- c("genre", "total count")
year.df <- merge(t2016,t2009, by="genre")
year.df <- merge(year.df,t2000, by="genre")
colnames(year.df) <-c("genre", "y2016","y2009","y2000")
year.df <- merge(year.df,t1985, by="genre")
colnames(year.df) <-c("genre", "y2016","y2009", "y2000","y1985")
year.df <- merge(year.df,t1970, by="genre")
colnames(year.df) <-c("genre", "y2016","y2009", "y2000","y1985","y1970")
# drop if entire are zeros
year.df <-  year.df[-c(11, 16, 19, 7,14),]
year.df

t1 <- melt(temp[118,])
t1 <- t1[,c("variable", "value")]
colnames(t1) <- c("genre", "total_count")
# backup genres.df
genres.df2 <- genres.df
# get the year with max count
maxyear <- nmovies.df[which.max(nmovies.df$Freq),]$Var1
genres.df$colour = "other years"
genres.df$colour[genres.df$title_year == maxyear] <- "2009"

#df$est[df$b == 0] <- (df$a[df$b == 0] - 5)/2.533 

#time series number of movies produced per year
ggplot(genres.df, aes(title_year, fill=colour)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  # scale_color_manual(labels = c("other year", "year with maximum productions: 2009")) +
  # guides(color=guide_legend("my title"))+
  theme_economist()

# IMDB %>%
#   plot_ly(x = ~genres, y = ~imdb_score, color = ~content_rating , mode = "markers", text = ~content_rating, alpha = 0.7, type = "scatter")

t2 <- t1[-c(11, 16, 19),]
# different genres produced in total 
t2 <- t2[order(t2$total_count),]
rownames(t2) <- seq(length=nrow(t2)) 

ggplot(data=t2, aes(x=reorder(genre, -total_count),y=total_count)) +
  geom_bar(stat = "identity",  color="red", fill="red", alpha=0.1)+
  labs(x = "Genre", y = "Movie Count", title = "Total number of movies released by genre ") +
  geom_text(aes(label = genre, y=total_count+80), size=3, angle=45, fontface="bold", color="cyan4")+
  theme_economist()+
  scale_y_continuous(breaks=seq(0,2800,300))+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))

year.df2 <- melt(year.df, id.vars='genre')
colnames(year.df2) <- c("genre", "year", "count")
ggplot(year.df2, aes(x=reorder(genre, -count), y=count, fill=year)) +
  geom_bar(stat = "identity", alpha=0.6)+
  labs(x = "Genre", y = "Movie Count", title = "Number of different genres by year ") +
  # geom_text(aes(label = year), position = position_stack(vjust = 0.5),size=3, fontface="bold", color="cyan4")+
  geom_text(aes(label = genre, y=290), size=3, angle=45, fontface="bold", color="cyan4")+
  theme_economist()+
  theme(axis.text.x = element_text(size=6, angle=45,face="bold", color="cyan4"),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))

ggplot(year.df2, aes(x=reorder(genre, -count), y=count, fill=year)) +
  geom_bar(stat = "identity", alpha=0.6, position='dodge')+
  labs(x = "Genre", y = "Movie Count", title = "Number of different genres by year ") +
  # geom_text(aes(label = year), position = position_stack(vjust = 0.5),size=3, fontface="bold", color="cyan4")+
  geom_text(aes(label = genre, y=20), size=3, angle=45, fontface="bold", color="cyan4")+
  theme_economist()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))


# # plot using facet_grid to show the data of different genres differently
# ggplot(ansperyear, aes(x=Year,y=Count)) +
#   geom_bar(stat="identity") + scale_y_continuous(expand = c(0,10)) +
#   facet_grid(Genre ~ .,scales="free")+
#   theme_economist()+
#   labs(x = "Year", y = "Movie Count", title = "Total number of movies released by genre") +
#   theme(panel.background = element_rect(fill = "white",color="black",linetype="dotted"),
#         axis.title.x = element_text(color="cyan4", size=12, face="bold"),
#         axis.title.y = element_text(color="cyan4", size=12, face="bold"))
  

# different genres produced in 2016
ggplot(data=year.df, aes(x=reorder(genre, -y2016),y=y2016)) +
  geom_bar(stat = "identity",  color="red", fill="red", alpha=0.1)+
  labs(x = "Genre", y = "Movie Count", title = "different genres produced in 2016") +
  geom_text(aes(label = genre, y=y2016+5), size=3, angle=45, fontface="bold", color="cyan4")+
  theme_economist()+
  # scale_y_continuous(breaks=seq(0,2800,300))+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))

# # only get the first genre as defined for each title
# ggplot(data=IMDB, aes(x=title_year, y=,imdb_score, color = genres)) +
#   geom_line()
#   # labs(x = "number of movies released", y = "genre", title = "genre vs released")
# 
# ggplot(data=t1, aes(x=genre, y= count, fill = genre)) + geom_boxplot()+theme_wsj()
# 
