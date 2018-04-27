#install.packages("janitor")
library(dplyr)
library(plyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(plotly)

foodata = read.csv("data/movie-sun.csv")
colnames(foodata)
foodata$ratings<-as.numeric(foodata$imdb_score)

g <- strsplit(as.character(foodata$genres),'|',fixed=TRUE)
g1 <- do.call(rbind.data.frame, g)
colnames(g1) <- c('gen1','gen2','gen3', 'gen4','gen5','gen6','gen7','gen8')
g1$gen1 <- as.character(g1$gen1)
g1$gen2 <- as.character(g1$gen2)
g1$gen3 <- as.character(g1$gen3)
g1$gen4 <- as.character(g1$gen4)
g1$gen5 <- as.character(g1$gen5)
g1$gen6 <- as.character(g1$gen6)
g1$gen7 <- as.character(g1$gen7)
g1$gen8 <- as.character(g1$gen8)
foo <- foodata
foo <- data.frame(cbind(foodata,g1),stringsAsFactors=FALSE)
foo <- foo %>% drop_na()
foo<-foo[complete.cases(foo),]


#Action Comedy Romance Thriller Drama Crime Musical
#Adventure Sci-Fi Mystery History Family

bol<-T
Actionrt<-0; Comedyrt<-0; Romancert<-0; Thrillerrt<-0; Crimert<-0;Dramart<-0;
Actioncnt<-0; Comedycnt<-0; Romancecnt<-0; Thrillercnt<-0; Crimecnt<-0;Dramacnt<-0;
Genreyear<-0
foo <- foo[order(foo$title_year),]

j<-1;k<-0
m<-1
i<-1
rows<-nrow(foo)
n<-1:rows
foo[rows+1,]<-foo[rows,]

while(i %in% n)
{ #Actionrt[m]<-0;Actioncnt[m]<-0
  Actionrt[m]<-0; Comedyrt[m]<-0; Romancert[m]<-0; Thrillerrt[m]<-0; Crimert[m]<-0;Dramart[m]<-0;
  Actioncnt[m]<-0; Comedycnt[m]<-0; Romancecnt[m]<-0; Thrillercnt[m]<-0; Crimecnt[m]<-0;Dramacnt[m]<-0;
  
  Genreyear[m]<-0
  k<-j
  #j<-i
  cat("Year:",foo$title_year[j],"\t")
  while(j==k & i<rows)
  { 
    #i<-i+1
    Genreyear[m]<-foo$title_year[i]
    bol<-foo[i,] %like% "Action"
    
    if(any(bol==T))
    { 
      #Genreyear[m]<-foo$year[i]
      
      Actionrt[m]<-Actionrt[m]+foo$ratings[i]
      Actioncnt[m]<-Actioncnt[m]+1
      
      cat("\tActionrt[",m,"]:",Actionrt[m])
    }
    
    bol<-foo[i,] %in% c("Comedy")
    
    if(any(bol==T))
    {
      Comedyrt[m]<-Comedyrt[m]+foo$ratings[i]
      Comedycnt[m]<-Comedycnt[m]+1
    }
    
    bol<-foo[i,] %in% "Romance"
    
    if(any(bol==T))
    {
      Romancert[m]<-Romancert[m]+foo$ratings[i]
      Romancecnt[m]<-Romancecnt[m]+1
    }
    
    bol<-foo[i,] %in% "Drama"
    
    if(any(bol==T))
    {
      Dramart[m]<-Dramart[m]+foo$ratings[i]
      Dramacnt[m]<-Dramacnt[m]+1
    }
    
    bol<-foo[i,] %in% "Thriller"
    
    if(any(bol==T))
    {
      Thrillerrt[m]<-Thrillerrt[m]+foo$ratings[i]
      Thrillercnt[m]<-Thrillercnt[m]+1
    }
    
    bol<-foo[i,] %in% "Crime"
    
    if(any(bol==T))
    {
      Crimert[m]<-Crimert[m]+foo$ratings[i]
      Crimecnt[m]<-Crimecnt[m]+1
    }
    
    
    if(foo$title_year[j]!=foo$title_year[j+1])
    {k<-0
    m<-m+1
    }
    
    #cat("in:",i,"\t")
    #cat("\t j:",j,"\t")
    #cat("year",foo$year[j])
    
    if(foo$title_year[j]==foo$title_year[j+1])
    {i<-i+1}
    
    j<-j+1;k<-k+1;
  }
  
  cat("\t outn:",i,"\n")
  i<-i+1
  
}

foo<-foo[-(rows+1),]


#Data frame creation of all the above computed values
#Action
ansperyear<-data.frame(Genre="Action",Year=Genreyear,Ratings=Actionrt,Count=Actioncnt)

#Comedy
ansperyeartemp<-data.frame(Genre="Comedy",Year=Genreyear,Ratings=Comedyrt,Count=Comedycnt)
ansperyear<-rbind(ansperyear,ansperyeartemp)

#Romance
ansperyeartemp<-data.frame(Genre="Romance",Year=Genreyear,Ratings=Romancert,Count=Romancecnt)
ansperyear<-rbind(ansperyear,ansperyeartemp)

#Thriller
ansperyeartemp<-data.frame(Genre="Thriller",Year=Genreyear,Ratings=Thrillerrt,Count=Thrillercnt)
ansperyear<-rbind(ansperyear,ansperyeartemp)

#Crime
ansperyeartemp<-data.frame(Genre="Crime",Year=Genreyear,Ratings=Crimert,Count=Crimecnt)
ansperyear<-rbind(ansperyear,ansperyeartemp)

#Drama
ansperyeartemp<-data.frame(Genre="Drama",Year=Genreyear,Ratings=Dramart,Count=Dramacnt)
ansperyear<-rbind(ansperyear,ansperyeartemp)


#dataframe ansperyear contains all the recordings of every genre for every year individually
ansperyear1<-ansperyear[ansperyear$Count>0,]
ansperyear1<- ddply(ansperyear, "Count", transform, label_y=cumsum(Count)) 



# a plot with graphs build over one another and a transperancy value is set
ggplot(ansperyear, aes(x=Year,y=Count,fill=Genre)) + 
  geom_histogram(stat="identity",position="identity",alpha=0.4) 

#ggplot(ansperyear1, aes(x=Year, y=Count, fill=Genre)) +    geom_bar(stat="identity")

# plot using facet_grid to show the data of different genres differently
ggplot(ansperyear, aes(x=Year,y=Count)) + 
  geom_bar(stat="identity") + scale_y_continuous(expand = c(0,0.2)) + 
  facet_grid(Genre ~ .,scales="free")+
  theme(panel.background = element_rect(fill = "white",color="black",linetype="dotted"))


#Variable being summed up together
Actioncnttot<-sum(Actioncnt);Comedycnttot<-sum(Comedycnt);Romancecnttot<-sum(Romancecnt);Thrillercnttot<-sum(Thrillercnt); Crimecnttot<-sum(Crimecnt);Dramacnttot<-sum(Dramacnt)
Actionrttot<-sum(Actionrt);Comedyrttot<-sum(Comedyrt);Romancerttot<-sum(Romancert);Thrillerrttot<-sum(Thrillerrt); Crimerttot<-sum(Crimert);Dramarttot<-sum(Dramart)

ans<-data.frame(Genre=c("Action","Comedy","Romance","Thriller","Crime","Drama"))
ans$cnt<-c(Actioncnttot,Comedycnttot,Romancecnttot,Thrillercnttot, Crimecnttot,Dramacnttot)
ans$totrat<-c(Actionrttot,Comedyrttot,Romancerttot,Thrillerrttot, Crimerttot,Dramarttot)
ans$avgrat<-c(Actionrttot/Actioncnttot,Comedyrttot/Comedycnttot,Romancerttot/Romancecnttot,Thrillerrttot/Thrillercnttot, Crimerttot/Crimecnttot,Dramarttot/Dramacnttot)

ggplot(ansperyear1, aes(x=Year, y=Count, fill=Genre)) + 
  geom_bar(position="dodge",stat="identity")+ 
  geom_text(aes(label=Count),  colour="white")


# Plots

ggplot(ans, aes(x=Genre, y=Cnt)) +    geom_bar(stat="identity", colour="black")+ ylab("Genre Count") + geom_text(aes(label=Cnt), vjust=-0.2)

# a take at the googleVis package
#plot through that package
newdf=data.frame(country=ans$Genre, 
                 GenreCount=ans$Cnt, 
                 TotalRating=ans$totrat)

Area <- gvisAreaChart(newdf)
plot(Area)

#ggplot2 package graph


ggplot(ans, aes(x=avgrat, y=totrat, fill=Genre)) +  
  geom_bar(stat="identity", position="dodge") +  
  geom_text(aes(label=totrat), vjust=-1.5, colour="black",      
            position=position_dodge(.9), size=3)+
  xlim(min(ans$avgrat)-0.2,max(ans$avgrat)+0.2)+
  xlab("Average Rating")+ ylab("Total Rating")

