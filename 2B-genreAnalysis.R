#install.packages("janitor")
# install.packages(dplyr)
# install.packages(plyr)
# install.packages(tidyr)
# install.packages(janitor)
# install.packages(ggplot2)
# install.packages(ggthemes)
# install.packages(plotly)
# install.packages(googleVis)
library(dplyr)
library(data.table)
library(plyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(plotly)
library(googleVis)

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
Actionrt<-0; Comedyrt<-0; Romancert<-0; Thrillerrt<-0; Crimert<-0;Dramart<-0;Animationrt<-0;SFrt<-0;Adventurert<-0;
Actiongross<-0; Comedygross<-0; Romancegross<-0; Thrillergross<-0; Crimegross<-0;Dramagross<-0;Animationgross<-0;SFgross<-0;Adventuregross<-0;
Actioncnt<-0; Comedycnt<-0; Romancecnt<-0; Thrillercnt<-0; Crimecnt<-0;Dramacnt<-0;Adventurecnt<-0
Animationcnt<-0;SFcnt<-0;
Actionbudget<-0; Comedybudget<-0; Romancebudget<-0; Thrillerbudget<-0; Crimebudget<-0;Dramabudget<-0;Animationbudget<-0;SFbudget<-0;Adventurebudget<-0
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
  Actionrt[m]<-0; Comedyrt[m]<-0; Romancert[m]<-0; Thrillerrt[m]<-0; Crimert[m]<-0;Dramart[m]<-0;Animationrt[m]<-0;SFrt[m]<-0;Adventurert[m]<-0;
  Actioncnt[m]<-0; Comedycnt[m]<-0; Romancecnt[m]<-0; Thrillercnt[m]<-0; Crimecnt[m]<-0;Dramacnt[m]<-0;Animationcnt[m]<-0;SFcnt[m]<-0;Adventurecnt[m]<-0;
  Actiongross[m]<-0; Comedygross[m]<-0; Romancegross[m]<-0; Thrillergross[m]<-0; Crimegross[m]<-0;Dramagross[m]<-0;Animationgross[m]<-0;SFgross[m]<-0;Adventuregross[m]<-0;
  Actionbudget[m]<-0; Comedybudget[m]<-0; Romancebudget[m]<-0; Thrillerbudget[m]<-0; Crimebudget[m]<-0;Dramabudget[m]<-0;Animationbudget[m]<-0;SFbudget[m]<-0;Adventurebudget[m]<-0;
  
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
      Actiongross[m]<-Actiongross[m]+foo$gross[i]
      Actionbudget[m]<-Actionbudget[m]+foo$budget[i]
      Actioncnt[m]<-Actioncnt[m]+1
      
      cat("\tActionrt[",m,"]:",Actionrt[m])
    }
    
    bol<-foo[i,] %in% c("Comedy")
    
    if(any(bol==T))
    {
      Comedyrt[m]<-Comedyrt[m]+foo$ratings[i]
      Comedygross[m]<-Comedygross[m]+foo$gross[i]
      Comedybudget[m]<-Comedybudget[m]+foo$budget[i]
      Comedycnt[m]<-Comedycnt[m]+1
    }
    
    bol<-foo[i,] %in% c("Adventure")
    
    if(any(bol==T))
    {
      Adventurert[m]<-Adventurert[m]+foo$ratings[i]
      Adventuregross[m]<-Adventuregross[m]+foo$gross[i]
      Adventurebudget[m]<-Adventurebudget[m]+foo$budget[i]
      Adventurecnt[m]<-Adventurecnt[m]+1
    }
    
    bol<-foo[i,] %in% "Romance"
    
    if(any(bol==T))
    {
      Romancert[m]<-Romancert[m]+foo$ratings[i]
      Romancegross[m]<-Romancegross[m]+foo$gross[i]
      Romancebudget[m]<-Romancebudget[m]+foo$budget[i]
      Romancecnt[m]<-Romancecnt[m]+1
    }
    
    bol<-foo[i,] %in% "Drama"
    
    if(any(bol==T))
    {
      Dramart[m]<-Dramart[m]+foo$ratings[i]
      Dramagross[m]<-Dramagross[m]+foo$gross[i]
      Dramabudget[m]<-Dramabudget[m]+foo$budget[i]
      Dramacnt[m]<-Dramacnt[m]+1
    }
    
    bol<-foo[i,] %in% "Thriller"
    
    if(any(bol==T))
    {
      Thrillerrt[m]<-Thrillerrt[m]+foo$ratings[i]
      Thrillergross[m]<-Thrillergross[m]+foo$gross[i]
      Thrillerbudget[m]<-Thrillerbudget[m]+foo$budget[i]
      Thrillercnt[m]<-Thrillercnt[m]+1
    }
    
    bol<-foo[i,] %in% "Crime"
    
    if(any(bol==T))
    {
      Crimert[m]<-Crimert[m]+foo$ratings[i]
      Crimegross[m]<-Crimegross[m]+foo$gross[i]
      Crimebudget[m]<-Crimebudget[m]+foo$budget[i]
      Crimecnt[m]<-Crimecnt[m]+1
    }
    
    bol<-foo[i,] %in% "Animation"
    
    if(any(bol==T))
    {
      Animationrt[m]<-Animationrt[m]+foo$ratings[i]
      Animationgross[m]<-Animationgross[m]+foo$gross[i]
      Animationbudget[m]<-Animationbudget[m]+foo$budget[i]
      Animationcnt[m]<-Animationcnt[m]+1
    }
    
    bol<-foo[i,] %in% "Sci-Fi"
    
    if(any(bol==T))
    {
      SFrt[m]<-SFrt[m]+foo$ratings[i]
      SFgross[m]<-SFgross[m]+foo$gross[i]
      SFbudget[m]<-SFbudget[m]+foo$budget[i]
      SFcnt[m]<-SFcnt[m]+1
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

# save progress
write_csv(foo,path="Final_Project_Documentation/data/foo.csv")

#Data frame creation of all the above computed values
#Action
genperyear<-data.frame(Genre="Action",Year=Genreyear,Ratings=Actionrt,Count=Actioncnt,Gross = Actiongross, Budget = Actionbudget)

#Comedy
genperyeartemp<-data.frame(Genre="Comedy",Year=Genreyear,Ratings=Comedyrt,Count=Comedycnt, Gross = Comedygross, Budget = Comedybudget)
genperyear<-rbind(genperyear,genperyeartemp)

#Adventure
genperyeartemp<-data.frame(Genre="Adventure",Year=Genreyear,Ratings=Adventurert,Count=Adventurecnt, Gross = Adventuregross, Budget = Adventurebudget)
genperyear<-rbind(genperyear,genperyeartemp)

#Romance
genperyeartemp<-data.frame(Genre="Romance",Year=Genreyear,Ratings=Romancert,Count=Romancecnt, Gross = Romancegross, Budget = Romancebudget)
genperyear<-rbind(genperyear,genperyeartemp)

#Thriller
genperyeartemp<-data.frame(Genre="Thriller",Year=Genreyear,Ratings=Thrillerrt,Count=Thrillercnt, Gross = Thrillergross, Budget = Thrillerbudget)
genperyear<-rbind(genperyear,genperyeartemp)

#Crime
genperyeartemp<-data.frame(Genre="Crime",Year=Genreyear,Ratings=Crimert,Count=Crimecnt, Gross = Crimegross, Budget = Crimebudget)
genperyear<-rbind(genperyear,genperyeartemp)

#Drama
genperyeartemp<-data.frame(Genre="Drama",Year=Genreyear,Ratings=Dramart,Count=Dramacnt, Gross = Dramagross, Budget = Dramabudget)
genperyear<-rbind(genperyear,genperyeartemp)

#Animation
genperyeartemp<-data.frame(Genre="Animation",Year=Genreyear,Ratings=Animationrt,Count=Animationcnt, Gross = Animationgross, Budget = Animationbudget)
genperyear<-rbind(genperyear,genperyeartemp)

#SciFi
genperyeartemp<-data.frame(Genre="SciFi",Year=Genreyear,Ratings=SFrt,Count=SFcnt, Gross = SFgross, Budget = Animationbudget)
genperyear<-rbind(genperyear,genperyeartemp)


#dataframe genperyear contains all the recordings of every genre for every year individually
genperyear1<-genperyear[genperyear$Count>0,]
genperyear1<- ddply(genperyear, "Count", trgenform, label_y=cumsum(Count)) 

# save progress:
write_csv(genperyear1,path="Final_Project_Documentation/data/genperyear1.csv")

# a plot with graphs build over one another and a trgenperancy value is set
# ggplot(genperyear, aes(x=Year,y=Count,fill=Genre)) +
#   geom_histogram(stat="identity",position="identity",alpha=0.4)

# ggplot(genperyear1, aes(x=Year, y=Count, fill=Genre)) +    geom_bar(stat="identity")

#Variable being summed up together
Actioncnttot<-sum(Actioncnt);Comedycnttot<-sum(Comedycnt);Romancecnttot<-sum(Romancecnt);Thrillercnttot<-sum(Thrillercnt); Crimecnttot<-sum(Crimecnt);Dramacnttot<-sum(Dramacnt);Animationcnttot<-sum(Animationcnt);SFcnttot<-sum(SFcnt);Adventurecnttot<-sum(Adventurecnt)
Actionrttot<-sum(Actionrt);Comedyrttot<-sum(Comedyrt);Romancerttot<-sum(Romancert);Thrillerrttot<-sum(Thrillerrt); Crimerttot<-sum(Crimert);Dramarttot<-sum(Dramart);Animationrttot<-sum(Animationrt);SFrttot<-sum(SFrt);Adventurerttot<-sum(Adventurert)
Actiongrosstot<-sum(Actiongross);Comedygrosstot<-sum(Comedygross);Romancegrosstot<-sum(Romancegross);Thrillergrosstot<-sum(Thrillergross); Crimegrosstot<-sum(Crimegross);Dramagrosstot<-sum(Dramagross);Animationgrosstot<-sum(Animationgross);SFgrosstot<-sum(SFgross);Adventuregrosstot<-sum(Adventuregross)
Actionbudgettot<-sum(Actionbudget);Comedybudgettot<-sum(Comedybudget);Romancebudgettot<-sum(Romancebudget);Thrillerbudgettot<-sum(Thrillerbudget); Crimebudgettot<-sum(Crimebudget);Dramabudgettot<-sum(Dramabudget);Animationbudgettot<-sum(Animationbudget);SFbudgettot<-sum(SFbudget);Adventurebudgettot<-sum(Adventurebudget)

gen<-data.frame(Genre=c("Action","Comedy","Romance","Thriller","Crime","Drama","Animation","Sci.Fi","Adventure"))
gen$cnt<-c(Actioncnttot,Comedycnttot,Romancecnttot,Thrillercnttot, Crimecnttot,Dramacnttot,Animationcnttot,SFcnttot,Adventurecnttot)
gen$totrat<-c(Actionrttot,Comedyrttot,Romancerttot,Thrillerrttot, Crimerttot,Dramarttot,Animationrttot,SFrttot,Adventurerttot)
gen$avgrat<-c(Actionrttot/Actioncnttot,Comedyrttot/Comedycnttot,Romancerttot/Romancecnttot,Thrillerrttot/Thrillercnttot, Crimerttot/Crimecnttot,Dramarttot/Dramacnttot,Animationrttot/Animationcnttot,SFrttot/SFcnttot,Adventurerttot/Adventurecnttot)
gen$grosstot<-c(Actiongrosstot,Comedygrosstot,Romancegrosstot,Thrillergrosstot, Crimegrosstot,Dramagrosstot,Animationgrosstot,SFgrosstot,Adventuregrosstot)
gen$avggross<-c(Actiongrosstot/Actioncnttot,Comedygrosstot/Comedycnttot,Romancegrosstot/Romancecnttot,Thrillergrosstot/Thrillercnttot, Crimegrosstot/Crimecnttot,Dramagrosstot/Dramacnttot,Animationgrosstot/Animationcnttot,SFgrosstot/SFcnttot,Adventuregrosstot/Adventurecnttot)
gen$budgettot<-c(Actionbudgettot,Comedybudgettot,Romancebudgettot,Thrillerbudgettot, Crimebudgettot,Dramabudgettot,Animationbudgettot,SFbudgettot,Adventurebudgettot)
gen$budgetavg <-c(Actionbudgettot/Actioncnttot,Comedybudgettot/Comedycnttot,Romancebudgettot/Romancecnttot,Thrillerbudgettot/Thrillercnttot, Crimebudgettot/Crimecnttot,Dramabudgettot/Dramacnttot,Animationbudgettot/Animationcnttot,SFbudgettot/SFcnttot,Adventurebudgettot/Adventurecnttot)

# save progress
write_csv(gen,path="Final_Project_Documentation/data/gen.csv")

# ggplot(genperyear1, aes(x=Year, y=Count, color=Genre)) +
#   geom_bar(position="dodge",stat="identity")
# scale down everything to millions
gen$avggross = gen$avggross / 1000000
gen$grosstot = gen$grosstot / 1000000
gen$budgetavg = gen$budgetavg / 1000000
#plot genre vs year
ggplot(genperyear1, aes(x=Year, y=Count, color=Genre)) + 
  geom_line(size=0.8)+
  theme_economist()+
  labs(x = "Year", y = "Movie Count", title = "Movie Count VS Genre") +  
  theme(axis.text.x = element_text(size=10, angle=45,face="bold", color="cyan4"),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))

ggplot(gen, aes(x=reorder(Genre,-avggross), y=avggross)) + 
  geom_bar(stat = "identity",  color="red", fill="red", alpha=0.1)+
  labs(x = "Genre", y = "Average BoxOffice (in millions)", title = "Average BoxOffice (in millions) by different Genre") +
  geom_text(aes(label = Genre, y=avggross), position = position_stack(vjust = 0.5), size=3, fontface="bold", color="cyan4")+
  theme_economist()+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))

ggplot(gen, aes(x=reorder(Genre,-budgetavg), y=budgetavg)) + 
  geom_bar(stat = "identity",  color="red", fill="red", alpha=0.1)+
  labs(x = "Genre", y = "Average Budget (in millions)", title = "Average Budget (in millions) by different Genre") +
  geom_text(aes(label = Genre, y=budgetavg), position = position_stack(vjust = 0.5), size=3, fontface="bold", color="cyan4")+
  theme_economist()+
  theme(axis.text.x=element_blank(),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))



# a take at the googleVis package
#plot through that package
# newdf=data.frame(country=gen$Genre, 
#                  GenreCount=gen$cnt, 
#                  TotalRating=gen$totrat,
#                  TotalGross=gen$grosstot)
# 
# Area <- gvisAreaChart(newdf)
# plot(Area)

#ggplot2 package graph

# rating vs genre
ggplot(gen, aes(x=avgrat, y=totrat, color=Genre)) +  
  geom_point(stat="identity", size=3)+ 
  theme_economist()+
  guides(color = FALSE)+
  labs(x = "Average Rating ", y = "Total Number of Ratings", title = "Average Rating Score VS total Number of Ratings") +
  geom_label(aes(label = Genre, y=totrat, vjust="outward", hjust = "outward"), size=3, fontface="bold", color="cyan4")+
  theme(axis.text.x = element_text(size=10, angle=45,face="bold", color="cyan4"),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))+
  xlim(min(gen$avgrat)-0.05,max(gen$avgrat)+0.08)
  
# genre vs box office 
ggplot(gen, aes(x=avggross, y=cnt, color=Genre)) + 
  geom_point(stat="identity", size=3)+ 
  theme_economist()+
  guides(color = FALSE)+
  geom_label(aes(label = Genre, y=cnt, vjust= "inward", hjust = "inward"), size=3, fontface="bold", color="cyan4")+
  labs(x = "Average BoxOffice (in millions) ", y = "Movie Count", title = "BoxOffice (in millions)  VS Genre") +
  theme(axis.text.x = element_text(size=10, angle=45,face="bold", color="cyan4"),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))

# rating vs box office 
ggplot(gen, aes(x=avgrat, y=avggross, color=Genre)) + 
  geom_point(stat="identity", size=3)+ 
  theme_economist()+
  geom_label(aes(label = Genre, y=avggross, vjust="inward", hjust = "inward"), size=3, fontface="bold", color="cyan4")+
  labs(x = "Average Rating", y = "Average BoxOffice (in millions)", title = "BoxOffice (in millions)  VS Rating") +
  guides(color=FALSE)+
  theme(axis.text.x = element_text(size=10, angle=45,face="bold", color="cyan4"),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))

require(stats)
reg<-lm(avggross ~ budgetavg, data = gen)
# line <- data.frame(gen_reg = predict(reg, gen), gen$avggross)
# budget vs box office 

ggplot(gen, aes(x=avggross, y=budgetavg, color=Genre)) + 
  geom_point(stat="identity", size=3)+ 
  theme_economist()+
  geom_label(aes(label = Genre, y=budgetavg, vjust="inward", hjust = "inward"), size=3, fontface="bold", color="cyan4")+
  labs(x = "Average BoxOffice (in millions) ", y = "Average Budget (in millions) ", title = "BoxOffice VS Budget (in millions) ") +
  geom_abline(intercept= reg$coefficients[1], slope = reg$coefficients[2] , color="cyan4")+
  # geom_abline(intercept= 9.210, slope = 0.904 , color="cyan4")+
  guides(color=FALSE)+
  theme(axis.text.x = element_text(size=6, angle=45,face="bold", color="cyan4"),
        axis.title.x = element_text(color="cyan4", size=12, face="bold"),
        axis.title.y = element_text(color="cyan4", size=12, face="bold"))

# ggplot(gen, aes(x=avggross, y=grosstot, color=Genre)) +  
#   geom_point(stat="identity", position="dodge", size = 3) +  
#   theme_economist()+
#   xlim(min(gen$avggross)-20,max(gen$avggross)+20)+
#   xlab("Average BoxOffice")+ ylab("Total BoxOffice")
# 
