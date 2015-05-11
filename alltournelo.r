#elo rating for every tournament every year since 2000#

###Libraries###
library(XML)
library(plyr)
library(dplyr)
library(stringr)
library(scrapeR)
library(stringr)
library(ggplot2)
library(reshape)
library(lubridate)
library(PlayerRatings)

fulltourn <- c("http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=404&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=339&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=891&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=451&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=338&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=301&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=375&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=7161&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=2276&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=402&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=407&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=533&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=499&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=496&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=6932&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=807&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=506&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=495&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=580&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=339&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=891&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=451&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=301&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=338&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=375&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=505&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=2276&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=506&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=402&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=407&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=499&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=496&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=6932&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=807&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=495&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=533&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=404&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=403&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=360&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=717&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=410&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=425&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=773&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=308&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=468&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=1536&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=416&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=6710&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=6120&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=500&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=311&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=440&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=741&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=316&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=315&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=321&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=6718&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=414&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=6116&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=314&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=439&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=319&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=418&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=421&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=422&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=6242&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=341&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=6003&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=6967&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=747&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=329&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=5014&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=438&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=429&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=337&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=328&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=573&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=352&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=580&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=520&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=540&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2014&EventId=560&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=339&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=891&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=451&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=301&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=338&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=375&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=505&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=2276&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=506&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=402&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=407&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=499&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=496&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=807&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=495&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=533&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=404&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=403&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=360&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=717&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=410&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=425&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=773&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=308&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=468&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=1536&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=416&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=6710&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=6120&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=500&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=311&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=440&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=741&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=316&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=315&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=321&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=6718&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=414&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=6116&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=314&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=439&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=319&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=418&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=421&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=422&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=6242&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=341&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=6003&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=747&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=329&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=5014&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=438&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=429&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=337&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=328&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=573&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=352&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=580&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=520&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=540&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=560&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=424&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=568&Draw=ms",
               "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2013&EventId=1720&Draw=ms")

alltourns = NULL
for (j in 1:length(fulltourn))
{
  sum <- 0
  #Find all links on the page of the draws#
  h1 <- getLinks()
  htmlTreeParse(file = fulltourn[j],
                handlers = h1, useInternalNodes = TRUE)
  h1$links()
  
  #Subset the links to only links for the match stats#
  files.1 <- h1$links()[str_detect(h1$links(), "/Share/Match-Facts")]
  #head(files.1)
  #files.1
  
  #Remove the beginning and ending of the javascript links
  files.2 <- substr(files.1, 21, 75)
  #head(files.2)
  
  #Add the beginning of the url to get the full web address
  files.3 <- paste("http://www.atpworldtour.com", files.2, sep="")
  #head(files.3)
  
  #Find beginning date of tournament
  text <- fulltourn[j]
  text <- htmlToText(text)  
  
  
  scores <- gsub("[a-z]", "", text)
  scores <- gsub("\r\n", "", scores)
  #scores <- gsub("[A-Z]", "", scores)
  scores <- gsub(" [0-9] ", "", scores)
  scores <- gsub("[0-3][0-9].[0-1][0-9].[2][0][0-1][0-9]-[0-3][0-9].[0-1][0-9].[2][0][0-1][0-9]", "", scores)
  scores <- gsub(" , ", "", scores)
  scores <- gsub("\\([0-9]\\)", "", scores)
  scores <- gsub("\\([0-9][0-9]\\)", "", scores)
  scores <- str_match_all(scores, "[0-9]-[0-9], [0-9]-[0-9] | [0-9]-[0-9], [0-9]-[0-9], [0-9]-[0-9] | W/O")
  scores <- data.frame(scores)
  names(scores)[1] <- "Score"
  scores$Score <- as.character(scores$Score)
  scores$Score <- gsub("^ ", "", scores$Score)
  
  #want to get a vector of the scores, so can just refer the row later on#
  
  
  
  date <- gsub(".*([0-3][0-9].[0-1][0-9].[2][0][0-1][0-9]-[0-3][0-9].[0-1][0-9].[2][0][0-1][0-9]).*", "\\1", text)
  begdate <- as.Date(substr(date, 1, 10), "%d.%m.%Y")
  enddate <- as.Date(substr(date, 12, 21), "%d.%m.%Y")
  lengthtourn <- enddate-begdate
  
  draw <- gsub(".*Draw:","\\1", text)
  draw <- substr(draw, 1, 5)
  draw <- gsub(" ", "", draw)
  draw <- as.numeric(draw)
  
  if(draw==32){
    nround <- 5
  }
  if(draw==64){
    nround <- 6
  }
  if(draw==128){
    nround <- 7
  }
  
  betwmatch <- floor(lengthtourn/nround)
  
  #Scrape the website for all the match facts for a given tournament#
  fulltable = NULL
  for (i in 1:length(files.3))
  {
    table <- as.data.frame(readHTMLTable(files.3[i]) [[1]])
    players <- table
    players <- t(players)
    players <- as.data.frame(players, row.names=F)
    players$Player <- players$V12[[1]]
    players$Opponent <- players$V12[[2]]
    players$Winner <- players$V10[[1]]
    players <- players[1,]
    players$Player <- as.character(players$Player)
    players$Opponent <- as.character(players$Opponent)
    
    players$Tournament <- players$V4
    
    if(players$Player==players$Winner){
      players$Result <- 1
    } else{
      players$Result <- 0
    }
    
    players$Round <- players$V6
    
    if(draw==32){
      if(players$Round=="R32"){
        players$Date <- begdate
      }
      if(players$Round=="R16"){
        players$Date <- begdate+betwmatch
      }
      if(players$Round=="Q"){
        players$Date <- begdate+(2*betwmatch)
      }
      if(players$Round=="S"){
        players$Date <- begdate+(3*betwmatch)
      }
      if(players$Round=="F"){
        players$Date <- begdate+(4*betwmatch)
      }
    }
    
    if(draw==64){
      if(players$Round=="R64"){
        players$Date <- begdate
      }
      if(players$Round=="R32"){
        players$Date <- begdate+betwmatch
      }
      if(players$Round=="R16"){
        players$Date <- begdate+(2*betwmatch)
      }
      if(players$Round=="Q"){
        players$Date <- begdate+(3*betwmatch)
      }
      if(players$Round=="S"){
        players$Date <- begdate+(4*betwmatch)
      }
      if(players$Round=="F"){
        players$Date <- begdate+(5*betwmatch)
      }
    }
    
    if(draw==128){
      if(players$Round=="R128"){
        players$Date <- begdate
      }
      if(players$Round=="R64"){
        players$Date <- begdate+betwmatch
      }
      if(players$Round=="R32"){
        players$Date <- begdate+(2*betwmatch)
      }
      if(players$Round=="R16"){
        players$Date <- begdate+(3*betwmatch)
      }
      if(players$Round=="Q"){
        players$Date <- begdate+(4*betwmatch)
      }
      if(players$Round=="S"){
        players$Date <- begdate+(5*betwmatch)
      }
      if(players$Round=="F"){
        players$Date <- begdate+(6*betwmatch)
      }
    }
    
    players <- players[c(50:56)]
    
    #Add in score
    if(i == 1){
      if(players$Opponent!="N/A Bye"){
        players$Score <- scores$Score[i]
      }
      if(players$Opponent=="N/A Bye"){
        players$Score <- NA
      }
    }
    
    if(players$Opponent=="N/A Bye"){
      sum <- sum+1
    }
    
    if(i != 1){
      if(players$Opponent!="N/A Bye"){
        players$Score <- scores$Score[i-sum]
      }
      if(players$Opponent=="N/A Bye"){
        players$Score <- NA
      }
    }
    
    str_count(players$Score, ',')
    
    
    if(i == 1){
      fulltable <- players
    } else {
      fulltable = rbind(fulltable, players)
    }
  }
  if(j == 1){
    alltourns = fulltable
  } else {
    alltourns = rbind(alltourns, fulltable)
  }
}

alltourns3 <- alltourns
startdateallt <- as.Date("2012-12-30", "%Y-%m-%d")

#Add tournaments to the alltourns3 database
alltourns$Day <- alltourns$Date - startdateallt
alltourns$Day <- as.numeric(alltourns$Day)

alltourns <- alltourns[alltourns$Opponent!="N/A Bye",]

alltourns <- alltourns[c(8,1,2,5)]

alltourns3 <- rbind(alltourns, alltourns3)

#From original alltourns3 database
alltourns3$Day <- alltourns3$Date - startdateallt
alltourns3$Day <- as.numeric(alltourns3$Day)

alltourns3 <- alltourns3[alltourns3$Opponent!="N/A Bye",]

alltourns3 <- alltourns3[c(8,1,2,5)]

alltourns3elo <- elo(alltourns3, status=NULL, gamma=0, sort=T)

#Using a lag of 1 year
lagrange <- as.Date("2014-03-01", "%Y-%m-%d")
lagrange <- lagrange - startdateallt
lag1yearrange <- alltourns3[(alltourns3$Day>=0)&(alltourns3$Day<=lagrange),]
lag1year <- elo(lag1yearrange, status=NULL, gamma=0, sort=T)
lag1year <- ldply(lag1year, data.frame)
lag1year <- lag1year[c(2,3)]
lag1year <- head(lag1year,-3)

lastyear <- alltourns3[alltourns3$Day>lagrange,]
lastyearelo <- elo(lastyear, status=lag1year, gamma=0)
lastyearelon <- elo(lastyear, status=NULL, gamma=0, sort=T)

#using glicko rating
allglicko <- glicko(alltourns3, gamma=0, status=NULL, sort=T)
allglicko <- glicko(lastyear, gamma=0, status=NULL, sort=T)



#Player ratings for current year based on lag of previous year, which is based on lag of previous year
#start2013 <- as.Date("2013-01-01", "%Y-%m-%d")
end2013 <- as.Date("2013-12-31", "%Y-%m-%d")
end2013 <- end2013 - startdateallt
matches2013 <- alltourns3[(alltourns3$Day>=0)&(alltourns3$Day<=end2013),]

start2014 <- as.Date("2014-01-01", "%Y-%m-%d")
start2014 <- start2014 - startdateallt
end2014 <- as.Date("2014-12-31", "%Y-%m-%d")
end2014 <- end2014 - startdateallt
matches2014 <- alltourns3[(alltourns3$Day>=start2014)&(alltourns3$Day<=end2014),]

start2015 <- as.Date("2015-01-01", "%Y-%m-%d")
start2015 <- start2015 - startdateallt
matches2015 <- alltourns3[alltourns3$Day>=start2015,]

ratings2013 <- elo(matches2013, status=NULL, gamma=0, sort=T)
ratings2013 <- ldply(ratings2013, data.frame)
ratings2013 <- ratings2013[c(2,3)]
ratings2013 <- head(ratings2013,-3)

ratings2014 <- elo(matches2014, status=ratings2013, gamma=0, sort=T)
ratings2014 <- ldply(ratings2014, data.frame)
ratings2014 <- ratings2014[c(2,3)]
ratings2014 <- head(ratings2014,-3)

ratings2015 <- elo(matches2015, status=ratings2014, gamma=0, sort=T)

#Player ratings for current year based on lag of previous year, which is based on lag of previous year
ratings2014nolag <- elo(matches2014, status=NULL, gamma=0, sort=T)
ratings2014nolag <- ldply(ratings2014nolag, data.frame)
ratings2014nolag <- ratings2014nolag[c(2,3)]
ratings2014nolag <- head(ratings2014nolag,-3)

ratings2015lag1 <- elo(matches2015, status=ratings2014nolag, gamma=0, sort=T)
