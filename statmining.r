#Code#

#Libraries
library(XML)
library(plyr)
library(dplyr)
library(stringr)
library(scrapeR)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(lubridate)
library(PlayerRatings)
library(RCurl)
library(randomForest)
library(xtable)
library(MASS)


#Uploading the full tournament database
tournstats10.15 <- read.csv("FullStats.csv")
tournstats10.15$Date <- as.Date(tournstats10.15$Date, "%Y-%m-%d")
tournstats10.15$Player <- as.character(tournstats10.15$Player)
tournstats10.15$Opponent <- as.character(tournstats10.15$Opponent)
tournstats10.15$Result <- as.character(tournstats10.15$Result)


#Full code for creating the tournament database
###Get links function to get the links of the match facts for each of the matches###
getLinks = function() { 
  links = character() 
  list(a = function(node, ...) { 
    links <<- c(links, xmlGetAttr(node, "href"))
    node 
  }, 
  links = function()links)
}
###

###Tournament Websites###
tournwebsites <- c("http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=403&Draw=ms",
                   "http://www.atpworldtour.com/Share/Event-Draws.aspx?Year=2015&EventId=404&Draw=ms",
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
###

###Table of all match stats###
tournstats10.15 = NULL
length(tournwebsites)
for (j in 1:length(tournwebsites))
{
  #Find all links on the page of the draws#
  h1 <- getLinks()
  htmlTreeParse(file = tournwebsites[j],
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
  
  #Pull out text from website
  text <- tournwebsites[j]
  text <- htmlToText(text)  
  
  #Obtain the beginning and end dates of the tournament  
  date <- gsub(".*([0-3][0-9].[0-1][0-9].[2][0][0-1][0-9]-[0-3][0-9].[0-1][0-9].[2][0][0-1][0-9]).*", "\\1", text)
  begdate <- as.Date(substr(date, 1, 10), "%d.%m.%Y")
  enddate <- as.Date(substr(date, 12, 21), "%d.%m.%Y")
  
  #Determine the length of time between matches
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
  eachtourn = NULL
  for (i in 1:length(files.3))
  {
    table <- as.data.frame(readHTMLTable(files.3[i]) [[1]])
    matchtable <- table
    
    matchtable <- t(matchtable)
    matchtable <- as.data.frame(matchtable)
    matchtable <- matchtable[-c(1,2,3,4,5,6,7,8,10,11,13,15,16,17,19,21,23,25,27,29,31,32,33,
                                35,37,39,41,42,43,45,47,49)]
    names(matchtable)[1] <- "Result"
    names(matchtable)[2] <- "Player"
    names(matchtable)[3] <- "Nationality_1"
    names(matchtable)[4] <- "Aces"
    names(matchtable)[5] <- "Double_Faults"
    names(matchtable)[6] <- "First_Serve"
    names(matchtable)[7] <- "First_Serve_Points_Won"
    names(matchtable)[8] <- "Second_Serve_Points_Won"
    names(matchtable)[9] <- "Break_Points_Saved"
    names(matchtable)[10] <- "Service_Games_Played"
    names(matchtable)[11] <- "First_Serve_Return_Points_Won"
    names(matchtable)[12] <- "Second_Serve_Return_Points_Won"
    names(matchtable)[13] <- "Break_Points_Converted"
    names(matchtable)[14] <- "Return_Games_Played"
    names(matchtable)[15] <- "Total_Service_Points_Won"
    names(matchtable)[16] <- "Total_Return_Points_Won"
    names(matchtable)[17] <- "Total_Points_Won"
    
    year <- substr(tournwebsites, 57, nchar(tournwebsites))
    year <- substr(year, 1, 4)
    year <- as.numeric(year)
    matchtable$Year <- year[j]
    
    matchtable$Tournament <- table$V1[4]
    matchtable$Round <- table$V1[6]
    matchtable$Time <- table$V1[8]
    
    matchtable$Winner <- table$V1[10]
    
    matchtable$Result <- as.character(matchtable$Result)
    
    if(matchtable$Result[[1]] == "Winner"){
      matchtable$Result[[1]] <- 1
      matchtable$Result[[2]] <- 0
    } else{
      matchtable$Result[[1]] <- 0
      matchtable$Result[[2]] <- 1
    }
    
    matchtable$MatchID <- i
    
    matchtable$Opponent <- matchtable$Player[[2]]
    matchtable$Opponent[[2]] <- matchtable$Player[[1]]
    
    matchtable$Nationality_2 <- matchtable$Nationality_1[[2]]
    matchtable$Nationality_2[[2]] <- matchtable$Nationality_1[[1]]
    
    #matchtable <- matchtable[c(22,18,19,20,21,1,2,3,23,24,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
    if(draw==32){
      if(matchtable$Round[[1]]=="R32"){
        matchtable$Date <- begdate
      }
      if(matchtable$Round[[1]]=="R16"){
        matchtable$Date <- begdate+betwmatch
      }
      if(matchtable$Round[[1]]=="Q"){
        matchtable$Date <- begdate+(2*betwmatch)
      }
      if(matchtable$Round[[1]]=="S"){
        matchtable$Date <- begdate+(3*betwmatch)
      }
      if(matchtable$Round[[1]]=="F"){
        matchtable$Date <- begdate+(4*betwmatch)
      }
    }
    
    if(draw==64){
      if(matchtable$Round[[1]]=="R64"){
        matchtable$Date <- begdate
      }
      if(matchtable$Round[[1]]=="R32"){
        matchtable$Date <- begdate+betwmatch
      }
      if(matchtable$Round[[1]]=="R16"){
        matchtable$Date <- begdate+(2*betwmatch)
      }
      if(matchtable$Round[[1]]=="Q"){
        matchtable$Date <- begdate+(3*betwmatch)
      }
      if(matchtable$Round[[1]]=="S"){
        matchtable$Date <- begdate+(4*betwmatch)
      }
      if(matchtable$Round[[1]]=="F"){
        matchtable$Date <- begdate+(5*betwmatch)
      }
    }
    
    if(draw==128){
      if(matchtable$Round[[1]]=="R128"){
        matchtable$Date <- begdate
      }
      if(matchtable$Round[[1]]=="R64"){
        matchtable$Date <- begdate+betwmatch
      }
      if(matchtable$Round[[1]]=="R32"){
        matchtable$Date <- begdate+(2*betwmatch)
      }
      if(matchtable$Round[[1]]=="R16"){
        matchtable$Date <- begdate+(3*betwmatch)
      }
      if(matchtable$Round[[1]]=="Q"){
        matchtable$Date <- begdate+(4*betwmatch)
      }
      if(matchtable$Round[[1]]=="S"){
        matchtable$Date <- begdate+(5*betwmatch)
      }
      if(matchtable$Round[[1]]=="F"){
        matchtable$Date <- begdate+(6*betwmatch)
      }
    }
    
    matchtable$First_Serve_Total <- gsub(".*/", '', matchtable$First_Serve)
    matchtable$First_Serve_Total <- gsub(")", '', matchtable$First_Serve_Total)
    matchtable$First_Serve_Per <- gsub("%.*", '', matchtable$First_Serve)
    
    matchtable$First_Serve_Points_Won_Total <- gsub(".*/", '', matchtable$First_Serve_Points_Won)
    matchtable$First_Serve_Points_Won_Total <- gsub(")", '', matchtable$First_Serve_Points_Won_Total)
    matchtable$First_Serve_Points_Won_Per <- gsub("%.*", '', matchtable$First_Serve_Points_Won)
    
    matchtable$Second_Serve_Points_Won_Total <- gsub(".*/", '', matchtable$Second_Serve_Points_Won)
    matchtable$Second_Serve_Points_Won_Total <- gsub(")", '', matchtable$Second_Serve_Points_Won_Total)
    matchtable$Second_Serve_Points_Won_Per <- gsub("%.*", '', matchtable$Second_Serve_Points_Won)
    
    matchtable$Break_Points_Saved_Total <- gsub(".*/", '', matchtable$Break_Points_Saved)
    matchtable$Break_Points_Saved_Total <- gsub(")", '', matchtable$Break_Points_Saved_Total)
    matchtable$Break_Points_Saved_Per <- gsub("%.*", '', matchtable$Break_Points_Saved)
    
    matchtable$First_Serve_Return_Points_Won_Total <- gsub(".*/", '', matchtable$First_Serve_Return_Points_Won)
    matchtable$First_Serve_Return_Points_Won_Total <- gsub(")", '', matchtable$First_Serve_Return_Points_Won_Total)
    matchtable$First_Serve_Return_Points_Won_Per <- gsub("%.*", '', matchtable$First_Serve_Return_Points_Won)
    
    matchtable$Second_Serve_Return_Points_Won_Total <- gsub(".*/", '', matchtable$Second_Serve_Return_Points_Won)
    matchtable$Second_Serve_Return_Points_Won_Total <- gsub(")", '', matchtable$Second_Serve_Return_Points_Won_Total)
    matchtable$Second_Serve_Return_Points_Won_Per <- gsub("%.*", '', matchtable$Second_Serve_Return_Points_Won)
    
    matchtable$Break_Points_Converted_Total <- gsub(".*/", '', matchtable$Break_Points_Converted)
    matchtable$Break_Points_Converted_Total <- gsub(")", '', matchtable$Break_Points_Converted_Total)
    matchtable$Break_Points_Converted_Per <- gsub("%.*", '', matchtable$Break_Points_Converted)
    
    matchtable$Total_Service_Points_Won_Total <- gsub(".*/", '', matchtable$Total_Service_Points_Won)
    matchtable$Total_Service_Points_Won_Total <- gsub(")", '', matchtable$Total_Service_Points_Won_Total)
    matchtable$Total_Service_Points_Won_Per <- gsub("%.*", '', matchtable$Total_Service_Points_Won)
    
    matchtable$Total_Return_Points_Won_Total <- gsub(".*/", '', matchtable$Total_Return_Points_Won)
    matchtable$Total_Return_Points_Won_Total <- gsub(")", '', matchtable$Total_Return_Points_Won_Total)
    matchtable$Total_Return_Points_Won_Per <- gsub("%.*", '', matchtable$Total_Return_Points_Won)
    
    matchtable$Total_Points_Won_Total <- gsub(".*/", '', matchtable$Total_Points_Won)
    matchtable$Total_Points_Won_Total <- gsub(")", '', matchtable$Total_Points_Won_Total)
    matchtable$Total_Points_Won_Per <- gsub("%.*", '', matchtable$Total_Points_Won)
    
    matchtable$Aces <- as.numeric(as.character(matchtable$Aces))
    matchtable$Double_Faults <- as.numeric(as.character(matchtable$Double_Faults))
    matchtable$Service_Games_Played <- as.numeric(as.character(matchtable$Service_Games_Played))
    matchtable$Return_Games_Played <- as.numeric(as.character(matchtable$Return_Games_Played))
    matchtable$First_Serve_Total <- as.numeric(matchtable$First_Serve_Total)
    matchtable$First_Serve_Per <- as.numeric(matchtable$First_Serve_Per)
    matchtable$First_Serve_Points_Won_Total <- as.numeric(matchtable$First_Serve_Points_Won_Total)
    matchtable$First_Serve_Points_Won_Per <- as.numeric(matchtable$First_Serve_Points_Won_Per)
    matchtable$Second_Serve_Points_Won_Total <- as.numeric(matchtable$Second_Serve_Points_Won_Total)
    matchtable$Second_Serve_Points_Won_Per <- as.numeric(matchtable$Second_Serve_Points_Won_Per)
    matchtable$Break_Points_Saved_Total <- as.numeric(matchtable$Break_Points_Saved_Total)
    matchtable$Break_Points_Saved_Per <- as.numeric(matchtable$Break_Points_Saved_Per)
    matchtable$First_Serve_Return_Points_Won_Total <- as.numeric(matchtable$First_Serve_Return_Points_Won_Total)
    matchtable$First_Serve_Return_Points_Won_Per <- as.numeric(matchtable$First_Serve_Return_Points_Won_Per)
    matchtable$Second_Serve_Return_Points_Won_Total <- as.numeric(matchtable$Second_Serve_Return_Points_Won_Total)
    matchtable$Second_Serve_Return_Points_Won_Per <- as.numeric(matchtable$Second_Serve_Return_Points_Won_Per)
    matchtable$Break_Points_Converted_Total <- as.numeric(matchtable$Break_Points_Converted_Total)
    matchtable$Break_Points_Converted_Per <- as.numeric(matchtable$Break_Points_Converted_Per)
    matchtable$Total_Service_Points_Won_Total <- as.numeric(matchtable$Total_Service_Points_Won_Total)
    matchtable$Total_Service_Points_Won_Per <- as.numeric(matchtable$Total_Service_Points_Won_Per)
    matchtable$Total_Return_Points_Won_Total <- as.numeric(matchtable$Total_Return_Points_Won_Total)
    matchtable$Total_Return_Points_Won_Per <- as.numeric(matchtable$Total_Return_Points_Won_Per)
    matchtable$Total_Points_Won_Total <- as.numeric(matchtable$Total_Points_Won_Total)
    matchtable$Total_Points_Won_Per <- as.numeric(matchtable$Total_Points_Won_Per)
    
    matchtable$Aces_Diff <- matchtable$Aces[[1]] - matchtable$Aces[[2]]
    matchtable$Aces_Diff[[2]] <- matchtable$Aces[[2]] - matchtable$Aces[[1]]
    
    matchtable$Double_Faults_Diff <- matchtable$Double_Faults[[1]] - matchtable$Double_Faults[[2]]
    matchtable$Double_Faults_Diff[[2]] <- matchtable$Double_Faults[[2]] - matchtable$Double_Faults[[1]]
    
    matchtable$First_Serve_Per_Diff <- matchtable$First_Serve_Per[[1]] - matchtable$First_Serve_Per[[2]]
    matchtable$First_Serve_Per_Diff[[2]] <- matchtable$First_Serve_Per[[2]] - matchtable$First_Serve_Per[[1]]
    
    matchtable$First_Serve_Points_Won_Per_Diff <- matchtable$First_Serve_Points_Won_Per[[1]] - matchtable$First_Serve_Points_Won_Per[[2]]
    matchtable$First_Serve_Points_Won_Per_Diff[[2]] <- matchtable$First_Serve_Points_Won_Per[[2]] - matchtable$First_Serve_Points_Won_Per[[1]]
    
    matchtable$Second_Serve_Points_Won_Per_Diff <- matchtable$Second_Serve_Points_Won_Per[[1]] - matchtable$Second_Serve_Points_Won_Per[[2]]
    matchtable$Second_Serve_Points_Won_Per_Diff[[2]] <- matchtable$Second_Serve_Points_Won_Per[[2]] - matchtable$Second_Serve_Points_Won_Per[[1]]
    
    matchtable$Break_Points_Saved_Per_Diff <- matchtable$Break_Points_Saved_Per[[1]] - matchtable$Break_Points_Saved_Per[[2]]
    matchtable$Break_Points_Saved_Per_Diff[[2]] <- matchtable$Break_Points_Saved_Per[[2]] - matchtable$Break_Points_Saved_Per[[1]]
    
    matchtable$First_Serve_Return_Points_Won_Per_Diff <- matchtable$First_Serve_Return_Points_Won_Per[[1]] - matchtable$First_Serve_Return_Points_Won_Per[[2]]
    matchtable$First_Serve_Return_Points_Won_Per_Diff[[2]] <- matchtable$First_Serve_Return_Points_Won_Per[[2]] - matchtable$First_Serve_Return_Points_Won_Per[[1]]
    
    matchtable$Second_Serve_Return_Points_Won_Per_Diff <- matchtable$Second_Serve_Return_Points_Won_Per[[1]] - matchtable$Second_Serve_Return_Points_Won_Per[[2]]
    matchtable$Second_Serve_Return_Points_Won_Per_Diff[[2]] <- matchtable$Second_Serve_Return_Points_Won_Per[[2]] - matchtable$Second_Serve_Return_Points_Won_Per[[1]]
    
    matchtable$Break_Points_Converted_Per_Diff <- matchtable$Break_Points_Converted_Per[[1]] - matchtable$Break_Points_Converted_Per[[2]]
    matchtable$Break_Points_Converted_Per_Diff[[2]] <- matchtable$Break_Points_Converted_Per[[2]] - matchtable$Break_Points_Converted_Per[[1]]
    
    matchtable$Total_Service_Points_Won_Per_Diff <- matchtable$Total_Service_Points_Won_Per[[1]] - matchtable$Total_Service_Points_Won_Per[[2]] 
    matchtable$Total_Service_Points_Won_Per_Diff[[2]] <- matchtable$Total_Service_Points_Won_Per[[2]] - matchtable$Total_Service_Points_Won_Per[[1]]
    
    matchtable$Total_Return_Points_Won_Per_Diff <- matchtable$Total_Return_Points_Won_Per[[1]] - matchtable$Total_Return_Points_Won_Per[[2]] 
    matchtable$Total_Return_Points_Won_Per_Diff[[2]] <- matchtable$Total_Return_Points_Won_Per[[2]] - matchtable$Total_Return_Points_Won_Per[[1]]
    
    matchtable$Total_Points_Won_Per_Diff <- matchtable$Total_Points_Won_Per[[1]] - matchtable$Total_Points_Won_Per[[2]] 
    matchtable$Total_Points_Won_Per_Diff[[2]] <- matchtable$Total_Points_Won_Per[[2]] - matchtable$Total_Points_Won_Per[[1]]
    
    #Delete duplicate statistics
    matchtable <- matchtable[-c(6,7,8,9,11,12,13,15,16,17)]
    
    #Reorder variables in table
    matchtable <- matchtable[c(13,8,16,9,10,11,2,3,14,15,12,1,4,37,5,38,17,18,39,19,20,40,
                               21,22,41,23,24,42,6,25,26,43,27,28,44,29,30,45,7,31,32,46,
                               33,34,47,35,36,48)]
    
    if(matchtable$Player[[2]] == "N/A Bye"){
      matchtable$Player[[2]] <- NA
      matchtable$Nationality_1[[2]] <- NA
      matchtable$Opponent[[1]] <- NA
      matchtable$Nationality_2[[1]] <- NA
      matchtable$Aces <- NA
      matchtable$Aces_Diff <- NA
      matchtable$Double_Faults <- NA
      matchtable$Double_Faults_Diff <- NA
      matchtable$First_Serve_Total <- NA
      matchtable$First_Serve_Per <- NA
      matchtable$First_Serve_Per_Diff <- NA
      matchtable$First_Serve_Points_Won_Total <- NA
      matchtable$First_Serve_Points_Won_Per <- NA
      matchtable$First_Serve_Points_Won_Per_Diff <- NA
      matchtable$Second_Serve_Points_Won_Total <- NA
      matchtable$Second_Serve_Points_Won_Per <- NA
      matchtable$Second_Serve_Points_Won_Per_Diff <- NA
      matchtable$Break_Points_Saved_Total <- NA
      matchtable$Break_Points_Saved_Per <- NA
      matchtable$Break_Points_Saved_Per_Diff <- NA
      matchtable$Service_Games_Played <- NA
      matchtable$First_Serve_Return_Points_Won_Total <- NA
      matchtable$First_Serve_Return_Points_Won_Per <- NA
      matchtable$First_Serve_Return_Points_Won_Per_Diff <- NA
      matchtable$Second_Serve_Return_Points_Won_Total <- NA
      matchtable$Second_Serve_Return_Points_Won_Per <- NA
      matchtable$Second_Serve_Return_Points_Won_Per_Diff <- NA
      matchtable$Break_Points_Converted_Total <- NA
      matchtable$Break_Points_Converted_Per <- NA
      matchtable$Break_Points_Converted_Per_Diff <- NA
      matchtable$Return_Games_Played <- NA
      matchtable$Total_Service_Points_Won_Total <- NA
      matchtable$Total_Service_Points_Won_Per <- NA
      matchtable$Total_Service_Points_Won_Per_Diff <- NA
      matchtable$Total_Return_Points_Won_Total <- NA
      matchtable$Total_Return_Points_Won_Per <- NA
      matchtable$Total_Return_Points_Won_Per_Diff <- NA
      matchtable$Total_Points_Won_Total <- NA
      matchtable$Total_Points_Won_Per <- NA
      matchtable$Total_Points_Won_Per_Diff <- NA
    }
    
    
    
    #matchtable <- matchtable[c(1,2,47,3,4:46)]
    
    if(i == 1){
      eachtourn <- matchtable
    } else {
      eachtourn <- rbind(eachtourn, matchtable)
    }
  }
  if(j == 1){
    tournstats10.15 <- eachtourn
  } else {
    tournstats10.15 <- rbind(tournstats10.15, eachtourn)
  }
}

#Edit time outside the loop
tournstats10.15$Time <- as.character(tournstats10.15$Time)
tournstats10.15$Time <- as.numeric(substr(tournstats10.15$Time, 1, nchar(tournstats10.15$Time)-8))

#Change player to character
tournstats10.15$Player <- as.character(tournstats10.15$Player)
tournstats10.15$Opponent <- as.character(tournstats10.15$Opponent)


#Player Ratings
elodata <- tournstats10.15[c(3,7,9,12)]
startdate <- min(elodata$Date)
elodata$Date <- elodata$Date - startdate
names(elodata)[1] <- "Day"
elodata$Day <- as.numeric(elodata$Day)
elodata$Result <- as.numeric(elodata$Result)

#Remove NA and duplicate rows#
elodata <- elodata[(seq(1, nrow(elodata), by=2)),]
elodata <- na.omit(elodata)

elo <- elo(elodata, status=NULL, gamma=0, sort=T)
elo <- ldply(elo, data.frame)
elo <- elo[c(2,3)]
elo <- head(elo,-3)


#Find average statistics for each player over all the tournaments
#Subsetting the elo ratings for only players who have played more than 10 games
elo <- elo(elodata, status=NULL, gamma=0, sort=T)
elo <- ldply(elo, data.frame)
elo <- elo[elo$Games>=10,]
elo <- elo[c(2,3)]
elo <- head(elo,-3)

#Top 20
top20 <- elo[c(1:20),]
top20$Level <- "top 20"

#81-100 in rankings
ratings81.100 <- elo[c(81:100),]
ratings81.100$Level <- "81 - 100"

#Combine top 10 and 81-100
cgroups <- rbind(top20, ratings81.100)

#Merge these ratings to the match statistics
subset <- na.omit(tournstats10.15)
fulltable <- merge(subset, cgroups, by="Player")

#Remove not needed variables
forestdata <- fulltable[-c(2:12,14,16,19,22,25,28,29,32,35,38,39,42,45,48,49)]

#Calculate average stats per player
forestdata <- ddply(forestdata, c("Player","Level"), summarise,
                    Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                    First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                    First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                    Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                    Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                    First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                    Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                    Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))


#Create boxplots for comparison
forestdata$Level <- as.factor(forestdata$Level)
boxplots <- forestdata
boxplots$Level <- factor(boxplots$Level,levels(boxplots$Level)[c(2,1)])
boxplots$Level <- revalue(boxplots$Level, c("top 20"="1-20", "81 - 100"="81-100"))

#First Serve Percentage#
a <- qplot(data=boxplots, x=Level, y=First_Serve_Per, geom="boxplot", xlab="Rating", ylab="Percentage") +
  ggtitle("First Serve")

#Second Serve Points Won Percentage
b <- qplot(data=boxplots, x=Level, y=Second_Serve_Points_Won_Per, geom="boxplot", xlab="Rating",
           ylab="Percentage") + ggtitle("Second Serve Points Won")

#Second Serve Return Points Won Percentage
c <- qplot(data=boxplots, x=Level, y=Second_Serve_Return_Points_Won_Per, geom="boxplot", xlab="Rating",
           ylab="Percentage") + ggtitle("Second Serve Return Points Won")

#First Serve Points Won Percentage
d <- qplot(data=boxplots, x=Level, y=First_Serve_Points_Won_Per, geom="boxplot", xlab="Rating",
           ylab="Percentage") + ggtitle("First Serve Points Won")

#First Serve Return Points Won Percentage
e <- qplot(data=boxplots, x=Level, y=First_Serve_Return_Points_Won_Per, geom="boxplot", xlab="Rating",
           ylab="Percentage") + ggtitle("First Serve Return Points Won")

#Break Points Saved Percentage
f <- qplot(data=boxplots, x=Level, y=Break_Points_Saved_Per, geom="boxplot", xlab="Rating",
           ylab="Percentage") + ggtitle("Break Points Saved")

#Break Points Converted Percentage
g <- qplot(data=boxplots, x=Level, y=Break_Points_Converted_Per, geom="boxplot", xlab="Rating",
           ylab="Percentage") + ggtitle("Break Points Converted")

top20.100 <- grid.arrange(d,e,b,c,a,f,g, nrow=4, main="Player Comparison")


#Regression Random Forest
elo <- elo(elodata, status=NULL, gamma=0, sort=T)
elo <- ldply(elo, data.frame)
elo <- elo[elo$Games>=10,]
elo <- elo[c(2,3)]
elo <- head(elo,-3)

#Merge these ratings to the match statistics
subset <- na.omit(tournstats10.15)
fulltable <- merge(subset, elo, by="Player")

#Remove not needed variables
forestdata <- fulltable[-c(2:12,14,16,19,22,25,28,29,32,35,38,39,42,45,48)]

#Calculate average stats per player
forestdata <- ddply(forestdata, "Player", summarise,
                    Rating=mean(Rating), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                    First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                    First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                    Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                    Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                    First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                    Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                    Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))

rf4 <- randomForest(Rating~., data=forestdata[,-1], importance=T)

#Plot importance variables
m <- qplot(1:9, rf4$importance[order(rf4$importance[,1], decreasing=T),1], xlab="Variable",ylab="%IncMSE")
n <- qplot(1:9, rf4$importance[order(rf4$importance[,2], decreasing=T),2], xlab="Variable",ylab="IncNodePurity")
grid.arrange(m,n, ncol=2, main="Importance")


#Classification random forest with 2 groups
elo <- elo(elodata, status=NULL, gamma=0, sort=T)
elo <- ldply(elo, data.frame)
elo <- elo[elo$Games>=10,]
elo <- elo[c(2,3)]
elo <- head(elo,-3)

#Top 20
top20 <- elo[c(1:20),]
top20$Level <- "top 20"

#81-100 in rankings
ratings81.100 <- elo[c(81:100),]
ratings81.100$Level <- "81 - 100"

#Combine top 10 and 81-100
cgroups <- rbind(top20, ratings81.100)

#Merge these ratings to the match statistics
subset <- na.omit(tournstats10.15)
fulltable <- merge(subset, cgroups, by="Player")

#Remove not needed variables
forestdata <- fulltable[-c(2:12,14,16,19,22,25,28,29,32,35,38,39,42,45,48,49)]

#Calculate average stats per player
forestdata <- ddply(forestdata, c("Player","Level"), summarise,
                    Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                    First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                    First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                    Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                    Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                    First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                    Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                    Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))

forestdata$Player <- as.factor(forestdata$Player)
rf3 <- randomForest(as.factor(Level)~., data=forestdata[,-1], importance=T)



#Classification random forest with 4 groups
elo <- elo(elodata, status=NULL, gamma=0, sort=T)
elo <- ldply(elo, data.frame)
elo <- elo[elo$Games>=10,]
elo <- elo[c(2,3)]
elo <- head(elo,-3)

#Top 20
top20 <- elo[c(1:20),]
top20$Level <- "top 20"

#21-50
ratings21.50 <- elo[c(21:50),]
ratings21.50$Level <- "21 - 50"

#51-80
ratings51.80 <- elo[c(51:80),]
ratings51.80$Level <- "51 - 80"

#81-100 in rankings
ratings81.100 <- elo[c(81:100),]
ratings81.100$Level <- "81 - 100"

#Combine groups of players
cgroups <- rbind(top20, ratings21.50, ratings51.80, ratings81.100)

#Merge these ratings to the match statistics
subset <- na.omit(tournstats10.15)
fulltable <- merge(subset, cgroups, by="Player")

#Remove not needed variables
forestdata <- fulltable[-c(2:12,14,16,19,22,25,28,29,32,35,38,39,42,45,48,49)]

#Calculate average stats per player
forestdata <- ddply(forestdata, c("Player","Level"), summarise,
                    Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                    First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                    First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                    Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                    Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                    First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                    Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                    Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))

forestdata$Player <- as.factor(forestdata$Player)
rf5 <- randomForest(as.factor(Level)~., data=forestdata[,-1], importance=T)



#Linear Discriminant Analysis
top20 <- elo[c(1:20),]
top20$Level <- "top 20"

#81-100 in rankings
ratings81.100 <- elo[c(81:100),]
ratings81.100$Level <- "81 - 100"

#Combine top 10 and 81-100
cgroups <- rbind(top20, ratings81.100)

#Merge these ratings to the match statistics
subset <- na.omit(tournstats10.15)
fulltable <- merge(subset, cgroups, by="Player")

#Remove not needed variables
forestdata <- fulltable[-c(2:12,14,16,19,22,25,28,29,32,35,38,39,42,45,48,49)]

#Calculate average stats per player
forestdata <- ddply(forestdata, c("Player","Level"), summarise,
                    Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                    First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                    First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                    Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                    Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                    First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                    Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                    Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))

forestdata$Player <- as.factor(forestdata$Player)

lda <- lda(as.factor(Level)~., data=forestdata[,c(-1,-3,-4,-5,-8,-10,-11)])
plot(lda, dimen=1, type="both")
title(main="LD1 by Group") 




#Monthly Rating
monthlyelo <- tournstats10.15[c(3,7,9,12)]
startdate <- min(monthlyelo$Date)
monthlyelo$Day <- monthlyelo$Date - startdate
monthlyelo$Day <- as.numeric(monthlyelo$Day)
monthlyelo$Result <- as.numeric(monthlyelo$Result)

#Remove NA and duplicate rows#
monthlyelo <- monthlyelo[(seq(1, nrow(monthlyelo), by=2)),]
monthlyelo <- na.omit(monthlyelo)
monthlyelo <- monthlyelo[c(5,2,3,4,1)]

jan13 <- monthlyelo[monthlyelo$Date<"2013-02-01",]
jan13elo <- elo(jan13[,-5], status=NULL, gamma=0, sort=T)
jan13elo <- ldply(jan13elo, data.frame)
jan13elo <- jan13elo[c(2,3)]
jan13elo <- head(jan13elo,-3)
jan13elo$Month <- "2013-01-01"
jan13elo$Month <- as.Date(jan13elo$Month, "%Y-%m-%d")

feb13 <- monthlyelo[monthlyelo$Date<"2013-03-01" & monthlyelo$Date>="2013-02-01",]
feb13elo <- elo(feb13[,-5], status=jan13elo, gamma=0, sort=T)
feb13elo <- ldply(feb13elo, data.frame)
feb13elo <- feb13elo[c(2,3)]
feb13elo <- head(feb13elo,-3)
feb13elo$Month <- "2013-02-01"
feb13elo$Month <- as.Date(feb13elo$Month, "%Y-%m-%d")

mar13 <- monthlyelo[monthlyelo$Date<"2013-04-01" & monthlyelo$Date>="2013-03-01",]
mar13elo <- elo(mar13[,-5], status=feb13elo, gamma=0, sort=T)
mar13elo <- ldply(mar13elo, data.frame)
mar13elo <- mar13elo[c(2,3)]
mar13elo <- head(mar13elo,-3)
mar13elo$Month <- "2013-03-01"
mar13elo$Month <- as.Date(mar13elo$Month, "%Y-%m-%d")

apr13 <- monthlyelo[monthlyelo$Date<"2013-05-01" & monthlyelo$Date>="2013-04-01",]
apr13elo <- elo(apr13[,-5], status=mar13elo, gamma=0, sort=T)
apr13elo <- ldply(apr13elo, data.frame)
apr13elo <- apr13elo[c(2,3)]
apr13elo <- head(apr13elo,-3)
apr13elo$Month <- "2013-04-01"
apr13elo$Month <- as.Date(apr13elo$Month, "%Y-%m-%d")

may13 <- monthlyelo[monthlyelo$Date<"2013-06-01" & monthlyelo$Date>="2013-05-01",]
may13elo <- elo(may13[,-5], status=apr13elo, gamma=0, sort=T)
may13elo <- ldply(may13elo, data.frame)
may13elo <- may13elo[c(2,3)]
may13elo <- head(may13elo,-3)
may13elo$Month <- "2013-05-01"
may13elo$Month <- as.Date(may13elo$Month, "%Y-%m-%d")

jun13 <- monthlyelo[monthlyelo$Date<"2013-07-01" & monthlyelo$Date>="2013-06-01",]
jun13elo <- elo(jun13[,-5], status=may13elo, gamma=0, sort=T)
jun13elo <- ldply(jun13elo, data.frame)
jun13elo <- jun13elo[c(2,3)]
jun13elo <- head(jun13elo,-3)
jun13elo$Month <- "2013-06-01"
jun13elo$Month <- as.Date(jun13elo$Month, "%Y-%m-%d")

jul13 <- monthlyelo[monthlyelo$Date<"2013-08-01" & monthlyelo$Date>="2013-07-01",]
jul13elo <- elo(jul13[,-5], status=jun13elo, gamma=0, sort=T)
jul13elo <- ldply(jul13elo, data.frame)
jul13elo <- jul13elo[c(2,3)]
jul13elo <- head(jul13elo,-3)
jul13elo$Month <- "2013-07-01"
jul13elo$Month <- as.Date(jul13elo$Month, "%Y-%m-%d")

aug13 <- monthlyelo[monthlyelo$Date<"2013-09-01" & monthlyelo$Date>="2013-08-01",]
aug13elo <- elo(aug13[,-5], status=jul13elo, gamma=0, sort=T)
aug13elo <- ldply(aug13elo, data.frame)
aug13elo <- aug13elo[c(2,3)]
aug13elo <- head(aug13elo,-3)
aug13elo$Month <- "2013-08-01"
aug13elo$Month <- as.Date(aug13elo$Month, "%Y-%m-%d")

sep13 <- monthlyelo[monthlyelo$Date<"2013-10-01" & monthlyelo$Date>="2013-09-01",]
sep13elo <- elo(sep13[,-5], status=aug13elo, gamma=0, sort=T)
sep13elo <- ldply(sep13elo, data.frame)
sep13elo <- sep13elo[c(2,3)]
sep13elo <- head(sep13elo,-3)
sep13elo$Month <- "2013-09-01"
sep13elo$Month <- as.Date(sep13elo$Month, "%Y-%m-%d")

oct13 <- monthlyelo[monthlyelo$Date<"2013-11-01" & monthlyelo$Date>="2013-10-01",]
oct13elo <- elo(oct13[,-5], status=sep13elo, gamma=0, sort=T)
oct13elo <- ldply(oct13elo, data.frame)
oct13elo <- oct13elo[c(2,3)]
oct13elo <- head(oct13elo,-3)
oct13elo$Month <- "2013-10-01"
oct13elo$Month <- as.Date(oct13elo$Month, "%Y-%m-%d")

nov13 <- monthlyelo[monthlyelo$Date<"2013-12-01" & monthlyelo$Date>="2013-11-01",]
nov13elo <- elo(nov13[,-5], status=oct13elo, gamma=0, sort=T)
nov13elo <- ldply(nov13elo, data.frame)
nov13elo <- nov13elo[c(2,3)]
nov13elo <- head(nov13elo,-3)
nov13elo$Month <- "2013-11-01"
nov13elo$Month <- as.Date(nov13elo$Month, "%Y-%m-%d")

dec13 <- monthlyelo[monthlyelo$Date<"2014-01-01" & monthlyelo$Date>="2013-12-01",]
dec13elo <- elo(dec13[,-5], status=nov13elo, gamma=0, sort=T)
dec13elo <- ldply(dec13elo, data.frame)
dec13elo <- dec13elo[c(2,3)]
dec13elo <- head(dec13elo,-3)
dec13elo$Month <- "2013-12-01"
dec13elo$Month <- as.Date(dec13elo$Month, "%Y-%m-%d")

jan14 <- monthlyelo[monthlyelo$Date<"2014-02-01" & monthlyelo$Date>="2014-01-01",]
jan14elo <- elo(jan14[,-5], status=dec13elo, gamma=0, sort=T)
jan14elo <- ldply(jan14elo, data.frame)
jan14elo <- jan14elo[c(2,3)]
jan14elo <- head(jan14elo,-3)
jan14elo$Month <- "2014-01-01"
jan14elo$Month <- as.Date(jan14elo$Month, "%Y-%m-%d")

feb14 <- monthlyelo[monthlyelo$Date<"2014-03-01" & monthlyelo$Date>="2014-02-01",]
feb14elo <- elo(feb14[,-5], status=jan14elo, gamma=0, sort=T)
feb14elo <- ldply(feb14elo, data.frame)
feb14elo <- feb14elo[c(2,3)]
feb14elo <- head(feb14elo,-3)
feb14elo$Month <- "2014-02-01"
feb14elo$Month <- as.Date(feb14elo$Month, "%Y-%m-%d")

mar14 <- monthlyelo[monthlyelo$Date<"2014-04-01" & monthlyelo$Date>="2014-03-01",]
mar14elo <- elo(mar14[,-5], status=feb14elo, gamma=0, sort=T)
mar14elo <- ldply(mar14elo, data.frame)
mar14elo <- mar14elo[c(2,3)]
mar14elo <- head(mar14elo,-3)
mar14elo$Month <- "2014-03-01"
mar14elo$Month <- as.Date(mar14elo$Month, "%Y-%m-%d")

apr14 <- monthlyelo[monthlyelo$Date<"2014-05-01" & monthlyelo$Date>="2014-04-01",]
apr14elo <- elo(apr14[,-5], status=mar14elo, gamma=0, sort=T)
apr14elo <- ldply(apr14elo, data.frame)
apr14elo <- apr14elo[c(2,3)]
apr14elo <- head(apr14elo,-3)
apr14elo$Month <- "2014-04-01"
apr14elo$Month <- as.Date(apr14elo$Month, "%Y-%m-%d")

may14 <- monthlyelo[monthlyelo$Date<"2014-06-01" & monthlyelo$Date>="2014-05-01",]
may14elo <- elo(may14[,-5], status=apr14elo, gamma=0, sort=T)
may14elo <- ldply(may14elo, data.frame)
may14elo <- may14elo[c(2,3)]
may14elo <- head(may14elo,-3)
may14elo$Month <- "2014-05-01"
may14elo$Month <- as.Date(may14elo$Month, "%Y-%m-%d")

jun14 <- monthlyelo[monthlyelo$Date<"2014-07-01" & monthlyelo$Date>="2014-06-01",]
jun14elo <- elo(jun14[,-5], status=may14elo, gamma=0, sort=T)
jun14elo <- ldply(jun14elo, data.frame)
jun14elo <- jun14elo[c(2,3)]
jun14elo <- head(jun14elo,-3)
jun14elo$Month <- "2014-06-01"
jun14elo$Month <- as.Date(jun14elo$Month, "%Y-%m-%d")

jul14 <- monthlyelo[monthlyelo$Date<"2014-08-01" & monthlyelo$Date>="2014-07-01",]
jul14elo <- elo(jul14[,-5], status=jun14elo, gamma=0, sort=T)
jul14elo <- ldply(jul14elo, data.frame)
jul14elo <- jul14elo[c(2,3)]
jul14elo <- head(jul14elo,-3)
jul14elo$Month <- "2014-07-01"
jul14elo$Month <- as.Date(jul14elo$Month, "%Y-%m-%d")

aug14 <- monthlyelo[monthlyelo$Date<"2014-09-01" & monthlyelo$Date>="2014-08-01",]
aug14elo <- elo(aug14[,-5], status=jul14elo, gamma=0, sort=T)
aug14elo <- ldply(aug14elo, data.frame)
aug14elo <- aug14elo[c(2,3)]
aug14elo <- head(aug14elo,-3)
aug14elo$Month <- "2014-08-01"
aug14elo$Month <- as.Date(aug14elo$Month, "%Y-%m-%d")

sep14 <- monthlyelo[monthlyelo$Date<"2014-10-01" & monthlyelo$Date>="2014-09-01",]
sep14elo <- elo(sep14[,-5], status=aug14elo, gamma=0, sort=T)
sep14elo <- ldply(sep14elo, data.frame)
sep14elo <- sep14elo[c(2,3)]
sep14elo <- head(sep14elo,-3)
sep14elo$Month <- "2014-09-01"
sep14elo$Month <- as.Date(sep14elo$Month, "%Y-%m-%d")

oct14 <- monthlyelo[monthlyelo$Date<"2014-11-01" & monthlyelo$Date>="2014-10-01",]
oct14elo <- elo(oct14[,-5], status=sep14elo, gamma=0, sort=T)
oct14elo <- ldply(oct14elo, data.frame)
oct14elo <- oct14elo[c(2,3)]
oct14elo <- head(oct14elo,-3)
oct14elo$Month <- "2014-10-01"
oct14elo$Month <- as.Date(oct14elo$Month, "%Y-%m-%d")

nov14 <- monthlyelo[monthlyelo$Date<"2014-12-01" & monthlyelo$Date>="2014-11-01",]
nov14elo <- elo(nov14[,-5], status=oct14elo, gamma=0, sort=T)
nov14elo <- ldply(nov14elo, data.frame)
nov14elo <- nov14elo[c(2,3)]
nov14elo <- head(nov14elo,-3)
nov14elo$Month <- "2014-11-01"
nov14elo$Month <- as.Date(nov14elo$Month, "%Y-%m-%d")

dec14 <- monthlyelo[monthlyelo$Date<"2015-01-01" & monthlyelo$Date>="2014-12-01",]
dec14elo <- elo(dec14[,-5], status=nov14elo, gamma=0, sort=T)
dec14elo <- ldply(dec14elo, data.frame)
dec14elo <- dec14elo[c(2,3)]
dec14elo <- head(dec14elo,-3)
dec14elo$Month <- "2014-12-01"
dec14elo$Month <- as.Date(dec14elo$Month, "%Y-%m-%d")

jan15 <- monthlyelo[monthlyelo$Date<"2015-02-01" & monthlyelo$Date>="2015-01-01",]
jan15elo <- elo(jan15[,-5], status=dec14elo, gamma=0, sort=T)
jan15elo <- ldply(jan15elo, data.frame)
jan15elo <- jan15elo[c(2,3)]
jan15elo <- head(jan15elo,-3)
jan15elo$Month <- "2015-01-01"
jan15elo$Month <- as.Date(jan15elo$Month, "%Y-%m-%d")

feb15 <- monthlyelo[monthlyelo$Date<"2015-03-01" & monthlyelo$Date>="2015-02-01",]
feb15elo <- elo(feb15[,-5], status=jan15elo, gamma=0, sort=T)
feb15elo <- ldply(feb15elo, data.frame)
feb15elo <- feb15elo[c(2,3)]
feb15elo <- head(feb15elo,-3)
feb15elo$Month <- "2015-02-01"
feb15elo$Month <- as.Date(feb15elo$Month, "%Y-%m-%d")

mar15 <- monthlyelo[monthlyelo$Date<"2015-04-01" & monthlyelo$Date>="2015-03-01",]
mar15elo <- elo(mar15[,-5], status=feb15elo, gamma=0, sort=T)
mar15elo <- ldply(mar15elo, data.frame)
mar15elo <- mar15elo[c(2,3)]
mar15elo <- head(mar15elo,-3)
mar15elo$Month <- "2015-03-01"
mar15elo$Month <- as.Date(mar15elo$Month, "%Y-%m-%d")

monthlyrating <- rbind(jan13elo, feb13elo, mar13elo, apr13elo, may13elo, jun13elo, jul13elo,
                       aug13elo, sep13elo, oct13elo, nov13elo, dec13elo, jan14elo, feb14elo,
                       mar14elo, apr14elo, may14elo, jun14elo, jul14elo, aug14elo, sep14elo,
                       oct14elo, nov14elo, dec14elo, jan15elo, feb15elo, mar15elo)



#Monthly Statistics
jan13s <- tournstats10.15[tournstats10.15$Date<"2013-02-01",]
jan13s <- na.omit(jan13s)
jan13stats <- summarise(group_by(jan13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
jan13total <- merge(jan13stats, jan13elo, by="Player")

feb13s <- tournstats10.15[tournstats10.15$Date<"2013-03-01" & tournstats10.15$Date>="2013-02-01",]
feb13s <- na.omit(feb13s)
feb13stats <- summarise(group_by(feb13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
feb13total <- merge(feb13stats, feb13elo, by="Player")

mar13s <- tournstats10.15[tournstats10.15$Date<"2013-04-01" & tournstats10.15$Date>="2013-03-01",]
mar13s <- na.omit(mar13s)
mar13stats <- summarise(group_by(mar13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
mar13total <- merge(mar13stats, mar13elo, by="Player")

apr13s <- tournstats10.15[tournstats10.15$Date<"2013-05-01" & tournstats10.15$Date>="2013-04-01",]
apr13s <- na.omit(apr13s)
apr13stats <- summarise(group_by(apr13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
apr13total <- merge(apr13stats, apr13elo, by="Player")

may13s <- tournstats10.15[tournstats10.15$Date<"2013-06-01" & tournstats10.15$Date>="2013-05-01",]
may13s <- na.omit(may13s)
may13stats <- summarise(group_by(may13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
may13total <- merge(may13stats, may13elo, by="Player")

jun13s <- tournstats10.15[tournstats10.15$Date<"2013-07-01" & tournstats10.15$Date>="2013-06-01",]
jun13s <- na.omit(jun13s)
jun13stats <- summarise(group_by(jun13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
jun13total <- merge(jun13stats, jun13elo, by="Player")

jul13s <- tournstats10.15[tournstats10.15$Date<"2013-08-01" & tournstats10.15$Date>="2013-07-01",]
jul13s <- na.omit(jul13s)
jul13stats <- summarise(group_by(jul13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
jul13total <- merge(jul13stats, jul13elo, by="Player")

aug13s <- tournstats10.15[tournstats10.15$Date<"2013-09-01" & tournstats10.15$Date>="2013-08-01",]
aug13s <- na.omit(aug13s)
aug13stats <- summarise(group_by(aug13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
aug13total <- merge(aug13stats, aug13elo, by="Player")

sep13s <- tournstats10.15[tournstats10.15$Date<"2013-10-01" & tournstats10.15$Date>="2013-09-01",]
sep13s <- na.omit(sep13s)
sep13stats <- summarise(group_by(sep13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
sep13total <- merge(sep13stats, sep13elo, by="Player")

oct13s <- tournstats10.15[tournstats10.15$Date<"2013-11-01" & tournstats10.15$Date>="2013-10-01",]
oct13s <- na.omit(oct13s)
oct13stats <- summarise(group_by(oct13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
oct13total <- merge(oct13stats, oct13elo, by="Player")

nov13s <- tournstats10.15[tournstats10.15$Date<"2013-12-01" & tournstats10.15$Date>="2013-11-01",]
nov13s <- na.omit(nov13s)
nov13stats <- summarise(group_by(nov13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
nov13total <- merge(nov13stats, nov13elo, by="Player")

dec13s <- tournstats10.15[tournstats10.15$Date<"2014-01-01" & tournstats10.15$Date>="2013-12-01",]
dec13s <- na.omit(dec13s)
dec13stats <- summarise(group_by(dec13s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
dec13total <- merge(dec13stats, dec13elo, by="Player")

jan14s <- tournstats10.15[tournstats10.15$Date<"2014-02-01" & tournstats10.15$Date>="2014-01-01",]
jan14s <- na.omit(jan14s)
jan14stats <- summarise(group_by(jan14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
jan14total <- merge(jan14stats, jan14elo, by="Player")

feb14s <- tournstats10.15[tournstats10.15$Date<"2014-03-01" & tournstats10.15$Date>="2014-02-01",]
feb14s <- na.omit(feb14s)
feb14stats <- summarise(group_by(feb14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
feb14total <- merge(feb14stats, feb14elo, by="Player")

mar14s <- tournstats10.15[tournstats10.15$Date<"2014-04-01" & tournstats10.15$Date>="2014-03-01",]
mar14s <- na.omit(mar14s)
mar14stats <- summarise(group_by(mar14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
mar14total <- merge(mar14stats, mar14elo, by="Player")

apr14s <- tournstats10.15[tournstats10.15$Date<"2014-05-01" & tournstats10.15$Date>="2014-04-01",]
apr14s <- na.omit(apr14s)
apr14stats <- summarise(group_by(apr14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
apr14total <- merge(apr14stats, apr14elo, by="Player")

may14s <- tournstats10.15[tournstats10.15$Date<"2014-06-01" & tournstats10.15$Date>="2014-05-01",]
may14s <- na.omit(may14s)
may14stats <- summarise(group_by(may14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
may14total <- merge(may14stats, may14elo, by="Player")

jun14s <- tournstats10.15[tournstats10.15$Date<"2014-07-01" & tournstats10.15$Date>="2014-06-01",]
jun14s <- na.omit(jun14s)
jun14stats <- summarise(group_by(jun14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
jun14total <- merge(jun14stats, jun14elo, by="Player")

jul14s <- tournstats10.15[tournstats10.15$Date<"2014-08-01" & tournstats10.15$Date>="2014-07-01",]
jul14s <- na.omit(jul14s)
jul14stats <- summarise(group_by(jul14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
jul14total <- merge(jul14stats, jul14elo, by="Player")

aug14s <- tournstats10.15[tournstats10.15$Date<"2014-09-01" & tournstats10.15$Date>="2014-08-01",]
aug14s <- na.omit(aug14s)
aug14stats <- summarise(group_by(aug14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
aug14total <- merge(aug14stats, aug14elo, by="Player")

sep14s <- tournstats10.15[tournstats10.15$Date<"2014-10-01" & tournstats10.15$Date>="2014-09-01",]
sep14s <- na.omit(sep14s)
sep14stats <- summarise(group_by(sep14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
sep14total <- merge(sep14stats, sep14elo, by="Player")

oct14s <- tournstats10.15[tournstats10.15$Date<"2014-11-01" & tournstats10.15$Date>="2014-10-01",]
oct14s <- na.omit(oct14s)
oct14stats <- summarise(group_by(oct14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
oct14total <- merge(oct14stats, oct14elo, by="Player")

nov14s <- tournstats10.15[tournstats10.15$Date<"2014-12-01" & tournstats10.15$Date>="2014-11-01",]
nov14s <- na.omit(nov14s)
nov14stats <- summarise(group_by(nov14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
nov14total <- merge(nov14stats, nov14elo, by="Player")

dec14s <- tournstats10.15[tournstats10.15$Date<"2015-01-01" & tournstats10.15$Date>="2014-12-01",]
dec14s <- na.omit(dec14s)
dec14stats <- summarise(group_by(dec14s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
dec14total <- merge(dec14stats, dec14elo, by="Player")

jan15s <- tournstats10.15[tournstats10.15$Date<"2015-02-01" & tournstats10.15$Date>="2015-01-01",]
jan15s <- na.omit(jan15s)
jan15stats <- summarise(group_by(jan15s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
jan15total <- merge(jan15stats, jan15elo, by="Player")

feb15s <- tournstats10.15[tournstats10.15$Date<"2015-03-01" & tournstats10.15$Date>="2015-02-01",]
feb15s <- na.omit(feb15s)
feb15stats <- summarise(group_by(feb15s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
feb15total <- merge(feb15stats, feb15elo, by="Player")

mar15s <- tournstats10.15[tournstats10.15$Date<"2015-04-01" & tournstats10.15$Date>="2015-03-01",]
mar15s <- na.omit(mar15s)
mar15stats <- summarise(group_by(mar15s, Player), Aces=mean(Aces), Double_Faults=mean(Double_Faults),
                        First_Serve_Per=sum(First_Serve_Total*First_Serve_Per)/sum(First_Serve_Total),
                        First_Serve_Points_Won_Per=sum(First_Serve_Points_Won_Total*First_Serve_Points_Won_Per)/sum(First_Serve_Points_Won_Total),
                        Second_Serve_Points_Won_Per=sum(Second_Serve_Points_Won_Total*Second_Serve_Points_Won_Per)/sum(Second_Serve_Points_Won_Total),
                        Break_Points_Saved_Per=sum(Break_Points_Saved_Total*Break_Points_Saved_Per)/sum(Break_Points_Saved_Total),
                        First_Serve_Return_Points_Won_Per=sum(First_Serve_Return_Points_Won_Total*First_Serve_Return_Points_Won_Per)/sum(First_Serve_Return_Points_Won_Total),
                        Second_Serve_Return_Points_Won_Per=sum(Second_Serve_Return_Points_Won_Total*Second_Serve_Return_Points_Won_Per)/sum(Second_Serve_Return_Points_Won_Total),
                        Break_Points_Converted_Per=sum(Break_Points_Converted_Total*Break_Points_Converted_Per)/sum(Break_Points_Converted_Total))
mar15total <- merge(mar15stats, mar15elo, by="Player")

monthlystats <- rbind(jan13total, feb13total, mar13total, apr13total, may13total, jun13total, jul13total,
                      aug13total, sep13total, oct13total, nov13total, dec13total, jan14total, feb14total,
                      mar14total, apr14total, may14total, jun14total, jul14total, aug14total, sep14total,
                      oct14total, nov14total, dec14total, jan15total, feb15total, mar15total)


#Individual player plots of statistics and rating over time
playerplot <- monthlystats[monthlystats$Player=="David Goffin",]

a <- ggplot(playerplot,aes(x=playerplot$Month,y=playerplot$First_Serve_Points_Won)) + geom_line() + labs(x="Date",y="First Serve Points Won Percentage")
b <- ggplot(playerplot,aes(x=playerplot$Month,y=playerplot$Rating)) + geom_line() + labs(x="Date",y="Rating")
grid.arrange(a,b, main="David Goffin")




#Plot Importance

import4 <- data.frame(rf4$importance)
import4 <- cbind(Variable = rownames(import4), import4)

#import4 <- import4[order(import4[,2], decreasing=T),]
names(import4)[2] <- "IncMSE"

import4$Variable <-factor(import4$Variable, levels=import4[order(-import4$IncMSE), "Variable"])
w <- qplot(data=import4, y=IncMSE, x=Variable) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
z <- qplot(data=import4, y=IncNodePurity, x=Variable) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(w,z,nrow=1)



import3 <- data.frame(rf3$importance)
import3 <- cbind(Variable = rownames(import3), import3)
import3$Variable <- factor(import3$Variable, levels=import3[order(-import3$MeanDecreaseAccuracy), "Variable"])

w <- qplot(data=import3, y=MeanDecreaseAccuracy, x=Variable) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
z <- qplot(data=import3, y=MeanDecreaseGini, x=Variable) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(w,z,nrow=1)
