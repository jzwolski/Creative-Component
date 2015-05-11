#Full Match Statistics for Grand Slam, 1000, 500, and 250 level tournaments from 2010 to 2015

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
for (j in 1:5)
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

#Edit time outside the loop#
tournstats10.15$Time <- as.character(stats.order$Time)
stats.order$Time <- as.numeric(substr(stats.order$Time, 1, nchar(stats.order$Time)-8))

#Change player to character
stats.order$Player <- as.character(stats.order$Player)
stats.order$Opponent <- as.character(stats.order$Opponent)

#Write to csv#
write.csv(stats.order, "Stats.csv", row.names=FALSE)