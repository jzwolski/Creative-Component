###Libraries###
library(XML)
library(plyr)
library(dplyr)
library(stringr)
library(scrapeR)
library(ggplot2)
library(reshape)
library(lubridate)
library(PlayerRatings)

startdate <- as.Date("17.01.2000", "%d.%m.%Y")
row.has.na <- apply(stats.order, 1, function(x){any(is.na(x))})
sum(row.has.na)
row.final.filtered <- stats.order[row.has.na,]

###Table of all match stats###
stats.order = NULL
for (j in 151:length(tourn))
{
  #Find all links on the page of the draws#
  h1 <- getLinks()
  htmlTreeParse(file = tourn[j],
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
  
  #Find beginning and end dates of tournament
  date <- tourn[j]
  date <- htmlToText(date)  
  date <- gsub(".*([0-3][0-9].[0-1][0-9].[2][0][0-1][0-9]-[0-3][0-9].[0-1][0-9].[2][0][0-1][0-9]).*", "\\1", date)
  begdate <- as.Date(substr(date, 1, 10), "%d.%m.%Y")
  enddate <- as.Date(substr(date, 12, 21), "%d.%m.%Y")
  
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
    
    year <- substr(tourn, 57, nchar(tourn))
    year <- substr(year, 1, 4)
    year <- as.numeric(year)
    players$Year <- year[j]
    
    players$Tournament <- players$V4
      
    if(players$Player==players$Winner){
      players$Result <- 1
    } else{
      players$Result <- 0
    }
    
    players$Round <- players$V6
        
    if(players$Round == "R128"){
      players$Round <- 128
    }
    
    if(players$Round == "R64"){
      players$Round <- 64
    }
    
    if(players$Round == "R32"){
      players$Round <- 32
    }
    
    if(players$Round == "R16"){
      players$Round <- 16
    }
    
    if(players$Round == "Q"){
      players$Round <- 8
    }
    
    if(players$Round == "S"){
      players$Round <- 4
    }
    
    if(players$Round == "F"){
      players$Round <- 2
    }
    
    matchtable <- players
    
    matchtable$Date <- begdate
    if((matchtable$Round==64)&((matchtable$Tournament=="Australian Open")|(matchtable$Tournament=="Roland Garros")|
                                      (matchtable$Tournament=="Wimbledon")|(matchtable$Tournament=="US Open"))){
      matchtable$Date <- begdate+3
    }
    
    if((matchtable$Round==32)&((matchtable$Tournament=="Australian Open")|(matchtable$Tournament=="Roland Garros")|
                                      (matchtable$Tournament=="Wimbledon")|(matchtable$Tournament=="US Open"))){
      matchtable$Date <- begdate+5
    }
    
    if((matchtable$Round==16)&((matchtable$Tournament=="Australian Open")|(matchtable$Tournament=="Roland Garros")|
                                      (matchtable$Tournament=="Wimbledon")|(matchtable$Tournament=="US Open"))){
      matchtable$Date <- begdate+7
    }
    
    if((matchtable$Round==8)&((matchtable$Tournament=="Australian Open")|(matchtable$Tournament=="Roland Garros")|
                                     (matchtable$Tournament=="Wimbledon")|(matchtable$Tournament=="US Open"))){
      matchtable$Date <- begdate+9
    }
    
    if((matchtable$Round==4)&((matchtable$Tournament=="Australian Open")|(matchtable$Tournament=="Roland Garros")|
                                     (matchtable$Tournament=="Wimbledon")|(matchtable$Tournament=="US Open"))){
      matchtable$Date <- begdate+11
    }
    
    if(matchtable$Round==2){
      matchtable$Date <- enddate
    }
    
    if((matchtable$Round==32)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year<=2003)){
      matchtable$Date <- begdate+2
    }
    
    if((matchtable$Round==16)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year<=2003)){
      matchtable$Date <- begdate+3
    }
    
    if((matchtable$Round==8)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year<=2003)){
      matchtable$Date <- begdate+4
    }
    
    if((matchtable$Round==4)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year<=2003)){
      matchtable$Date <- begdate+5
    }
    
    if((matchtable$Round==64)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year>=2004)&
         (matchtable$Year<=2008)){
      matchtable$Date <- begdate+3
    }
    
    if((matchtable$Round==32)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year>=2004)&
         (matchtable$Year<=2008)){
      matchtable$Date <- begdate+5
    }
    
    if((matchtable$Round==16)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year>=2004)&
         (matchtable$Year<=2008)){
      matchtable$Date <- begdate+7
    }
    
    if((matchtable$Round==8)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year>=2004)&
         (matchtable$Year<=2008)){
      matchtable$Date <- begdate+9
    }
    
    if((matchtable$Round==4)&(matchtable$Tournament=="ATP Masters Series Indian Wells")&(matchtable$Year>=2004)&
         (matchtable$Year<=2008)){
      matchtable$Date <- begdate+11
    }
    
    if((matchtable$Round==64)&(matchtable$Tournament=="ATP World Tour Masters 1000 Indian Wells")&(matchtable$Year>=2009)){
      matchtable$Date <- begdate+3
    }
    
    if((matchtable$Round==32)&(matchtable$Tournament=="ATP World Tour Masters 1000 Indian Wells")&(matchtable$Year>=2009)){
      matchtable$Date <- begdate+5
    }
    
    if((matchtable$Round==16)&(matchtable$Tournament=="ATP World Tour Masters 1000 Indian Wells")&(matchtable$Year>=2009)){
      matchtable$Date <- begdate+7
    }
    
    if((matchtable$Round==8)&(matchtable$Tournament=="ATP World Tour Masters 1000 Indian Wells")&(matchtable$Year>=2009)){
      matchtable$Date <- begdate+9
    }
    
    if((matchtable$Round==4)&(matchtable$Tournament=="ATP World Tour Masters 1000 Indian Wells")&(matchtable$Year>=2009)){
      matchtable$Date <- begdate+11
    }
    
    if((matchtable$Round==64)&((matchtable$Tournament=="ATP Masters Series Miami")|(matchtable$Tournament=="ATP World Tour Masters 1000 Miami"))){
      matchtable$Date <- begdate+3
    }
    
    if((matchtable$Round==32)&((matchtable$Tournament=="ATP Masters Series Miami")|(matchtable$Tournament=="ATP World Tour Masters 1000 Miami"))){
      matchtable$Date <- begdate+5
    }
    
    if((matchtable$Round==16)&((matchtable$Tournament=="ATP Masters Series Miami")|(matchtable$Tournament=="ATP World Tour Masters 1000 Miami"))){
      matchtable$Date <- begdate+7
    }
    
    if((matchtable$Round==8)&((matchtable$Tournament=="ATP Masters Series Miami")|(matchtable$Tournament=="ATP World Tour Masters 1000 Miami"))){
      matchtable$Date <- begdate+9
    }
    
    if((matchtable$Round==4)&((matchtable$Tournament=="ATP Masters Series Miami")|(matchtable$Tournament=="ATP World Tour Masters 1000 Miami"))){
      matchtable$Date <- begdate+11
    }
    
    if((matchtable$Round==32)&((matchtable$Tournament=="ATP Masters Series Monte Carlo")|(matchtable$Tournament=="ATP World Tour Masters 1000 Monte Carlo")|
                                      (matchtable$Tournament=="ATP Masters Series Rome")|(matchtable$Tournament=="ATP World Tour Masters 1000 Rome")|
                                      (matchtable$Tournament=="ATP Masters Series Hamburg")|(matchtable$Tournament=="ATP Masters Series Stuttgart")|
                                      (matchtable$Tournament=="ATP Masters Series Madrid")|(matchtable$Tournament=="ATP World Tour Masters 1000 Madrid")|
                                      (matchtable$Tournament=="ATP Masters Series Canada")|(matchtable$Tournament=="ATP World Tour Masters 1000 Canada")|
                                      (matchtable$Tournament=="ATP Masters Series Cincinnati")|(matchtable$Tournament=="ATP World Tour Masters 1000 Cincinnati")|
                                      (matchtable$Tournament=="ATP World Tour Masters 1000 Shanghai")|(matchtable$Tournament=="ATP Masters Series Paris")|
                                      (matchtable$Tournament=="ATP World Tour Masters 1000 Paris"))){
      matchtable$Date <- begdate+2
    }
    
    if((matchtable$Round==16)&((matchtable$Tournament=="ATP Masters Series Monte Carlo")|(matchtable$Tournament=="ATP World Tour Masters 1000 Monte Carlo")|
                                      (matchtable$Tournament=="ATP Masters Series Rome")|(matchtable$Tournament=="ATP World Tour Masters 1000 Rome")|
                                      (matchtable$Tournament=="ATP Masters Series Hamburg")|(matchtable$Tournament=="ATP Masters Series Stuttgart")|
                                      (matchtable$Tournament=="ATP Masters Series Madrid")|(matchtable$Tournament=="ATP World Tour Masters 1000 Madrid")|
                                      (matchtable$Tournament=="ATP Masters Series Canada")|(matchtable$Tournament=="ATP World Tour Masters 1000 Canada")|
                                      (matchtable$Tournament=="ATP Masters Series Cincinnati")|(matchtable$Tournament=="ATP World Tour Masters 1000 Cincinnati")|
                                      (matchtable$Tournament=="ATP World Tour Masters 1000 Shanghai")|(matchtable$Tournament=="ATP Masters Series Paris")|
                                      (matchtable$Tournament=="ATP World Tour Masters 1000 Paris"))){
      matchtable$Date <- begdate+3
    }
    
    if((matchtable$Round==8)&((matchtable$Tournament=="ATP Masters Series Monte Carlo")|(matchtable$Tournament=="ATP World Tour Masters 1000 Monte Carlo")|
                                     (matchtable$Tournament=="ATP Masters Series Rome")|(matchtable$Tournament=="ATP World Tour Masters 1000 Rome")|
                                     (matchtable$Tournament=="ATP Masters Series Hamburg")|(matchtable$Tournament=="ATP Masters Series Stuttgart")|
                                     (matchtable$Tournament=="ATP Masters Series Madrid")|(matchtable$Tournament=="ATP World Tour Masters 1000 Madrid")|
                                     (matchtable$Tournament=="ATP Masters Series Canada")|(matchtable$Tournament=="ATP World Tour Masters 1000 Canada")|
                                     (matchtable$Tournament=="ATP Masters Series Cincinnati")|(matchtable$Tournament=="ATP World Tour Masters 1000 Cincinnati")|
                                     (matchtable$Tournament=="ATP World Tour Masters 1000 Shanghai")|(matchtable$Tournament=="ATP Masters Series Paris")|
                                     (matchtable$Tournament=="ATP World Tour Masters 1000 Paris"))){
      matchtable$Date <- begdate+4
    }
    
    if((matchtable$Round==4)&((matchtable$Tournament=="ATP Masters Series Monte Carlo")|(matchtable$Tournament=="ATP World Tour Masters 1000 Monte Carlo")|
                                     (matchtable$Tournament=="ATP Masters Series Rome")|(matchtable$Tournament=="ATP World Tour Masters 1000 Rome")|
                                     (matchtable$Tournament=="ATP Masters Series Hamburg")|(matchtable$Tournament=="ATP Masters Series Stuttgart")|
                                     (matchtable$Tournament=="ATP Masters Series Madrid")|(matchtable$Tournament=="ATP World Tour Masters 1000 Madrid")|
                                     (matchtable$Tournament=="ATP Masters Series Canada")|(matchtable$Tournament=="ATP World Tour Masters 1000 Canada")|
                                     (matchtable$Tournament=="ATP Masters Series Cincinnati")|(matchtable$Tournament=="ATP World Tour Masters 1000 Cincinnati")|
                                     (matchtable$Tournament=="ATP World Tour Masters 1000 Shanghai")|(matchtable$Tournament=="ATP Masters Series Paris")|
                                     (matchtable$Tournament=="ATP World Tour Masters 1000 Paris"))){
      matchtable$Date <- begdate+5
    }
    
    matchtable <- matchtable[c(50:57)]
    
    if(i == 1){
      fulltable <- matchtable
    } else {
      fulltable = rbind(fulltable, matchtable)
    }
  }
  if(j == 1){
    stats.order = fulltable
  } else {
    stats.order = rbind(stats.order, fulltable)
  }
}

day50 <- stats.order
day100 <- stats.order
day120 <- stats.order
day130 <- stats.order
day140 <- stats.order
day150 <- stats.order
day196 <- stats.order

matchtable <- rbind(day50, day100, day120, day130, day140, day150, day196)

matchtable$Day <- matchtable$Date - startdate
matchtable$Day <- as.numeric(matchtable$Day)

matchtable <- matchtable[matchtable$Opponent!="N/A Bye",]

x <- matchtable[c(9,1,2,6)]

elo <- elo(x, status=NULL, init=1, sort=T)
head(elo)

elo <- as.data.frame(elo)

#running rating system, enter a time period, and get a ranking for that time
#this just creates the elo ratings, no data frame
#does not include a lag of a month before the desired interval
interval.nolag <- function(date1, date2){
  days1 <- date1 - startdate
  days2 <- date2 - startdate
  x.subset <- x[which((x$Day>=days1)&(x$Day<=days2)),]
  elo.subset <- elo(x.subset, status=elo.lagsubset, init=1, sort=T)
  return(elo.subset)
}


#for all the tournaments from 2013 to present
#running rating system, enter a time period, and get a ranking for that time
#this just creates the elo ratings, no data frame
#does not include a lag of a month before the desired interval
interval.alltourns <- function(date1, date2){
  days1 <- date1 - startdateallt
  days2 <- date2 - startdateallt
  x.subset <- alltourns3[which((alltourns3$Day>=days1)&(alltourns3$Day<=days2)),]
  elo.subset <- elo(x.subset, status=NULL, init=1, gamma=0, sort=T)
  return(elo.subset)
}



#running rating system, enter a time period, and get a ranking for that time
#this just creates the elo ratings, no data frame
#includes a lag of a month before the desired interval
interval.or <- function(date1, date2){
  lagdate1 <- date1 %m-% months(1)
  lagdate2 <- date1 - 1
  lagdays1 <- lagdate1 - startdate
  lagdays2 <- lagdate2 - startdate
  x.lagsubset <- x[which((x$Day>=lagdays1)&(x$Day<=lagdays2)),]
  elo.lagsubset <- elo(x.lagsubset, status=NULL, init=1, sort=T)
  elo.lagsubset <- ldply(elo.lagsubset, data.frame)
  elo.lagsubset <- elo.lagsubset[c(2,3)]
  elo.lagsubset <- head(elo.lagsubset,-3)
  
  days1 <- date1 - startdate
  days2 <- date2 - startdate
  x.subset <- x[which((x$Day>=days1)&(x$Day<=days2)),]
  elo.subset <- elo(x.subset, status=elo.lagsubset, init=1, sort=T)
  return(elo.subset)
}


#running rating system, enter a time period, and get a ranking for that time
#this makes it a data frame
#includes a lag of a month before the desired interval
interval <- function(date1, date2){
  lagdate1 <- date1 %m-% months(1)
  lagdate2 <- date1 - 1
  lagdays1 <- lagdate1 - startdate
  lagdays2 <- lagdate2 - startdate
  x.lagsubset <- x[which((x$Day>=lagdays1)&(x$Day<=lagdays2)),]
  elo.lagsubset <- elo(x.lagsubset, status=NULL, init=1, sort=T)
  elo.lagsubset <- ldply(elo.lagsubset, data.frame)
  elo.lagsubset <- elo.lagsubset[c(2,3)]
  elo.lagsubset <- head(elo.lagsubset,-3)
  
  days1 <- date1 - startdate
  days2 <- date2 - startdate
  x.subset <- x[which((x$Day>=days1)&(x$Day<=days2)),]
  elo.subset <- elo(x.subset, status=elo.lagsubset, init=1, sort=T)
  elo.subset <- ldply(elo.subset, data.frame)
  elo.subset <- elo.subset[-c(1,9,10,11)]
  elo.subset <- head(elo.subset,-3)
  return(elo.subset)
}

date1 <- as.Date("2014-10-01", "%Y-%m-%d")
date2 <- as.Date("2014-12-01", "%Y-%m-%d")
intervalr <- interval(date1, date2)


nostatus <- elo.subset

#determine what lag means
  #Lag is the number of time periods since a player last played a game


#calculate the rating monthly, and then track players over time
  #write a function that collects the ratings every month and stores each of them in a different data set
startdate2000 <- as.Date("2000-01-01", "%Y-%m-%d")
range <- today() - startdate
months <- as.interval(range, start=startdate)
months <- as.period(months, unit="months")
months <- gsub("m.*", "", months)
months <- as.numeric(months)

#ratings data frame of just the first month, from January 1, 2000 to January 31, 2000#
month <- startdate2000 %m+% months(0)
fdaym <- month
fdaym <- fdaym - startdate2000
ldaym <- month %m+% months(1) - 1
ldaym <- ldaym - startdate
x.month <- x[which((x$Day>=fdaym)&(x$Day<=ldaym)),]
elo.1stmonth <- elo(x.month, init=1, sort=T)
elo.1stmonth <- ldply(elo.1stmonth, data.frame)
elo.1stmonth <- elo.1stmonth[-c(1,9,10,11)]
elo.1stmonth <- head(elo.1stmonth,-3)
elo.1stmonth$Month <- month

#ratings data frame of just the second month, from February 1, 2000 to February 29, 2000#
month <- startdate2000 %m+% months(1)
fdaym <- month
fdaym <- fdaym - startdate
ldaym <- month %m+% months(1) - 1
ldaym <- ldaym - startdate
elo.2ndmonth <- elo.1stmonth
elo.2ndmonth$Month <- month

#for all months after the 1st and 2nd months#
full.elo <- NULL
for(i in 2:(months-1)){
  if(i==2){
    month <- startdate2000 %m+% months(i)
    fdaym <- month
    fdaym <- fdaym - startdate
    ldaym <- month %m+% months(1) - 1
    ldaym <- ldaym - startdate  
    x.month <- x[which((x$Day>=fdaym)&(x$Day<=ldaym)),]
    elo.month <- elo(x.month, status=elo.2ndmonth, init=1, sort=T)
    elo.month <- ldply(elo.month, data.frame)
    elo.month <- elo.month[-c(1,9,10,11)]
    elo.month <- head(elo.month,-3)
    elo.month$Month <- month
  } else {
    month <- startdate2000 %m+% months(i)
    fdaym <- month
    fdaym <- fdaym - startdate
    ldaym <- month %m+% months(1) - 1
    ldaym <- ldaym - startdate  
    x.month <- x[which((x$Day>=fdaym)&(x$Day<=ldaym)),]
  
    if(nrow(x.month)==0){
      elo.month <- elo.month
      elo.month$Month <- month
    } else {
      elo.month <- elo(x.month, status=elo.month, init=1, sort=T)
      elo.month <- ldply(elo.month, data.frame)
      elo.month <- elo.month[-c(1,9,10,11)]
      elo.month <- head(elo.month,-3)
      elo.month$Month <- month
    }
  }

  if(i == 2){
    full.elo <- elo.month
  } else {
    full.elo <- rbind(full.elo, elo.month)
  }
}

full.elo <- rbind(elo.1stmonth, elo.2ndmonth, full.elo)

indplrating <- function(name){
  indrating <- full.elo[which(full.elo$Player==name),]
  plot <- qplot(x=indrating$Month, y=indrating$Rating, ylab="Rating", xlab="Time",
                main="Rating History") + geom_line()
  return(plot)
}

indplrating("Juan Martin Del Potro")

#determine when to use lag based on the different analysis

  #write another function that takes in a player's name, and combines all their ratings into one data frame
      #(maybe rbind all the different ratings data sets and just extract the rows for a given player),
    #with a date to distinguish the ratings, and then plot the ratings for the player

#use the gamma parameter to incorporate the match statistics
  #come up with a way to combine all the statistics into a single number, and
  #then assign this positive number to player 1 and a negative number to player 2

#use the status parameter to incorporate player ratings from the previous month


#find the ATP ranking for the last decade and compare (only top 100)
#track statistics over time(aces, double faults, have a baseline if want to look at ratios(minimum number of aces/faults))

#look at top 10 versus 50-100 for classification
#create models for weekly analysis
#don't use the gamma parameter for developing the ratings
#use the ratings and compare with the match statistics
#538 Carl Bialik
#which match statistics help to predict the ratings (top 10 versus 50-100)