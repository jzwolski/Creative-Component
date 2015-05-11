#Predictions for Indian Wells 2015 Tournament#
library(XML)
library(plyr)
library(dplyr)
library(stringr)
library(scrapeR)
library(ggplot2)
library(reshape)
library(lubridate)
library(PlayerRatings)

#Indian Wells 2014 Tournament
indianwells = NULL
j=3
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
  
#Scrape the website for the draw for a given tournament#
for (i in 1:length(files.3))
{
  table <- as.data.frame(readHTMLTable(files.3[i]) [[1]])
  players <- table
  players <- t(players)
  players <- as.data.frame(players, row.names=F)
  players$Day <- NA
  players$Player <- players$V12[[1]]
  players$Opponent <- players$V12[[2]]
  players$Winner <- players$V10[[1]]
  players <- players[,c(50,51,52,53)]
  players <- players[1,]
  players$Player <- as.character(players$Player)
  players$Opponent <- as.character(players$Opponent)
  
  if(i == 1){
    indianwells <- players
  } else {
    indianwells <- rbind(indianwells,players)
  }
}

indianwells <- indianwells[indianwells$Opponent!="N/A Bye",]
indianwells.predict <- indianwells[,-4]

date1 <- as.Date("2012-03-03", "%Y-%m-%d")
date2 <- as.Date("2014-03-02", "%Y-%m-%d")
intervalr <- interval.nolag(date1, date2)

elo.predict <- predict(intervalr, indianwells.predict, tng=1)
elo.predict <- as.data.frame(elo.predict)
indianwells.compare <- cbind(indianwells, elo.predict)
indianwells.compare <- indianwells.compare[,-1]
indianwells.compare <- na.omit(indianwells.compare)

for(k in 1:nrow(indianwells.compare)){
  if(indianwells.compare$elo.predict[[k]]>.5){
    if(indianwells.compare$Player[[k]]==indianwells.compare$Winner[[k]]){
      indianwells.compare$Accuracy[[k]] <- 1
    } else{
      indianwells.compare$Accuracy[[k]] <- 0
    }
  }
  if(indianwells.compare$elo.predict[[k]]<.5){
    if(indianwells.compare$Opponent[[k]]==indianwells.compare$Winner[[k]]){
      indianwells.compare$Accuracy[[k]] <- 1
    } else{
      indianwells.compare$Accuracy[[k]] <- 0
    }
  }
}

sum(indianwells.compare$Accuracy)/nrow(indianwells.compare)


#2015 Australian Open
ausopen15 = NULL
j=1
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

#Scrape the website for the draw for a given tournament#
for (i in 1:length(files.3))
{
  table <- as.data.frame(readHTMLTable(files.3[i]) [[1]])
  players <- table
  players <- t(players)
  players <- as.data.frame(players, row.names=F)
  players$Day <- NA
  players$Player <- players$V12[[1]]
  players$Opponent <- players$V12[[2]]
  players$Winner <- players$V10[[1]]
  players <- players[,c(50,51,52,53)]
  players <- players[1,]
  players$Player <- as.character(players$Player)
  players$Opponent <- as.character(players$Opponent)
  
  if(i == 1){
    ausopen15 <- players
  } else {
    ausopen15 <- rbind(ausopen15,players)
  }
}

ausopen15 <- ausopen15[ausopen15$Opponent!="N/A Bye",]
ausopen15.predict <- ausopen15[,-4]

date1 <- as.Date("2014-01-18", "%Y-%m-%d")
date2 <- as.Date("2015-01-18", "%Y-%m-%d")
intervalr <- interval.nolag(date1, date2)

elo.predict <- predict(intervalr, ausopen15.predict, gamma=0, tng=1)
elo.predict <- as.data.frame(elo.predict)
ausopen15.compare <- cbind(ausopen15, elo.predict)
ausopen15.compare <- ausopen15.compare[,-1]

#adding probabilities for missing predictions
ausopen15.compare$elo.predict <- as.character(ausopen15.compare$elo.predict)
days1 <- date1 - startdate
days2 <- date2 - startdate
x.subset <- x[which((x$Day>=days1)&(x$Day<=days2)),]
for(k in 1:nrow(ausopen15.compare)){
  if(is.na(ausopen15.compare$elo.predict[[k]])==T){
    if((((ausopen15.compare$Player[[k]] %in% x.subset$Player)|
          (ausopen15.compare$Player[[k]] %in% x.subset$Opponent))==T)&
         ((ausopen15.compare$Opponent[[k]] %in% x.subset$Player)|
          (ausopen15.compare$Opponent[[k]] %in% x.subset$Opponent))==F){
      ausopen15.compare$elo.predict[[k]] <- "0.75"
    }
    if((((ausopen15.compare$Player[[k]] %in% x.subset$Player)|
          (ausopen15.compare$Player[[k]] %in% x.subset$Opponent))==F)&
         ((ausopen15.compare$Opponent[[k]] %in% x.subset$Player)|
          (ausopen15.compare$Opponent[[k]] %in% x.subset$Opponent))==T){
      ausopen15.compare$elo.predict[[k]] <- "0.25"
    }
    if((((ausopen15.compare$Player[[k]] %in% x.subset$Player)|
          (ausopen15.compare$Player[[k]] %in% x.subset$Opponent))==F)&
         ((ausopen15.compare$Opponent[[k]] %in% x.subset$Player)|
          (ausopen15.compare$Opponent[[k]] %in% x.subset$Opponent))==F){
      ausopen15.compare$elo.predict[[k]] <- "0.50"
    }
  }
}

ausopen15.compare$elo.predict <- as.numeric(ausopen15.compare$elo.predict)

ausopen15.compare <- na.omit(ausopen15.compare)

for(k in 1:nrow(ausopen15.compare)){
  if(ausopen15.compare$elo.predict[[k]]>.5){
    if(ausopen15.compare$Player[[k]]==ausopen15.compare$Winner[[k]]){
      ausopen15.compare$Accuracy[[k]] <- 1
    } else{
      ausopen15.compare$Accuracy[[k]] <- 0
    }
  }
  if(ausopen15.compare$elo.predict[[k]]<.5){
    if(ausopen15.compare$Opponent[[k]]==ausopen15.compare$Winner[[k]]){
      ausopen15.compare$Accuracy[[k]] <- 1
    } else{
      ausopen15.compare$Accuracy[[k]] <- 0
    }
  }
  if(ausopen15.compare$elo.predict[[k]]==.5){
    ausopen15.compare$Accuracy[[k]] <- .5
  }
}

sum(ausopen15.compare$Accuracy)/nrow(ausopen15.compare)


#2015 Indian Wells
inwells15 = NULL
j=1
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

#Scrape the website for the draw for a given tournament#
for (i in 1:length(files.3))
{
  table <- as.data.frame(readHTMLTable(files.3[i]) [[1]])
  players <- table
  players <- t(players)
  players <- as.data.frame(players, row.names=F)
  players$Day <- NA
  players$Player <- players$V12[[1]]
  players$Opponent <- players$V12[[2]]
  players$Winner <- players$V10[[1]]
  players <- players[,c(50,51,52,53)]
  players <- players[1,]
  players$Player <- as.character(players$Player)
  players$Opponent <- as.character(players$Opponent)
  
  if(i == 1){
    inwells15 <- players
  } else {
    inwells15 <- rbind(inwells15,players)
  }
}

inwells15 <- inwells15[inwells15$Opponent!="N/A Bye",]
inwells15.predict <- inwells15[,-4]

date1 <- as.Date("2014-01-18", "%Y-%m-%d")
date2 <- as.Date("2015-01-18", "%Y-%m-%d")
intervalr <- interval.nolag(date1, date2)

elo.predict <- predict(intervalr, inwells15.predict, gamma=NULL)
elo.predict <- as.data.frame(elo.predict)
inwells15.compare <- cbind(inwells15, elo.predict)
inwells15.compare <- inwells15.compare[,-1]

#adding probabilities for missing predictions
inwells15.compare$elo.predict <- as.character(ausopen15.compare$elo.predict)
days1 <- date1 - startdate
days2 <- date2 - startdate
x.subset <- x[which((x$Day>=days1)&(x$Day<=days2)),]
for(k in 1:nrow(ausopen15.compare)){
  if(is.na(ausopen15.compare$elo.predict[[k]])==T){
    if((((ausopen15.compare$Player[[k]] %in% x.subset$Player)|
           (ausopen15.compare$Player[[k]] %in% x.subset$Opponent))==T)&
         ((ausopen15.compare$Opponent[[k]] %in% x.subset$Player)|
            (ausopen15.compare$Opponent[[k]] %in% x.subset$Opponent))==F){
      ausopen15.compare$elo.predict[[k]] <- "0.75"
    }
    if((((ausopen15.compare$Player[[k]] %in% x.subset$Player)|
           (ausopen15.compare$Player[[k]] %in% x.subset$Opponent))==F)&
         ((ausopen15.compare$Opponent[[k]] %in% x.subset$Player)|
            (ausopen15.compare$Opponent[[k]] %in% x.subset$Opponent))==T){
      ausopen15.compare$elo.predict[[k]] <- "0.25"
    }
    if((((ausopen15.compare$Player[[k]] %in% x.subset$Player)|
           (ausopen15.compare$Player[[k]] %in% x.subset$Opponent))==F)&
         ((ausopen15.compare$Opponent[[k]] %in% x.subset$Player)|
            (ausopen15.compare$Opponent[[k]] %in% x.subset$Opponent))==F){
      ausopen15.compare$elo.predict[[k]] <- "0.50"
    }
  }
}

ausopen15.compare$elo.predict <- as.numeric(ausopen15.compare$elo.predict)

ausopen15.compare <- na.omit(ausopen15.compare)

for(k in 1:nrow(ausopen15.compare)){
  if(ausopen15.compare$elo.predict[[k]]>.5){
    if(ausopen15.compare$Player[[k]]==ausopen15.compare$Winner[[k]]){
      ausopen15.compare$Accuracy[[k]] <- 1
    } else{
      ausopen15.compare$Accuracy[[k]] <- 0
    }
  }
  if(ausopen15.compare$elo.predict[[k]]<.5){
    if(ausopen15.compare$Opponent[[k]]==ausopen15.compare$Winner[[k]]){
      ausopen15.compare$Accuracy[[k]] <- 1
    } else{
      ausopen15.compare$Accuracy[[k]] <- 0
    }
  }
  if(ausopen15.compare$elo.predict[[k]]==.5){
    ausopen15.compare$Accuracy[[k]] <- .5
  }
}

sum(ausopen15.compare$Accuracy)/nrow(ausopen15.compare)