#Indian Wells 2015 Predictions

Topplayer <- c("Marcos Baghdatis", "Viktor Troicki", "Dennis Novikov", "Federico Delbonis",
               "Teymuraz Gabashvili", "Jan-Lennard Struff", "Borna Coric", "Ivan Dodig",
               "Mikhail Kukushkin", "Tim Smyczek", "Mischa Zverev", "Daniel Gimeno-Traver",
               "Marinko Matosevic", "Jarkko Nieminen", "James Duckworth", "Mardy Fish",
               "Simone Bolelli", "Alexandr Dolgopolov", "Andrey Golubev", "Nick Kyrgios",
               "Tatsuma Ito", "Victor Estrella Burgos", "Donald Young", "Igor Sijsling",
               "Robin Haase", "Martin Klizan", "Steve Johnson", "Sam Querrey",
               "Denis Istomin", "Jack Sock", "Mikhail Youzhny", "Jerzy Janowicz",
               "Novak Djokovic", "Albert Ramos-Vinolas", "John Isner", "Federico Delbonis",
               "Marin Cilic", "Thanasi Kokkinakis", "Bernard Tomic", "Ivan Dodig",
               "Andy Murray", "Tim Smyczek", "Fabio Fognini", "Daniel Gimeno-Traver",
               "Feliciano Lopez", "Jarkko Nieminen", "Fernando Verdasco", "Ryan Harrison",
               "Milos Raonic", "Alexandr Dolgopolov", "Tommy Robredo", "Nick Kyrgios",
               "Gilles Simon", "Michael Berrer", "Jeremy Chardy", "Igor Sijsling",
               "Stan Wawrinka", "Martin Klizan", "Ivo Karlovic", "Sergiy Stakhovsky",
               "Roberto Bautista Agut", "Jack Sock", "Andreas Seppi", "Diego Schwartzman",
               "Novak Djokovic", "John Isner", "Juan Monaco", "Bernard Tomic",
               "Andy Murray", "Adrian Mannarino", "Feliciano Lopez", "Fernando Verdasco",
               "Milos Raonic", "Tommy Robredo", "Gilles Simon", "Donald Young",
               "Robin Haase", "Steve Johnson", "Roberto Bautista Agut", "Andreas Seppi",
               "Novak Djokovic", "Thanasi Kokkinakis", "Andy Murray", "Feliciano Lopez",
               "Milos Raonic", "Gilles Simon", "Lukas Rosol", "Jack Sock",
               "Novak Djokovic", "Andy Murray", "Milos Raonic", "Tomas Berdych",
               "Novak Djokovic","Milos Raonic","Novak Djokovic")

Botplayer <- c("Jiri Vesely", "Albert Ramos-Vinolas", "Jurgen Melzer", "Dusan Lajovic",
               "Juan Monaco", "Thanasi Kokkinakis", "Andreas Haider-Maurer", "Joao Sousa",
               "Vasek Pospisil", "Benjamin Becker", "Adrian Mannarino", "Sam Groth",
               "Edouard Roger-Vasselin", "Thiemo de Bakker", "Dominic Thiem", "Ryan Harrison",
               "Thomaz Bellucci", "Frank Dancevic", "Dustin Brown", "Denis Kudla",
               "Malek Jaziri", "Michael Berrer", "Pablo Carreno Busta", "Filip Krajinovic",
               "Alex Bolt", "Pablo Andujar", "Marcel Granollers", "Sergiy Stakhovsky",
               "Austin Krajicek", "Yen-Hsun Lu", "Victor Hanescu", "Diego Schwartzman",
               "Marcos Baghdatis", "Julien Benneteau", "Jurgen Melzer", "Kevin Anderson",
               "Juan Monaco", "Guillermo Garcia-Lopez", "Borna Coric", "David Ferrer",
               "Vasek Pospisil", "Philipp Kohlschreiber", "Adrian Mannarino", "Ernests Gulbis",
               "Edouard Roger-Vasselin", "Pablo Cuevas", "James Duckworth", "Kei Nishikori",
               "Simone Bolelli", "Santiago Giraldo", "Andrey Golubev", "Grigor Dimitrov",
               "Malek Jaziri", "Richard Gasquet", "Donald Young", "Rafael Nadal",
               "Robin Haase", "Lukas Rosol", "Steve Johnson", "Tomas Berdych",
               "Denis Istomin", "Gilles Muller", "Victor Hanescu", "Roger Federer",
               "Albert Ramos-Vinolas", "Kevin Anderson", "Thanasi Kokkinakis", "David Ferrer",
               "Philipp Kohlschreiber", "Ernests Gulbis", "Pablo Cuevas", "Kei Nishikori",
               "Alexandr Dolgopolov", "Grigor Dimitrov", "Michael Berrer", "Rafael Nadal",
               "Lukas Rosol", "Tomas Berdych", "Jack Sock", "Roger Federer",
               "John Isner", "Bernard Tomic", "Adrian Mannarino", "Kei Nishikori",
               "Tommy Robredo", "Rafael Nadal", "Tomas Berdych", "Roger Federer",
               "Bernard Tomic", "Feliciano Lopez", "Rafael Nadal", "Roger Federer",
               "Andy Murray","Roger Federer","Roger Federer")

inwells15 <- as.data.frame(cbind(Topplayer, Botplayer))
inwells15$Day <- NA
inwells15 <- inwells15[c(3,1,2)]

#Lag elo#
predictinwells15 <- predict(lastyearelo, inwells15, tng=10, gamma=0)

#No lag elo#
predictinwells15 <- predict(alltourns3elo, inwells15, tng=15, gamma=0)
predictinwells15 <- predict(lastyearelon, inwells15, tng=15, gamma=0)

#Glicko rating#
predictinwells15 <- predict(allglicko, inwells15, tng=15, gamma=0)

predictinwells15 <- as.data.frame(predictinwells15)
inwells15.compare <- cbind(inwells15, predictinwells15)
inwells15.compare <- inwells15.compare[,-1]

#for loop for lag elo#
for(k in 1:nrow(inwells15.compare)){
  if(is.na(inwells15.compare$predictinwells15[[k]])==T){
    if(((nrow(lastyear[lastyear$Player==inwells15.compare$Topplayer[[k]],])+
           nrow(lastyear[lastyear$Opponent==inwells15.compare$Topplayer[[k]],]))<15) &
         ((nrow(lastyear[lastyear$Player==inwells15.compare$Botplayer[[k]],])+
             nrow(lastyear[lastyear$Opponent==inwells15.compare$Botplayer[[k]],]))>=15)){
      inwells15.compare$predictinwells15[[k]] <- 0
    }
    if(((nrow(lastyear[lastyear$Player==inwells15.compare$Topplayer[[k]],])+
           nrow(lastyear[lastyear$Opponent==inwells15.compare$Topplayer[[k]],]))>=15) &
         ((nrow(lastyear[lastyear$Player==inwells15.compare$Botplayer[[k]],])+
             nrow(lastyear[lastyear$Opponent==inwells15.compare$Botplayer[[k]],]))<15)){
      inwells15.compare$predictinwells15[[k]] <- 1
    }
    if(((nrow(lastyear[lastyear$Player==inwells15.compare$Topplayer[[k]],])+
           nrow(lastyear[lastyear$Opponent==inwells15.compare$Topplayer[[k]],]))<15) &
         ((nrow(lastyear[lastyear$Player==inwells15.compare$Botplayer[[k]],])+
             nrow(lastyear[lastyear$Opponent==inwells15.compare$Botplayer[[k]],]))<15)){
      inwells15.compare$predictinwells15[[k]] <- 0.5
    }
  }
}

#for loop for no lag elo#
for(k in 1:nrow(inwells15.compare)){
  if(is.na(inwells15.compare$predictinwells15[[k]])==T){
    if(((nrow(alltourns3[alltourns3$Player==inwells15.compare$Topplayer[[k]],])+
          nrow(alltourns3[alltourns3$Opponent==inwells15.compare$Topplayer[[k]],]))<15) &
       ((nrow(alltourns3[alltourns3$Player==inwells15.compare$Botplayer[[k]],])+
          nrow(alltourns3[alltourns3$Opponent==inwells15.compare$Botplayer[[k]],]))>=15)){
      inwells15.compare$predictinwells15[[k]] <- 0
    }
    if(((nrow(alltourns3[alltourns3$Player==inwells15.compare$Topplayer[[k]],])+
           nrow(alltourns3[alltourns3$Opponent==inwells15.compare$Topplayer[[k]],]))>=15) &
         ((nrow(alltourns3[alltourns3$Player==inwells15.compare$Botplayer[[k]],])+
             nrow(alltourns3[alltourns3$Opponent==inwells15.compare$Botplayer[[k]],]))<15)){
      inwells15.compare$predictinwells15[[k]] <- 1
    }
    if(((nrow(alltourns3[alltourns3$Player==inwells15.compare$Topplayer[[k]],])+
           nrow(alltourns3[alltourns3$Opponent==inwells15.compare$Topplayer[[k]],]))<15) &
         ((nrow(alltourns3[alltourns3$Player==inwells15.compare$Botplayer[[k]],])+
             nrow(alltourns3[alltourns3$Opponent==inwells15.compare$Botplayer[[k]],]))<15)){
      inwells15.compare$predictinwells15[[k]] <- 0.5
    }
  }
}

Winner <- c("Marcos Baghdatis", "Albert Ramos-Vinolas", "Jurgen Melzer", "Federico Delbonis",
            "Juan Monaco", "Thanasi Kokkinakis", "Borna Coric", "Ivan Dodig",
            "Vasek Pospisil", "Tim Smyczek", "Adrian Mannarino", "Daniel Gimeno-Traver",
            "Edouard Roger-Vasselin", "Jarkko Nieminen", "James Duckworth", "Ryan Harrison",
            "Simone Bolelli", "Alexandr Dolgopolov", "Andrey Golubev", "Nick Kyrgios",
            "Malek Jaziri", "Michael Berrer", "Donald Young", "Igor Sijsling",
            "Robin Haase", "Martin Klizan", "Steve Johnson", "Sergiy Stakhovsky",
            "Denis Istomin", "Jack Sock", "Victor Hanescu", "Diego Schwartzman",
            "Novak Djokovic", "Albert Ramos-Vinolas", "John Isner", "Kevin Anderson",
            "Juan Monaco", "Thanasi Kokkinakis", "Bernard Tomic", "David Ferrer",
            "Andy Murray", "Philipp Kohlschreiber", "Adrian Mannarino", "Ernests Gulbis",
            "Feliciano Lopez", "Pablo Cuevas", "Fernando Verdasco", "Kei Nishikori",
            "Milos Raonic", "Alexandr Dolgopolov", "Tommy Robredo", "Grigor Dimitrov",
            "Gilles Simon", "Michael Berrer", "Donald Young", "Rafael Nadal",
            "Robin Haase", "Lukas Rosol", "Steve Johnson", "Tomas Berdych",
            "Roberto Bautista Agut", "Jack Sock", "Andreas Seppi", "Roger Federer",
            "Novak Djokovic", "John Isner", "Thanasi Kokkinakis", "Bernard Tomic",
            "Andy Murray", "Adrian Mannarino", "Feliciano Lopez", "Kei Nishikori",
            "Milos Raonic", "Tommy Robredo", "Gilles Simon", "Rafael Nadal",
            "Lukas Rosol", "Tomas Berdych", "Jack Sock", "Roger Federer",
            "Novak Djokovic", "Bernard Tomic", "Andy Murray", "Feliciano Lopez",
            "Milos Raonic", "Rafael Nadal", "Tomas Berdych", "Roger Federer",
            "Novak Djokovic", "Andy Murray", "Milos Raonic", "Roger Federer",
            "Novak Djokovic", "Roger Federer", "Novak Djokovic")

inwells15.compare <- cbind(inwells15.compare, Winner)
inwells15.compare$Topplayer <- as.character(inwells15.compare$Topplayer)
inwells15.compare$Botplayer <- as.character(inwells15.compare$Botplayer)
inwells15.compare$Winner <- as.character(inwells15.compare$Winner)


for(k in 1:nrow(inwells15.compare)){
  if(inwells15.compare$predictinwells15[[k]]>.5){
    if(inwells15.compare$Topplayer[[k]]==inwells15.compare$Winner[[k]]){
      inwells15.compare$Accuracy[[k]] <- 1
    } else{
      inwells15.compare$Accuracy[[k]] <- 0
    }
  }
  if(inwells15.compare$predictinwells15[[k]]<.5){
    if(inwells15.compare$Botplayer[[k]]==inwells15.compare$Winner[[k]]){
      inwells15.compare$Accuracy[[k]] <- 1
    } else{
      inwells15.compare$Accuracy[[k]] <- 0
    }
  }
}

sum(inwells15.compare$Accuracy)/nrow(inwells15.compare)

#using the data from 2013, 2014, and 2015: 67.36842% accuracy