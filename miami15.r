#Miami 2015 Predictions

Topplayer <- c("Paolo Lorenzi","Malek Jaziri","Lleyton Hewitt","Ricardas Berankis",
               "Mikhail Kukushkin","Michael Berrer","Sam Groth","Jiri Vesely",
               "Andrey Golubev","Marcos Baghdatis","Andreas Haider-Maurer","Jerzy Janowicz",
               "Vasek Pospisil","Andrey Rublev","Ryan Harrison","Pablo Andujar",
               "Thanasi Kokkinakis","Joao Sousa","Go Soeda","Dominic Thiem",
               "Victor Estrella Burgos","Jarkko Nieminen","Kyle Edmund","Donald Young",
               "Marcel Granollers","Denis Istomin","Filip Krajinovic","Tim Smyczek",
               "Ruben Bemelmans","Jan-Lennard Struff","Damir Dzumhur","Sergiy Stakhovsky",
               "Novak Djokovic","Steve Darcis","Pablo Cuevas","Alexandr Dolgopolov",
               "Gilles Simon","Alejandro Falla","Lukas Rosol","Federico Delbonis",
               "Kei Nishikori","Simone Bolelli","David Goffin","Jerzy Janowicz",
               "Grigor Dimitrov","Andrey Rublev","Jeremy Chardy","Teymuraz Gabashvili",
               "Stan Wawrinka","Albert Ramos-Vinolas","Fabio Fognini","Dominic Thiem",
               "Kevin Anderson","Jarkko Nieminen","Santiago Giraldo","Donald Young",
               "Tomas Berdych","Austin Krajicek","Gael Monfils","Tim Smyczek",
               "Ernests Gulbis","Jan-Lennard Struff","Fernando Verdasco","Nicolas Almagro",
               "Novak Djokovic","Thomaz Bellucci","Gilles Simon","Lukas Rosol",
               "Kei Nishikori","David Goffin","Grigor Dimitrov","Jeremy Chardy",
               "Stan Wawrinka","Jack Sock","Kevin Anderson","Santiago Giraldo",
               "Tomas Berdych","Gael Monfils","Juan Monaco","Fernando Verdasco",
               "Novak Djokovic","Gilles Simon","Kei Nishikori","John Isner",
               "Adrian Mannarino","Kevin Anderson","Tomas Berdych","Juan Monaco",
               "Novak Djokovic","Kei Nishikori","Dominic Thiem","Tomas Berdych",
               "Novak Djokovic","Andy Murray")

Botplayer <- c("Martin Klizan","Steve Darcis","Thomaz Bellucci","Alexandr Dolgopolov",
               "Steve Johnson","Alejandro Falla","Alexander Zverev","Federico Delbonis",
               "Mikhail Youzhny","Simone Bolelli","Borna Coric","Edouard Roger-Vasselin",
               "Juan Martin Del Potro","Pablo Carreno Busta","Jurgen Melzer","Teymuraz Gabashvili",
               "Carlos Berlocq","Albert Ramos-Vinolas","Jack Sock","Diego Schwartzman",
               "Sam Querrey","Marinko Matosevic","Robin Haase","Yen-Hsun Lu",
               "Hyeon Chung","Austin Krajicek","Dusan Lajovic","Adrian Menendez-Maceiras",
               "Juan Monaco","Benjamin Becker","James Duckworth","Nicolas Almagro",
               "Martin Klizan","Gilles Muller","Thomaz Bellucci","Tommy Robredo",
               "Mikhail Kukushkin","Ivo Karlovic","Alexander Zverev","David Ferrer",
               "Mikhail Youzhny","Viktor Troicki","Borna Coric","Roberto Bautista Agut",
               "Vasek Pospisil","John Isner","Jurgen Melzer","Milos Raonic",
               "Carlos Berlocq","Adrian Mannarino","Jack Sock","Feliciano Lopez",
               "Sam Querrey","Leonardo Mayer","Robin Haase","Andy Murray",
               "Hyeon Chung","Bernard Tomic","Filip Krajinovic","Jo-Wilfried Tsonga",
               "Juan Monaco","Guillermo Garcia-Lopez","James Duckworth","Rafael Nadal",
               "Steve Darcis","Alexandr Dolgopolov","Alejandro Falla","David Ferrer",
               "Viktor Troicki","Jerzy Janowicz","John Isner","Milos Raonic",
               "Adrian Mannarino","Dominic Thiem","Leonardo Mayer","Andy Murray",
               "Bernard Tomic","Jo-Wilfried Tsonga","Guillermo Garcia-Lopez","Rafael Nadal",
               "Alexandr Dolgopolov","David Ferrer","David Goffin","Milos Raonic",
               "Dominic Thiem","Andy Murray","Gael Monfils","Fernando Verdasco",
               "David Ferrer","John Isner","Andy Murray","Juan Monaco",
               "John Isner","Tomas Berdych")

miami15 <- as.data.frame(cbind(Topplayer, Botplayer))
miami15$Day <- NA
miami15 <- miami15[c(3,1,2)]

#2 Lag player ratings#
predictmiami15 <- predict(ratings2015, miami15, tng=2, gamma=0)

#Lag on 2014 matches#
predictmiami15 <- predict(ratings2015lag1, miami15, tng=0, gamma=0)

#Lag elo#
predictmiami15 <- predict(lastyearelo, miami15, tng=15, gamma=0)

#No lag elo#
predictmiami15 <- predict(alltourns3elo, miami15, tng=15, gamma=0)
predictmiami15 <- predict(lastyearelon, miami15, tng=15, gamma=0)

#Glicko rating#
predictinwells15 <- predict(allglicko, inwells15, tng=15, gamma=0)

predictmiami15 <- as.data.frame(predictmiami15)
miami15.compare <- cbind(miami15, predictmiami15)
miami15.compare <- miami15.compare[,-1]

#for loop for 2 lag elo#
for(k in 1:nrow(miami15.compare)){
  if(is.na(miami15.compare$predictmiami15[[k]])==T){
    if(((nrow(matches2015[matches2015$Player==miami15.compare$Topplayer[[k]],])+
           nrow(matches2015[matches2015$Opponent==miami15.compare$Topplayer[[k]],]))<1) &
         ((nrow(matches2015[matches2015$Player==miami15.compare$Botplayer[[k]],])+
             nrow(matches2015[matches2015$Opponent==miami15.compare$Botplayer[[k]],]))>=1)){
      miami15.compare$predictmiami15[[k]] <- 0
    }
    if(((nrow(matches2015[matches2015$Player==miami15.compare$Topplayer[[k]],])+
           nrow(matches2015[matches2015$Opponent==miami15.compare$Topplayer[[k]],]))>=1) &
         ((nrow(matches2015[matches2015$Player==miami15.compare$Botplayer[[k]],])+
             nrow(matches2015[matches2015$Opponent==miami15.compare$Botplayer[[k]],]))<1)){
      miami15.compare$predictmiami15[[k]] <- 1
    }
    if(((nrow(matches2015[matches2015$Player==miami15.compare$Topplayer[[k]],])+
           nrow(matches2015[matches2015$Opponent==miami15.compare$Topplayer[[k]],]))<1) &
         ((nrow(matches2015[matches2015$Player==miami15.compare$Botplayer[[k]],])+
             nrow(matches2015[matches2015$Opponent==miami15.compare$Botplayer[[k]],]))<1)){
      miami15.compare$predictmiami15[[k]] <- 0.5
    }
  }
}

#for loop for lag elo#
for(k in 1:nrow(miami15.compare)){
  if(is.na(miami15.compare$predictmiami15[[k]])==T){
    if(((nrow(lastyear[lastyear$Player==miami15.compare$Topplayer[[k]],])+
           nrow(lastyear[lastyear$Opponent==miami15.compare$Topplayer[[k]],]))<15) &
         ((nrow(lastyear[lastyear$Player==miami15.compare$Botplayer[[k]],])+
             nrow(lastyear[lastyear$Opponent==miami15.compare$Botplayer[[k]],]))>=15)){
      miami15.compare$predictmiami15[[k]] <- 0
    }
    if(((nrow(lastyear[lastyear$Player==miami15.compare$Topplayer[[k]],])+
           nrow(lastyear[lastyear$Opponent==miami15.compare$Topplayer[[k]],]))>=15) &
         ((nrow(lastyear[lastyear$Player==miami15.compare$Botplayer[[k]],])+
             nrow(lastyear[lastyear$Opponent==miami15.compare$Botplayer[[k]],]))<15)){
      miami15.compare$predictmiami15[[k]] <- 1
    }
    if(((nrow(lastyear[lastyear$Player==miami15.compare$Topplayer[[k]],])+
           nrow(lastyear[lastyear$Opponent==miami15.compare$Topplayer[[k]],]))<15) &
         ((nrow(lastyear[lastyear$Player==miami15.compare$Botplayer[[k]],])+
             nrow(lastyear[lastyear$Opponent==miami15.compare$Botplayer[[k]],]))<15)){
      miami15.compare$predictmiami15[[k]] <- 0.5
    }
  }
}

#for loop for no lag elo#
for(k in 1:nrow(miami15.compare)){
  if(is.na(miami15.compare$predictmiami15[[k]])==T){
    if(((nrow(alltourns3[alltourns3$Player==miami15.compare$Topplayer[[k]],])+
           nrow(alltourns3[alltourns3$Opponent==miami15.compare$Topplayer[[k]],]))<15) &
         ((nrow(alltourns3[alltourns3$Player==miami15.compare$Botplayer[[k]],])+
             nrow(alltourns3[alltourns3$Opponent==miami15.compare$Botplayer[[k]],]))>=15)){
      miami15.compare$predictmiami15[[k]] <- 0
    }
    if(((nrow(alltourns3[alltourns3$Player==miami15.compare$Topplayer[[k]],])+
           nrow(alltourns3[alltourns3$Opponent==miami15.compare$Topplayer[[k]],]))>=15) &
         ((nrow(alltourns3[alltourns3$Player==miami15.compare$Botplayer[[k]],])+
             nrow(alltourns3[alltourns3$Opponent==miami15.compare$Botplayer[[k]],]))<15)){
      miami15.compare$predictmiami15[[k]] <- 1
    }
    if(((nrow(alltourns3[alltourns3$Player==miami15.compare$Topplayer[[k]],])+
           nrow(alltourns3[alltourns3$Opponent==miami15.compare$Topplayer[[k]],]))<15) &
         ((nrow(alltourns3[alltourns3$Player==miami15.compare$Botplayer[[k]],])+
             nrow(alltourns3[alltourns3$Opponent==miami15.compare$Botplayer[[k]],]))<15)){
      miami15.compare$predictmiami15[[k]] <- 0.5
    }
  }
}

Winner <- c("Martin Klizan","Steve Darcis","Thomaz Bellucci","Alexandr Dolgopolov",
            "Mikhail Kukushkin","Alejandro Falla","Alexander Zverev","Federico Delbonis",
            "Mikhail Youzhny","Simone Bolelli","Borna Coric","Jerzy Janowicz",
            "Vasek Pospisil","Andrey Rublev","Jurgen Melzer","Teymuraz Gabashvili",
            "Carlos Berlocq","Albert Ramos-Vinolas","Jack Sock","Dominic Thiem",
            "Sam Querrey","Jarkko Nieminen","Robin Haase","Donald Young",
            "Hyeon Chung","Austin Krajicek","Filip Krajinovic","Tim Smyczek",
            "Juan Monaco","Jan-Lennard Struff","James Duckworth","Nicolas Almagro",
            "Novak Djokovic","Steve Darcis","Thomaz Bellucci","Alexandr Dolgopolov",
            "Gilles Simon","Alejandro Falla","Lukas Rosol","David Ferrer",
            "Kei Nishikori","Viktor Troicki","David Goffin","Jerzy Janowicz",
            "Grigor Dimitrov","John Isner","Jeremy Chardy","Milos Raonic",
            "Stan Wawrinka","Adrian Mannarino","Jack Sock","Dominic Thiem",
            "Kevin Anderson","Leonardo Mayer","Santiago Giraldo","Andy Murray",
            "Tomas Berdych","Bernard Tomic","Gael Monfils","Jo-Wilfried Tsonga",
            "Juan Monaco","Guillermo Garcia-Lopez","Fernando Verdasco","Rafael Nadal",
            "Novak Djokovic","Alexandr Dolgopolov","Gilles Simon","David Ferrer",
            "Kei Nishikori","David Goffin","John Isner","Milos Raonic",
            "Adrian Mannarino","Dominic Thiem","Kevin Anderson","Andy Murray",
            "Tomas Berdych","Gael Monfils","Juan Monaco","Fernando Verdasco",
            "Novak Djokovic","David Ferrer","Kei Nishikori","John Isner",
            "Dominic Thiem","Andy Murray","Tomas Berdych","Juan Monaco",
            "Novak Djokovic","John Isner","Andy Murray","Tomas Berdych",
            "a","a")

miami15.compare <- cbind(miami15.compare, Winner)
miami15.compare$Topplayer <- as.character(miami15.compare$Topplayer)
miami15.compare$Botplayer <- as.character(miami15.compare$Botplayer)
miami15.compare$Winner <- as.character(miami15.compare$Winner)


for(k in 1:nrow(miami15.compare)){
  if(miami15.compare$predictmiami15[[k]]>.5){
    if(miami15.compare$Topplayer[[k]]==miami15.compare$Winner[[k]]){
      miami15.compare$Accuracy[[k]] <- 1
    } else{
      miami15.compare$Accuracy[[k]] <- 0
    }
  }
  if(miami15.compare$predictmiami15[[k]]<.5){
    if(miami15.compare$Botplayer[[k]]==miami15.compare$Winner[[k]]){
      miami15.compare$Accuracy[[k]] <- 1
    } else{
      miami15.compare$Accuracy[[k]] <- 0
    }
  }
}

sum(miami15.compare$Accuracy)/nrow(miami15.compare)

#using the data from 2013, 2014, and 2015: 67.36842% accuracy