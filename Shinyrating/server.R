#shiny app for player ratings#
library(shiny)
library(ggplot2)
library(lubridate)

elo <- read.csv("data/Full_elo.csv")
elo$Month <- as.Date(elo$Month, "%Y-%m-%d")

shinyServer(function(input, output) {
  
  output$pltable1 <- renderDataTable({
    elo[elo$Player==input$player1,]
  })
  
  output$pltable2 <- renderDataTable({
    elo[elo$Player==input$player2,]
  })
  
  plotdata = reactive({
    if(input$checkplayer2==T){
      indrating <- rbind(elo[elo$Player==input$player1,],
                         elo[elo$Player==input$player2,])
    } else{
      indrating <- elo[elo$Player==input$player1,]
    }
    return(indrating)
  })
  
  output$plPlot <- renderPlot({
    indrating = plotdata()
    ggplot(indrating) + 
      ylab("Rating") +
      xlab("Time") +
      geom_point(aes(x = Month, y = Rating, group = Player, color = Player)) + 
      geom_line(aes(x = Month, y = Rating, group = Player, color = Player)) +
      ggtitle("Rating History") +
      #geom_line(indrating2, aes(x=Month, y=Rating)) +
      ylim(c(min(elo$Rating), max(elo$Rating)))
      #geom_line(indrating2, aes(x=Month, y=Rating)) + geom_point()
    #+ geom_line(data=indrating1, aes(x=indrating1$Month, y=indrating1$Rating))
                         #ylab="Rating", xlab="Time", ylim(c(min(data$Rating),
                          #                                  max(data$Rating))))
                         
    #qplot(x=indrating1$Month, y=indrating1$Rating, ylab="Rating", xlab="Time",
    #            main="Rating History") + geom_line() + ylim(c(min(data$Rating),
    #                                                        max(data$Rating)))
    
  })
  
})