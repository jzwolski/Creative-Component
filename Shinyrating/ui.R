#shiny app for player ratings#
library(shiny)
library(ggplot2)
library(lubridate)

#make the choices box a search box as well
#graph 2 players on same graph (have to make range of x-axis the min and max of the dates)
#create a tab to see the plot and the data table

elo <- read.csv("data/Full_elo.csv")

shinyUI(fluidPage(
  titlePanel("Tennis Ratings"),
  
  sidebarLayout(
    sidebarPanel(
      width=3,
      selectInput("player1", #what we will call in the function
                  "Player 1", #label for the display
                  choices = sort(unique(as.character(elo$Player))),
                  selected = "Andy Murray"
      ),
      selectInput("player2", #what we will call in the function
                  "Player 2", #label for the display
                  choices = sort(unique(as.character(elo$Player))),
                  #selected = sort(unique(as.character(elo$Player)))[2],
                  selected = "Roger Federer"
      ),
      checkboxInput("checkplayer2",
                    "Add Player2",
                    value=F
      )
    ), 
   
    mainPanel(
      width=9,
      tabsetPanel(type = "tabs",
        tabPanel("Plot", plotOutput("plPlot")),
        tabPanel("Player 1 Data", dataTableOutput("pltable1")),
        tabPanel("Player 2 Data", dataTableOutput("pltable2"))
      )
    )
  )
))