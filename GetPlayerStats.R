library(plyr)
library(dplyr)
library(httr)
library(XML)

GetPlayerStats <- function(){
  
  teamsurl <- "http://www.ungrandt.com.ar/planteles/"
  #Get teams
  teams <- read.csv("teams.csv")
  #Scrape teams player stats
  stats <- lapply(teams$team,GetTeamPlayers)
  stats <- ldply(stats, data.frame)
  #Add player IDs
  #stats <- data.frame(ID = rownames(stats),stats)
  names(stats)[1] <- "Pos"
  #Write stats to a csv file
  write.csv(stats,"playerstats.csv")
  return(stats) 
}

##Input: a team name
##Gets a given teams players' stats
##Output: table of players stats
GetTeamPlayers <- function(team){
  address <- paste(teamsurl,team,sep = "")
  list <- readHTMLTable(address)
  if(length(list) > 1){
    players <- readHTMLTable(address)[[2]]  
  }
  else{
      players <- readHTMLTable(address)[[1]]      
  }
    
  players <- mutate(players,equipo = team)
}