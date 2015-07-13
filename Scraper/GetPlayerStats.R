library(plyr)
library(dplyr)
library(httr)
library(XML)

url <- "http://www.ungrandt.com.ar/planteles/"
#Get teams
teams <- read.csv("teams.csv")
#Scrape teams player stats
stats <- lapply(teams$team,GetTeamPlayers)
stats <- ldply(stats, data.frame)
#Write stats to a csv file
write.csv(stats,"playerstats.csv")
return(stats)

##Input: a team name
##Gets a given teams players' stats
##Output: table of players stats
GetTeamPlayers <- function(team){
  address <- paste(url,team,sep = "")
  players <- readHTMLTable(address)[[1]]
  players <- mutate(players,equipo = team)
}