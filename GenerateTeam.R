library(GA)
library(dplyr)
setwd("D:/workspace/AutoDT")
source("GetPlayerStats.R")

normalize <- function(values) {
    max <- max(values)
    min <- min(values)
    ret <- (values - min) / (max - min)
}

getPlayersByPos<- function(players,pos){
    filter(players,Pos == pos)
}

getPlayersByID <- function(players,ids){
    filter(players,ID %in% ids)
}

getRandomPlayers <- function(players, pos, count = 1){
    playersbypos <- getPlayersByPos(players,pos)
    indexes <- runif(n = count, min = 1, max = dim(playersbypos)[1])
    playersbypos[indexes,"ID"]
}

fitnessFunc <- function(chromosome){
    
    players <- getPlayersByID(stats.norm,as.integer(chromosome))
    if(sum(players$Valor) > 75000000)
        return(-9000)
    
    #players <- stats[which(players == 1),]
    ARQ <- getPlayersByPos(players,"ARQ")
    ARQfit <- sum(ARQ$PT) + sum(3*ARQ$PJ) + sum(2*ARQ$F) + sum(ARQ$GP) + sum(4*ARQ$VI) + sum(ARQ$PA) - sum(ARQ$A) - sum(2*ARQ$GR)  
    DEF <- getPlayersByPos(players,"DEF")
    DEFfit <- sum(DEF$PT) + sum(3*DEF$PJ) + sum(2*DEF$F) + sum(DEF$GP) + sum(DEF$GC) + sum(4*DEF$VI) - sum(DEF$A) - sum(2*DEF$R)
    VOL <- getPlayersByPos(players,"VOL")
    VOLfit <- sum(2*VOL$PT) + sum(3*VOL$PJ) + sum(3*VOL$F) + sum(VOL$GP) + sum(3*VOL$GC) - sum(VOL$A) - sum(2*VOL$R)
    DEL <- getPlayersByPos(players,"DEL")
    DELfit <- sum(DEL$PT) + sum(3*DEL$PJ) + sum(3*DEL$F) + sum(DEL$GP) + sum(4*DEL$GC) - sum(DEL$A) - sum(2*DEL$R)
    
    #if((dim(ARQ)[1] != 2) || (dim(DEF)[1] > 5 || dim(DEF)[1] < 4) || (dim(VOL)[1] > 5 || dim(VOL)[1] < 4) || (dim(DEL)[1] > 4 || dim(DEL)[1] < 3))
    #    return(-9000)
    
    ARQfit + DEFfit + VOLfit + DELfit
}

crossoverFunc <- function(ga, parents){
    parents
    p1 <- parents[1]
    p2 <- parents[2]
    splitIndex <- as.integer(runif(1,1,length(p1)))
    child1 <- p1
    child2 <- p2
    temp <- p1[1:splitIndex]
    child1[1:splitIndex] <- child2[1:splitIndex]
    child2[1:splitIndex] <- temp
    
    list(c(child1,child2),c(NA,NA))
}

mutationFunc <- function(ga,chromosome) {
    playerOutIndex <- as.integer(runif(1,1,length(chromosome)))
    #Get player position
    pos <- getPlayersByID(stats.norm,c(chromosome[playerOutIndex]))$Pos
    #Get all players on that position
    players <- getPlayersByPos(stats.norm,pos)
    #Get a new player
    playerInID <- sample(players$ID,1)
    #Replace player
    chromosome[playerOutIndex] <- playerInID
    chromosome
}

# mutationOp <- function(ga,indexes) {
#     playerstomutate <- ga$population()
#     playerOutIndex <- runif(1,1,dim(chromosome)[1])
#     players <- getPlayersByPos(stats.norm,chromosome[playerOutIndex,]$Pos)
#     playerInIndex <- runif(1,1,dim(players)[1])
#     chromosome[playerOutIndex,] <- players[playerInIndex,]
#     chromosome
# }

InitPopulation <- function(ga,tactic = "343"){
    size <- ga@popSize
    t(replicate(size,generateTeam(tactic)))
    
}

generateTeam <- function(tactic){
    
    arqcount <- 2; defcount <- 0; volcount <- 0; delcount <- 0
    switch(tactic,
           "442" =  {
               defcount <- 5; volcount <- 5; delcount <- 3             
           },
           "343" =  {
               defcount <- 4; volcount <- 5; delcount <- 4             
           },
           "433" =  {
               defcount <- 5; volcount <- 4; delcount <- 4             
           }
    )
    
    arq <- getRandomPlayers(stats.norm,"ARQ",arqcount)
    def <- getRandomPlayers(stats.norm,"DEF",defcount)
    vol <- getRandomPlayers(stats.norm,"VOL",volcount)
    del <- getRandomPlayers(stats.norm,"DEL",delcount)
    
    c(arq,def,vol,del)
}

#Read Stats
#stats <- GetPlayerStats()
stats <- read.csv("playerstats.csv",stringsAsFactors = FALSE)
names(stats)[1] <- "ID"
#Format Data
stats$Prom <- as.numeric(gsub(",",".",stats$Prom))
stats$Reg <- as.numeric(gsub(",",".",stats$Reg))
stats$Valor <- gsub("[$]","",stats$Valor)
stats$Valor <- as.numeric(gsub("[.]","",stats$Valor))

#Normalize or scale stats
stats.norm <- stats
#maybe scale instead of normalizing?
stats.norm <- stats.norm %>% mutate_each_(funs(normalize),vars=c("PT","Prom","Reg","PJ","F","GC","GP","GR","EC","VI","A","R","PE","PA")) 
#stats.norm <- stats.norm %>% mutate_each_(funs(as.numeric(scale(.))),vars=c("PT","Prom","Reg","PJ","F","GC","GP","GR","EC","VI","A","R","PE","PA"))

formation <- "343"
pSize <- 500
executions <- 1

solPlayers <- {}

for (i in 1:executions) {
    
    GA <- ga(type = "real-valued",
             min = rep(min(stats.norm$ID),each=15),
             max = rep(max(stats.norm$ID),each=15),
             parallel = TRUE,
             fitness = fitnessFunc,
             population =  function(...) InitPopulation(...,formation), #gareal_Population, 
             selection = gareal_lrSelection, 
             crossover = gareal_spCrossover,
             mutation = mutationFunc, #gareal_nraMutation,
             popSize = pSize, pcrossover = 0.8, pmutation = 0.05,
             elitism = max(1, round(pSize * 0.05)),
             monitor = gaMonitor, maxiter = 200, run = 150,
             names = paste0("x",1:15))
    
    
    #summary(GA)
    #Get GA solution
    sol <- as.integer(GA@solution[1,])
    #Append to final list of players
    solPlayers <- c(solPlayers,sol)
}

finalList <- count(solPlayers)
finalList <- finalList[with(finalList,order(-freq)),]

finalTeam <- stats[finalList$x,]

#sum(finalteam$Valor)
#finalteam[with(finalteam,order(Pos)),]