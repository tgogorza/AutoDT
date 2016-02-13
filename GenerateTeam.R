# GenerateTeam <- function(stats){
#   library(genalg)
#   
#   #Solve using Genetic Algorithm
#   #evalutaion <- function(x){ sample(x = c(0,1), size = 890, prob = c(98.32,1.68), replace=TRUE) }
#   solution <- rbga.bin(size=nrow(stats),zeroToOneRatio = 60,popSize=200, iters=50,
#                        mutationChance=0.02, evalFunc=EvalFunc)
#   last <- solution$population[200,]
#   length(last[last != 0])
#   team <- stats[which(last == 1),]
#   
# }

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

# fitnessFunc <- function(chromosome){
#     
#     if(sum(chromosome$valor) > 60000000)
#         return(Inf)
#     
#     #players <- stats[which(players == 1),]
#     ARQ <- getPlayersByPos(chromosome,"ARQ")
#     ARQfit <- sum(as.numeric(ARQ$PT)) + sum(as.numeric(ARQ$PJ)) + sum(as.numeric(ARQ$F))
#     DEF <- getPlayersByPos(chromosome,"DEF")
#     DEFfit <- sum(as.numeric(DEF$PT)) + sum(as.numeric(DEF$PJ)) + sum(as.numeric(DEF$F))
#     MED <- getPlayersByPos(chromosome,"MED")
#     MEDfit <- sum(as.numeric(MED$PT)) + sum(as.numeric(MED$PJ)) + sum(as.numeric(MED$F))
#     DEL <- getPlayersByPos(chromosome,"DEL")
#     DELfit <- sum(as.numeric(DEL$PT)) + sum(as.numeric(DEL$PJ)) + sum(as.numeric(DEL$F))
#   
#     fit <- ARQ + DEF + MED + DEL
#     
#     return (-fit)
# }

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
    
    ARQfit + DEFfit + VOLfit + DELfit
    
}

crossoverFunc <- function(ga, indexes){
    
    splitIndex <- as.integer(runif(1,1,dim(chromo1)[1]))
    temp <- chromo1[1:splitIndex,]
    chromo1[1:splitIndex,] <- chromo2[1:splitIndex,]
    chromo2[1:splitIndex,] <- temp
    
}


mutatePlayer <- function(chromosome) {
    playerOutIndex <- runif(1,1,dim(chromosome)[1])
    #players <- getPlayersByPos(stats.norm,chromosome[playerOutIndex,]$Pos)
    #playerInIndex <- runif(1,1,dim(players)[1])
    playerInIndex <- runif(1,1,dim(stats.norm)[1])
    chromosome[playerOutIndex] <- playerInIndex
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

InitPopulation <- function(ga){
    size <- ga@popSize
    
}

library(GA)
setwd("D:/workspace/AutoDT")
source("GetPlayerStats.R")

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
stats.norm <-stats.norm %>% mutate_each_(funs(normalize),vars=c("PT","Prom","Reg","PJ","F","GC","GP","GR","EC","VI","A","R","PE","PA")) 

GA <- ga(type = "real-valued",
   min = rep(1,each=15),
   max = rep(999,each=15),
   parallel = TRUE,
   fitness = fitnessFunc,
   population = gareal_Population, #gaperm_Population,
   selection = gareal_rwSelection, #gaperm_rwSelection,
   crossover = gareal_laCrossover, #gaperm_pbxCrossover,
   mutation = gareal_nraMutation, #gaperm_swMutation,
   popSize = 300, pcrossover = 0.8, pmutation = 0.1,
   elitism = max(1, round(popSize * 0.05)),
   monitor = gaMonitor, maxiter = 300, run = 150,
   names = paste0("x",1:15))

summary(GA)
sol <- as.integer(GA@solution[1,])
finalteam <- getPlayersByID(stats.norm,sol)
sum(finalteam$Valor)
finalteam
