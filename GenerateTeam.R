GenerateTeam <- function(stats){
  library(genalg)
  
  #Solve using Genetic Algorithm
  #evalutaion <- function(x){ sample(x = c(0,1), size = 890, prob = c(98.32,1.68), replace=TRUE) }
  solution <- rbga.bin(size=nrow(stats),zeroToOneRatio = 60,popSize=200, iters=50,
                       mutationChance=0.02, evalFunc=EvalFunc)
  last <- solution$population[200,]
  length(last[last != 0])
  team <- stats[which(last == 1),]
  
}

EvalFunc <- function(players){
  players <- stats[which(players == 1),]
  value <- sum(as.numeric(players$PT)) + sum(as.numeric(players$PJ)) + sum(as.numeric(players$F))
  return (-value)
}