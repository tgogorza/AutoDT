# AutoDT
Automatic Argentina Fantasy Soccer GranDT team selector
This weekend project scrapes players statistics and uses genetic programming to create a ranking of best candidates for a fantasy soccer team

- GetPlayerStats: Scrapes a web site and generates a csv file with player stats for all the teams in Argentina First Division
- GenerateTeam: Runs a genetic algorithm and returns a list of the players that best rank for creating a Fantasy Soccer team

## Selection Algorithm
The genetic algorithm creates several random teams as initial population and runs an evolutive process by mixing and mutating teams for several generations and taking the team that best evolves as the optimal team. The fitness function used to evaluate how good a team is takes into account team budget, players positions, goals, bookings, man of match, and many other player stats. 
This algorithm is executed several times and outputs a list of players sorted by number of appearances in the optimal team of each execution of the algorithm.

Output example:  
| Rank | Player | Best-Team |  
| --- | --- | --- |  
| 1 | Carlos Tevez | 59 |  
| 2 | Lisandro Lopez | 42 |  
| 3 | Maxi Rodriguez | 37 |  

This means that after running the selection algorithm several times (remember each time the algorithm is run it outputs 1 optimal team of 15 players) we can see that Carlitos Tevez was selected in the optimal team in 59 executions of the algorithm, Licha Lopez got drafted for the best team 42 times, and Maxi Rodriguez got picked 37 times by the evolutive process.
