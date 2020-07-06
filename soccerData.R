load("Summer2k20/soccerStuff/data/portugal.rda")

library(ggplot2)

for(i in 1:length(portugal$home)) {
  if(portugal$home[i] == "Feirense ") {
    portugal$home[i] == "Feirense"
  }
  
  if(portugal$visitor[i] == "Feirense ") {
    portugal$visitor[i] == "Feirense"
  }
}
  

#portugal <- data.frame(lapply(portugal, function(x) {gsub("Feirense ", "Feirense", x)}))


#Makes a dataframe with 3 columns: Season, Number of Wins that season, and team name given, the reason benfica is the argument 
findWinsSeasons <- function(teamName) {
  benfica <- cbind(unique(portugal$Season),rep(0,length(unique(portugal$Season))), rep(teamName,length(unique(portugal$Season))))
  colnames(benfica) <- c("Season","Wins", "Team")
  benfica <- as.data.frame(benfica)
  benfica$Wins <- as.numeric(benfica$Wins)
  
  for(i in 1:length(portugal$Season)) {
    
    if(portugal$home[i] == teamName || portugal$visitor[i] == teamName) {
      for(j in 1:length(benfica[,1])) {
        if(portugal$Season[i] == benfica[j,1]) {
          if(portugal$result[i] == "A" && portugal$visitor[i] == teamName || portugal$result[i] == "H" && portugal$home[i] == teamName) {
            benfica[j,2] = benfica[j,2]+1
          }
        }
      }
    }
  }
  return(benfica)
}


totalTeams <- unique(c(unique(portugal$home), unique(portugal$visitor)))
totalTable <- findWinsSeasons(totalTeams[1])

for(i in 2:length(totalTeams)) {
  totalTable <- rbind(totalTable, findWinsSeasons(totalTeams[i]))
}

totalTable <- as.data.frame(totalTable)
colnames(totalTable) <- c("Season","Wins","Team")


subsetTeams <- totalTable[totalTable$Team %in% c("Benfica", "Porto", "Sp Lisbon"),]

subsetYears <- totalTable[totalTable$Season %in% 1994:1996,]


ggplot(data=subsetTeams, aes(x=Season, y=Wins, group =Team, colour=factor(Team))) +
  geom_line()+
  geom_point()


save(totalTeams, totalTable, file = "por.RData")
