
nfl_week1 <- read.csv(file.choose())
nfl_week1 <- nfl_week1[-c(4)]

n <- length(nfl_week1[,1])
rec_data <- read.csv(file.choose())
pass_data <- read.csv(file.choose())
rush_data <- read.csv(file.choose())
rec_data$Player <- lapply(rec_data$Player, as.character)
pass_data$Player <- lapply(pass_data$Player, as.character)
rush_data$Player <- lapply(rush_data$Player, as.character)

for (i in 1:n){
  if (nfl_week1$Name[i] %in% rec_data$Player){
    index <- which(rec_data$Player == nfl_week1$Name[i])
    nfl_week1$rec_yards_per_game[i] <- rec_data$rec_yards_per_game[index]
    nfl_week1$rec_TD[i] <- rec_data$rec_TD[index]
  }
  else{
    nfl_week1$rec_yards_per_game[i] <- NA
    nfl_week1$rec_TD[i] <- NA
  }
}

for (i in 1:n){
  if (nfl_week1$Name[i] %in% pass_data$Player){
    index <- which(pass_data$Player == nfl_week1$Name[i])
    nfl_week1$pass_complete[i] <- pass_data$pass_complete.[index]
    nfl_week1$avg_pass_yards_per_game[i] <- pass_data$avg_pass_yards_per_game[index]
    nfl_week1$passing_TD[i] <- pass_data$passing_TD[index]
  }
  else{
    nfl_week1$pass_complete[i] <- NA
    nfl_week1$avg_pass_yards_per_game[i] <- NA
    nfl_week1$passing_TD[i] <- NA
  }
}


for (i in 1:n){
  if (nfl_week1$Name[i] %in% rush_data$Player){
    index <- which(rush_data$Player == nfl_week1$Name[i])
    nfl_week1$avg_rush_yards_per_game[i] <- rush_data$avg_rush_yards_per_game[index]
    nfl_week1$rush_TD[i] <- rush_data$rush_TD[index]
  }
  else{
    nfl_week1$avg_rush_yards_per_game[i] <- NA
    nfl_week1$rush_TD[i] <- NA
  }
}

nfl_week1 <- nfl_week1[-c(7)]
names(nfl_week1) <- c("Position", "Name", "Salary", "AvgPointsPerGame", "AvgRecYardsPerGame", "RecTDs",
                      "AvgPassYardsPerGame", "PassTDs", "AvgRushYardsPerGame", "RushTDs")

write.csv(nfl_week1, file="nfl_week1_DATA.csv")

rbs = subset(nfl_week1, Position=="RB", 1:4)
dst = subset(nfl_week1, Position=="DST", 1:4)
qbs = subset(nfl_week1, Position=="QB", 1:4)
tes = subset(nfl_week1, Position=="TE", 1:4)
wrs = subset(nfl_week1, Position=="WR", 1:4)

