#Optimize draftKings baseball




#Download CSV from DraftKings and read it in to "baseball"
baseball = read.csv(file.choose())
baseball$PointSalaryRatio <- 1000*(baseball$AvgPointsPerGame/baseball$Salary)
baseball$Index <- 1:length(baseball[, 1])
team <- baseball

#Subset data based on position
P <- subset(team, Position == "SP", 1:7)
C <- subset(team, Position == "C", 1:7)
B1 <- subset(team, Position == "1B", 1:7)
B2 <- subset(team, Position == "2B", 1:7)
B3 <- subset(team, Position == "3B", 1:7)
SS <- subset(team, Position == "SS", 1:7)
OF <- subset(team, Position == "OF", 1:7)
nums <- c(nrow(P), nrow(C), nrow(B1), nrow(B2), nrow(B3), nrow(SS), nrow(OF))


#Function that generates a random baseball team of 10
random_baseball_team <- function(numbers = nums){
  team_new <- NULL
  team_new <- P[floor(runif(1,1,numbers[1])),]
  team_new <- rbind.data.frame(team_new, P[floor(runif(1,1,numbers[1])),])
  team_new <- rbind.data.frame(team_new, C[floor(runif(1,1,numbers[2])),])
  team_new <- rbind.data.frame(team_new, B1[floor(runif(1,1,numbers[3])),])
  team_new <- rbind.data.frame(team_new, B2[floor(runif(1,1,numbers[4])),])
  team_new <- rbind.data.frame(team_new, B3[floor(runif(1,1,numbers[5])),])
  team_new <- rbind.data.frame(team_new, SS[floor(runif(1,1,numbers[6])),])
  team_new <- rbind.data.frame(team_new, OF[floor(runif(1,1,numbers[7])),])
  team_new <- rbind.data.frame(team_new, OF[floor(runif(1,1,numbers[7])),])
  team_new <- rbind.data.frame(team_new, OF[floor(runif(1,1,numbers[7])),])
  return(team_new)
}

#Function that calculates the salary of a random baseball team
calc_salary <- function(TEAM){
  salary <- sum(TEAM$Salary)
  return(salary)
}

#Function that calculates the points of a random baseball team
calc_points <- function(TEAM){
  points <- sum(TEAM$AvgPointsPerGame)
  return(points)
}

#Generate teams
TRIALS <- 1000

gen_teams <- function(trials){
  team_indices <- NULL
  team_points <- NULL
  for(i in 1:trials){
    repeat{
      t <- random_baseball_team()
      if(calc_salary(t)<=50000)
        break
    }
    team_indices <- rbind.data.frame(team_indices, t$Index)
    team_points[i] <- calc_points(t)
  }
  names(team_indices) <- c("P_1", "P_2", "C", "1B", "2B", "3B", "SS", "OF", "OF", "OF")
  team_indices <- cbind.data.frame(team_indices, team_points)
  team_indices$Index <- 1:length(team_indices[,1])
  return(team_indices[order(-team_points),])
}

head(gen_teams(20000))
