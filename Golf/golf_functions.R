#Golf Functions

golf_rec <- read.csv(file.choose())
golf <- golf_rec[-c(1,4,6)]
cuts_made <- read.csv(file.choose())

augment_DK_dataframe <- function(DK_df, aug_df){
  N <- length(DK_df[,1])
  for (i in 1:N){
    golfer_index <- which(aug_df$PLAYER == DK_df$Name[i])
    DK_df$pct_cuts_made[i] <- aug_df$pct_cuts_made[golfer_index]
  }
  return(DK_df)
}

golf <- augment_DK_dataframe(golf, cuts_made)
golf$Index <- 1:70

golf$Name <- as.character(golf$Name)

random_golf_team <- function(){
  repeat{
    nums <- sample(1:70, 6, replace = FALSE)
    team <- NULL
    for (i in 1:6){
      team <- rbind.data.frame(team, golf[nums[i],])
    }
    if (sum(team$Salary)<=50000)
      break
  }
  return(team)
}

gen_golf_teams <- function(trials){
  team_indices <- NULL
  team_points <- NULL
  for (i in 1:trials){
    t <- random_golf_team()
    team_indices <- rbind.data.frame(team_indices, t$Index)
    team_points[i] <- sum(t$ExpectedValue)
  }
  team_indices <- cbind.data.frame(team_indices, team_points)
  names(team_indices) <- c("G1", "G2", "G3", "G4", "G5", "G6", "EV_total")
  df <- team_indices[order(-team_points),]
  for (j in 1:100){
    for (k in 1:6){
      value <- df[j,k]
      df[j,k] <- golf$Name[as.integer(value)]
    }
  }
  return(df)
}

head(gen_golf_teams(25000))

golf_new <- golf
# golf_new$ExpectedValue <- golf_new$AvgPointsPerGame*golf_new$pct_cuts_made + 
#   (golf_new$AvgPointsPerGame/2)*(1 - golf_new$pct_cuts_made)

#doesn't really make sense to compute EV since avgPoints already accounts for cuts missed







