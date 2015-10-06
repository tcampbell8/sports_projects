#random sampling for fantasy football teams using aggregated week 1 data

week1 <- read.csv(file.choose())
week1$Index <- 1:length(week1$Name)
#names(week1)[6] <- "Index"
rbs = subset(week1, week1$Position =="RB", 1:7)
dst = subset(week1, Position =="DST", 1:7)
qbs = subset(week1, Position =="QB", 1:7)
tes = subset(week1, Position =="TE", 1:7)
wrs = subset(week1, Position =="WR", 1:7)

n_rbs <- length(rbs[,1])
n_dst <- length(dst[,1])
n_qbs <- length(qbs[,1])
n_tes <- length(tes[,1])
n_wrs <- length(wrs[,1])

random_football_team <- function(N_rbs=n_rbs, N_dst=n_dst, N_qbs=n_qbs, N_tes=n_tes, N_wrs=n_wrs){
  repeat{
    nums_rbs <- sample(1:N_rbs, 3, replace = FALSE)
    nums_qbs <- sample(1:N_qbs, 1, replace = FALSE)
    nums_dst <- sample(1:N_dst, 1, replace = FALSE)
    nums_tes <- sample(1:N_tes, 1, replace = FALSE)
    nums_wrs <- sample(1:N_wrs, 3, replace = FALSE)
    team_new <- qbs[nums_qbs[1],]
    team_new <- rbind.data.frame(team_new, rbs[nums_rbs[1],])
    team_new <- rbind.data.frame(team_new, rbs[nums_rbs[2],])
    team_new <- rbind.data.frame(team_new, rbs[nums_rbs[3],])
    team_new <- rbind.data.frame(team_new, wrs[nums_wrs[1],])
    team_new <- rbind.data.frame(team_new, wrs[nums_wrs[2],])
    team_new <- rbind.data.frame(team_new, wrs[nums_wrs[3],])
    team_new <- rbind.data.frame(team_new, tes[nums_tes[1],])
    team_new <- rbind.data.frame(team_new, dst[nums_dst[1],])
    if (sum(team_new$Salary)<=50000)
      break
  }
  return(team_new)
}

gen_football_teams <- function(trials){
  team_indices <- NULL
  team_points <- NULL
  for (i in 1:trials){
    t <- random_football_team()
    team_indices <- rbind.data.frame(team_indices, t$Index)
    team_points[i] <- sum(t$AvgPointsPerGame)  #Insert appropriate predictive statistic to sum
  }
  team_indices <- cbind.data.frame(team_indices, team_points)
  team_indices$Trial <- 1:length(team_indices[,1])
  names(team_indices) <- c("QB", "RB1", "RB2", "RB3", "WR1", "WR2", "WR3", "TE", "DST", "TeamPoints", "Trial")
  df <- team_indices[order(-team_points),]
  for (j in 1:100){
    for (k in 1:9){
      value <- df[j,k]
      df[j,k] <- as.character(week1$Name[as.integer(value)])
      #print(week1$Name[as.integer(value)])
    }
  }
  return(df)
}

head(gen_football_teams(1000))
