
setwd("~/Documents")
golf = read.csv("DKSalaries(5).csv")
golf <- golf[c(2,3,5)]
fedex <- read.csv("deutche_champ_fedexPoints.csv")
fedex$Golfer <- lapply(fedex$Golfer, as.character)
driving <- read.csv("deutche_drive_mod.csv")
driving$PLAYER.NAME <- lapply(driving$PLAYER.NAME, as.character)
drive_acc <- read.csv("deutche_drive_acc_mod.csv")
drive_acc$PLAYER.NAME <- lapply(drive_acc$PLAYER.NAME, as.character)
greens_in_reg <- read.csv("deutche_greensinreg_mod.csv")
greens_in_reg$PLAYER.NAME <- lapply(greens_in_reg$PLAYER.NAME, as.character)
scramble <- read.csv("deutche_scramble_mod.csv")
scramble$PLAYER.NAME <- lapply(scramble$PLAYER.NAME, as.character)
fedex_points <- NULL



for (i in 1:99){
  if(golf$Name[i] %in% fedex$Golfer){
    index <- which(fedex$Golfer == golf$Name[i])
    golf$fedex[i] <- fedex$Points[index]
  }
}

for (i in 1:99){
  if(golf$Name[i] %in% driving$PLAYER.NAME){
    index <- which(driving$PLAYER.NAME == golf$Name[i])
    golf$avg_drive_dist[i] <- driving$AVG.[index]
  }
  else 
    golf$avg_drive_dist[i] <- NA
}

for (i in 1:99){
  if(golf$Name[i] %in% drive_acc$PLAYER.NAME){
    index <- which(drive_acc$PLAYER.NAME == golf$Name[i])
    golf$drive_acc[i] <- drive_acc$drive_acc[index]
  }
  else
    golf$drive_acc[i] <- NA
}

for (i in 1:99){
  if(golf$Name[i] %in% scramble$PLAYER.NAME){
    index <- which(scramble$PLAYER.NAME == golf$Name[i])
    golf$scramble[i] <- scramble$scramble[index]
  }
  else
    golf$scramble[i] <- NA
}

for (i in 1:99){
  if(golf$Name[i] %in% greens_in_reg$PLAYER.NAME){
    index <- which(greens_in_reg$PLAYER.NAME == golf$Name[i])
    golf$greens_in_regs[i] <- greens_in_reg$greens_in_reg[index]
  }
  else
    golf$greens_in_reg[i] <- NA
}



golf <- golf[-c(3,48),]
golf <- golf[-c(8)]
golf$avg_drive_dist[25] <- mean(golf$avg_drive_dist, na.rm=TRUE)

for (i in 1:97){
  S <- 0.4*golf$AvgPointsPerGame[i]/max(golf$AvgPointsPerGame)
  S <- S + 0.1*golf$fedex[i]/max(golf$fedex)
  S <- S + 0.1*golf$avg_drive_dist[i]/max(golf$avg_drive_dist)
  S <- S + 0.1*golf$drive_acc[i]/max(golf$drive_acc)
  S <- S + 0.1*golf$scramble[i]/max(golf$scramble)
  S <- S + 0.2*golf$greens_in_reg[i]/max(golf$greens_in_reg)
  golf$T1[i] <- S*100
}
for (i in 1:97){
  S <- 0.1*golf$AvgPointsPerGame[i]/max(golf$AvgPointsPerGame)
  S <- S + 0.075*golf$fedex[i]/max(golf$fedex)
  S <- S + 0.3*golf$avg_drive_dist[i]/max(golf$avg_drive_dist)
  S <- S + 0.15*golf$drive_acc[i]/max(golf$drive_acc)
  S <- S + 0.075*golf$scramble[i]/max(golf$scramble)
  S <- S + 0.3*golf$greens_in_reg[i]/max(golf$greens_in_reg)
  golf$T2[i] <- S*100
}
for (i in 1:97){
  S <- 0.3*golf$AvgPointsPerGame[i]/max(golf$AvgPointsPerGame)
  S <- S + 0.1*golf$fedex[i]/max(golf$fedex)
  S <- S + 0.05*golf$avg_drive_dist[i]/max(golf$avg_drive_dist)
  S <- S + 0.15*golf$drive_acc[i]/max(golf$drive_acc)
  S <- S + 0.15*golf$scramble[i]/max(golf$scramble)
  S <- S + 0.25*golf$greens_in_reg[i]/max(golf$greens_in_reg)
  golf$T3[i] <- S*100
}

names(golf)
g1 = golf[order(-golf$T1),]

g2 = golf[order(-golf$T2),]

g3 = golf[order(-golf$T3),]

test_models <- cbind.data.frame(golf[1], g1[1], g2[1], g3[1])
names(test_models) <- c("Original", "T1", "Ty1", "Nas1")


random_golf_team <- function(){
  repeat{
    nums <- sample(1:97, 6, replace = FALSE)
    team <- golf[nums[1],]
    for (i in 2:6){
      team <- rbind.data.frame(team, golf[nums[i],])
    }
    if (sum(team$Salary)<=50000)
      break
  }
  return(team)
}



random_golf_team()





