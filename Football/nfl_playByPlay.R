nfl2013 <- read.csv(file.choose())
nfl2014 <- read.csv(file.choose())

nfl2014$play_type <- playFun(nfl2014$Play.Type)
nfl2014$time_int <- timeFun(nfl2014$Time)
nfl2014$togo_sq <- nfl2014$ToGo^2
nfl2014$Down_sq <- nfl2014$Down^2


nfl13_14 <- rbind.data.frame(nfl2013, nfl2014)


total_lm <- lm(, data = nfl2013)
summary(total_lm)

nfl2013$YdsW_sq <- nfl2013$YdsW^2
nfl2013$YdsL_sq <- nfl2013$YdsL^2
nfl2013$YdsW_YdsL <- nfl2013$YdsW*nfl2013$YdsL



playFun <- function(v){
  k = NULL
  for (i in 1:length(v)){
    if (v[i]=="Pass")
      k[i]=1
    if (v[i]=="Run")
      k[i]=0
    else 
      if (v[i]=="Sack")
        k[i]=-1
  }
  return(k) 
}

timeFun <- function(t){
  n=length(t)
  k=NULL
  for (i in 1:n){
    k[i]=as.numeric(gsub(":", "", t[i]))
  }
  return(k)
}


nfl2013$time_int <- timeFun(nfl2013$Time)
nfl2013$togo_sq <- nfl2013$ToGo^2
nfl2013$Down_sq <- nfl2013$Down^2

nfl_lm=lm(play_type ~ time_int + ToGo + Down + togo_sq + Down_sq, data=nfl13_14)
summary(nfl_lm)

nfl2013$play_type <- playFun(nfl2013$Play.Type)

predPlayType <- function(Time, Yards_To_Go, Down){
  t <- as.numeric(gsub(":", "", Time))
  y <- Yards_To_Go
  M <- -3.203e-02 + (-4.836e-07*t) + (4.380e-02*y) + (2.111e-01*Down) + (-1.226e-03*y^2) + (-1.446e-02*Down^2)
  return(M)
}

steelers_lm <- lm(play_type ~ time_int + ToGo + Down + togo_sq, data = steelers2013)
summary(steelers_lm)



