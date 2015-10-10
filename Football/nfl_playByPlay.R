#read in files and combine first 25 columns into 1 data.frame
nfl2013 <- read.csv(file.choose())
nfl2014 <- read.csv(file.choose())
names(nfl2014) <- names(nfl2013)
nfl13_14 <- rbind.data.frame(nfl2013[1:25], nfl2014[1:25])

#reformat data and add 2nd order values for Time_To_Go and Down
nfl13_14$play_type <- playFun(nfl13_14$Play.Type)
nfl13_14$time_int <- timeFun(nfl13_14$Time)
nfl13_14$togo_sq <- nfl13_14$ToGo^2
nfl13_14$Down_sq <- nfl13_14$Down^2

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

#getting rid of NA values in data.frame 
for (i in 1:ncol(nfl13_14)){
  for (j in 1:nrow(nfl13_14)){
    if (is.na(nfl13_14[j,i]))
      nfl13_14[j,i] = 0
  }
}
#uncomment this to subset data to a specific team 

# nfl_subset <- NULL
# for (i in 1:nrow(nfl13_14)){
#   c=0
#   for (j in col_set){
#     if (!is.na(nfl13_14[i, j]))
#       c=c+1
#   }
#   if (c==length(col_set))
#     nfl_subset <- rbind.data.frame(nfl_subset, nfl13_14[i,])
# }


#linear model to predict whether the play will result in a pass, run or sack.

nfl_lm=lm(play_type ~ time_int + ToGo + Down + togo_sq + Down_sq, data=nfl13_14)
summary(nfl_lm)

predPlayType <- function(Time, Yards_To_Go, Down){
  t <- as.numeric(gsub(":", "", Time))
  y <- Yards_To_Go
  M <- -3.203e-02 + (-4.836e-07*t) + (4.380e-02*y) + (2.111e-01*Down) + (-1.226e-03*y^2) + (-1.446e-02*Down^2)
  return(M)
}


#k-NN machine learning algoritm. Attempt to predict whether the play will result in a pass, run or sack.
ind <- sample(2, nrow(nfl13_14), replace=TRUE, prob=c(0.67, 0.33))
col_set <- c(4, 6, 7, 9, 10, 11, 28, 29)
nfl_training <- nfl13_14[ind==1, col_set]
nfl_test <- nfl13_14[ind==2, col_set]
nfl_training_labels <- nfl13_14[ind==1, c(14)]
nfl_test_labels <- nfl13_14[ind==2, c(14)]
library(class)
library(gmodels)
nfl_pred <- knn(train = nfl_training, test = nfl_test, cl = nfl_training_labels, k=3)
CrossTable(x = nfl_test_labels, y = nfl_pred, prop.chisq=FALSE)


