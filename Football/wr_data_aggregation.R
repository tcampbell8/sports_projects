recs <- read.csv(file.choose())
rec_espn <- read.csv(file.choose())
names(recs)


n <- length(recs$player)

for (i in 1:n){
  tf_mat <- grepl(as.character(recs$player[i]), as.character(rec_espn$PLAYER))
  if (TRUE %in% tf_mat){
    ind <- which(tf_mat==TRUE)
    recs$targets[i] <- as.numeric(rec_espn$TAR[ind])
    recs$yards[i] <- as.numeric(rec_espn$YDS[ind])
    recs$TDs[i] <- as.numeric(rec_espn$TD[ind])
    recs$yards_ac[i] <- as.numeric(rec_espn$YAC[ind])
  }
}
recs <- recs[-c(4)]
recs
save(recs, file="~/Documents/week8_WRs.Rda")

library(corrplot)
lm_1 <- lm(projection ~ salary + I(targets/yards), data = recs)
summary(lm_1)
cor(recs[5:13])
corrplot(recs[5:13])

par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(lm_1$fitted, lm_1$residuals) # plot of fitted values vs residuals
qqnorm(lm_1$residuals) #qq-plot of residuals
qqline(lm_1$residuals) # plotting the line, along which the dots in qq-plot should lie
plot(lm_1$residuals) # plotting the residuals vs time
abline(h=0,lty=2) # plotting a horizontal line at 0
acf(lm_1$residuals) #sample acf plot of residuals

wr7 <- read.csv(file.choose())
wr7$impact <- wr7$Points/(wr7$Salary/1000)

#aggregate past 7 weeks of data: compute prediction intervals for IMPACT => create player pool of high impact players




