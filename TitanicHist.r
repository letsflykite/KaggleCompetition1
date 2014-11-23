rm(list = ls())
# titanic histogram
setwd("C:/Users/Saeed/Documents/My Projects/Titanic/R")
dataTi <- read.csv(file = "../Data/train.csv", header = TRUE, stringsAsFactors = FALSE)
attach(dataTi)
names(dataTi)
mode(dataTi)

hist(fare)

dataTi$fare<-round(dataTi$fare/10)
dataTi$fare<-factor(dataTi$fare)
fare
summary(fare)


brk=(0:2:100)
table(cut(fare, breaks = brk))


str(dataTi)
max_num <- max(fare)
hist(fare, col=heat.colors(max_num), breaks=max_num, xlim=c(0,max_num), right=F, main="Autos Histogram", las=1)


brk <- c(0,3,4,5,6,10,16,50,100,600)
hist(fare, col=heat.colors(length(brk)), breaks=brk,    xlim=c(0,max_num), right=F, main="Probability Density",    las=1, cex.axis=0.8, freq=F)
fare
fare[fare>100]<- 100
hist(fare, col=heat.colors(max_num), breaks=max_num, xlim=c(0,max_num), right=F, main="Autos Histogram", las=1)


#z-score
fare=(fare-mean(fare))/sd(fare)
max_num <- max(fare)
hist(fare, col=heat.colors(max_num), breaks=max_num, xlim=c(0,max_num), right=F, main="Autos Histogram", las=1)