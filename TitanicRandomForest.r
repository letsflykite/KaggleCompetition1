# Test with random forest
# Random forest 
ms<-function(trainData,testData)
{
	library(randomForest)
	rf<-randomForest(survived~pclass+sex+age+sibsp+parch+fare+embarked,data=trainData,ntree=100,proximity=TRUE) 
	rf
	plot(rf)
	varImpPlot(rf)
	predict(rf)
	predict(rf,newdata=trainData)
	predict(rf,newdata=testData)
	
	table(trainData$survived)
	table(testData$survived)
	
	
	table(predict(rf))
	table(predict(rf,newdata=trainData))
	table(predict(rf,newdata=testData))


	table(predict(rf),trainData$survived)
	
	ptr<-predict(rf,newdata=trainData)
	table(ptr,trainData$survived)
	
	pts<-predict(rf,newdata=testData)
	table(pts,testData$survived)
	
}
# random forest with submit file
mst<-function(trainData,testData)
{
	library(randomForest)
	rf<-randomForest(survived~pclass+sex+age+sibsp+parch+fare+embarked,data=trainData,ntree=100,proximity=TRUE) 
	
	predict(rf)
	predict(rf,newdata=trainData)
#	table(trainData$survived)
#	table(predict(rf))
	ptr<-predict(rf,newdata=trainData)
#	table(ptr)
#	table(predict(rf),trainData$survived)
	ptr<-predict(rf,newdata=trainData)
#	table(ptr,trainData$survived)
	
	pts<-predict(rf,newdata=testData)	
	submitTable<-c()
	submitTable[ pts =="No" ] <- 0
	submitTable[ pts == "Yes" ] <- 1
	#submitTable
	write.table(submitTable,"../Submision/submit8_rforest_fixed.csv",row.names=FALSE,col.names=FALSE, sep=",");
		
}

setwd("C:/Users/Saeed/Documents/My Projects/Titanic/R")  
dataTi <- read.csv(file = "../Data/train.csv", header = TRUE, stringsAsFactors = FALSE)
dataTi$survived[dataTi$survive==1]<-"Yes"
dataTi$survived[dataTi$survive==0]<-"No"

nrow(dataTi[!complete.cases(dataTi),])
#apply(dataTi, 1, function(x) sum(is.na(x)))
dataTi$age[is.na(dataTi$age)]=median(dataTi$age[!is.na(dataTi$age)])
dataTi$embarked[dataTi$embarked==""]<-"C"

dataTi$survived<-factor(dataTi$survived)
dataTi$sex<-factor(dataTi$sex)
dataTi$embarked<-factor(dataTi$embarked)
# dataTi$fare<-round(dataTi$fare/10)
# dataTi$fare<-factor(dataTi$fare)

summary(dataTi)
str(dataTi)
 # set.seed(1234)
 # ind <- sample(2,nrow(dataTi),replace=TRUE,prob=c(0.7,0.3))
 # trainData<-dataTi[ind==1,]
 # testData<-dataTi[ind==2,]
# rf( trainData,testData)

trainData<-dataTi
testData<- read.csv(file = "../Data/test.csv", header = TRUE, stringsAsFactors = FALSE)
testData$sex<-factor(testData$sex)
testData$embarked<-factor(testData$embarked)
# testData$fare<-round(testData$fare/10)
# testData$fare<-factor(testData$fare)

testData$age[is.na(testData$age)]=median(dataTi$age[!is.na(dataTi$age)])

testData$fare[is.na(testData$fare) & testData$pclass==1]=median(dataTi$fare[!is.na(dataTi$fare) & dataTi$pclass==1])
testData$fare[is.na(testData$fare) & testData$pclass==2]=median(dataTi$fare[!is.na(dataTi$fare) & dataTi$pclass==2])
testData$fare[is.na(testData$fare) & testData$pclass==3]=median(dataTi$fare[!is.na(dataTi$fare) & dataTi$pclass==3])
str(testData)

mst(dataTi,testData)

#testData



