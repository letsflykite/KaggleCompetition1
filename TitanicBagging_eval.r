# Test with cross fold
dtModel1<-function(trainData,testData,evalu)
{
	err.resub<-0
	library(party)
	myFormula<-survived~pclass+sex+age+sibsp+parch+fare+embarked
	titanic_ctree<-ctree(myFormula,data=trainData)
	#print(titanic_ctree)
	# plot(titanic_ctree)
	train_pred<-predict(titanic_ctree)
	# table(train_pred,trainData$survived)
	if (evalu==TRUE)
	{
		test_pred<-predict(titanic_ctree,testData)
		mc<-table(test_pred,testData$survived)
		# print(mc)
		err.resub <- 1.0-(mc[1,1]+mc[2,2])/sum(mc)
		print(err.resub)
	}
	return (err.resub)
}

dtModel2<-function(trainData,testData,evalu)
{
	library(rpart)
	#Induction of the decision tree on the whole dataset.
	myFormula<-survived~pclass+sex+age+sibsp+parch+fare+embarked
	titanic_dtree<-rpart(myFormula,data=trainData,method="class")
	#print(titanic_dtree)
	#plot(titanic_dtree)
	train_pred<-predict(titanic_dtree)
	#table(train_pred)
	if (evalu==TRUE)
	{
		test_pred<-predict(titanic_dtree,newdata=testData,type="class")
		mc<-table(test_pred,testData$survived)
		#print(mc)
		err.resub <- 1.0-(mc[1,1]+mc[2,2])/sum(mc)
		#print(err.resub)
	}
	return (err.resub)
}


setwd("C:/Users/Saeed/Documents/My Projects/Titanic/R")
dataTi <- read.csv(file = "../Data/train.csv", header = TRUE, stringsAsFactors = FALSE)
attach(dataTi)
names(dataTi)
mode(dataTi)

dataTi$survived[dataTi$survive==1]<-"Yes"
dataTi$survived[dataTi$survive==0]<-"No"

nrow(dataTi[!complete.cases(dataTi),])
#apply(dataTi, 1, function(x) sum(is.na(x)))

dataTi$age[is.na(dataTi$age)]=median(dataTi$age[!is.na(dataTi$age)])
dataTi$embarked[embarked==""]<-"C"

dataTi$survived<-factor(dataTi$survived)
dataTi$sex<-factor(dataTi$sex)
dataTi$embarked<-factor(dataTi$embarked)

#dataTi$fare[dataTi$fare>100]<-100
dataTi$fare<-round(dataTi$fare/10)
dataTi$fare<-factor(dataTi$fare)


 set.seed(1234)
 ind <- sample(2,nrow(dataTi),replace=TRUE,prob=c(0.7,0.3))
 trainData<-dataTi[ind==1,]
 testData<-dataTi[ind==2,]
 

#Bagging
library(foreach)
length_divisor<-4
iterations<-200
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
training_positions <- sample(nrow(trainData), size=floor((nrow(trainData)/length_divisor)))
train_pos<-1:nrow(trainData) %in% training_positions
#lm_fit<-lm(y~x1+x2+x3,data=trainData[train_pos,])
#predict(lm_fit,newdata=testing)
er <- dtModel1( trainData[train_pos,],testData,TRUE)
}
predictions
#predictions<-rowMeans(predictions)
#error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
mean(predictions) 
plot(t(predictions))




