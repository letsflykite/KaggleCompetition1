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
dataTi$fare<-round(dataTi$fare/5)
dataTi$fare<-factor(dataTi$fare)


# summary(dataTi)
# str(dataTi)
 # set.seed(1234)
 # ind <- sample(2,nrow(dataTi),replace=TRUE,prob=c(0.8,0.2))
 # trainData<-dataTi[ind==1,]
 # testData<-dataTi[ind==2,]
 
#dtModel1( trainData,testData,TRUE)
#dtModel2( trainData,testData,TRUE)

 
# --- cross-fold 
n<-nrow(dataTi)
k<-10
taille <-n%/%k
set.seed(5)
alea <-runif(n)
rang <-rank(alea)
bloc <-(rang-1)%/%taille+1
bloc <-as.factor(bloc)
 
 all.err <-numeric(0)
 for(k in 1:k){
	er <- dtModel2( dataTi[bloc!=k,],dataTi[bloc==k,],TRUE)
	all.err <-rbind(all.err,er)
 }
print(all.err)
mean(all.err) 
 


