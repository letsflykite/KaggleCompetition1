# the first test with the ctree and handelling missing values
setwd("C:/Users/Saeed/Documents/My Projects/Titanic/R")
dataTi <- read.csv(file = "../Data/train.csv", header = TRUE, stringsAsFactors = FALSE)
attach(dataTi)
names(dataTi)
mode(dataTi)

# --- pre-processing
dataTi$survived[dataTi$survive==1]<-"Yes"
dataTi$survived[dataTi$survive==0]<-"No"
nrow(dataTi[!complete.cases(dataTi),])
apply(dataTi, 1, function(x) sum(is.na(x)))
dataTi$age[is.na(dataTi$age)]=median(dataTi$age[!is.na(dataTi$age)])
dataTi$embarked[embarked==""]<-"C"
dataTi$survived<-factor(dataTi$survived)
dataTi$sex<-factor(dataTi$sex)
dataTi$embarked<-factor(dataTi$embarked)
dataTi$fare<-factor(dataTi$fare)


# --- read the test data
trainData<-dataTi
testData<- read.csv(file = "../Data/test.csv", header = TRUE, stringsAsFactors = FALSE)
testData$sex<-factor(testData$sex)
testData$embarked<-factor(testData$embarked)
testData$fare<-factor(testData$fare)
str(testData)

# --- modelling by ctree
library(party)
myFormula<-survived~pclass+sex+age+sibsp+parch+fare+embarked
titanic_ctree<-ctree(myFormula,data=trainData)
print(titanic_ctree)
plot(titanic_ctree)
#plot(titanic_ctree,type="simple")
train_pred<-predict(titanic_ctree)
table(train_pred,trainData$survived)
test_pred<-predict(titanic_ctree,testData)
#table(test_pred,evalData$survived)



# --- printing the results
test_pred
attributes(test_pred)
submitTable<-c()
submitTable[ test_pred =="No" ] <- 0
submitTable[ test_pred == "Yes" ] <- 1
submitTable
write.table(submitTable,"../Submision/submit8_ctree_factorFare.csv",row.names=FALSE,col.names=FALSE, sep=",");