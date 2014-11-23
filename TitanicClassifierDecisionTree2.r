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

# --- modelling by dtree
library(rpart)
#Induction of the decision tree on the whole dataset.
myFormula<-survived~pclass+sex+age+sibsp+parch+fare+embarked
titanic_dtree<-rpart(myFormula,data=trainData,method="class")
print(titanic_dtree)
plot(titanic_dtree)
train_pred<-predict(titanic_dtree)
table(train_pred)
test_pred<-predict(titanic_dtree,newdata=testData,type="class")



# --- printing the results
test_pred
attributes(test_pred)
submitTable<-c()
submitTable[ test_pred =="No" ] <- 0
submitTable[ test_pred == "Yes" ] <- 1
submitTable
write.table(submitTable,"../Submision/submit8_ctree_factorFare.csv",row.names=FALSE,col.names=FALSE, sep=",");