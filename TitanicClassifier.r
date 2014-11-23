setwd("C:/Users/Saeed/Documents/My Projects/Titanic/R")
dataTi <- read.csv(file = "../Data/train.csv", header = TRUE, stringsAsFactors = FALSE)
attach(dataTi)
names(dataTi)
mode(dataTi)

dataTi$survived[dataTi$survive==1]<-"Yes"
dataTi$survived[dataTi$survive==0]<-"No"


dataTi$survived<-factor(dataTi$survived)
dataTi$sex<-factor(dataTi$sex)
dataTi$embarked<-factor(dataTi$embarked)



summary(dataTi)
str(dataTi)
# set.seed(1234)
# ind <- sample(2,nrow(dataTi),replace=TRUE,prob=c(0.8,0.2))
# trainData<-dataTi[ind==1,]
# testData<-dataTi[ind==2,]

trainData<-dataTi



testData<- read.csv(file = "../Data/test.csv", header = TRUE)
test
library(party)
myFormula<-survived~pclass+sex+age+sibsp+parch+fare+embarked
titanic_ctree<-ctree(myFormula,data=trainData)

print(titanic_ctree)
plot(titanic_ctree)
plot(titanic_ctree,type="simple")
train_pred<-predict(titanic_ctree)
table(train_pred,trainData$survived)


summary(test_pred)
test_pred<-predict(titanic_ctree,testData)
table(test_pred,testData$survived)


test_pred