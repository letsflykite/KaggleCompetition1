set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-c(1:1000)*runif(1000,min=0,max=2)
x3<-c(1:1000)*runif(1000,min=0,max=2)

#Fitting a linear model to the variables results in an R squared of .7042:
lm_fit<-lm(y~x1+x2+x3)
summary(lm_fit)

#training and testing sets
set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))  #randomly selects 3/4 of the data to be the training set
training<- all_data[positions,]
testing<- all_data[-positions,]

#Fitting a linear model to the variables 
lm_fit<-lm(y~x1+x2+x3,data=training)
# applying model to the testing set
predictions<-predict(lm_fit,newdata=testing)  
#to calculate the prediction error by subtracting the actual values from the predicted values
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))  #the error calculation here is root mean squared error
error


library(foreach)
length_divisor<-4
iterations<-10
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
train_pos<-1:nrow(training) %in% training_positions
lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
predict(lm_fit,newdata=testing)
}
predictions
predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error