d1 = read.csv('data_3/dataset_1.csv')
d1$Survival = as.factor(d1$Survival)
s <- svm(Survival~., data = d1)

plot(s, d1)
plot(s, d1, age~Freq) #Ok
summary(s)

y_hat = predict(s, d1)
t = table(y_hat, d1$Survival)


library(pastecs)
library(Hmisc)
library(lattice)
library(corrplot)
library("PerformanceAnalytics")
library(factoextra)
library(e1071)

#Tuning Hyper perameter optimization 
set.seed(123)
newmodel <- tune(svm, Survival~., data = d1,
                 ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
plot(newmodel)
summary(newmodel)
# best model
bestmodel <- newmodel$best.model
summary(bestmodel)
#prediction
pred <- predict(bestmodel,d1)
tab <- table(Predicted = pred , Actual = d1$Survival)
tab  #confusion matrix
error = 1-sum(diag(tab))/sum(tab) # mis classification error
acc = sum(diag(tab))/sum(tab)
acc

newdata = data.frame(age=c(25,30,60), year=c(60,58,67), Freq=c(15,1,47))
pred <- predict(bestmodel,newdata)







# Regression 

data = read.csv('data_3/dataset_1.csv')
data$Survival <- as.numeric(data$Survival)

model <- svm(Survival ~ . , data)

predictedY <- predict(model, data)

points(data$Survival, predictedY, col = "red", pch=4)



## SVM RMSE
# /!\ this time  svrModel$residuals  is not the same as data$Y - predictedY
# so we compute the error like this
error <- data$Survival - predictedY
svrPredictionRMSE <- rmse(error) 
svrPredictionRMSE