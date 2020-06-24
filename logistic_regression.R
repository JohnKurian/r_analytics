library(pastecs)
library(Hmisc)
library(lattice)
library(corrplot)
library("PerformanceAnalytics")
library(factoextra)
library(e1071)

# Logistic regression

d1 = read.csv('data_3/dataset_1.csv')
model <- glm( Survival ~., data = d1, family = 'binomial')
# Summarize the model
summary(model)
# Make predictions
probabilities <- predict(model, d1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")
# Model accuracy
mean(predicted.classes == d1$Survival)

newdata = data.frame(age=c(25,30,60), year=c(60,58,67), Freq=c(15,1,47))
probabilities <- predict(model, newdata, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")
predicted.classes