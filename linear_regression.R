library(pastecs)
library(Hmisc)
library(lattice)
library(corrplot)
library("PerformanceAnalytics")
library(factoextra)
library(e1071)

# Linear regression

d1 = read.csv('data_3/dataset_1.csv')
d1$Survival <- as.numeric(d1$Survival)

fitlm <- lm(d1$Survival~., data=d1)
summary(fitlm)

plot(Survival~., data=d1, main="Comparison SVM and OLS regression")

cols <- c("red", "dodgerblue")
abline(fitlm, col=cols[1], lwd=2)
points(d1$X, d1$ypred, pch=20, col=cols[2], cex=2)
legend("topleft", leg=c("lda", "SVM"),
       col=cols, pch=c(20,20), inset=.01,
       bg="wheat")


rmse <- function(e) {
  sqrt(mean(e^2))
}


## LM RMSE
errorlm <- fitlm$residuals
rmse(errorlm)