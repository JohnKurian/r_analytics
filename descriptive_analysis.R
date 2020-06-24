#Q1


library(pastecs)
library(Hmisc)
library(lattice)
library(corrplot)
library("PerformanceAnalytics")
library(factoextra)
library(e1071)
library(caTools)


d <- read.csv("data_3/students.csv")

head(d, 6)

#mean, median, max
summary(d)

#type of variables
str(d)

#missing, distinct, frequency
describe(d)

#standard deviation, variance
stat.desc(d)

#Find the 3rd quartile 
summary(d$SCORE)[['3rd Qu.']]

#visualise the outliers
stripplot(d$SCORE)

#detect outliers
OutVals = boxplot(e)$out
which(e %in% OutVals)


length(d$SCORE)

boxplot(d)



#Find the correlation between two variable
e = d[,1:4]
res <- cor(e)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)




chart.Correlation(e, histogram=TRUE, pch=19)

#Finding the statistically important values with associated p-values
rcorr(as.matrix(d[,1:4]))


#Splitting the dataset
set.seed(123)
split = sample.split(d$Survival, SplitRatio = 0.5)
training_set = subset(d, split == TRUE)
test_set = subset(d, split == FALSE)










