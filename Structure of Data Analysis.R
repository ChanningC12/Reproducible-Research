library(kernlab)
str(spam[,1:5])
help(spam)

# use training set to build the model, use test set to see the performance
set.seed(3435)
trainIndicator = rbinom(4601,size=1,prob=0.5)
table(trainIndicator)

# subsampling the data
trainSpam = spam[trainIndicator==1,]
testSpam = spam[trainIndicator==0,]

# Names
names(trainSpam)
# Head
head(trainSpam)
# Summaries
table(trainSpam$type)
# plots
plot(trainSpam$capitalAve~trainSpam$type)
plot(log(trainSpam$capitalAve+1)~trainSpam$type)
plot(log10(trainSpam[,1:4]+1))
# clustering
hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)
hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

# Statistical prediction/modeling
str(spam[,"type"])
trainSpam$numType = as.numeric(trainSpam$type)-1
predictionModel = glm(numType~charDollar,family="binomial",data=trainSpam)
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam",dim(testSpam)[1])
predictedSpam[predictionModel$fitted>0.5]="spam"

# Get a measure of uncertainty
table(predictedSpam,testSpam$type)
library(caret)

# generate confustion matrix
confusionMatrix(predictedSpam,testSpam$type,positive="spam")

# Draw ROC and calculate AUC
library(gplots)
library(ROCR)
S = predict(predictionModel, type="response",newdata = testSpam)
testSpam$numType = as.numeric(testSpam$type)-1
Y = testSpam$numType
pred = prediction(prediction=S, labels=Y)
perf = performance(pred,measure="tpr",x.measure="fpr")
plot(perf,main="ROC Curve",col="blue",lwd=3)
abline(a=0,b=1,lwd=2,lty=2)
perf.auc = performance(pred,measure="auc")
unlist(perf.auc@y.values)
perf.auc

# Draw lift curve
perf.lift = performance(pred,measure="lift",x.measure="rpp") # rate of positive predictions
plot(perf.lift,main="Lift Curve",lwd=3)
perf.lift

install.packages("lift")
library(lift)
plotLift(S,Y,cumulative = T, n.buckets = 10)
plotLift(S,Y,cumulative = F, n.buckets = 10)
TopDecileLift(S,Y)




