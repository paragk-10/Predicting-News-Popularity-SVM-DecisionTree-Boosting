##################  SVM  ##################
rm(list=ls(all=TRUE))
library(data.table) 
library(caret) 
library(caTools)
library(e1071)
library(ROCR)
library(caTools)

context = read.csv('OnlineNewsPopularity.csv')
context1 = context[,-c(1,2,38,39)]
summary(context1)
context1=context1[!context1$n_unique_tokens==701,]
summary(context1)
mean(context1$shares)

context1$shares<-ifelse(context1$shares>3395,1,0)
summary(context1)

set.seed(100)
split = sample.split(context1$shares, SplitRatio = 0.70)
training_set = subset(context1, split == TRUE)
test_set = subset(context1, split == FALSE)

training_set[,1:56] = scale(training_set[,1:56])
test_set[,1:56] = scale(test_set[,1:56])

classifier = svm(formula = training_set$shares ~ .,
                 data = training_set,
                 type = 'C-classification',
                 cost=10,
                 gamma=c(0.01,0.1,1,10),
                 kernel = 'radial')
classifier

prob_pred = predict(classifier, newdata = test_set[-57])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set[, 57], prob_pred)
cm
prediction_rate=(cm[1,1]+cm[2,2])/sum(cm)
print(paste("Prediction rate: ",prediction_rate))
print(paste("Error rate: ",1-prediction_rate))

pred<-prediction(as.numeric(prob_pred),as.numeric(test_set[,57]))
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf)
area_under_curve<- performance(pred, measure = "auc")
print(paste("Area under Curve:",area_under_curve@y.values[[1]]))

classifier1 = svm(formula = training_set$shares ~ .,
                 data = training_set,
                 type = 'C-classification',
                 cost=10,
                 kernel = 'polynomial')
classifier1

prob_pred1 = predict(classifier1, newdata = test_set[-57])
y_pred1 = ifelse(prob_pred1 > 0.5, 1, 0)
cm1 = table(test_set[, 57], prob_pred1)
cm1
prediction_rate1=(cm1[1,1]+cm1[2,2])/sum(cm1)
print(paste("Prediction rate: ",prediction_rate1))
print(paste("Error rate: ",1-prediction_rate1))

pred1<-prediction(as.numeric(prob_pred1),as.numeric(test_set[,57]))
perf1<-performance(pred1,measure="tpr",x.measure="fpr")
plot(perf1)
area_under_curve1<- performance(pred1, measure = "auc")
print(paste("Area under Curve:",area_under_curve1@y.values[[1]]))


classifier2 = svm(formula = training_set$shares ~ .,
                  data = training_set,
                  type = 'C-classification',
                  cost=10,
                  kernel = 'linear')
classifier2

prob_pred2 = predict(classifier2, newdata = test_set[-57])
y_pred2 = ifelse(prob_pred2 > 0.5, 1, 0)
cm2 = table(test_set[, 57], prob_pred2)
cm2
prediction_rate2=(cm2[1,1]+cm2[2,2])/sum(cm2)
print(paste("Prediction rate: ",prediction_rate2))
print(paste("Error rate: ",1-prediction_rate2))

pred2<-prediction(as.numeric(prob_pred2),as.numeric(test_set[,57]))
perf2<-performance(pred2,measure="tpr",x.measure="fpr")
plot(perf2)
area_under_curve2<- performance(pred2, measure = "auc")
print(paste("Area under Curve:",area_under_curve2@y.values[[1]]))

##################  Decision Tree  ##################
rm(list=ls(all=TRUE))
library(data.table) 
library(caret) 
library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
library(ROCR)

context = read.csv('OnlineNewsPopularity.csv')
context1 = context[,-c(1,2,38,39)]
summary(context1)
context1=context1[!context1$n_unique_tokens==701,]
summary(context1)
mean(context1$shares)

context1$shares<-ifelse(context1$shares>3395,1,0)
summary(context1)

set.seed(100)
split = sample.split(context1$shares, SplitRatio = 0.70)
training_set = subset(context1, split == TRUE)
test_set = subset(context1, split == FALSE)

training_set[,1:56] = scale(training_set[,1:56])
test_set[,1:56] = scale(test_set[,1:56])

tr_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
grid_dt <- expand.grid(cp=seq(0,0.005,0.00125))
set.seed(100)

classifier <- train(shares ~., data = training_set, method = "rpart",
                    parms = list(split = "gini"),
                    trControl=tr_ctrl,
                    tuneLength = 10,
                    tuneGrid=grid_dt)
classifier
plot(classifier)

y_pred = predict(classifier, newdata = test_set[-57])
y_pred
y_pred = ifelse(y_pred > mean(y_pred), 1, 0)

cm = table(test_set[, 57], y_pred)
cm

prediction_rate=(cm[1,1]+cm[2,2])/sum(cm)
print(paste("Prediction rate: ",prediction_rate))
print(paste("Error rate: ",1-prediction_rate))
pred<-prediction(as.numeric(y_pred),as.numeric(test_set[,57]))
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf)
area_under_curve<- performance(pred, measure = "auc")
print(paste("Area under Curve:",area_under_curve@y.values[[1]]))

y_pred11 = predict(classifier, newdata = training_set[-57])
y_pred11 = ifelse(y_pred11 > mean(y_pred11), 1, 0)
cm11 = table(training_set[, 57], y_pred11)
cm11
prediction_rate11=(cm11[1,1]+cm11[2,2])/sum(cm11)
print(paste("Prediction rate: ",prediction_rate11))
print(paste("Error rate: ",1-prediction_rate11))
pred11<-prediction(as.numeric(y_pred11),as.numeric(training_set[,57]))
perf11<-performance(pred11,measure="tpr",x.measure="fpr")
plot(perf11)
area_under_curve11<- performance(pred11, measure = "auc")
print(paste("Area under Curve:",area_under_curve11@y.values[[1]]))


tr_ctrl2 <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
grid_dt <- expand.grid(cp=seq(0,0.005,0.00125))
set.seed(123)
classifier2 <- train(shares ~., data = training_set, method = "rpart",
                     parms = list(split = "information"),
                     trControl=tr_ctrl2,
                     tuneLength = 5,
                     tuneGrid=grid_dt)
classifier2
plot(classifier2)

y_pred2 = predict(classifier2, newdata = test_set[-57])
y_pred2 = ifelse(y_pred2 > mean(y_pred2), 1, 0)
cm2 = table(test_set[, 57], y_pred2)
cm2
prediction_rate2=(cm2[1,1]+cm2[2,2])/sum(cm2)
print(paste("Prediction rate: ",prediction_rate2))
print(paste("Error rate: ",1-prediction_rate2))
pred2<-prediction(as.numeric(y_pred2),as.numeric(test_set[,57]))
perf2<-performance(pred2,measure="tpr",x.measure="fpr")
plot(perf2)
area_under_curve2<- performance(pred2, measure = "auc")
print(paste("Area under Curve:",area_under_curve@y.values[[1]]))

##################  Boosting  ##################
library(ada)
library(plyr)
library(pROC)

grid_ada <- expand.grid(iter = c(100), maxdepth= c(1,2.5,5,10), nu = c(0.01))
tr_ctrl3 <- trainControl(method = "cv", number = 5)
set.seed(100)

training_set$shares<-as.factor(training_set$shares)
classifier3 <- train(shares ~., data = training_set, method = "ada",
                     trControl=tr_ctrl3,
                     tuneGrid=grid_ada)
classifier3
plot(classifier3)

y_pred3 = predict(classifier3, newdata = test_set[-57])
cm3 = table(test_set[, 57], y_pred3)
cm3
prediction_rate3=(cm3[1,1]+cm3[2,2])/sum(cm3)
print(paste("Prediction rate: ",prediction_rate3))
print(paste("Error rate: ",1-prediction_rate3))
pred3<-prediction(as.numeric(y_pred3),as.numeric(test_set[,57]))
perf3<-performance(pred3,measure="tpr",x.measure="fpr")
plot(perf3)
area_under_curve3<- performance(pred3, measure = "auc")
print(paste("Area under Curve:",area_under_curve3@y.values[[1]]))


y_pred31 = predict(classifier3, newdata = training_set[-57])
cm31 = table(training_set[, 57], y_pred31)
cm31
prediction_rate31=(cm31[1,1]+cm31[2,2])/sum(cm31)
print(paste("Prediction rate: ",prediction_rate31))
print(paste("Error rate: ",1-prediction_rate31))
pred31<-prediction(as.numeric(y_pred31),as.numeric(training_set[,57]))
perf31<-performance(pred31,measure="tpr",x.measure="fpr")
plot(perf31)
area_under_curve31<- performance(pred31, measure = "auc")
print(paste("Area under Curve:",area_under_curve31@y.values[[1]]))
