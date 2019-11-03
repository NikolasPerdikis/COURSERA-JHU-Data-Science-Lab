n <- 25445
prevalence = 1/1000
sens <- .99
spec <- .99
clicked <- prevalence * n
TP <- sens * clicked
Notclicked <- n - clicked
TN <- spec * Notclicked
FP <- Notclicked * sens
FP<- Notclicked-TN
PPV <- TP/ (TP+FP)
 PPV

# Week2 QUIZ

 library(caret)
 library(AppliedPredictiveModeling)
 set.seed(5555)
 data(AlzheimerDisease)
 
 
 
 preProcess(adData[,il_names],method="pca",thresh=0.9)
 
 # Q4:
 library(DescTools)
 set.seed(3433)
 res <- names(adData) %like% "IL%" 
 il_names <- names(adData[,res=="TRUE"])
 IL_str <- grep("^IL", colnames(training), value = TRUE)
 predictors_il <- predictors[,il_names]
 df <- data.frame(diagnosis,predictors_il)
 inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
 
 #without PCA
 training = df[inTrain,]
 testing=df[-inTrain,]
modelFit <- train(diagnosis~.,method="glm",data=training)
predictions <- predict(modelFit,newdata=testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
C1$overall[1]

 
#with PCA
modelFit <- train(training$diagnosis ~ ., method = "glm", preProcess = "pca", 
data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8))) 
C2 <- confusionMatrix(predict(modelFit,testing), testing$diagnosis)


preProc <- preProcess(df2[inTrain,-13],method="pca",thresh=0.8)
preProc <- preProcess(df2[inTrain,-13],method="pca",thresh=0.8)
trainPC <- predict(preProc,df2)
modelFit <- train(df2$diagnosis~.,method="glm",data=trainPC)



data(iris); 
library(ggplot2)
library(caret)
inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

library(ISLR); 
data(Wage); 
library(ggplot2); 
library(caret);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)


data(iris); 
library(ggplot2)
names(iris)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)
modlda = train(Species ~ .,data=training,method="lda")
modnb = train(Species ~ ., data=training,method="nb")
plda = predict(modlda,testing);
pnb = predict(modnb,testing)
table(plda,pnb)

# WEEK 3 QUIZ
#Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

inTrain <- createDataPartition(y=segmentationOriginal$Case,p=0.7, list=FALSE)
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

set.seed(125)
library(caret)
modFit <- train(Class ~ .,method="rpart",data=training)
print(modFit$finalModel)
library(rattle)
fancyRpartPlot(modFit$finalModel)

#Q3
library(pgmm)
data(olive)
olive = olive[,-1]
inTrain <- createDataPartition(y=olive$Area,p=0.7, list=FALSE)
training <- olive[inTrain,]
testing <- olive[-inTrain,]
modFit <- train(Area ~ .,method="rpart",data=training)

#Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modFit <- train(chd ~ age+alcohol+obesity+tobacco+typea+ldl,method="glm",family="binomial",data=trainSA)

missClass = function(values,prediction)
{
        sum(((prediction > 0.5)*1) != values)/length(values)
}
pred_test <- predict(modFit,testSA)
pred_train <- predict(modFit,trainSA)
missClass(trainSA$chd,pred_train)
missClass(testSA$chd,pred_test)

#Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
set.seed(33833)
library(randomForest)
modFit <- randomForest(as.factor(y) ~., data = vowel.train)
# return the second tree 
order(varImp(modFit), decreasing = T)


## week 4 quiz

#Q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
library(caret)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
rfvowel <- train(y ~ ., vowel.train, method = "rf") 
predrf <- predict(rfvowel, newdata = vowel.test)
confusionMatrix(predrf, vowel.test$y) 
gbmvowel <- train(y ~ ., vowel.train, method = "gbm", verbose = FALSE)
predgbm <- predict(gbmvowel, vowel.test)
confusionMatrix(predgbm, vowel.test$y)
modelagreed <- (predrf == predgbm)
confusionMatrix(vowel.test$y[modelagreed], predrf[modelagreed])$overall['Accuracy']

#Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
mod_rf <- train(diagnosis ~ ., training, method = "rf")
mod_gbm <- train(diagnosis ~ ., training, method = "gbm", verbose = FALSE)
mod_lda <- train(diagnosis ~ ., training, method = "lda")
rfpred <- predict(mod_rf, testing)
gbmpred <- predict(mod_gbm, testing)
ldapred <- predict(mod_lda, testing)
confusionMatrix(rfpred, testing$diagnosis)$overall['Accuracy']
confusionMatrix(gbmpred, testing$diagnosis)$overall['Accuracy']
confusionMatrix(ldapred, testing$diagnosis)$overall['Accuracy']
combine <- data.frame(rfpred, gbmpred, ldapred, diagnosis = testing$diagnosis)
mod_comb <- train(diagnosis ~ ., combine, method = "rf")

#Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
library(elasticnet)
modlasso <- train(CompressiveStrength ~ ., training, method = "lasso")
plot.enet(modlasso$finalModel, xvar="penalty", use.color=TRUE)

#Q4
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(url, destfile = "gaData.csv")
dat = read.csv("gaData.csv")
library(lubridate)
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)
modfc <- bats(tstrain)
predts <- forecast(modfc, level = 95, nrow(testing))
fslower95 <- predts$lower
fsupper95 <- predts$upper
table ((testing$visitsTumblr>fslower95) & (testing$visitsTumblr<fsupper95))

#Q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]
library(e1071)
set.seed(325)
fit <- svm(CompressiveStrength ~ ., training)
predsvm <- predict(fit, testing)
error = predsvm - testing$CompressiveStrength
mse <- sqrt(mean(error^2))
mse
