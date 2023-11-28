install.packages("adabag")
library(adabag)

install.packages("ada")
library(ada)

install.packages("randomForest")
library(randomForest)

install.packages("party")
library(party)

data <- read.csv("data/Ionosphere.csv",header=T)
data <- data.frame(data)
data <- subset(data, select=c(-V1,-V2))
data <- na.omit(data)

id <- sample(1:nrow(data), as.integer(0.7*nrow(data)))
train <- data[id,]
test <- data[-id,]


bag <- bagging(Class ~., train, mfinal=10)
plot(bag$trees[[10]])
text(bag$trees[[10]])

new <- data.frame(actual = test$Class)
predict <- predict(bag,test)
new$predict <- predict$class
head(new)
str(new)
new$predict <- as.factor((new$predict))
str(new)

confusionMatrix(new$predict, new$actual)
roc(new$actual, as.integer(new$predict))$auc


bst <- boosting(Class ~., train, boos=TRUE, mfinal=100)
plot(bst$trees[[10]])
text(bst$trees[[10]])

new <- data.frame(actual = test$Class)
new$predict <- as.factor((new$predict)$class)

confusionMatrix(new$predict, new$actual)
roc(new$actual, as.integer(new$predict))$auc


adabst <- ada(Class ~., train, iter=20 , nu=1 ,type="distance")
new <- data.frame(actual = test$Class)
new$predict <- predict(adabst, test)

confusionMatrix(new$predict, new$actual)
roc(new$actual, as.integer(new$predict))$auc


data <- read.csv("data/Ionosphere.csv",header=T)
data <- data.frame(data)
data <- subset(data, select=c(-V1,-V2))
data <- na.omit(data)

id <- sample(1:nrow(data), as.integer(0.7*nrow(data)))
train <- data[id,]
test <- data[-id,]

rfmodel <- randomForest(Class ~., train, ntree=100, proximity=TRUE)
rfmodel

plot(rfmodel)
varImpPlot(rfmodel)
plot(margin(rfmodel))

new <- data.frame(actual = test$Class)
new$predict <- predict(rfmodel,test)

#new$actual <- as.factor(new$actual)
#new$predict <- as.factor(new$predict)

confusionMatrix(new$predict, new$actual)
roc(new$actual,as.integer(new$predict))$auc

model <- train(Class ~., train, method="rf", trControl=trainControl(method="cv", number=5), prox=TRUE, allowParallel=TRUE)
xplot(model)

new <- data.frame(actual = test$Class)
new$predict <- predict(model,test)

confusionMatrix(new$predict, new$actual)
roc(new$actual, as.integer(new$predict))$auc


model <- cforest(Class ~., train)
predict <- predict(model, newdata= test, OOB=TRUE, type="responser")
new <- data.frame(actual = test$Class)

confusionMatrix(new$predict, new$actual)
roc(new$actual, as.integer(new$predict))$auc
plot(predict)


#setwd("result")
#write.csv(new, "980415;csv")
#result <- read.csv("980415.csv", header=T, fileEncoding = "EUC-KR")
#View(result)