#########################################################################
#
#   EXAM TYPE 2
#
#########################################################################

# 1. library load
library(dplyr)
library(randomForest)
library(pROC)
library(caret)

# 2. data load

# 3. pre handler

# 4. model

# 5. predict 

# 6. check

# 7. write csv

#########################################################################
library(e1071)
library(caret)
library(pROC)

#1
data <- read.csv("data/train_commerce.csv", header=T)
id <- sample(1:nrow(data), as.integer(nrow(data)*0.7))
train <- data[id,]
test <- data[-id,]
train <- train[,-1]
test <- test[,-1]
train$Reached.on.Time_Y.N <- as.factor(train$Reached.on.Time_Y.N)
test$Reached.on.Time_Y.N <- as.factor(test$Reached.on.Time_Y.N)

#2
model <- svm(Reached.on.Time_Y.N ~., train, type="C-classification", kernel="radial", cost=10, gamma=0.1)

#3
new <- data.frame(actual = test$Reached.on.Time_Y.N)
new$predict <- predict(model, test, decision.values=TRUE)

#4
cross_table <- table(new$predict, new$actual)
names(dimnames(cross_table)) <- c("Predicted", "Actual")
cross_table

accuracy <- sum(diag(cross_table)) / sum(cross_table) * 100
accuracy

error <- 100 - accuracy
error 

#5
confusionMatrix(cross_table)

# plot.roc(new$actual, as.integer(new$predict), legacy.axes = TRUE)
roc(new$actual, as.integer(new$predict))$auc

# write.csv(new,"result.csv")

#########################################################################
id <- sample(1:nrow(iris), as.integer(nrow(iris)*0.7))
train <- iris[id,]
test <- iris[-id,]

library(rpart)
tree <- rpart(Species ~., data = train)
# rpart.plot(tree)

new_tree <- data.frame(actual=test$Species)
new_tree$predict <- predict(tree, test, type="class")

cross_table_tree <- table(new_tree$predict, new_tree$actual)
names(dimnames(cross_table_tree)) <- c("Predict","Actual")
cross_table_tree

accuracy_tree <- sum(diag(cross_table_tree))/sum(cross_table_tree) * 100
accuracy_tree

error <- 100 - accuracy_tree
error

library(cacrt)
confunsionMatrix(cross_table_tree)

library(pROC)
plot.roc(new_tree$actual, as.integer(new_tree$predict), leagacy.axes = TRUE)
result_validation_tree <-  roc(new_tree$actual, as.integer(new_tree$predict))
names(result_validation_tree)
result_validation_tree$auc

### 
library(e1071)
svm <- svm(Species ~., train, type="C-classification")
new_svm <- data.frame(actual = test$Species)
new_svm$predict <- predict(svm, test, decision.values=TRUE)

cross_table_svm <- table(new_svm$predict, new_svm$actual)
names(dimnames(cross_table_svm)) <- c("Predict","Actual")
cross_table_svm

accuracy_svm <- sum(diag(cross_table_svm))/sum(cross_table_svm) * 100
accuracy_svm

error <- 100 - accuracy_svm
error

library(cacrt)
confunsionMatrix(cross_table_svm)

library(pROC)
plot.roc(new_svm$actual, as.integer(new_svm$predict), leagacy.axes = TRUE)
result_validation_svm <-  roc(new_svm$actual, as.integer(new_svm$predict))
names(result_validation_svm)
result_validation_svm$auc

# write.csv(new_svm,"result.csv")
# data <- read.csv("result.csv", header=T)
# View(data)

#########################################################################
data <- read.csv("data/insurance.csv", header=T)
data$sex <- as.factor(data$sex)

id <- sample(1:nrow(data), as.integer(nrow(data)*0.7))
train <- data[id,]
test <- data[-id,]

library(randomForest)
rfmodel <- randomForest(sex ~., train, ntree=100, proximity=TRUE) 

new <- data.frame(actual=test$sex)
new$predict <- predict(rfmodel, test)

result <- F1_Score(new$predict, new$actual)
result


trainmodel <- train(sex ~., train, method="rf", trControl=trainControl(method="cv",number=5), prox=TRUE, allowParallel=TRUE)
new <- data.frame(actual= test$sex)
new$predict <- predict(trainmodel, test)

result <- F1_Score(new$predict, new$actual)
result

data <- read.csv("data/insurance.csv", header=T)
data$sex <- as.factor(data$sex)
data$smoker <- as.factor(data$smoker)
data$region <- as.factor(data$region)

id <- sample(1:nrow(data), as.integer(nrow(data)*0.7))
train <- data[id,]
test <- data[-id,]

new <- data.frame(actual=test$sex)
library(party)
forestmodel <- cforest(Sex~., train)
predict <- predict(forestmodel,newdata=test, OOB=TRUE, type="response")
new$predict <- predict
result <- F1_Score(new$predict, new$actual)
result_validation_tree

# write.csv(new, "result.csv")
# result_cforest <- read.csv("result.csv", header=T)
# View(result_cforest)

f1score <- c(0.5445025,0.6095718,0.5592417)
macrofl_score <- mean(flscore)
macrofl_score

#########################################################################
data <- read.csv("data/carprice.csv", header=T)

id <- sample(1:nrow(data), as.integer(nrow(data)*0.75))
train <- data[id,]
test <- data[-id,]

regression <- lm(price~year+mileage+tax+mpg+engineSize, train)
regression_rslt <- data.frame(actual= test$price)
regression_rslt$predict <- predict(regression,newdata=test)
accuracy(regression_rslt$actual,regression_rslt$predict)

decisiontree_rpart <- rpart(price~year+mileage+tax+mpg+engineSize, data=train)
decisiontree_rpart_rslt <- data.frame(actual=test$price)
decisiontree_rpart_rslt$predict <- predict(decisiontree_rpart, newdata=test, type="vector")
accuracy(decisiontree_rpart_rslt$actual,decisiontree_rpart_rslt$predict)

decisiontree_tree <- tree(price~year+mileage+tax+mpg+engineSize,data=train)
decisiontree_tree_rslt <- data.frame(actual=test$price)
decisiontree_tree_rslt$predict <- predict(decisiontree_tree, newdata=test, type="vector")
accuracy(decisiontree_tree_rslt$actual,decisiontree_tree_rslt$predict)

rfmodel <- train(price~year+mileage+tax+mpg+engineSize,data=train, method="rf", trControl=trainControl(method="cv",number=2), prox=TRUE, allowParallel=TRUE)
rfmodel_rslt <- data.frame(actual=test$price)
rfmodel_rslt$predict <- predict(rfmodel,newdata=test)
accuracy(rfmodel_rslt$actual,rfmodel_rslt$predict)
plot(rfmodel)

rfmodel2 <- cforest(price~year+mileage+tax+mpg+engineSize,data=train)
rfmodel2_rslt <- data.frame(actual=test$price)
rfmodel2_rslt$predict <- predict(rfmodel2,newdata=test)
accuracy(rfmodel2_rslt$actual,rfmodel2_rslt$predict)

# write.csv(rfmodel_rslt,"990108.csv")
# result <- read.csv("990108.csv", header=T)
# View(result)

rf <- randomForest(price~year+mileage+tax+mpg+engineSize, data=train, ntree=100, proxmity=TRUE)
rf_rslt <- data.frame(actual=test$price)
rf_rslt$predict <- predict(rf,newdat=test)
accuracy(rf_rslt$actual, rf_rslt$predict)
# plot(rf)

data <- read.csv("carprice.csv", header=T)
data <- subset(data, select=c(year,mileage,tax,mpg, engineSize,price))

id <- sample(1:nrow(data), as.integer(0.75*nrow(data)))
train <- data[id,]
test <- data[-id,]

normalize <- function (x){
    return ((x-min(x))/(max(x)-min(x)))
}

norm_train <- as.data.frame(lapply(train,normalize))
norm_test <- as.data.frame(lapply(test,normalize))

norm_train$price <- train$price

model <- neuralnet(price~year+mileage+tax+mpg+engineSize, norm_train, hidden=c(5,5,5))
plot(model)

result <- data.frame(actual=test$price)
result$predict <- compute(model, norm_test[-length(norm_test)])$net.result
accuracy(result$actual, result$predict)

#########################################################################
#라이브러리 로딩
library(dplyr)
library(randomForest)
library(pROC)
library(caret)

#데이터
train = read.csv("data/customer_train.csv")
test = read.csv("data/customer_test.csv")

#전처리
train$환불금액[is.na(train$환불금액)] <- 0
test$환불금액[is.na(test$환불금액)] <- 0
train$주구매상품 <- as.integer(train$주구매상품)
test$주구매상품 <- as.integer(test$주구매상품)
train$성별 <- as.factor(train$성별)

#검증 위한 데이터 분리
set.seed(23)
idx <- createDataPartition(train$성별,p=0.8)
train <- train[idx$Resample1,]
val <- train[idx$Resample1,]
val_y <- val[,11]
val <- val[,-11]

#모델 학습 후 검증, 성능평가
model <- randomForest(성별~., train)
pred <- predict(model, val, type="class")
result <- data.frame(pred=pred)
confusionMatrix(result$pred, val_y, mode="prec_recall")

#########################################################################
# 출력을 원할 경우 print() 함수 활용
# 예시) print(df.head())

# setwd(), getwd() 등 작업 폴더 설정 불필요
# 파일 경로 상 내부 드라이브 경로(C: 등) 접근 불가

# 사용자 코딩
print("----------------1")
library(dplyr)
library(randomForest)
library(pROC)
library(caret)

print("----------------2")
train = read.csv("data/customer_train.csv")
test = read.csv("data/customer_test.csv")

#전처리
print("----------------3")
train$환불금액[is.na(train$환불금액)] <- 0
test$환불금액[is.na(test$환불금액)] <- 0
train$주구매상품 <- as.integer(train$주구매상품)
test$주구매상품 <- as.integer(test$주구매상품)
train$성별 <- as.factor(train$성별)
print("----------------4")
# id <- sample(1:nrow(data), as.integer(nrow(data)*0.7))
test <- test[,-11]
test$성별 <- '성별'
print("----------------5")

library(randomForest)
rfmodel <- randomForest(성별 ~., train, ntree=100) 
print("----------------6")

new <- data.frame(actual=test$성별)
new$predict <- predict(rfmodel, test)
print("----------------7")

library(pROC)
result <-  roc(new$acutal, as.integer(new$predict))
print("----------------8")
names(result)
print(result$auc)
print("----------------9")

# 답안 제출 참고
# 아래 코드는 예시이며 변수명 등 개인별로 변경하여 활용
# write.csv(data.frame변수,"result.csv",row.names = FALSE)

