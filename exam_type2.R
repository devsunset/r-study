#########################################################################
#
#   EXAM TYPE 2
#
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
library(randomForest)
library(caret)
library(dplyr)
library(e1071)

# 데이터 로드 
X_train = read.csv("data/X_train.csv", header=T, fileEncoding = "EUC-KR")
y_train = read.csv("data/y_train.csv", header=T, fileEncoding = "EUC-KR")
X_test = read.csv("data/X_test.csv", header=T, fileEncoding = "EUC-KR")

# 전처리 
X_train$환불금액  <- ifelse(is.na(X_train$환불금액),0,X_train$환불금액)
X_test$환불금액  <- ifelse(is.na(X_test$환불금액),0,X_test$환불금액)

X_train$총구매액  <- ifelse(is.na(X_train$총구매액),0,X_train$총구매액)
X_test$총구매액  <- ifelse(is.na(X_test$총구매액),0,X_test$총구매액)

X_train <- X_train %>% filter(총구매액 >= 0)
X_train <- X_train %>% filter(최대구매액 >= 0)

X_test <- X_test %>% filter(총구매액 >= 0)
X_test <- X_test %>% filter(최대구매액 >= 0)

X_train <- X_train %>% mutate_if(is.character, as.factor)
X_test <- X_test %>% mutate_if(is.character, as.factor)

y_train$gender <- as.factor(y_train$gencer)

train <- merge(X_train, y_train, by="cust_id")
test <- X_test

# 분리 
set.seed(123)
idx <- sample(1:nrow(train),as.integer(nrow(train)*0.7))
train_data <- train[idx,]
valid_data <- train[-idx,]

# 정규화 
train_pre <- preProcess(train_data, method="range")
valid_pre <- preProcess(valid_data, method="range")

train_scaled <- predict(train_pre, train_data)
valid_scaled <- predict(valid_pre, valid_data)

train_data <- train_scaled
valid_data <- valid_scaled

# 모델생성 
md.rf <- randomForest(gender~.-cust_id, data=train_data, ntree=300)
md.glm <- train(gender~. -cust_id, data=train_data, mthod="glm" )
md.svm <- svm(gender~. -cust_id, data=train_data, cost=10, gamma=0.01)

# 예측 
pred.rf <- predict(md.rf, newdata=valid_data)
pred.glm <- predict(md.glm, newdata=valid_data)
pred.svm <- predict(md.svm, newdata=valid_data)

# 정확도 
acc.rf <- caret::confusionMatrix(valid_data$gender, pred.rf)$overall[1]
acc.glm <- caret::confusionMatrix(valid_data$gender, pred.glm)$overall[1]
acc.svm <- caret:: confusionMatrix(valid_data$gender, pred.svm)$overall[1]

print(acc.rf)
print(acc.glm)
print(acc.svm)

# glm 선택 , train 데이터셋 전체로 모델링 
md.fit <- train(gender~. - cust_id, data=train, medtho="glm")
pred.fit <- predict(md.fit, newdata=test)

df <- data.frame(custid=test$cust_id, gender=pred.fit)

write.csv("result.csv", row.names=F)

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
