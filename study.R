#########################################################################
install.packages("ggplot2")
library(ggplot2)

install.packages("psych")
library(psych)

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

install.packages("pROC")
library(pROC)

install.packages("adabag")
library(adabag)

install.packages("ada")
library(ada)

install.packages("randomForest")
library(randomForest)

install.packages("party")
library(party)

install.packages("tree")
library(tree)
#########################################################################

#########################################################################
#
#   STUDY TYPE 1
#
#########################################################################
a <- read.csv("data/mtcars.csv")
data <- a[order(-a$mpg), ]
data <- data[1:10,]
c2_hp <- mean(data$hp[data$carb == 2])
c1_hp <- mean(data$hp[data$carb == 1])
print(c2_hp - c1_hp)

#########################################################################
a <- read.csv("data/mtcars.csv")
data <- subset(a, am == 1 & cy1 == 4)
result <- mean(data$mpg)+sd(data$hp)
print(result)
data <- a %>% filter(am == 1 & cy1 == 4)
print(mean(data$mpg)+sd(data$hp))

#########################################################################
a <- read.csv("data/mtcars.csv")
data <- subset(a, am == 0)
print(boxplot(a))
q <- quantile(data$mpg)
print(q)
print(q[4] - q[2])
iqr <- IQR(data$mpg)
print(iqr)
r1 <- mean(data$mpg) - iqr
print(r1)
r2 <- mean(data$mpg) + iqr
print(r2)
result <- data$mpg <= r1 | data$mpg >= r2
print(result)
print(sum(data$mpg[result]))
print(sum(result))
print(sum(data$mpg[result])/sum(result))

#########################################################################
print(mean(iris$Petal.Length)+sd(iris$Petal.Length))
print(mean(iris$Petal.Width)+sd(iris$Petal.Width))
data <- subset(iris,Species == 'setosa')
maxpl <- max(data$Petal.Length)
print(maxpl)
minpl <- min(data$Petal.Length)
print(minpl)
minmax <- function(x){
	return ((x-min(x))/(max(x)-min(x)))
}
data$Petal.Length <- minmax(data$Petal.Length)
# data$Petal.Length <-((data$Petal.Length-min(data$Petal.Length))/(max(data$Petal.Length)-min(data$Petal.Length)))
print(mean(data$Petal.Length))

data <- subset(iris,Species == 'setosa')
zscore <- function (x) {
	return ((x-mean(x))/sd(x))
}
data$Petal.Length <- zscore(data$Petal.Length)
# data$Petal.Length <- ((data$Petal.Length - mean(data$Petal.Length))/sd(data$Petal.Length))
print(mean(data$Petal.Length))

#########################################################################
data <- subset(iris, Species =='versicolor' | Species =='virginica')
print(dim(data))
print(summary(data))
median_sepal <- median(data$Sepal.Length)
print(median_sepal)
median_petal <- median(data$Petal.Length)
print(median_petal)
result1 <- data$Sepal.Length >= median_sepal & data$Species == "versicolor"
print(sum(result1))
result2 <- data$Petal.Length >= median_petal & data$Species == "virginica"
print(sum(result2))
print(nrow(data[data$Species=="versicolor",]))
ratio_versicolor <- sum(result1) / nrow(data[data$Species=="versicolor",])
print(ratio_versicolor)
ratio_virginica <- sum(result2) / nrow(data[data$Species=="virginica",])
print(ratio_virginica)
print(ratio_virginica - ratio_versicolor)

#########################################################################
data <- subset(iris, Species =='setosa')
print(dim(data))
median_sepal <- median(data$Sepal.Length)
print(median_sepal)
data$add_data <- ifelse(data$Sepal.Length > median_sepal ,1, 0)
print(head(data))
print(mean(data$Sepal.Length[data$add_data==1]))

#########################################################################
airquality[airquality$Month == 8 & airquality$Day == 26,]
airquality[airquality$Month == 8 & airquality$Day == 26,]$Ozone
airquality[airquality$Month == 8 & airquality$Day == 26,]$Solar.R
d1 <- airquality[airquality$Month == 8 & airquality$Day == 26,]$Ozone
d2 <- airquality[airquality$Month == 8 & airquality$Day == 26,]$Solar.R
data <- subset(airquality, airquality$Month == 8)
head(data)
nrow(data)
dim(data)
describe(data)

result1 <- data$Ozone >= d1
sum(result1, na.rm =TRUE)
print(mean(data$Ozone[result1], na.rm = TRUE))

result2 <- data$Solar.R >= d2
sum(result2, na.rm = TRUE)
print(mean(data$Solar.R[result2], na.rm = TRUE))

print(sum(result1, na.rm = TRUE)+sum(result2, na.rm = TRUE))

#########################################################################
data <- airquality[order(-airquality$Solar.R),]
data <- data[1:(nrow(data)*0.8),]
median_before <- median(data$Ozone, na.rm = TRUE)
median_before
mean <- mean(data$Ozone, na.rm = TRUE)
mean
data$Ozone <- ifelse(is.na(data$Ozone),mean,data$Ozone)
median_after <- median(data$Ozone)
median_after
print(median_before - median_after)

#########################################################################
data <- na.omit(airquality)
summary(data)
q <- quantile(data$Ozone)
q
str(q)
data$Ozone <- ifelse(data$Ozone >=q[4] | data$Ozone <= q[2],0,data$Ozone)
summary(data)
print(mean(data$Ozone)+sd(data$Ozone))

#########################################################################
data <- read.csv("data/diamonds.csv")
data <- data[order(-data$price),]
dim(data)
data <- data[1:200,]
nrow(data)
data <- subset(data, cut=="Premium")
dim(data)
mean <- mean(data$price)
mean

#########################################################################
data <- read.csv("data/diamonds.csv")
data <- subset(data, depth >=60 & table >=60)
data <- data[order(-data$price),]
data <- data[1:100,]
dim(data)
max_v <- max(data$price)
min_v <- min(data$price)
print(max_v - min_v)

#########################################################################
data <- read.csv("data/diamonds.csv")
data <- subset(data, cut == 'Ideal')
dim(data)
c1 <- cor(data$x,data$price)
c1
c2 <- cor(data$y,data$price)
c2
c3 <- cor(data$z,data$price)
c3
result <- data.frame(c1,c2,c3)
result
result$max_cor <- max(result)
result
setwd("result")
write.csv(result,"cor.csv",row.names=TRUE)
check <- read.csv("cor.csv",header=T, fileEncoding="EUC-KR")
check
View(check)
par(mfrow=c(1,3))
plot(data$x, data$price, type="p")
plot(data$y, data$price, type="l")
plot(data$z, data$price, type="b")

#########################################################################
data <- read.csv("data/diamonds.csv")
data <- subset(data, carat >= 1 & cut == 'Premium')
data <- data[order(-data$price),]
data <- data[1:100,]
dim(data)
f <- sum(data$color =="F") / nrow(data)
f
g <- sum(data$color =="G") / nrow(data)
g
h <- sum(data$color =="H") / nrow(data)
h
result <- data.frame(f,g,h,max(f,g,h))
result
setwd("result")
write.csv(result,"color.csv",row.names=FALSE)
check <- read.csv("color.csv", header=T)
check
View(check)

#########################################################################
data <- read.csv("data/Boston.csv")
dim(data)
data <- data[order(-data$medv),]
print(data$medv[1:20])
data$medv <- ifelse(data$medv == max(data$medv), median(data$medv), data$medv)
print(data$medv[1:20])
data_age80 <- data[data$age >=80,]
dim(data_age80)
result <- mean(data_age80$medv)
print(result)
data <- read.csv("data/Boston.csv")
check <- data[data$age >= 80,]
check_mean <- mean(check$medv)
print(check_mean)

#########################################################################
presidents
str(presidents)
summary(presidents)
data <- matrix(presidents, ncol=4, byrow=TRUE)
data <- data.frame(data)
names(data) <- c("Qtr1","Qtr2","Qtr3","Qtr4")
data
result <- is.na(data)
head(result)
no_na <- apply(result, 2, sum)
no_na
print(max(no_na))
summary(data)

#########################################################################
minmax <- function (x){
    return ((x - min(x))/(max(x)-min(x)))
}
data <- data.frame(state.x77)
result <- minmax(data$Income)
head(result)
print(sum(result > 0.5))
length(result)
zscore <- function (x) {
    return ((x - mean(x))/sd(x))
}
result <- zscore(data$Income)
head(result)
print(sum(result > 0))

#########################################################################
precip
data <- data.frame(names(precip))
data$precip <- precip
names(data) <- c("US city","precip")
summary(data)
q25 <- quantile(data$precip,0.25)
q25
q75 <- quantile(data$precip,0.75)
q75
iqr <- q75 - q25
iqr
result <- data$precip >= (q75+1.5*iqr) | data$precip <= (q25-1.5*iqr)
result
outlier <- data$precip[result]
print(outlier)
boxplot(data$precip)

#########################################################################
data <- read.csv("data/housing.csv", header=T, fileEncoding="EUC-KR")
median_value <- median(data$total_bedrooms, na.rm = T)
median_value
data$total_bedrooms <- ifelse(is.na(data$total_bedrooms),median_value,data$total_bedrooms)
data1 <- data
summary(data1)
mean_value <- mean(data1$total_bedrooms)
mean_value
sd_value <- sd(data1$total_bedrooms)
sd_value
low <- mean_value - sd_value  * 1.5
low
upper <- mean_value + sd_value * 1.5
upper
result <- data1$total_bedrooms >= upper | data1$total_bedrooms <= low
result
outlier <- data$total_bedrooms[result]
outlier
print(mean(outlier))
boxplot(data1$total_bedrooms)

#########################################################################
data <- read.csv("data/train_commerce.csv", header=T)
dim(data)

d1 <- data[order(-data$Customer_care_calls),][1:500,]
dim(d1)
d1_rate <- sum(ifelse(d1$Reached.on.Time_Y.N == 1, 1, 0))/nrow(d1)
d1_rate

d2 <- data[order(-data$Cost_of_the_Product),][1:500,]
dim(d2)
d2_rate <- sum(ifelse(d2$Reached.on.Time_Y.N == 1, 1, 0))/nrow(d2)
d2_rate

d3 <- data[order(-data$Weight_in_gms),][1:500,]
dim(d3)
d3_rate <- sum(ifelse(d3$Reached.on.Time_Y.N == 1, 1, 0))/nrow(d3)
d3_rate

a <- c(d1_rate,d2_rate,d3_rate,max(d1_rate,d2_rate,d3_rate))
a

d1_cor <- cor(d1$Customer_care_calls,d1$Reached.on.Time_Y.N)
d1_cor
d2_cor <- cor(d2$Cost_of_the_Product,d2$Reached.on.Time_Y.N)
d2_cor
d3_cor <- cor(d3$Weight_in_gms,d3$Reached.on.Time_Y.N)
d3_cor

b <- c(d1_cor,d2_cor,d3_cor,max(d1_cor,d2_cor,d3_cor))
b

result <- data.frame(rbind(a,b))
result

colnames(result) <- c("Care_calls","Cost_product","Weight","Max")
rownames(result) <- c("Ration_onTime", "Correlation")

result
# setwd("")
# write.csv("result","result.csv",row.names=TRUE)
# check <- read.csv("result.csv",header=T, fileEncoding="EUC-KR")
# check
# View(check)
# par(mfrow=c(1,3))
# plot(d1$Customer_care_calls,d1$Reached.on.Time_Y.N)
# plot(d2$Cost_of_the_Product,d2$Reached.on.Time_Y.N
# plot(d3$Weight_in_gms,d3$Reached.on.Time_Y.N

#########################################################################
data <- read.csv("data/insurance.csv", header=T)
dim(data)
mean_v <- mean(data$bmi)
mean_v
sd_v <- sd(data$bmi)
sd_v
upper <- mean_v + 1.5 * sd_v
upper
low <- mean_v - 1.5 * sd_v
low

outlier <- data$bmi >= upper | data$bmi <= low
outlier

result <- data$bmi[outlier]
result

print(mean(result))

#########################################################################
data <- read.csv("data/country.csv", header=T)
data_naomit <- na.omit(data)
data_naomit

mean <- apply(data_naomit[9,c(2:8)],1, mean)
mean
n <- 0
for (i in 2:length(data_naomit)){
    if(data_naomit[9,i]>mean) n <- n+1
    print(n)
}
result <- ifelse(data_naomit[9,-1] > mean, TRUE, FALSE)
result
print(sum(result))

#########################################################################
#
#   STUDY TYPE 2
#
#########################################################################
data <- read.csv("data/Ionosphere.csv", header=T)
data <- data.frame(data)
data <- subset(data, select=c(-V1,-V2))
data <- na.omit(data)

data$Class <- as.numeric(data$Class)
id <- sample(1:nrow(data),as.integer(0.7*nrow(data)))
train <- data[id,]
test <- data[-id,]

model <- glm(Class ~., data=train)

new <- data.frame(actual = test$Class)
new$predict <- round(predict(model,test),0)
new[(new[0,2]==0),]
new <- subset(new, !(new$predict ==0))
new$actual <- as.factor(new$actual)
new$predict <- as.factor(new$predict)

confusionMatrix(new$predict,new$actual)

#plot.roc(new$actual, as.integer(new$predict), legacy.axes = TRUE)
roc(new$actual, as.integer(new$predict))$auc

#########################################################################
data <- read.csv("data/Ionosphere.csv", header=T)
data <- data.frame(data)
data <- subset(data, select=c(-V1,-V2))
data <- na.omit(data)

id <- sample(1:nrow(data),as.integer(0.7*nrow(data)))
train <- data[id,]
test <- data[-id,]

model <- svm(Class ~., train, type="C-classification", kernel="radial", cost=10, gamma=0.1)

new <- data.frame(actual=test$Class)
new$predict <- predict(model,test, decision.values = TRUE)
head(new)
new$actual <- as.factor(new$actual)
new$predict <- as.factor(new$predict)

confusionMatrix(new$predict, new$actual)

#plot.roc(new$actual, as.integer(new$predict), legacy.axes=TRUE)
roc(new$actual, as.integer(new$predict))$auc

svmtune <- tune.svm(factor(Class) ~., data=train, gamma=c(0.1,2), cost=c(5, 15))
svmtune
summary(svmtune)

model <- svm(Class~., train, type="C-classification",kernel="radial",cost=15,gamma=0.1)
new <- data.frame(actual=test$Class)
new$predict <- predict(model, test, decision.values = TRUE)
new$actual <- as.factor(new$actual)
new$predict <- as.factor(new$predict)

confusionMatrix(new$predict, new$actual)

#setwd("result")
#write.csv(new,"980415.csv")
#result <- read.csv("980415.csv", header=T, fileEncoding = "EUC-KR")
#View(result)

#########################################################################
data <- read.csv("data/Ionosphere.csv", header=T)
data <- data.frame(data)
data <- subset(data, select=c(-V1,-V2))
data <- na.omit(data)

id <- sample(1:nrow(data), as.integer(0.7*nrow(data)))
train <- data[id,]
test <- data[-id,]

model <- naiveBayes(Class ~., train)

new <- data.frame(actual=test$Class)
new$predict <- predict(model,test)
new$actual <- as.factor(new$actual)
new$predict <- as.factor(new$predict)

confusionMatrix(new$predict, new$actual)

#plot.roc(new$actual, as.integer(new$predict), legacy.axes=TRUE)

roc(new$actual, as.integer((new$predict)))$auc

#########################################################################
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

#########################################################################
data <- data.frame(state.x77)

model <- lm(Population ~ Income+Illiteracy+Life.Exp+Murder+HS.Grad+Frost, data)
summary(model)

new <- data.frame(actual = data$Population)
new$predict <- -90796.639+2.501*data$Income-3416.368*data$Illiteracy+1353.677*data$Life.Exp+862.094*data$Murder-219.665*data$HS.Gard-25.785*data$Frost
head(new)

accuracy(new$actual, new$predict)
cor(new$actual, new$predict)
plot(new$actual, new$predict)
abline(lm(new$predict~new$actual))

#########################################################################
data <- data.frame(state.x77)

id <- sample(1:nrow(data), as.integer(0.7*nrow(data)))
train <- data[id,]
test <- data[-id,]

model <- ctree(Population-Income+lliteracy+Life.Exp+Murder+HS.Grade+Frost, train)
plot(model)

new <- data.frame(actual = test$Population)
new$predict <- predict(model, test)
new

plot(new$actual, new$predict)
abline(lm(new$predict~new$actual))

model <- rpart(Population~Income+lliteracy+Life.Exp+Murder+HS.Grade+Frost, train)
rpart.plot(model)

new <- data.frame(actual = test$Population)
new$predict <- predict(model, test)
new
accuracy(new$actual, new$predict)

model <- tree(Population~Income+lliteracy+Life.Exp+Murder+HS.Grade+Frost, train)
plot(model)
text(model)

new <- data.frame(actual =  test$Population)
new$predict <- predict(model, test)
new
accuracy(new$actual, new$predict)

#########################################################################
data <- data.frame(state.x77)

id <- sample(1:nrow(data), as.integer(0.7*nrow(data)))
train <- data[id,]
test <- data[-id,]

normalize <- function (x){
  return ((x-min(x))/(max(x)-min(x)))
}

norm_train <- as.data.frame(lapply(train,normalize))
norm_test <- as.data.frame(lapply(test,normalize))
norm_train$Population <- train$Population

model <- neuralnet(Population-Income+lliteracy+Life.Exp+Murder+HS.Grade+Frost, norm_train)
plot(model)

new <- data.frame(actual=test$Population)
new$predict <- compute(model, norm_test[-length(norm_test)])$net.result
new

accuracy(new$actual, new$predict)
cor(new$actual, new$predict)

model <- neuralnet(Population-Income+lliteracy+Life.Exp+Murder+HS.Grade+Frost, norm_train, hidden=c(3,3))
new <- data.frame(actual=test$Population)
new$predict <- compute(model, norm_test[-length(norm_test)])$net.result
new

accuracy(new$actual, new$predict)
plot(model)

#########################################################################
data <- read.csv("data/train_commerce.csv", header=T)
data <- na.omit(data)
data <- data[,-1]
summary(data)

data$Reached.on.Time_Y.N <- as.factor(data$Reached.on.Time_Y.N)
str(data)

id <- sample(1:nrow(data), as.integer(0.7*nrow(data)))
train <- data[id, ]
test <- data[-id, ]

model <- tree(Reached.on.Time_Y.N - Customer_care_calls+Customer_rating+Cost_of_the_Product+Prior_purchases+Weight_in_gms,train)

summary(model)
names(model)
nrow(train)
nrow(test)
plot(model)
text(model)

new <- data.frame(actual = test$Reached.on.Time_Y.N)
new$predict <- predict(model, test, type='class')
head(new)

confusionMatrix(new$predict, new$actual)
plot.roc(new$actual, as.integer(new$predict), legacy.axes=TRUE)

result <- roc(new$actual, as.integer(new$predict))
result
names(result)
result$auc

cvtree <- cv.tree(model, FUN=prune.misclass)
plot(cvtree)

prunetree <- prune.misclass(model, best=5)
plot(prunetree)
text(prunetree)

new <- data.frame(actual = test$Reached.on.Time_Y.N)
new$predict <- predict(prunetree, test, type='class')
head(new)

confusionMatrix(new$predict, new$actual)
#write.csv(new, "980415.csv")
#confirm <- read.csv("980415.csv", header=T)
#head(confirm)
#View(confirm)

#########################################################################
#
#   STUDY TYPE 3
#
#########################################################################
data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data1 <- subset(data, data$성별 == "남자")
data2 <- subset(data, data$성별 == "여자")
t_result <- t.test(data1$쇼핑만족도,data2$쇼핑만족도)
t_result

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data1 <- subset(data, data$주거지역 == "소도시")
data2 <- subset(data, data$주거지역 == "중도시")
t_result <- t.test(data1$쇼핑액,data2$쇼핑액)
t_result

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data1 <- subset(data, data$주거지역 == "소도시")
data2 <- subset(data, data$주거지역 == "대도시")
t_result <- t.test(data1$쇼핑액,data2$쇼핑액)
t_result

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data1 <- subset(data, data$주거지역 == "소도시")
data2 <- subset(data, data$주거지역 == "대도시")
t_result <- t.test(data1$쇼핑액,data2$쇼핑액)
t_result

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data <- subset(data, select=c(고객번호, 쇼핑1월, 쇼핑3월))
t_result <- t.test(data$쇼핑1월,data$쇼핑3월,paired=TRUE)
t_result

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data <- subset(data, select=c(고객번호, 쇼핑2월, 쇼핑3월))
t_result <- t.test(data$쇼핑2월,data$쇼핑3월,paired=TRUE)
t_result

#########################################################################
setosa_data <- subset(iris,Species == 'setosa')
versicolor <-  subset(iris,Species == 'versicolor')
virginica <-  subset(iris,Species == 'virginica')

t.test(setosa_data$Petal.Length,versicolor$Petal.Length)

t.test(setosa_data$Petal.Length,virginica$Petal.Length)

t.test(setosa_data$Petal.Length,setosa_data$Sepal.Length, paired=TRUE)

t.test(setosa_data$Petal.Width,setosa_data$Sepal.Width, paired=TRUE)

#########################################################################
data1 <- subset(mtcars, mtcars$vs == 0)
data2 <- subset(mtcars, mtcars$vs == 1)
t_result <- t.test(data1$mpg, data2$mpg)
t_result

data1 <- subset(mtcars, mtcars$am == 0)
data2 <- subset(mtcars, mtcars$am == 1)
t_result <- t.test(data1$mpg, data2$mpg)
t_result

data <- mtcars
data$id <- as.integer(as.factor(rownames(data)))
t_result <- t.test(data$id, data$wt)
t_result

t_result <- t.test(data$id, data$hp)
t_result

#########################################################################
data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
n1 <- length(which(data$주거지역 == "소도시"))
n1
n2 <- length(which(data$주거지역 == "중도시"))
n2
n <- c(n1, n2)
n
x1 <- length(which(data$주거지역 == "소도시" & data$쿠폰선호도 == "예"))
x1
x2 <- length(which(data$주거지역 == "중도시" & data$쿠폰선호도 == "예"))
x2
x <- c(x1,x2)
x

prop_result <- prop.test(x,n)
prop_result

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
n1 <- length(which(data$주거지역 == "소도시"))
n1
n2 <- length(which(data$주거지역 == "대도시"))
n2
n <- c(n1, n2)
n
x1 <- length(which(data$주거지역 == "소도시" & data$쿠폰선호도 == "예"))
x1
x2 <- length(which(data$주거지역 == "대도시" & data$쿠폰선호도 == "예"))
x2
x <- c(x1,x2)
x

prop_result <- prop.test(x,n)
prop_result

#########################################################################
x <- data.frame(Titanic)
n1 <- length(which(x$Sex == "Male"))
n1
n2 <- length(which(x$Sex == "Female"))
n2
n <- c(n1,n2)
n

x1 <- length(which(x$Sex == "Male" & x$Survived == "Yes"))
x1
x2 <- length(which(x$Sex == "Female" & x$Survived == "Yes"))
x2
x <- c(x1, x2)
x

props_result <- prop.test(x,n)
props_result

x <- data.frame(Titanic)
n1 <- length(which(x$Class == "1st"))
n1
n2 <- length(which(x$Class == "2nd"))
n2
n <- c(n1,n2)
n

x1 <- length(which(x$Class == "1st" & x$Survived == "Yes"))
x1
x2 <- length(which(x$Class == "2nd" & x$Survived == "Yes"))
x2
x <- c(x1, x2)
x

props_result <- prop.test(x,n)
props_result

#########################################################################
data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data <- subset(data, select=c(주거지역, 이용만족도))
head(data)

data1 <- subset(data, data$주거지역 == "소도시")
data2<- subset(data, data$주거지역 == "중도시")
data3<- subset(data, data$주거지역 == "대도시")

aov_result <- aov(이용만족도 ~ 주거지역 , data)
aov_result
summary(aov_result)

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data <- subset(data, select=c(주거지역, 쇼핑만족도))
 
aov_result <- aov(쇼핑만족도 ~ 주거지역 , data)
aov_result
summary(aov_result)

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
aov_data <- as.data.frame(rbind(cbind(data$이용만족도, 1), cbind(data$쇼핑만족도,2)))

colnames(aov_data) <- c("만족도","구분")
head(aov_data)

aov_result <- aov(만족도~구분, aov_data)
aov_result
summary(aov_result)

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
aov_data <- as.data.frame(rbind(cbind(data$품질, 1), cbind(data$가격,2), cbind(data$서비스,3), cbind(data$배송,4)))

colnames(aov_data) <- c("만족도","구분")
head(aov_data)

aov_result <- aov(만족도~구분, aov_data)
aov_result
summary(aov_result)

#########################################################################