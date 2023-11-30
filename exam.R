#########################################################################
#
#   EXAM TYPE 1
#
#########################################################################
data <- read.csv("data/Boston.csv",header=T)
data <- data[order(-data$crim),]
data$crim[1:10] <- data$crim[10]
print(data$crim[1:10])

data_age80 <- data[data$age >= 80,]
result <- mean(data_age80$crim)
print(result)

check <- read.csv("data/Boston.csv",header=T)
check <- check[check$age >= 80,]
check_mean <- mean(check$crim) 
print(check_mean)

#########################################################################
data <- read.csv("data/housing.csv",header=T)
dim(data)

n <- nrow(data) * 0.8
n

data1 <- data[c(1:n),]
dim(data1)

sd_data1 <- sd(data1$total_bedrooms, na.rm = T)
print(sd_data1)

median <- median(data1$total_bedrooms, na.rm = T)
print(median)

data1$total_bedrooms <- ifelse(is.na(data1$total_bedrooms), median, data1$total_bedrooms)
summary(data1)

sd_data1 <- sd(data1$total_bedrooms)
print(sd_data1)

#########################################################################
data <- read.csv("data/insurance.csv", header=T)
m <- mean(data$charges)
print(m)
n <- sd(data$charges)
print(n)

# print(sum(data$charges[data$charges >= mean(data$charges)+1.5*sd(data$charges)]))

outlier <- m +1.5 * n
outlier

result <- data$charges >= outlier
head(result)

print(sum(data$charges[result]))
print(sum(data$charges))
print(boxplot(data$charges))

#########################################################################
data <- read.csv("data/country.csv", header=T)
q7 <- quantile(na.omit(data)$Guam,0.3) 
# q7 <- quantile(na.omit(data)[,3],0.3) 
print(q7)

#########################################################################
data <- read.csv("data/country.csv", header=T)
data_naomit <- na.omit(data)
# print(sum(data_naomit[6,2:8])/(length(data)-1))
# m <- sum(data_naomit[6,2:8])/(length(data)-1)
# m

# n <- 0
# for(i in 2:length(data_naomit)){
#     if(data_naomit[6,i]> m){
#         n <- n +1
#     }
# }
# print(n)

m <- apply(data_naomit[6,c(2:8)], 1, mean)
m

n <- 0
for (i in 2:length(data_naomit)){
    if(data_naomit[6,i] > m){
        n <- n+1
    }
}
print(n)

#########################################################################
data <- read.csv("data/country.csv", header=T)
result <- lapply(data[,2:8], function(x){sum(is.na(x))})
result

f <- data.frame(country=colnames(data)[2:8], gap=as.numeric(result))
f
library(dplyr)
f %>% filter(gap==max(as.numeric(result)))
print(f %>% filter(gap==max(as.numeric(result))))

#########################################################################
q1 <- quantile(women$weight, 0.25)
q1

q3 <- quantile(women$weight, 0.75)
q3

value <- abs(q1-q3)
value

print(trunc(value))
print(floor(value))
print(ceiling(value))

#########################################################################
data <- read.csv("data/USvideos.csv", header=T)
data$ratio <- data$likes/data$view 
print(sum(ifelse(data$category_id == 10 & data$ratio > 0.04 &  data$ratio <0.05 , 1, 0)))

#########################################################################
data <- read.csv("netflix.csv",header=T)
data$year <- year(data$date_added)
date$month <- month(date_added)
date$day <- day(date_added)
print(sum(ifelse(data$country=='United Kingdom' & data$year ==2021 & ( data$month ==8 | data$month ==8) ,1,0 )))

#########################################################################
data <- read.csv("garbagebag.csv",header=T)
data1 <- subset(data, 종량제봉투처리방식 == "소각용" & 종량제봉투사용대상 == "가정용" & X2L가격 !=0)
price <- mean(data1$X2L가격)

#########################################################################
data <- read.csv("index.csv", header=T)
data$bmi <- data$Weight / ((data$Height/100)*(data$Height/100))
print(abs(sum(data$bmi>=25)-sum(data$bmi <25)))
print(sum(data$bmi >= 25))
print(sum(data$bmi < 25))

#########################################################################
data <- read.csv("student.csv",header=T)
data1 <- subset(data, (전입학생수합계.명. != is.na(전입학생수합계.명.) | 전출학생수합계.명. !=is.na(전출학생수합계.명.)))
data1$net <- 전입학생수합계.명. - 전출학생수합계.명.
data2 <- data1[order(-data1$net),]
print(data2[1,4])
print(data2[2,4])
print(data2[1,27])
print(data2$net[1])
print(data2[1,26])
print(전체학생수합계.명.[1])

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


#########################################################################
