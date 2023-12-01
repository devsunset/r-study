#########################################################################
#
#   EXAM TYPE 3
#
#########################################################################

df = read.csv("data/Titanic.csv")

library(dplyr)
options(scipen=999)
options(digits=8)

xtabs(~Gender+Survived, data=df)
m <- xtabs(~Gender+Survived, data=df)
print(head(m))

chisq.test(m)
# X-squared = 260.717, df = 1, p-value < 0.000000000000000222

df <- glm(Survived~Gender+SibSp+Parch+Fare,data=df,family="binomial")
print(df)

# (Intercept)   Gendermale        SibSp        Parch         Fare  
#    0.946635    -2.642219    -0.353892    -0.200724     0.014685  
print(round(-0.200724,3))
# -0.201

print(round(exp(-0.353892),2))
# 0.7

#########################################################################
### check point
t.test(data1$쇼핑만족도,data2$쇼핑만족도)

### check point
t_result <- t.test(data$쇼핑1월,data$쇼핑3월,paired=TRUE)

### check point
n1 <- length(which(data$주거지역 == "소도시"))
n1
n2 <- length(which(data$주거지역 == "중도시"))
n2
n <- c(n1, n2)
x1 <- length(which(data$주거지역 == "소도시" & data$쿠폰선호도 == "예"))
x1
x2 <- length(which(data$주거지역 == "중도시" & data$쿠폰선호도 == "예"))
x2
x <- c(x1,x2)

### check point
prop_result <- prop.test(x,n)

### check point
aov_result <- aov(이용만족도 ~ 주거지역 , data)

### check point
aov_data <- as.data.frame(rbind(cbind(data$이용만족도, 1), cbind(data$쇼핑만족도,2)))

### check point
if (t_result$p.value < 0.05){
    print("Reject of Null Hypothesis")
}else{
    print("Accept of Null Hypothesis")
}

### check point
c_result <- chisq.test(df$Sex , df$SurviedText)

### check poing
logit <- glm(Survived~Sex+SibSp+Parch+Fare, data=df, family="binomial")
print(summary(logit))
print(logit)
print(round(exp(-0.353892),3))

#########################################################################

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data1 <- subset(data, data$성별 == "남자")
data2 <- subset(data, data$성별 == "여자")
### check point
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
### check point
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
### check point
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

### check point
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

props_result <- prop.test(x,n)
props_result

#########################################################################
data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data <- subset(data, select=c(주거지역, 이용만족도))
head(data)

data1 <- subset(data, data$주거지역 == "소도시")
data2<- subset(data, data$주거지역 == "중도시")
data3<- subset(data, data$주거지역 == "대도시")

### check point
aov_result <- aov(이용만족도 ~ 주거지역 , data)
aov_result
summary(aov_result)

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data <- subset(data, select=c(주거지역, 쇼핑만족도))
 
aov_result <- aov(쇼핑만족도 ~ 주거지역 , data)
aov_result
summary(aov_result)

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
### check point
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
data <- read.csv("data/height.csv",header=T)
mean(data$h_after-data$h_before)
round(mean(data$h_after-data$h_before),2)

### check point
t_result <- t.test(data$h_after , data$h_before, alternative="greater", paired=TRUE)

### check point
summar(t_result)
print(t_result$statistic)
round(t_result$statistic,2)
print(t_result$p.value)
round(t_result$p.value,4)

### check point
if (t_result$p.value < 0.05){
    print("Reject of Null Hypothesis")
}else{
    print("Accept of Null Hypothesis")
}

#########################################################################
data <- read.csv("data/record.csv",header=T)
mean(data$r_after-data$r_before)
round(mean(data$r_after-data$r_before),2)

t_result <- t.test(data$r_after , data$r_before, alternative="greater", paired=TRUE)

summar(t_result)
print(t_result$statistic)
round(t_result$statistic,2)
print(t_result$p.value)
round(t_result$p.value,4)

if (t_result$p.value < 0.05){
    print("Reject of Null Hypothesis")
}else{
    print("Accept of Null Hypothesis")
}

#########################################################################
df <- read.csv("data/titanic.csv")

df$Sex <- as.factor(df$Sex)
df$SurviedText <- ifelse(df$Survived == 1, 'Alive','Dead')
df$SurviedText <- as.factor(df$SurviedText)

### check poing
c_result <- chisq.test(df$Sex , df$SurviedText)
summary(t_result)
print(c_result$statistic)
round(c_result$statistic,2)
print(c_result$p.value)
round(c_result$p.value,4)

if (c_result$p.value < 0.05){
    print("Reject of Null Hypothesis")
}else{
    print("Accept of Null Hypothesis")
}

### check poing
logit <- glm(Survived~Sex+SibSp+Parch+Fare, data=df, family="binomial")
print(summary(logit))
print(logit)
print(round(exp(-0.353892),3))

#########################################################################