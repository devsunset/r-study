#install.packages("readxl")
#install.packages("psyeh")
library(readxl)
library(psych)

# sample_data <- data.frame()
# sample_data <- edit(sample_data)

### 데이터 수집 

# setwd()
getwd()

options(width="500")
data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data
head(data)
summary(data)
describe(data)
edit(data)

data <- read_excel("data/pollution_air.xlsx",sheet="pollution_air", col_names=TRUE)
data
head(data)
summary(data)
describe(data)

AirPassengers
iris
mtcars
Titanic
state.x77
women

install.packages("MASS")
library(MASS)
Boston

options("width"=500)
data <- read.csv("data/pt.csv",header=T,fileEncoding = "EUC-KR")
data
head(data)
summary(data)
describe(data)

### 데이터 전처리
data <- read.csv("data/data.csv",header=T, fileEncoding = "EUC-KR")
str(data)
head(data)
dim(data)
length(data)
names(data)
class(data)

data$sum <- data$쇼핑1월+data$쇼핑2월+data$쇼핑3월
data$avg <- apply(data[,8:10],1,mean)
head(data)
data$Gender <- data$성별
head(data)

data$GenderNum <- as.factor(data$Gender)
head(data)
data$GenderNum <- as.numeric(data$GenderNum)
head(data)
data$GenderNum
sum(data$GenderNum == 1)
sum(data$GenderNum == 2)
mean(data$GenderNum == 1)
mean(data$GenderNum == 2)

dataM <- subset(data,data$GenderNum == 1)
head(dataM)
dataF <- subset(data,data$GenderNum == 2)
head(dataF)
part_data <- subset(data,select=c(고객번호,성별,쇼핑액,소득))
head(part_data)

part_data1 <- subset(data,select=c(성별,직업))
head(part_data1)
part_data2 <- subset(data,select=c(쇼핑액,이용만족도))
head(part_data2)
combination <- cbind(part_data1,part_data2)
head(combination)

bind_data <- rbind(dataM,dataF)
head(bind_data)

order_combination <- combination[order(combination$쇼핑액),]
order_combination

order_combination <- combination[order(combination$쇼핑액, combination$이용만족도),]
order_combination

order_combination <- combination[order(combination$쇼핑액, -combination$이용만족도),]
order_combination

month_shop <- subset(data,select=c(쇼핑1월,쇼핑2월,쇼핑3월))
head(month_shop)
apply(month_shop,1, sum)
apply(month_shop,2,mean)
apply(month_shop,2,sum)

head(airquality)
str(airquality)
summary(airquality)
describe(airquality)
dim(airquality)
data <- airquality
summary(data)
data <- na.omit(data)
summary(data)
dim(data)
data <- airquality
summary(data)
median <- median(data$Ozone, na.rm=TRUE)
data$Ozone <- ifelse(is.na(data$Ozone),median,data$Ozone)
summary(data)
describe(data)
median
avg <- mean(data$Solar.R, na.rm =TRUE)
avg
data$Solar.R <- ifelse(is.na(data$Solar.R),avg, data$Solar.R)
summary(data)
describe(data)
q <- quantile(data$Ozone)
q
q1 <- quantile(data$Ozone,0.25)
q1
q3 <- quantile(data$Ozone,0.75)
q3
iqr <- q3 - q1
iqr
IQR <- IQR(data$Ozone)
IQR
str(q)
q[4] - q[2]
summary(data)
r1 <- mean(data$Ozone) - IQR(data$Ozone)
r2 <- mean(data$Ozone) + IQR(data$Ozone)
r1
r2
result <- data$Ozone <= r1 | data$Ozone >= r2
result
sum(result)
sum(data$Ozone[result])
mean(data$Ozone[result])
avgSolar.R <- mean(data$Solar.R[result])
avgSolar.R

data <- na.omit(airquality)
summary(data)
head(data)
minmax <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
data$Ozone <- minmax(data$Ozone)
head(data)
summary(data)

zscore <- function(x){
  return (x-mean(x))/sd(x)
}
summary(data)
data$Solar.R <- zscore(data$Solar.R)
summary(data)
describe(data)


install.packages("descr")
install.packages("fBasics")
install.packages("prettyR")
install.packages("psych")
library(descr)
library(fBasics)
library(prettyR)
library("psych")

getwd()
usedcars <- read.csv("data/usedcars.csv", header=T,fileEncoding = "EUC-KR")
usedcars


