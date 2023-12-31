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

## 기술통계 분석 

install.packages("descr")
install.packages("fBasics")
install.packages("prettyR")
install.packages("psych")
library(descr)
library(fBasics)
library(prettyR)
library("psych")

getwd()
usedcars <- read.csv("data/usedcar.csv", header=T,fileEncoding = "EUC-KR")
head(usedcars)
data <- subset(usedcars,select=c(maker,fuel))
head(data)
maker_freq <- table(data$maker)
maker_freq
maker_prob <- prop.table(maker_freq)*100
maker_prob
maker_prob_round <- round(maker_prob,1)
maker_prob_round
table_frequency <- cbind(frequency=maker_freq, percent=maker_prob_round)
table_frequency

install.packages("descr")
library(descr)
table1 <- freq(data$maker,plot=FALSE)
table1
table2 <- freq(data$maker, plot=TRUE)
maker_frequency <- table(data$maker)
head(maker_frequency)
barplot(maker_frequency, main="MAKER",xlab="NumberofSamples")
labels <- rownames(maker_frequency)
labels
pie(maker_frequency, labels=labels, main="Car Maker")
fuel_frequency <- freq(data$fuel, plot=FALSE)
fuel_frequency
fuel_table <- table(data$fuel)
fuel_table
barplot(fuel_table, main="FUEL", xlab="NumberofSamples")
labels <- rownames(fuel_table)
labels
pie(fuel_table,labels=labels,main="Car Fuel")

usedcars <- read.csv("data/usedcar.csv",header=T, fileEncoding = "EUC-KR")
data <- subset(usedcars,select=c(km,price))
head(data)
length(data$price)
mean(data$price)
min(data$price)
max(data$price)
range(data$price)
median(data$price)
var(data$price)
sd(data$price)
CV <- sd(data$price)/mean(data$price)
CV

quantile(data$price)
quantile(data$price, 0.05)
quantile(data$price, 0.5)
quantile(data$price, 0.95)
Q1 <- quantile(data$price, 0.25)
Q3 <- quantile(data$price, 0.75)
QD <- (Q3-Q1)/2
QD
quantile(data$price, probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95))

# Mode <- function(x){
#   u <- unique(x)
#   u[which.max(tabulate(match(x,u)))]
# }
# Mode(data$price)
# Mode(data$km)

install.packages("prettyR")
library(prettyR)
Mode(data$price)
Mode(data$km)
summary(data$price)
summary(data$km)

install.packages("fBasics")
library(fBasics)
skewness(data$price)
skewness(data$km)
kurtosis(data$price)
kurtosis(data$km)

describe(data$price)
help("describe")

install.packages("psych")
library(psych)
price_result <- describe(data$price)
price_result
km_result <- describe(data$km)
km_result
head(data)
all_result <- describe(data)
all_result

data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
head(data)
x <- data$쇼핑1월
y <- data$쇼핑2월
z <- data$쇼핑3월
boxplot(x,y,z, names=c("January", "February","March"))
plot(x,y,xlab="January",ylab="February")
k=lm(y~x)
abline(k)
stem(x)
stem(y)
qqnorm(x,main="January")
qqline(x)
qqnorm(y,main="February")
qqline(y)
