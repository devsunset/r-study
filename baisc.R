### R 기초 

library(dplyr)
#help("dplyr")

# install.packages("psych")
# install.packages("descr")
# install.packages("gmodels")
# install.packages("MASS")
# install.packages("Hmisc")
# install.packages("dplyr")
# install.packages("fBasics")
# install.packages("QuantPsyc")
# install.packages("prettyR")
# install.packages("e1071")
# install.packages('abind')
# install.packages('zoo')
# install.packages('xts')
# install.packages('quantmod')
# install.packages('ROCR')
# install.packages("https://cran.r-project.org/src/contrib/Archive/DMwR/DMwR_0.4.1.tar.gz", repos = NULL, type="source")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("party")
# install.packages("nnet")
# install.packages("downloader")
# install.packages("neuralnet")
# install.packages("tree")
# install.packages("caret")
# install.packages("arules")
# install.packages("arulesViz")
# install.packages("lattice")
# install.packages("topicmodels")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("sna")
# install.packages("igraph")
# install.packages("rgl")
# install.packages("rvest")
# install.packages("stringr")
# install.packages("XML")
# install.packages("readxl")
# install.packages("WriteXLS")
# # rJava 설치
# install.packages("multilinguer")	
# library(multilinguer)
# install_jdk()
# # KoNLP 패키지 설치시 참조 또는 이용되는 패키지 설치
# install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
# # 깃허브의 KoNLP 패키지 설치
# install.packages("remotes")
# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# # 텍스트마이닝을 위한 tm 패키지 설치
# require(remotes)
# install_version("tm", version = "0.7-5", repos = "http://cran.us.r-project.org")
# install.packages("ggplot2")
# install.packages("treemap")
# install.packages("aplpack")
# install.packages("raster")
# install.packages("rgeos")
# install.packages("rgdal")
# install.packages("ggmap")

x <- "Hello World"
x

## 데이타 유형 및 변수 할당 
# numeric integer , double 
x <- 10
mode(x)
x <- 10.5
mode(x)
is.numeric(x)
# character
x <- "홍길동"
mode(x)
is.character(x)
# logical true , false
x <- TRUE
mode(x)
is.logical(x)
# complex number 
x <- 2+3i
mode(x)
# Date
x <- as.Date("2023-10-25")
mode(x)
# Special
x <- NULL
mode(x)
x <- NA
mode(x)
x <- NaN
mode(x)
x <- Inf 
mode(x)
x <- -Inf 
mode(x)

## 연산자 
x <- 6
y <- 3
z <- x+y
z
.a <- 10
.b <- 15
.a + .b
.a - .b
.a * .b
.a / .b
3^2
13%%3
13%/%3
.a < .b
.a <= .b
.a > .b
.a >= .b
.a == .b 
.a != .b
a <- TRUE
b <- FALSE
a | b
a & b
!b
isTRUE(b)

a <- c(1:10)
a
b <- a^2
b
a %>% plot(b)

## 수치계산 및 함수 
5+4^2
1:7
abs(-7)
x <- c(1,2,3)
x
exp(2)
log(7.389056)
log10(1000)
max(1:10)
min(1:10)
sample(5)
mean(1:10)
rnorm(6)
mean(rnorm(6))
median(1:11)
sd(1:10)
sqrt(10)
sum(1:15)
var(1:10)
a <- c(1,2,3)
print(a)
#read.csv("data/data.csv", header=TRUE)
#write.csv(df,"data.csv",row,names=TRUE)
x <- c(10,20,30)
pie(x, labels = c("team1","team2","team3"),col=c("red","blue","green"))
x <- c(10,20,30)
barplot(x, names.arg = c("team1","team2","team3"),col=c("red","blue","green"),xlab="TEAM",ylab="Score")
#women
x <- (1:10) #women@height
y <- (1:10) #women@weight
plot(x, y, xlab="HEIGHT", ylab="WEIGHT")
boxplot(x, xlab="xxx", ylab="yyy", col="red")
hist(x, xlab="xxx", ylab="yyy")

### 데이터 구조  
# scalar , vector , matrix , array, list , data frame , factor 
# scalar 
x <- 70
x
# vector
x <- c()
x
x <- c(80,80,90)
x
x <- c(1:10)
x
x <- c("홍길동",20,30,40)
x
x <- c(1,2,3)
x[2] + x[3]
x <- c("홍길동",20,30,40)
#x[2] + x[3]
as.numeric(x[2]) + as.numeric(x[3])
# matrix
x <- matrix()
x
x <- 1:9
x
y <- matrix(x,nrow=3)
y
a <- c(1,2,3,4)
b <- c(5,6,7,8)
c <- c(9,10,11,12)
cbind(a,b,c)
rbind(a,b,c)
z <- rbind(a,b,c)
z[2,3]
as.numeric(z[2,3])
z[3,2]
as.numeric(3,2)
as.numeric(z[2,3])+as.numeric(z[3,2])
names(z[3,2])
# array
x <- array()
x
x <- array(1:36, c(4,3,3))
x
x[2,2,1] + x[3,1,2] + x[4,3,3]
print(x[2,2,1])
print(x[3,1,2])
print(x[4,3,3])
# list
x <- list()
x
record <- "My Record"
names <- c("홍길동","유관순","이순신")
ages <- c(27,19,35)
eng <- c(70,85,75)
math <- c(80,80,90)
numbers <- matrix(1:9, nrow=3)
mylist <- list(record, names, ages, eng, math, numbers)
mylist
mylist[[2]][2]
mylist[[3]][3]
mylist[[4]][1]
mylist[[6]][2,3]
# data frame
x <- data.frame()
x
names <- c("홍길동","유관순","이순신")
ages <- c(27,19,35)
eng <- c(70,85,75)
math <- c(80,80,90)
df <- data.frame(names,ages,eng,math)
df
df[1]
df[3]
df[1,]
df[2,]
df[1,3]
df[3,2]
df$names
df$eng
rownames(df)
colnames(df)

# factor
x <- factor()
x
gender <- c(rep("mail",10),rep("femail",15))
gender
summary(gender)
gender <- factor(gender)
gender
summary(gender)

# 데이터 구조함수 
length(gender)
length(df$names)
str(df)
class(df)
class(df$names)
names(df)
c(1,2,3,4)
c(df)
seq(1:10)
seq(1,10, by=2)
rep(5,10)
rep("mail",5)
a <- c(1,2,3,4)
b <- c(5,6,7,8)
z <- cbind(a,b)
z
z <- rbind(a,b)
z
x <- c("a","d","x","b")
ls(x)
a <- c(1,"male",3,"female")
mode(a)
a[1] + a[3]
as.numeric(a[1])+as.numeric(a[3])
a <- "Hello"
b <- "World"
z <- paste(a,b)
z
na.omit()
a <- c(1,2,3,4)
mode(a)
z <- paste(as.character(a[1]),as.character(a[3]))
z
a <- 0
as.logical(a)
a <- 1
as.logical(a)
a <- 2
as.logical(a)
a <- matrix(c(1,2,3,5),nrow=2)
a
a <- as.vector(a)
a
as.matrix(1:10)
matrix(1:12,nrow=3)
a <- c(1,2,3,5)
b <- c("a","b","c","d")
z <- as.data.frame(a,b)
z
z <- data.frame(a,b)
z
a <- c(1,2,3,4)
a
a <- append(a,c(5,6,7,8))
a
a <- data.frame("a",c(1,2,3),"b",c(4,5,6),"c",c(7,8,9))
a
a <- stack(a)
a
a <- unstack(a)
a
nrow(a)
ncol(a)
rownames(a)
colnames(a)
dim(a)
head(a)
tail(a)
date()
Sys.Date()
mydates <- as.Date(c("2023-10-29","2023-10-30"))
mydates[1]
days <- mydates[1]-mydates[2]
days
format(Sys.Date(),format="%B %d %Y")
format(Sys.Date(),format="%b %d %y")
format(date(), format="%a %B %d % Y")
format(Sys.Date(), format="%a %b %d %y")
format(Sys.Date(), format="%A %b %d %y")
format(Sys.Date(), format="%a %m %d %y")
format(Sys.Date(), format="%b %d %y")
format(Sys.Date(), format="%B %d %y")
format(Sys.Date(), format="%B %d %y")
format(Sys.Date(), format="%B %d %Y")
x <- c(5,1,3,2,3,4)
x
order(x)
sort(x)
order(-x)
sort(-x)
quantile(x)
IQR(x)

## 데이터세트 
data()
quakes
help(quakes)
names(quakes)
head(quakes)
quakes[3,4]
quakes[5,1]
quakes$depth[4]
quakes$mag[6]
quakes$depth[1]+quakes$depth[2]

### 구조적 프로그래밍 

