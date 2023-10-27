library(dplyr)

x <- "Hello World"
x

############################################################################
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
############################################################################

### R 기초 

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



### 데이터 구조  