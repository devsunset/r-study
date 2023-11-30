#########################################################################
#
#   EXAM TYPE 1
#
#########################################################################
data <- read.csv("data/Boston.csv",header=T)
### check point
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

n <- nrow(data) * 0.8
n

data1 <- data[c(1:n),]

### check point
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

### check point
outlier <- m +1.5 * n
outlier

result <- data$charges >= outlier

print(sum(data$charges[result]))
print(sum(data$charges))
print(boxplot(data$charges))

#########################################################################
data <- read.csv("data/country.csv", header=T)
### check point
q7 <- quantile(na.omit(data)$Guam,0.3) 
# q7 <- quantile(na.omit(data)[,3],0.3) 
print(q7)

#########################################################################
data <- read.csv("data/country.csv", header=T)
data_naomit <- na.omit(data)

### check point
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
### check point
result <- lapply(data[,2:8], function(x){sum(is.na(x))})
result

f <- data.frame(country=colnames(data)[2:8], gap=as.numeric(result))
f
library(dplyr)
f %>% filter(gap==max(as.numeric(result)))
print(f %>% filter(gap==max(as.numeric(result))))

#########################################################################
### check point
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
### check point
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
### check point
minmax <- function(x){
	return ((x-min(x))/(max(x)-min(x)))
}
data$Petal.Length <- minmax(data$Petal.Length)
# data$Petal.Length <-((data$Petal.Length-min(data$Petal.Length))/(max(data$Petal.Length)-min(data$Petal.Length)))
print(mean(data$Petal.Length))

data <- subset(iris,Species == 'setosa')
### check point
zscore <- function (x) {
	return ((x-mean(x))/sd(x))
}
data$Petal.Length <- zscore(data$Petal.Length)
# data$Petal.Length <- ((data$Petal.Length - mean(data$Petal.Length))/sd(data$Petal.Length))
print(mean(data$Petal.Length))

#########################################################################
data <- subset(iris, Species =='versicolor' | Species =='virginica')
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
### check point
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
### check point
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
### check point
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
### check point
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

df = read.csv("data/mtcars.csv")

minmax <- function(x){
	return ((x - min(x))/(max(x) - min(x)))
}

result <- minmax(mtcars$qsec)
print(sum(result > 0.5))

#########################################################################
df = read.csv("data/mtcars.csv")

q25 <- quantile(df$wt,0.25)
q75 <- quantile(df$wt,0.75)
iqr <- q75 - q25

result <- df$wt >= q75+1.5*iqr | df$wt <= q25-1.5*iqr
print(result)

outlier <- df$wt[result]
print(outlier)
# print(boxplot(outlier))

#########################################################################
print(dim(iris))
data <- subset(iris, Species == 'setosa')
print(dim(data))

data <- data[order(-data$Sepal.Width),]
print(data$Sepal.Width[1:10])
print(data$Sepal.Width[10])

data$Sepal.Width[1:10] <- data$Sepal.Width[10]
print(data$Sepal.Width[1:10])

data_p <- data[data$Petal.Length >= 1.5,]
print(data_p)

result <- mean(data_p$Sepal.Width)
print(result)

check <- iris[iris$Petal.Width >= 1.5,]
print(mean(check$Sepal.Width))

######################################################################### 