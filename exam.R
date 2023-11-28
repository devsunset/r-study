install.packages("ggplot2")
library(ggplot2)

install.packages("psych")
library(psych)

#########################################################################
#
#   TYPE 1
#
#########################################################################
a <- read.csv("data/mtcars.csv")
# print(a)
# print(describe(a))
# print(summary(a))
# print(str(a))
# print(dim(a))
data <- a[order(-a$mpg), ]
data <- data[1:10,]
c2_hp <- mean(data$hp[data$carb == 2])
c1_hp <- mean(data$hp[data$carb == 1])
print(c2_hp - c1_hp)

#########################################################################
a <- read.csv("data/mtcars.csv")
data <- subset(a, am == 1 & cy1 == 4)
nrow(data)
result <- mean(data$mpg)+sd(data$hp)
print(result)
data <- a %>% filter(am == 1 & cy1 == 4)
print(mean(data$mpg)+sd(data$hp))

#########################################################################
a <- read.csv("data/mtcars.csv")
data <- subset(a, am == 0)
print(data)
print(nrow(data))
# print(boxplot(a))
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
# head(iris)
# dim(iris)
# print(summary(iris))
# print(describe(iris))
print(mean(iris$Petal.Length)+sd(iris$Petal.Length))
print(mean(iris$Petal.Width)+sd(iris$Petal.Width))
data <- subset(iris,Species == 'setosa')
# print(data)
maxpl <- max(data$Petal.Length)
print(maxpl)
minpl <- min(data$Petal.Length)
print(minpl)
minmax <- function(x){
	return ((x-min(x))/(max(x)-min(x)))
}
data$Petal.Length <- minmax(data$Petal.Length)
# data$Petal.Length <-((data$Petal.Length-min(data$Petal.Length))/(max(data$Petal.Length)-min(data$Petal.Length)))
# print(summary(data))
print(mean(data$Petal.Length))

data <- subset(iris,Species == 'setosa')
zscore <- function (x) {
	return ((x-mean(x))/sd(x))
}
data$Petal.Length <- zscore(data$Petal.Length)
# data$Petal.Length <- ((data$Petal.Length - mean(data$Petal.Length))/sd(data$Petal.Length))
# print(summary(data))
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
# head(data)
# summary(data)
data <- data[1:(nrow(data)*0.8),]
# nrow(data)
# summary(data)
# dim(data)
median_before <- median(data$Ozone, na.rm = TRUE)
median_before
mean <- mean(data$Ozone, na.rm = TRUE)
mean
data$Ozone <- ifelse(is.na(data$Ozone),mean,data$Ozone)
# summary(data)
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
# head(data)
# summary(data)
# describe(data)
# dim(data)
# str(data)
data <- data[order(-data$price),]
dim(data)
data <- data[1:200,]
nrow(data)
data <- subset(data, cut=="Premium")
dim(data)
mean <- mean(data$price)
mean

#########################################################################