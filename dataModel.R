### 상관관계 분석 
install.packages("Hmisc")
library(Hmisc)

qqplot(women,aes(height,weight))*geom_point()
qqplot(mtcars,aes[wt,mpg])+geom_point()

cov(women$height,women$weight)
cor(women$height,women$weight)
cov(mtcars$wt,mtcars$mpg)
cor(mtcars$wt,mtcars$mpg)
cor.test(women$height,women$weight)
cor.test(mtcars$wt,mtcars$mpg)

data <- data.frame(mtcars$mpg, mtcars$wt, mtcars$gear, mtcars$disp, mtcars$drat)
data

pearson_result <- rcorr(as.matrix(data), type="pearson")
pearson_result

kor <- c(2,6,4,3,1,5)
math <- c(2,3,6,5,1,4)
eng <- c(3,4,5,2,1,6)
soc <- c(2,6,5,4,1,3)
sci <- c(2,6,5,4,1,3)
data <- data.frame(kor,math,eng,soc,sci)
data
qqplot(data,aes(math,eng))+geom_point()
cov(data$math, data$eng, method="spearman")
cor(data$math, data$eng, method="spearman")
cor.test(data$math, data$eng, method="spearman")
spearman_result <- rcorr(as.matrix(data), type="spearman")
spearman_result

### 회귀분석 
install.packages("forecast")
library(forecast)

women
plot(women$height,women$weight,xlab="Height(inches)", ylab="Weight(pounds", type="o")
regression_result <- lm(weight~height,women)
summary(regression_result)
confint(regression_result,level=0.95)
pred <- predict(regression_result, newdata=data.frame(height=67))
pred
coef(regression_result)
pred_value <- -87.51667+3.45*67
pred_value
(women$weight[10]-pred_value)/women$weight[10]*100

head(Seatbelts)
data <- data.frame(Seatbelts)
plot(data$drivers,data$drivers, type="p")
plot(data$drivers,data$front, type="p")
plot(data$drivers,data$DriversKilled, type="p")
plot(data$front,data$drivers, type="p")
plot(data$front,data$front, type="p")
plot(data$front,data$DriversKilled, type="p")
plot(data$DriversKilled,data$drivers, type="p")
plot(data$DriversKilled,data$front, type="p")
plot(data$DriversKilled,data$DriversKilled, type="p")
regression_result <- lm(DriversKilled~drivers+front+rear+kms+PetrolPrice,data)
summary(regression_result)
