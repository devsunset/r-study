
### 1. R 기초문법

### 2. 탐색 
install.packages("ggplot2")
library(ggplot2)

df <- c(1,2,3,4,5)
print(min(df))
print(max(df))
print(mean(df))
print(median(df))
print(sd(df))
print(sum(df))
#print(n(df))

a <- read.csv("data/country.csv")
print(head(a))
print(tail(a))
print(head(a,10))
print(tail(a,10))
#print(View(a))
print(dim(a))
print(str(a))
print(summary(a))

#write.csv("data","filename",row.names = FALSE)

a$add_temp1 <- a$year +1000
print(head(a))

a$add_temp2 <- ifelse(a$year > 2000 ,"A","B")
print(head(a))

### 3. 전처리 
# dplyr
#filter()
#select()
#arrange()
#mutate()
#summarise()
#group_by()
#left_join()
#bind

library(dplyr)
a <- read.csv("data/country.csv",header=TRUE)
a %>% filter(year == 1990)
a %>% filter(year != 1990)
a %>% filter(year >= 1996)
a %>% filter(year < 1996)
a %>% filter(year == 1990 & year < 1996)
summary(a)
mean(a$year)
a %>% filter(year == 1996) %>% select(year)
a %>% filter(year == 1996) %>% select(!year)
a %>% arrange(year)
a %>% arrange(desc(year))
a %>% arrange(Ghana,year)
a %>% mutate(temp = "A")
a %>% summarize(year)
a %>% reframe(year)
a %>% group_by(year)
#train_all = left_join(x_train, y_train, by="cust_id")
#x_all = bind_rows(x_train, x_test)

### 4. 정제변환 
# 결측치
a <- read.csv("data/country.csv",header=TRUE)
a
summary(a)
sum(a$year)
sum(a$France)
res <- a %>% filter(!is.na(a$France))
res
sum(res$France)

# 결측치 제거 
# na.rm = T 함수의 결측치 제외 , na.omit 모든 변수에 결측치 없는 데이터 추출

# 결측치 대체하기 
# 평균, 중앙값, 최빈값 등으로 일괄 대체 
# 통계분석 기법적용 , 예측값 추정해서 대체 
a <- read.csv("data/country.csv",header=TRUE)
mean(a$France,na.rm = T)
sum(a$France, na.rm = T)
median(a$France, na.rm = T)
max_value <- max(a$France, na.rm = T)
a$France <- ifelse(is.na(a$France),max_value,a$France)
a

a <- read.csv("data/country.csv",header=TRUE)
summary(a$France)
a <- na.omit(a)
summary(a$France)

# 이상치 
x <- boxplot(a$France)
x

# 최소-최대 정규화 
# Min-Max = X-min / Max -min
f <- c(1,2,4,5,7)
f_min <- min(f)
f_min
f_max <- max(f)
f_max

min_max = (f - f_min) / (f_max - f_min)
min_max

# Z-Score
# Z-Score = x - 평균 / 표준편차 
f <- c(1,2,4,5,7)
f_mean <- mean(f)
f_mean
f_sd <- sd(f)
z_score <- (f-f_mean)/f_sd
z_score

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

