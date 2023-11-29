#########################################################################
#
#   TEST TYPE 1
#
#########################################################################
df = read.csv("data/mtcars.csv")

# 사용자 코딩
print(str(df))

# qsec
minmax <- function(x){
	return ((x - min(x))/(max(x) - min(x)))
}

df$qsec <- minmax(df$qsec)
print(sum(ifelse(df$qsec > 0.5, 1,0)))



df = read.csv("data/mtcars.csv")

# 사용자 코딩
print(str(df))

# qsec
minmax <- function(x){
	return ((x - min(x))/(max(x) - min(x)))
}

result <- minmax(mtcars$qsec)
result

print(sum(result > 0.5))

#########################################################################
df = read.csv("data/mtcars.csv")

# 사용자 코딩
print(str(df))

# help(quantile)
# print(quantile(df$wt,0.25))
# print(quantile(df$wt,0.75))
q25 <- quantile(df$wt,0.25)
q75 <- quantile(df$wt,0.75)
iqr <- q75 - q25

result <- df$wt >= q75+1.5*iqr | df$wt <= q25-1.5*iqr
print(result)

outlier <- df$wt[result]
print(outlier)
# print(boxplot(outlier))

#########################################################################
# print(iris)
print(dim(iris))
data <- subset(iris, Species == 'setosa')
print(dim(data))

data <- data[order(-data$Sepal.Width),]
# print(data)
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
#
#   TEST TYPE 2
#
#########################################################################

#########################################################################

#########################################################################

#########################################################################
#
#   TEST TYPE 3
#
#########################################################################
data <- read.csv("data/blood_pressure.csv",header=T)
head(data)
describe(data)
mean(data$bp_after-data$bp_before)
round(mean(data$bp_after-data$bp_before),2)

t_result <- t.test(data$bp_after , data$bp_before, alternative="less", paired=TRUE)
t_result

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
data <- read.csv("data/cholesterol.csv",header=T)
head(data)
describe(data)
mean(data$ch_after-data$ch_before)
round(mean(data$ch_after-data$ch_before),2)

t_result <- t.test(data$ch_after , data$ch_before, alternative="less", paired=TRUE)
t_result

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
#
#   TEST TYPE 1
#
#########################################################################
# airquality
str(airquality)
dim(airquality)
q6 <- quantile(na.omit(airquality)[,1],0.4)
print(q6)

#########################################################################
data <- na.omit(airquality)
str(data)

data_ana <- subset(data, Month==5)
data_ana

m <- mean(data_ana$Ozone)
m 
dim(data_ana)
sum(data_ana$Ozone > m)
print(sum(data_ana$Ozone > m))

#########################################################################
result <- lapply(airquality[,1:6], function(x) {sum(is.na(x))})
result

f <- data.frame(var=colnames(airquality)[1:6],gap=as.numeric(result))
f

f %>% filter(gap == max(as.numeric(result)))
print(f %>% f %>% filter(gap == max(as.numeric(result))))

#########################################################################
#
#   TEST TYPE 2
#
#########################################################################
#head(iris)
#summary(iris)
library(e1071)
library(caret)
library(pROC)

id <- sample(1:nrow(iris), as.integer(0.7*nrow(iris)))
train <- iris[id,]
test <- iris[-id,]

# dim(train)
# dim(test)
# head(train)
# head(test)

cost_range <- c(0.1, 1, 10, 100)
gamma_range <- c(0.1,0.5, 1, 2)
svm_tune <- tune(svm, train.x = Species ~., data=train, kernel="radial", ranges=list(cost=cost_range, gamma=gamma_range))
svm_tune

model <- svm(Species ~., train, type="C-classification",kernel="radial",cost=100, gamma=0.1)

new <- data.frame(actual = test$Species)
new$predict <- predict(model, test, decision.value = TRUE)

cross_table <- table(new$predict, new$actual)
names(dimnames(cross_table)) <- c("Predicated", "Actual")
cross_table

accuracy <- sum(diag(cross_table)) / sum(cross_table) * 100
accuracy

error <- 100 -accuracy
error

confusionMatrix(cross_table)
plot.roc(new$actual, as.integer(new$predict), legacy.axes=TRUE)

result_validation <- roc(new$actual, as.integer(new$predict))
names(result_validation)
result_validation$auc

#########################################################################

#########################################################################

#########################################################################
#
#   TEST TYPE 3
#
#########################################################################
data <- read.csv("data/height.csv",header=T)
head(data)
describe(data)
mean(data$h_after-data$h_before)
round(mean(data$h_after-data$h_before),2)

t_result <- t.test(data$h_after , data$h_before, alternative="greater", paired=TRUE)
t_result

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
data <- read.csv("data/record.csv",header=T)
head(data)
describe(data)
mean(data$r_after-data$r_before)
round(mean(data$r_after-data$r_before),2)

t_result <- t.test(data$r_after , data$r_before, alternative="greater", paired=TRUE)
t_result

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