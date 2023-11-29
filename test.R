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

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################