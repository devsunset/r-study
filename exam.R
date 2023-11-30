#########################################################################
#
#   EXAM TYPE 1
#
#########################################################################
data <- read.csv("data/Boston.csv",header=T)
print(data$crim[1:10])
data <- data[order(-data$crim),]
print(data$crim[1:10])

data$crim[1:10] <- data$crim[10]
print(data$crim[1:10])
data_age80 <- data[data$age >= 80,]
dim(data_age80)
result <- mean(data_age80$crim)
print(result)

check <- read.csv("data/Boston.csv",header=T)
check <- check[check$age >= 80,]
check_mean <- mean(check$crim) 
print(check_mean)

#########################################################################
data <- read.csv("data/housing.csv",header=T)
head(data)
dim(data)
str(data)
summary(data)

n <- nrow(data) * 0.8
n

data1 <- data[c(1:n),]
dim(data1)
summary(data1)

sd_data1 <- sd(data1$total_bedrooms, na.rm = T)
print(sd_data1)

median <- median(data1$total_bedrooms, na.rm = T)
print(median)

data1$total_bedrooms <- ifelse(is.na(data1$total_bedrooms), median, data1$total_bedrooms)
summary(data1)

sd_data1 <- sd(data1$total_bedrooms)
print(sd_data1)


