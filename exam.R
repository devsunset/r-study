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