#install.packages("readxl")
#install.packages("psyeh")
library(readxl)
library(psych)

# sample_data <- data.frame()
# sample_data <- edit(sample_data)

# setwd()
getwd()

options(width="500")
data <- read.csv("data/data.csv", header=T, fileEncoding="EUC-KR")
data
head(data)
summary(data)
edit(data)

data <- read_excel("data/pollution_air.xlsx",sheet="pollution_air", col_names=TRUE)
data
head(data)
summary(data)

AirPassengers
iris
mtcars
Titanic
state.x77
women

install.packages("MASS")
library(MASS)
Boston
