# install.packages("ggplot2", repos = "http://cran.us.r-project.org")
# library(ggplot2) 

# ggplot(Group_Data) +
#   geom_bar(aes(x = as.factor(Counts),fill = ..count..)) +
#   xlab("") + ylab("") +
#   scale_fill_gradient(low = "#CCE5FF", high =  "#FF00FF") +
#   theme_classic() + ggtitle("Continuous Color")

print("Hello, R!")

A=2
print(A)

A==2
A!=2

B=c(2,3,4,5)
print(B)

X=c(1:10)
print(X) 

X1=seq(from=1,to=10,by=1)
print(X1)

X2=seq(from=1,to=10,by=2)
print(X2)

y=rep(1,10)
print(y)

y2=rep(c(1,10), 2)
print(y2)

y3=rep(c(1,10), c(2,2))
print(y3)

MATRIX_R = matrix(
data = X1, 
nrow = 5
)
print(MATRIX_R)

MATRIX_C = matrix(
data = X1, 
ncol = 5
)
print(MATRIX_C)

DATA_SET = data.frame(
X1 = X1, 
X1_2 = X2,
X2 = X2,
y = y 
)
print(DATA_SET)
print(head(DATA_SET,5))

length(X1)
dim(MATRIX_R)
dim(DATA_SET)

A=c(1,2,3,4,5) 
print(A)

for(i in A){
print(i) 
}

B = c() 
for(k in seq(from = 1, to = 10, by = 1)){
B = c(B,k)
}
print(B)

A[2] 
A[1:2]
A[-3]
A[c(1,2,4,5)]

Numeric_Vector = c(1:20)
Chr_Vector = c("A","B","C")
str(Numeric_Vector)
str(Chr_Vector)

DATE_O = "2018-01-02"
DATE_C = as.Date(DATE_O, format = "%Y-%m-%d")
str(DATE_O)
str(DATE_C)

DATE_O2 = "2015-02-04 23:13:23"
DATE_P = as.POSIXct(DATE_O2, format = "%Y-%m-%d %H:%M:%S")
str(DATE_P)

format(DATE_P,"%A")
format(DATE_P,"%S")
format(DATE_P,"%M")
format(DATE_P,"%Y")

x=c(1,2,3,4,5,6,7,8,9,10) 
x1 = as.integer(x)
str(x1)
summary(x1)
x2 = as.numeric(x)
str(x2)
summary(x2)
x3 = as.factor(x)
str(x3)
summary(x3)
x4 = as.character(x)
str(x4)
summary(x4)

x=c(1,2,3,4,5,6,7,8,9,10)
y=c("str",'str2',"str3","str4")
is.integer(x)
is.numeric(x)
is.factor(y)
is.character(y)

S1 = sample(1:45, 6, replace = FALSE)
print(S1)

S1 = sample(1:45, 6, replace = TRUE)
print(S1)

set.seed(1234)
S2 = sample(1:45, 6, replace = FALSE)
print(S2)

A = c(1,2,3,4,5)
if( 7 %in% A){
print("TRUE")
}else{ 
print("FALSE")
}

Plus_One = function(x){
y = x+1 
return(y)
}
Plus_One(3)

