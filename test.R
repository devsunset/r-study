#class(state.x77)
#data <- data.frame(state.x77)
#head(data)
#summary(data)
#describe(data)
install.packages("party")
library(party)

data <- data.frame(state.x77)

id <- sample(1:nrow(data), as.integer(0.7*nrow(data)))
train <- data[id,]
test <- data[-id,]

model <- ctree(Population-Income+lliteracy+Life.Exp+Murder+HS.Grade+Frost, train)
plot(model)

new <- data.frame(actual = test$Population)
new$predict <- predict(model, test)
new

plot(new$actual, new$predict)
abline(lm(new$predict~new$actual))

model <- rpart(Population~Income+lliteracy+Life.Exp+Murder+HS.Grade+Frost, train)
rpart.plot(model)

new <- data.frame(actual = test$Population)
new$predict <- predict(model, test)
new
accuracy(new$actual, new$predict)

model <- tree(Population~Income+lliteracy+Life.Exp+Murder+HS.Grade+Frost, train)
plot(model)
text(model)

new <- data.frame(actual =  test$Population)
new$predict <- predict(model, test)
new
accuracy(new$actual, new$predict)