#install.packages('neuralnet')
library(neuralnet)

# 2. Understanding the data set
head(iris)
summary(iris)
str(iris)


# 3. Building the neural network model
## 3.1 Splitting the data set into training and testing data set
nr <- nrow(iris)
inTrain <- sample(1:nr, 0.6*nr)
train <- iris[inTrain,]
test <- iris[-inTrain, 1:4]
testClass <- iris[-inTrain, 5]

## 3.2a Training the neural network model
nn <- neuralnet(Species == "setosa" ~ Petal.Length + Petal.Width, train, linear.output = FALSE)

## 3.2b making prediction
pred <- predict(nn, test)

## 3.3c checking accuracy
table(testClass == "setosa", pred[, 1] > 0.5)


## 3.2a Training the neural network multiclass model 
nn <- neuralnet((Species == "setosa") + (Species == "versicolor") + (Species == "virginica") ~
                  Petal.Length + Petal.Width, train, linear.output = FALSE) 

## 3.2b making prediction
pred <- predict(nn, test)

## 3.3c checking accuracy
table(testClass, apply(pred, 1, which.max))











