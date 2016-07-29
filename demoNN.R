###############################################################################
# We will make a linear model to predict the mpg (miles/gallon) of a car 
# based on its design aspects
#############################################################################
# 1. Load the required libraries
##install.packages("nnet")
library(nnet)  # library for neural network

# 2. Building the neural network model
## 2.a Splitting the data set into training and testing data set
nr <- nrow(mtcars)
inTrain <- sample(1:nr, 0.6*nr)
train <- mtcars[inTrain,]
test <- mtcars[-inTrain, 2:11]
testV <- mtcars[-inTrain, 1]

## 2.b Training the neural network model
nnModel <- nnet(mpg ~ wt + cyl, data = train, size = 3,
                linout = TRUE, skip = TRUE)
nnModel <- nnet(mpg ~ ., data = train, size = 3,
                linout = TRUE, skip = TRUE)


## 2.c Making prediction with the tree model
test[1,]
predict(nnModel, test[1,])
testV[1]

predV <-  predict(nnModel, test)
predV
data.frame(predV, testV)

## 3. Checking the accuracy of the model
mean((predV - testV)^2) # RMS Error


## Challenge question
# Build a linear model to predict the House Price in Boston. Find the
# RMS error


