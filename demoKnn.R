##############################################################################
# We will use knn to predict the Species of a flower based
# on its sepal and petal measurement
##############################################################################

# 1. Load the required libraries
##install.packages("class")
library(class)

# 2. Splitting the data set into training and testing data set
nr <- nrow(iris)
inTrain <- sample(1:nr, 0.6*nr)
train <- iris[inTrain,]
test <- iris[-inTrain, 1:4]
testClass <- iris[-inTrain, 5]

# 3. Making Prediction
test[1,]
testCl <- knn(train[,1:4], test[1,], train[,5], k = 3)
testClass[1]

testCl <- knn(train[,1:4], test, train[,5], k = 3)

# 4. Checking prediction accuracy
mean(testCl == testClass)
table(testCl,testClass)


# 5. k-fold cross validation illustration
nn <- 1:10   ## nearest neighbour
kFolds <- 10  ## number of folds

folds <- sample(rep(1:kFolds, length.out=nrow(iris)))

predEvalMat <- matrix(data = NA, nrow = nrow(iris), ncol = 10)
predEvalMat

for (i in nn){
  for (j in 1:kFolds){
    inTest <- which(folds==j)
    train <- iris[-inTest,]
    test <- iris[inTest,]
    predClass <- knn(train[,1:4], test[,1:4], train[,5], k = i)
    testClass <- test[,5]
    predEvalMat[inTest,i] <- predClass == testClass
  }
}

apply(predEvalMat, 2, sum)


# 6. Challenge: Split the boston data sets into two parts containing 60% and 40%
# of the data. Use knn to predict the house price of the data set that has 40%
# of the data using the part that has 60% data and also find out the RMS error

##install.packages("FNN")
library(FNN)

# 1 Download the data set 
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
boston <- read.table(url, header = FALSE, nrows = -1)
names(boston) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD",
                   "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

head(boston)

# 2 Scale the data set
myScale <- function(x){
  xmin <- min(x)
  xmax <- max(x)
  (x - xmin)/(xmax - xmin)
}

# 3. divide the data into training and test set



# 4. make the prediction
predV <- knn.reg(.....)


# 5. Access the model performance. Use root mean square value
sqrt(sum((predV$pred - testV)^2))/length(predV$pred)





