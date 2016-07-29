#######################################################################
# 1. Splitting the data into training and testing set
#######################################################################

# Method A
nr <- nrow(iris)
set.seed(1)
inTrain <- sample(1:nr, 0.6*nr)  # 60% data to be used as training set
train <- iris[inTrain,]
test <- iris[-inTrain,]


# Method B
nr <- nrow(iris)
set.seed(1)
inTrain <- sample(c(TRUE, FALSE), nr, replace = TRUE, prob = c(0.6,0.4))  # 60% data to be used as training set
train <- iris[inTrain,]
test <- iris[!inTrain,]


###########################################################################
# k-fold cross validation
###########################################################################
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


#########################################################################
# Bootstrapping
########################################################################
nr <- nrow(iris)
sample(1:nr, nr, replace = TRUE)
length(unique(sample(1:nr, nr, replace = TRUE)))
nr
150*.6


