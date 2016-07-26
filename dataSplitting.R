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
##http://www.r-bloggers.com/cross-validation-for-predictive-analytics-using-r/

nTrain <- 150
kFolds <- 5
folds <- sample(rep(1:nFolds, length.out = nTrain))
cv_tmp <- matrix(NA, nrow = kFolds, ncol = length(iris))
for (i in 1:kFolds) {
  iTest <- which(folds == i)
  train <- iris[-iTest, ]
  test <- iris[iTest, ]
  fitted_models <- apply(t(df), 2, function(degf) lm(y ~ ns(x, df = degf)))
  x <- test_xy$x
  y <- test_xy$y
  pred <- mapply(function(obj, degf) predict(obj, data.frame(ns(x, df = degf))), 
                 fitted_models, df)
  cv_tmp[k, ] <- sapply(as.list(data.frame(pred)), function(y_hat) mean((y - 
                                                                           y_hat)^2))
}
cv <- colMeans(cv_tmp)



#########################################################################
# Bootstrapping
########################################################################
nr <- nrow(iris)
sample(1:nr, nr, replace = TRUE)
length(unique(sample(1:nr, nr, replace = TRUE)))
nr
150*.6


