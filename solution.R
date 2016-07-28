## Decision Tree Challenge
# Build a decision tree model to predict the House Price in Boston

# 1. Download the data set 
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
boston <- read.table(url, header = FALSE, nrows = -1)
names(boston) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD",
                   "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

head(boston)

# 2. Split the data into train (60%) and test (40%) set
nr <- nrow(boston)
set.seed(1)
inTrain <- sample(1:nr, 0.6*nr)
train <- boston[inTrain,]
test <- boston[-inTrain,]


# 3. Make the decision tree model (we want to predict MEDV using all other 
# variables)

tModel <- rpart(MEDV ~ ., data = boston)
rpart.plot(tModel)

# 4.Make the prediction
predV <- predict(tModel, newdata = test)
predtrain <- predict(tModel, newdata = train)

# 5. Access the model performance. Use root mean square value
data.frame(predV, test$MEDV)
sqrt(sum(predV - test$MEDV)^2)/length(predV)
sqrt(sum(predtrain - train$MEDV)^2)/length(predtrain)


## Random Forest Challenge
# Build a random Forest model to predict the House Price in Boston

# 1. Download the data set 
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
boston <- read.table(url, header = FALSE, nrows = -1)
names(boston) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD",
                   "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

head(boston)

# 2. Split the data into train (60%) and test (40%) set
nr <- nrow(boston)
set.seed(1)
inTrain <- sample(1:nr, 0.6*nr)
train <- boston[inTrain,]
test <- boston[-inTrain,]


# 3. Make the random forest model (we want to predict MEDV using all other 
# variables)

rfModel <- randomForest(MEDV ~ ., data = boston, mtry = 5, ntrees = 20)


# 4.Make the prediction
predV <- predict(rfModel, newdata = test)
predtrain <- predict(rfModel, newdata = train)

# 5. Access the model performance. Use root mean square value
data.frame(predV, test$MEDV)
sqrt(sum(predV - test$MEDV)^2)/length(predV)
sqrt(sum(predtrain - train$MEDV)^2)/length(predtrain)


## Linear model challenge
# Build a linear model to predict the House Price in Boston

# 1 Download the data set 
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
boston <- read.table(url, header = FALSE, nrows = -1)
names(boston) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD",
                   "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

head(boston)

# 2 Build the linear model (we want to predict MEDV using all other 
# variables)
lmModel <- lm(MEDV ~ ., data = boston)

# 3 Make the prediction
predV <- predict(lmModel, newdata = boston)

# 4 Look at the parameters associated with the model. coefficients, r-squared value
coef(lmModel)
sumMod <- summary(lmModel)
sumMod$r.squared

# 5 Access the model performance. Use root mean square value
sqrt(sum((predV - boston$MEDV)^2))/length(predV)


# Knn challenge
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

ncol(boston)
sboston <- apply(boston[,1:13], 2, myScale)
pboston <- boston[,14]

# 3. divide the data into train and test set
nr <- nrow(boston)
set.seed(1)
inTrain <- sample(1:nr, 0.6*nr)
train <- sboston[inTrain,]
trainV <- pboston[inTrain]
test <- sboston[-inTrain,]
testV <- pboston[-inTrain]

# 3 Make the prediction
predV <- knn.reg(train, test, trainV, k = 6)

# 5 Access the model performance. Use root mean square value
sqrt(sum((predV$pred - testV)^2))/length(predV$pred)

### k-means clustering challenge
head(mtcars)
length(names(mtcars))
mtcars1 <- scale(mtcars[,2:11], center = TRUE, scale = TRUE)
predGroup <- kmeans(mtcars1, centers = 3, nstart = 10)
mtcars2 <- mtcars
mtcars2$clusters <- predGroup$cluster
boxplot(mpg ~ clusters, data = mtcars2, col = c(2,3,4))

### hierarchical clustering challenge
#############################################################################
# Challenge: Find 3 clusters in mtcars dataset. Do not use mpg variable in 
# clustering. After clustering make a box plot of mpg vs clustering group 
#############################################################################
mtcars1 <- scale(mtcars[,2:11], center = TRUE, scale = TRUE)
distM <- dist(mtcars1)
carsCluster <- hclust(distM)
clusters <- cutree(carsCluster, k = 3)
mtcars2 <- mtcars
mtcars2$clusters <- clusters
boxplot(mpg ~ clusters, data = mtcars2, col = c(2,3,4))

### SVM Challenge
############################################################################
# Build a generalized SVM classifier that uses both Sepal and Petal 
# measurement to identify all 3 species. Find the accuracy and the confusion
# matrix for the classifier
##########################################################################
inTrain <- sample(1:nrow(iris), 0.6*nrow(iris))
train <- iris[inTrain,]
test <- iris[-inTrain,]

svmModel <- svm(Species ~ . , data = iris, kernel = "radial", 
                cost = 1, gamma = 0.5)

predClass <- predict(svmModel, test)

mean(predClass == test$Species)
table(predClass, test$Species)

## Challenge question Neural Network
# Build a linear model to predict the House Price in Boston. Find the
# RMS error
nnModel <- nnet(MEDV ~ ., data = boston, size = 10,
                linout = TRUE, skip = TRUE)
predV <-  predict(nnModel, boston)
head(data.frame(predV, boston$MEDV))
