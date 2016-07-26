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

# 7.1 Download the data set 
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
boston <- read.table(url, header = FALSE, nrows = -1)
names(boston) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD",
                   "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

head(boston)

# 7.2 Build the linear model (we want to predict MEDV using all other 
# variables)
lmModel <- lm(MEDV ~ ., data = boston)

# 7.3 Make the prediction
predV <- predict(lmModel, newdata = boston)

# 7.4 Look at the parameters associated with the model. coefficients, r-squared value
coef(lmModel)
sumMod <- summary(lmModel)
sumMod$r.squared

# 7.5 Access the model performance. Use root mean square value
sqrt(sum((predV - boston$MEDV)^2))/length(predV)




