#########################################################################################################
# Logistic Regression
#########################################################################################################
## 1. In order to have only two classes, observations corresponding 
## to Species, setosa, has been removed 
inSetosa <- iris$Species == "setosa"
myIris <- iris[!inSetosa, c("Petal.Length", "Petal.Width","Species")]
myIris$Species <- factor(myIris$Species, levels = c("versicolor", "virginica"))

# 2. Splitting the data set into training and testing data set
set.seed(100)
nr <- nrow(myIris)
inTrain <- sample(1:nr, 0.6*nr)
train <- myIris[inTrain,]
test <- myIris[-inTrain,]
testClass <- myIris[-inTrain,3]

# 3. Model training
glmModel <- glm(Species ~ Petal.Length, data = myIris, family = binomial(link="logit"))

# 3. Making Prediction
test[1,]
predVal <- predict(glmModel, newdata = test[1,], type = "response")
predVal
testClass[1]

predVal <- predict(glmModel, newdata = test, type = "response")

# 4. Checking prediction accuracy
predClass <- ifelse(predVal > 0.5, "virginica", "versicolor")
mean(predClass == testClass)
table(predClass,testClass)


# 5. Adding more predictor variable
glmModel <- glm(Species ~ Petal.Length + Petal.Width, data = myIris, 
                family = binomial(link="logit"))
predVal <- predict(glmModel, myIris, type = "response")
predClass <- ifelse(predVal > 0.5, "virginica", "versicolor")
table(predClass, myIris$Species)
