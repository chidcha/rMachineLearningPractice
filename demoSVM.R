##############################################################################
# We will use SVM to predict if a flower belongs to Versicolor or Virginica
# species based on its petal measurement
##############################################################################
# 1. Load the required libraries
##install.packages("e1071")
library(e1071)

# 2. Splitting the data set into training and testing data set
inSetosa <- iris$Species == "setosa"
myIris <- iris[!inSetosa, c("Petal.Length", "Petal.Width", "Species")]
myIris$Species <- factor(myIris$Species, levels = c("versicolor", "virginica"))
set.seed(100)
nr <- nrow(myIris)
inTrain <- sample(1:nr, 0.6*nr)
train <- myIris[inTrain,]
test <- myIris[-inTrain,1:2]
testClass <- myIris[-inTrain,3]

# 3. Model training
svmModel <- svm(Species ~ ., data = train, kernal = "linear", scale = TRUE, 
                cost = 1)

svmModel <- svm(Species ~ ., data = train, kernal = "radial", scale = TRUE,
                cost = 1, gamma = 0.5)

# 3. Making Prediction
test[1,]
predClass <- predict(svmModel, newdata = test[1,])
predClass
testClass[1]

predClass <- predict(svmModel, newdata = test)

# 4. Checking prediction accuracy
mean(predClass == testClass)
table(predClass,testClass)


# 5. Challenge
############################################################################
# Build a generalized SVM classifier that uses both Sepal and Petal 
# measurement to identify all 3 species. Find the accuracy and the confusion
# matrix for the classifier
##########################################################################

