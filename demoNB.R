library(e1071)

##1. In order to have only two classes, observations corresponding 
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

# 3. Making the naive bayes model
nbModel <- naiveBayes(Species ~ Petal.Length, data = train)

# 4. Making the prediction
predClass <- predict(nbModel, test)

# 5. Assessment
mean(predClass == testClass)
table(predClass, testClass)


## Challenge: Make a generalized naive bayes classifier to identify the three 
## species of the flower based on their petal and sepal measurement


