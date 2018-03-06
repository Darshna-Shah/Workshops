
#=======================================================================================
#
# File:        IntroToMachineLearning.R
# Description: This code illustrates the usage of the caret package for the An 
#              Introduction to Machine Learning with R and Caret".
#
#=======================================================================================

# Demo:the R environment, scripts, ?help,

# install packages
install.packages("caret")
install.package("ggplot2")
install.packages("e1071")

# Load the library
library(caret)
library(ggplot2)
library(e1071)

# View and set the working directory
path <- "insert your folder path here"

getwd()
setwd(path)

# read in the data
TitanicData <- read.csv("train.csv")

# Examine the dimensions of the data frame
dim(TitanicData)

# Examine the data types of variables in the data set
str(TitanicData)

# Convert data types where appropriate
TitanicData$Survived <- as.factor(TitanicData$Survived)
TitanicData$Pclass <- as.factor(TitanicData$Pclass)
TitanicData$Age <- as.integer(TitanicData$Age)

# Examine the the summary statistics for each variable in the data frame
summary(TitanicData)

# Explore relationships between variables with some basic plots

ggplot(data=TitanicData, aes(x=Sex, y=Survived, fill=Sex)) +
  geom_bar(stat="identity")+
  ggtitle("Differences between males and females that survived the Titanic")

ggplot(data=TitanicData, aes(x=Age, y=Survived, fill=Age)) +
  geom_bar(stat="identity")+
  ggtitle("Differences in different aged individuals that did not survive the Titanic")

ggplot(data=TitanicData, aes(x=Pclass, y=Survived, fill=Pclass)) +
  geom_bar(stat="identity")+
  ggtitle("Differences between different classes that did not survive the Titanic")

########################################################################################################

#================================================================
#correct data types forms for modelling
#================================================================
TitanicData$Pclass <- as.integer(TitanicData$Pclass)
TitanicData$Sex <- as.integer(TitanicData$Sex)
TitanicData$Embarked <- as.integer(TitanicData$Embarked)

levels(TitanicData$Survived)[levels(TitanicData$Survived)== "0"] <- "perished"
levels(TitanicData$Survived)[levels(TitanicData$Survived)== "1"] <- "survived"

summary(TitanicData)

#=================================================================
# Split Data
#=================================================================

# Use caret to create a 70/30% split of the training data,
# keeping the proportions of the Survived class label the
# same across splits.
set.seed(42)
indexes <- createDataPartition(TitanicData$Survived,
                               times = 1,
                               p = 0.7,
                               list = FALSE)
titanic.train <- TitanicData[indexes,]
titanic.test <- TitanicData[-indexes,]

#========================================================================
# Examine the proportions of the Survived class lable across the datasets.
#========================================================================
prop.table(table(TitanicData$Survived))
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))

#========================================
#  the classification model
#========================================

#model
model <- train(Survived~., data = titanic.train, method = "glm")
model
model$finalModel

#test
preds <- predict(model, titanic.test)
preds

#evaluate
confusionMatrix(preds, titanic.test$Survived)


# f1 score function
f1_score <- function(TP, FP, FN){
  precision <- TP/(TP + FP)
  recall <- TP/(TP + FN)
  F1 <- 2*(precision*recall)/(precision+recall)
  
  return(F1)
}

f1_score(141,28,23) #0.8468468

# investigate feature importance
varImp(object=model)
plot(varImp(object=model),main="glm  - Variable Importance")
################################################################################################################

#=================================================================
#  Use a decision tree algorithm to predict titanic survival
#================================================================

set.seed(42)
model_DT <- train(Survived ~., data = titanic.train,
                  method = "rpart",
                  trControl = trainControl(method = "cv", number = 5, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE))

model_DT
summary(model_DT)

#test
predictedvals <- predict(model_DT, titanic.test)
summary(predictedvals)

#evaluate
confusionMatrix(predictedvals, titanic.test$Survived)

f1_score(149,34,15) #0.8587896


