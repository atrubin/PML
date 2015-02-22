library(caret)
library(randomForest)

setwd("~/Documents/R/coursera/MachineLearning/")
TrainingData <- read.csv("pml-training.csv", header = TRUE, na.strings=c("NA","#DIV/0!",""))
TestingData <- read.csv("pml-testing.csv", header = TRUE, na.strings=c("NA","#DIV/0!",""))

keeps <- c()

for(ColName in names(TrainingData))
{
    if(sum(is.na(TrainingData[[ColName]])) == 0)
        keeps <- c(keeps, ColName)
}

keeps <- keeps[8:length(keeps)]
TrainingData <- TrainingData[keeps]


set.seed(150984)

Fields <- createFolds(y=TrainingData$classe, k = 10, list = TRUE, returnTrain = FALSE)

Training <- TrainingData[Fields[[1]],]
Testing <- TrainingData[Fields[[2]],]

Cntrl <- trainControl(method = "cv", number = 4)

ModFit <- train(Training$classe ~ ., method="rf", 
                preProcess=c("center", "scale"), 
                trControl=Cntrl, 
                data=Training)

print(ModFit, digits=3)

Prediction <- predict(ModFit, newdata=Testing)

CM <- confusionMatrix(Prediction, Testing$classe)
print(CM, digits=3)


1-CM$overall[[1]]

