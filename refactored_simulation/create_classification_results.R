##
# File that creates classification results for datasets in datasets/imputed
##

# install and import all packages
neededPackages <- c("caret", "dplyr", "farff", "nnet")
installedPackages <- neededPackages %in% installed.packages()
if (length(neededPackages[!installedPackages]) > 0) {
  install.packages(neededPackages[!installedPackages], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
}
lapply(neededPackages, require, character.only=TRUE)

source("helper_functions.R")

# -------------------------------------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------------------------------------

##
# Preprocess data. 
#
# Specifically, mark class as factor for regression model.
##
preprocessData <- function(data) {
  
  data$class <- as.factor(data$class)
  return (data)
}

##
# Returns multinomial logistic regression model trained on data.
##
trainModel <- function(data) {
  # create logistic regression model (https://stats.idre.ucla.edu/r/dae/logit-regression/)
  ob <- as.formula(paste("class ~ .", sep=""))
  train_control <- trainControl(method = "none", classProbs = FALSE)
  model <- train(ob, data=data, method="glm", trControl=train_control, family="binomial", na.action=na.pass)
  
  return (model)
}


##
# Returns predictions of data based on the given trained model.
##
testData <-  function(model, data) {
  preds <- predict(model, newdata=data)
  return (preds)
}


# -------------------------------------------------------------------------------------------------
# Main
# -------------------------------------------------------------------------------------------------

datasets <- list.files(file.path("datasets", "original"), pattern="*.arff")  # read original file

for (dataset in datasets) {
  dataframe <- readARFF(file.path("datasets", "original", dataset))
  index <- createDataPartition(dataframe, p = 0.75, list=FALSE) # split data in 75% train, 25% test

  imputationMethods <- c("ld", "knn", "modeImputation", "missingForestImputation", "mice")
  
  for (imputationMethod in imputationMethods) {
    datasetName <- paste(imputationMethod, dataset, sep = "_")
    dataframe <- readARFF(file.path("datasets", "imputed", datasetName))
    dataframe <- preprocessData(dataframe)
    trainData <- dataframe[index,]
    testData <- dataframe[-index,]  
    
    # train model
    trainedModel <- trainModel(trainData)
    
    # create predictions for test dataset
    preds <- testData(trainedModel, testData)
    
    # TODO: compute and save accuracy/spd of result on test
  }
  

}
