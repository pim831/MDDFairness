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
# Returns multinomial logistic regression model trained on data.
##
runModel <- function(data) {
  x <- dataTrain[, ncol(data)]
  y <- dataTrain[, -ncol(data)]
  
  # TODO: do multinomial logistic regression
  
  #multinomModel <- multinomial(paste("class", " ~ .", sep=""), d)
  
}


##
# Returns predictions of data based on the given trained model.
##
testData <-  function(model, data) {
  # TODO: implement
}


# -------------------------------------------------------------------------------------------------
# Main
# -------------------------------------------------------------------------------------------------

datasets <- list.files(file.path("datasets", "original"), pattern="*.arff")  # read original file

for (dataset in datasets) {
  dataframe <- readARFF(file.path("datasets", "original", dataset))
  index <- createDataPartition(data, p = 0.75, list=FALSE)

  imputationMethods <- c("ld", "knn", "modeImputation", "missingForestImputation", "mice")
  
  for (imputationMethod in imputationMethods) {
    datasetName <- paste(imputationMethod, dataset, sep = "_")
    dataframe <- readARFF(file.path("datasets", "imputed", datasetName))
    trainData <- data[index,]
    testData <- data[-index,]  
    
    # train model
    trainedModel <- runModel(trainData)
    
    # create predictions for test dataset
    preds <- testData(trainedModel, testData)
    
    # TODO: compute and save accuracy/spd of result on test
  }
  

}
