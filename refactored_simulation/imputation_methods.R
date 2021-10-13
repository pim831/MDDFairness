##
# File containing the imputation method functions.
##

missingForest <- function(dataframe) {
  imputedData <- missForest(dataframe)
  return (imputedData)
}

listwiseDeletion <- function(dataframe) {
  # TODO: implement
  return (dataframe)
}

modeImputation <- function(dataframe) {
  # TODO: implement
  return (dataframe)
}

kNeirestImputation <- function(dataframe) {
  #Set all missing values to NA
  dataframe[dataframe == "character"] <- NA
  
  #Find name of first and last column
  firstCol <- names(dataframe)[1]
  nCols <- ncol(dataframe)
  lastCol <- names(dataframe)[nCols]
  print(firstCol)
  print(lastCol)
  
  #Run the k-Nearest Neighbour imputation method and save it in a new variable. Note: the k-value is not fixed
  kNNdf <- kNN(dataframe, k = 5)
  
  #Delete the two columns added by the kNN imputation function
  kNNdf <- subset(kNNdf, select = firstCol:lastCol)
  return (kNNdf)
}

miceImputation <- function(dataframe) {
  # TODO: implement
  return (dataframe)
}