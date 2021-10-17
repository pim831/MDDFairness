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
  str(dataframe)
  #Set all missing values to NA
  dataframe[dataframe == "character"] <- NA
  
  #Find name of first and last column
  firstCol <- names(dataframe)[1]
  nCols <- ncol(dataframe)
  lastCol <- names(dataframe)[nCols]
  
  #Run the k-Nearest Neighbour imputation method and save it in a new variable. Note: the k-value is not fixed
  dataframe <- kNN(dataframe, variable = colnames(dataframe), k = 5)
  
  #Delete the columns added by the kNN imputation function
  dataframe <- subset(dataframe, select = firstCol:lastCol)
  return (dataframe)
}

miceImputation <- function(dataframe) {
  # TODO: implement
  return (dataframe)
}
