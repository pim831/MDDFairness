##
# File containing the imputation method functions.
##

missingForest <- function(dataframe) {
  # if c_charge_desc in dataframe do not consider the feature in imputation
  if ("c_charge_desc" %in% colnames(dataframe)){
    data_dropped <- subset(dataframe, select = -c(c_charge_desc)) # without c_charge_desc
    imp_data_dropped <- missForest(data_dropped)$ximp
    imp_data <- cbind(imp_data_dropped, c_charge_desc = dataframe$c_charge_desc) #add c_charge_desc back in
    dataframe <- na.omit(imp_data) #omit rows with missing values (so only for c_charge_desc)
  } else {
    dataframe <- missForest(dataframe)$ximp
  }
  return (imputedData)
}

listwiseDeletion <- function(dataframe) {
  dataframe = na.omit(dataframe)
  return (dataframe)
}

modeImputation <- function(dataframe) {
  # TODO: implement
  cols = colnames(dataframe)[colSums(is.na(dataframe)) > 0]
  
  for(i in cols){
    if(class(dataframe[,colnames(dataframe)%in%i]) == "factor"){
      dataframe[,colnames(dataframe)%in%i] <-  impute(dataframe[,i], mode) 
      dataframe[,colnames(dataframe)%in%i] <- as.factor(dataframe[,colnames(dataframe)%in%i])
    }
    else{
      if (class(dataframe[,colnames(dataframe)%in%i]) == "numeric" | class(dataframe[,colnames(dataframe)%in%i]) == "integer"){
        dataframe[,colnames(dataframe)%in%i] <-  impute(dataframe[,i], mean) 
      }else{
        print("error: column is neither categorical nor numeric")
      }
      
    }
  }
  
  
  return (dataframe)
}

kNeirestImputation <- function(dataframe) {
  str(dataframe)
  #Set all missing values to NA
  dataframe[dataframe == "character"] <- NA
  
  #Find name of first and last column
  originalCols <- names(dataframe)
  
  #Run the k-Nearest Neighbour imputation method and save it in a new variable. Note: the k-value is not fixed
  dataframe <- kNN(dataframe, variable = colnames(dataframe), k = 5, makeNA = NULL)
  #Delete the columns added by the kNN imputation function
  dataframe <- subset(dataframe, select = originalCols)
  return (dataframe)
}

miceImputation <- function(dataframe) {
  # if c_charge_desc in dataframe do not consider the feature in imputation
  if ("c_charge_desc" %in% colnames(dataframe)){
    data_dropped <- subset(dataframe, select = -c(c_charge_desc)) # without c_charge_desc
    imp_data_dropped <- complete(mice(data_dropped, m = 1), action=1)
    imp_data <- cbind(imp_data_dropped, c_charge_desc = dataframe$c_charge_desc) #add c_charge_desc back in
    dataframe <- na.omit(imp_data) #omit rows with missing values (so only for c_charge_desc)
  } else {
    dataframe <- complete(mice(dataframe, m = 1, nnet.MaxNWts = 4000), action=1) 
  }
  return (dataframe)
}
