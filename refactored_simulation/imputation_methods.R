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
  # TODO: implement
  return (dataframe)
}

modeImputation <- function(dataframe) {
  # TODO: implement
  return (dataframe)
}

kNeirestImputation <- function(dataframe) {
  # TODO: implement
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