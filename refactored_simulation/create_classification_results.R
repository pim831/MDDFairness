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
# Metadata: Protected attributes & (non-)privileged values
# -------------------------------------------------------------------------------------------------

lnoprivs<-list()
lnamepos<-list()
lprivs<- list()

lprivs[["adult_Sex.arff"]]<-c("sex","Male")
lnoprivs[["adult_Sex.arff"]]<-c("sex","Female")
lnamepos[["adult_Sex.arff"]]<-">50K"

lprivs[["adult_Race.arff"]]<-c("race","White")
lnoprivs[["adult_Race.arff"]]<-c("race","No White")
lnamepos[["adult_Race.arff"]]<-">50K"


lprivs[["TitanicKaggle_Sex.arff"]]<-c("sex","female")
lnoprivs[["TitanicKaggle_Sex.arff"]]<-c("sex","male")
lnamepos[["TitanicKaggle_Sex.arff"]]<-"1"

lprivs[["TitanicKaggle_Class.arff"]]<-c("pclass","1")
lnoprivs[["TitanicKaggle_Class.arff"]]<-c("pclass","2-3")
lnamepos[["TitanicKaggle_Class.arff"]]<-"1"


lprivs[["Recidivism_Sex.arff"]]<-c("sex","Female")
lnoprivs[["Recidivism_Sex.arff"]]<-c("sex","Male")
lnamepos[["Recidivism_Sex.arff"]]<-"0"

lprivs[["Recidivism_Race.arff"]]<-c("race","Caucasian")
lnoprivs[["Recidivism_Race.arff"]]<-c("race","No Caucasian")
lnamepos[["Recidivism_Race.arff"]]<-"0"

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
getPreds <- function(model, data) {
  preds <- predict(model, newdata=data)
  return (preds)
}

##
# Get table with statistics about preds. 
#
# Not exactly sure what this does since I just copied it but it works I think.
##
cm <- function(preds, mlabs, posClass)
{
  rescm<-list()
  posInd <- which(levels(preds) == posClass)
  negInd <- which(levels(preds) != posClass)
  
  pos<-levels(preds)[posInd]
  neg<-levels(preds)[negInd]
  rescm$TP<-sum(mlabs==pos&preds==pos, na.rm = TRUE)
  rescm$FP<-sum(mlabs==neg&preds==pos, na.rm = TRUE)
  rescm$TN<-sum(mlabs==neg&preds==neg, na.rm = TRUE)
  rescm$FN<-sum(mlabs==pos&preds==neg, na.rm = TRUE)
  rescm$N<-rescm$FN+rescm$FP+rescm$TN+rescm$TP
  rescm$acc<-(rescm$TP+rescm$TN)/length(preds)
  rescm$TPR<-rescm$TP/(rescm$TP+rescm$FN)
  rescm$FNR<-rescm$FN/(rescm$TP+rescm$FN)
  rescm$TNR<-rescm$TN/(rescm$TN+rescm$FP)
  rescm$FPR<-rescm$FP/(rescm$TN+rescm$FP)
  return(rescm)
}

##
# Returns spd given the predictions, test set and information on the privilege columns.
##
get_spd <- function(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, posClass){
  resColumnPriv <- cm(preds[test[,WhatColPriv]==WhoPriv], test$class[test[,WhatColPriv]==WhoPriv], posClass)
  resColumnNotPriv <- cm(preds[test[,WhatColPriv]==WhoNoPriv], test$class[test[,WhatColPriv]==WhoNoPriv], posClass)
  res <- compute_spd(resColumnPriv, resColumnNotPriv)
  return(res)
}

# -------------------------------------------------------------------------------------------------
# Main
# -------------------------------------------------------------------------------------------------

datasets <- list.files(file.path("datasets", "original"), pattern="*.arff")  # read original file

# create empty results dataframe
results <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(results) <- c("acc", "spd", "imp_method", "classifier", "dataset")


for (dataset in datasets) {
  dataframe <- readARFF(file.path("datasets", "original", dataset))
  index <- createDataPartition(dataframe$class, p = 0.75, list=FALSE) # split data in 75% train, 25% test

  imputationMethods <- c("knn", "missingForest", "mode", "mice")
  
  for (imputationMethod in imputationMethods) {
    datasetName <- paste(imputationMethod, dataset, sep = "_")
    dataframe <- readARFF(file.path("datasets", "imputed", datasetName))
    dataframe <- preprocessData(dataframe)
    trainData <- dataframe[index,]
    testData <- dataframe[-index,]  
    
    # train model
    trainedModel <- trainModel(trainData)
    
    # create predictions for test dataset
    preds <- getPreds(trainedModel, testData)
    
    # compute accuracy
    accuracy <- sum((preds == testData$class)/length(preds))
    
    WhatColPriv = lprivs[[dataset]][1]
    WhoPriv = lprivs[[dataset]][2]
    WhoNoPriv = lnoprivs[[dataset]][2]
    
    # compute spd
    spd <- get_spd(preds, testData, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[dataset]][1])
    
    # append results to results dataframe
    results[nrow(results) + 1,] = c(accuracy, spd, imputationMethod, "LogisticRegression", dataset)
  }
  
}

# save results in file

print(results)
filename <- paste(dataset, ".csv", sep = "")
filepath <- file.path("results", filename)
write.csv(x = results, file = filepath)

