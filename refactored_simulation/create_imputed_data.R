###
# R script file that creates imputed versions of the files in datasets/original and stores them in datasets/imputed
##

# install and import all packages
neededPackages <- c("dplyr", "mice", "missForest", "farff")
installedPackages <- neededPackages %in% installed.packages()
if (length(neededPackages[!installedPackages]) > 0) {
  install.packages(neededPackages[!installedPackages], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
}
lapply(neededPackages, require, character.only=TRUE)

source("helper_functions.R")
source("imputation_methods.R")

# iterate over all datasets
datasets <- list.files(file.path("datasets", "original"), pattern="*.arff")
for (dataset in datasets) {
  dataframe <- readARFF(file.path("datasets", "original", dataset))
  
  # start imputation
  dataframe <- missingForest(dataframe)
  # save files
  filename <- paste("missingForest", dataframe, sep="_")
  writeARFF(dataframe, file.path("datasets", "imputed", filename), overwrite=TRUE)
}


# TODO: finish file
