###
# R script file that creates imputed versions of the files in datasets/original and stores them in datasets/imputed
##

# install and import all packages
neededPackages <- c("dplyr", "mice", "missForest", "farff", "VIM")
installedPackages <- neededPackages %in% installed.packages()
if (length(neededPackages[!installedPackages]) > 0) {
  install.packages(neededPackages[!installedPackages], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
}
lapply(neededPackages, require, character.only=TRUE)

source("C:/Users/20168482/OneDrive/Documents/TUe_master/TUe_year_5/Kwartiel1/Research_Topics_in_Data_Mining/MDDFairness/refactored_simulation/helper_functions.R")
source("C:/Users/20168482/OneDrive/Documents/TUe_master/TUe_year_5/Kwartiel1/Research_Topics_in_Data_Mining/MDDFairness/refactored_simulation/imputation_methods.R")

# -------------------------------------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------------------------------------

saveImputedFile <- function(dataframe, name, method) {
  imputedDataframe <- missingForest(dataframe)
  filename <- paste(name, dataset, sep="_")
  writeARFF(dataframe, file.path("C:/Users/20168482/OneDrive/Documents/TUe_master/TUe_year_5/Kwartiel1/Research_Topics_in_Data_Mining/MDDFairness/refactored_simulation/datasets", "imputed", filename), overwrite=TRUE)
}

# -------------------------------------------------------------------------------------------------
# Main
# -------------------------------------------------------------------------------------------------

# iterate over all datasets
datasets <- list.files(file.path("C:/Users/20168482/OneDrive/Documents/TUe_master/TUe_year_5/Kwartiel1/Research_Topics_in_Data_Mining/MDDFairness/refactored_simulation/datasets", "original"), pattern="*.arff")
for (dataset in datasets) {
  dataframe <- readARFF(file.path("C:/Users/20168482/OneDrive/Documents/TUe_master/TUe_year_5/Kwartiel1/Research_Topics_in_Data_Mining/MDDFairness/refactored_simulation/datasets", "original", dataset))
  
  # start imputation
  saveImputedFile(dataframe, "knn", kNeirestImputation)
  saveImputedFile(dataframe, "missingForest", missingForest)
  saveImputedFile(dataframe, "ld", listwiseDeletion)
  saveImputedFile(dataframe, "modeImputation", modeImputation)
  saveImputedFile(dataframe, "mice", miceImputation)
}
