###
# R script file that creates imputed versions of the files in datasets/original and stores them in datasets/imputed
##

# install and import all packages
neededPackages <- c("dplyr", "mice", "missForest")
installedPackages <- neededPackages %in% installed.packages()
if (length(neededPackages[!installedPackages]) > 0) {
  install.packages(neededPackages[!installedPackages], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
}
lapply(neededPackages, require, character.only=TRUE)

source("helper_functions.R")

# TODO: finish file
