##
# File that creates classification results for datasets in datasets/imputed
##

# install and import all packages
neededPackages <- c("dplyr")
installedPackages <- neededPackages %in% installed.packages()
if (length(neededPackages[!installedPackages]) > 0) {
  install.packages(neededPackages[!installedPackages], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
}
lapply(neededPackages, require, character.only=TRUE)

source("helper_functions.R")


# TODO: create file
