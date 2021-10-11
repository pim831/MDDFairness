##
# File creates visualizations and tables for the results in the results folder
##

source("helper_functions.R")

# install and import all packages
neededPackages <- c("dplyr", "ggplot2")
installedPackages <- neededPackages %in% installed.packages()
if (length(neededPackages[!installedPackages]) > 0) {
  install.packages(neededPackages[!installedPackages], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
}
lapply(neededPackages, require, character.only=TRUE)

source("helper_functions.R")


# TODO: finish doc
