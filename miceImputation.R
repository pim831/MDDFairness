rm(list = ls()) # clear environment variables

# load in packages
.lib<- c("DescTools","digest", "plyr", "dplyr","mice","missForest","Hmisc", "ggplot2", "reshape2", "farff", "VIM", "parallel")
.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)
set.seed(288)

# read in data
titanic_data <- readARFF("datasets/TitanicKaggle_Sex.arff")
adult_data <- readARFF("datasets/adult_Race.arff")
recid_data <- readARFF("datasets/Recidivism_Race.arff")

# create missingness patterns visualizations
plot_miss <- function(data) {
  aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
}
plot_miss(titanic_data)
plot_miss(adult_data)
plot_miss(recid_data)

# do standard mice imputation
imp_titanic_data <- complete(mice(titanic_data, m = 1), action=1)
imp_adult_data <- complete(mice(adult_data, m = 1, nnet.MaxNWts = 4000), action=1)
imp_recid_data <- complete(mice(recid_data, m = 1), action=1)

# check if imputation worked by creating missing data plot again
plot_miss(imp_titanic_data) # seems to have worked
plot_miss(imp_adult_data)# seems to have worked too

# save the imputed datasets
write.csv(imp_titanic_data, file = "datasets/titanic_data_mice_default.csv", row.names = FALSE)
write.csv(imp_adult_data, file = "datasets/adult_data_mice_default.csv", row.names = FALSE)


# Some Questions:
#     mice has many parameters, do we just want to take the default or
#         take these as some sort of hyperparam in training?
#     mice only supports categorical variables with up to 50 categories,
#         therefore an imputation of recid_data cannot be created
#         do we not want to consider this variable as a whole?
#         or perhaps use listwise deletion for observations on that variable?
