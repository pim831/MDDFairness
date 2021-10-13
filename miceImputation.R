rm(list = ls()) # clear environment variables

# load in packages
.lib<- c("DescTools","digest", "plyr", "dplyr","mice","missForest","Hmisc", "ggplot2", "reshape2", "farff", "VIM", "parallel")
.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)
set.seed(288)

# read in data
titanic_data <- readARFF("datasets/Orig/TitanicKaggle_Sex.arff")
adult_data <- readARFF("datasets/Orig/adult_Race.arff")
recid_data <- readARFF("datasets/Orig/Recidivism_Race.arff")

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

# note that we cannot impute recid_data because feature c_charge_desc has too many levels
# hence we will drop the observations with missing values for that feature and not include
# the factor into the imputation.
recid_data_dropped <- subset(recid_data, select = -c(c_charge_desc)) # without c_charge_desc
imp_recid_data_dropped <- complete(mice(recid_data_dropped, m = 1), action=1)
imp_recid_data <- cbind(imp_recid_data_dropped, recid_data$c_charge_desc) #add c_charge_desc back in
imp_recid_data <- na.omit(imp_recid_data) #omit rows with missing values (so only for c_charge_desc)

# check if imputation worked by creating missing data plot again
plot_miss(imp_titanic_data) # seems to have worked, no missings
plot_miss(imp_adult_data) # seems to have worked, no missings
plot_miss(imp_recid_data) # seems to have worked, no missings

# save the imputed datasets
write.csv(imp_titanic_data, file = "datasets/titanic_data_mice_default.csv", row.names = FALSE)
write.csv(imp_adult_data, file = "datasets/adult_data_mice_default.csv", row.names = FALSE)
write.csv(imp_recid_data, file = "datasets/recid_data_mice_default.csv", row.names = FALSE)

# Some Questions:
#     mice has many parameters, do we just want to take the default or
#         take these as some sort of hyperparam in training?
#     mice only supports categorical variables with up to 50 categories,
#         therefore an imputation of recid_data cannot be created
#         do we not want to consider this variable as a whole?
#         or perhaps use listwise deletion for observations on that variable?
