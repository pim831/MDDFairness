##
# File creates visualizations of the results
##

source("helper_functions.R")

# install and import all packages
neededPackages <- c("dplyr", "ggplot2", "ggrepel")
installedPackages <- neededPackages %in% installed.packages()
if (length(neededPackages[!installedPackages]) > 0) {
  install.packages(neededPackages[!installedPackages], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
}
lapply(neededPackages, require, character.only=TRUE)

# -------------------------------------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------------------------------------
# returns a dataframe with the perfect and majority classifier added to it
add_extra_classifiers <-function(dataframe){
  n_imp <-length(unique(dataframe$imp_method)) # number of imputation methods
  datasets <- unique(dataframe$dataset) # list of datasets
  n_datasets <- length(datasets) # number of different datasets
  df_out <- dataframe
  
  # initialize new variables for perfect and majority classifier
  df_out$perf_acc <- NA
  df_out$perf_spd <- NA
  df_out$maj_acc <- NA
  df_out$maj_spd <- NA
  
  # spd of perfect classifier and acc of majority in order of the datasets
  # now assumed order Adult_R, Adult_S, Recidivism_R, Recidivism_S, Titanic_C, Titanic_S
  perf_spds <- c(0.1014, 0.1945, 0.0864, 0.1161, 0.3149, 0.5365)
  maj_accs <- c(0.7607182, 0.7607182, 0.618029, 0.618029, 0.5493485, 0.5493485)
  new_dataset_names <- c("Adult (Race)", "Adult (Sex)", "Recidivism (Race)", 
                         "Recidivism (Sex)", "Titanic (Class)", "Titanic (Sex)")
  
  for (i in 1:n_datasets){
    #insert the acc and spd values for the majority and perfect classifier
    df_out$perf_acc[1+((i-1)*n_imp)] <- 1.0
    df_out$perf_spd[1+((i-1)*n_imp)] <- perf_spds[i]
    df_out$maj_acc[1+((i-1)*n_imp)] <- maj_accs[i]
    df_out$maj_spd[1+((i-1)*n_imp)] <- 0
    
    # rename the dataset
    df_out$dataset[df_out$dataset==datasets[i]] <- new_dataset_names[i]
  }
  
  return (df_out)
}

# assumes as input a dataframe having the structure like datasets/toy_data
create_plot <- function(dataframe){
  # some settings for the general layout of the plot
  Fideal = 0; Fymin = -0.1; Fymin2 = 0.1;Fymax = 0.1 #SPD
  
  ### create helper arguments for the plot
  classifiers <- unique(dataframe$classifier) # list of classifiers
  n_clf <- length(classifiers) # number of different classifiers
  datasets <- unique(dataframe$dataset) # list of datasets
  n_datasets <- length(datasets) # number of different datasets
  n_imp <-length(unique(dataframe$imp_method)) # number of imputation methods

  # define some colors that could be used in options
  base_colors = c("#f98a92", "#7dc37e", "#78cdd1", "#96b1dc", "#d89ac5", "#c8b669")
  colors = rep(base_colors[1:n_clf], n_clf)
  
  # start creating the ggplot on extended dataset
  plot <- ggplot(dataframe)+
    # first just some general layout, red areas, y=0 line etc.
    geom_rect(xmin = -Inf, xmax = +Inf,   ymin = Fymin, ymax = Fymax,   fill = "grey", alpha = 0.05, colour = "white")+
    geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.06, colour = "white")+
    geom_rect(xmin = -Inf, xmax = +Inf,   ymin = -Inf, ymax = Fymin,   fill = "lightcoral", alpha = 0.06, colour = "white") +
    geom_hline(yintercept=Fideal, linetype="dashed", color = "gray", size = 1.5) +
    annotate("text", x = -Inf + 0.1, y = Fideal, label = "Fair", angle = 90, hjust = 1.2, vjust = 1.4, colour = "darkgrey")+
    annotate("text", x = -Inf + 0.1, y = Fymin, label = "Bias", angle = 90, hjust = 1.2, vjust = 1.4, colour = "red")+ 
    annotate("text", x = -Inf + 0.1, y = Fymin2, label = "Bias", angle = 90, hjust = 0, vjust = 1.4, colour = "red") +
    
    # plot the perfect classifier
    geom_point(data = dataframe, aes(perf_acc, perf_spd), size = 8, shape = 11)+
    geom_point(aes(perf_acc, perf_spd), size = 2, colour = "white") +
    geom_point(aes(perf_acc, perf_spd), size = 1, colour = "black") +
    
    # plot the majority classifier
    geom_point(data = dataframe, aes(maj_acc, maj_spd), size = 8, shape = 13)+
    geom_point(aes(maj_acc, maj_spd), size = 2, colour = "white") +
    geom_point(aes(maj_acc, maj_spd), size = 1, colour = "black") +

    # plot the accuracies and spd's for each imputation method and classifier
    geom_point(aes(acc, spd, colour = imp_method, shape = imp_method), size = 10, alpha = 0.6)+
    geom_point(aes(acc, spd), size = 2, colour = "white") +
    geom_point(aes(acc, spd), size = 1, colour = "Black") +

    # create split over different "datasets"
    facet_wrap(~dataset, ncol = 2, scales = "free_y")+
    
    # some settings for the plot
    labs(x="Accuracy", y = "SPD") +
    scale_shape_manual(values=c(15, 16, 17, 18, 20))+
    # scale_color_manual(values=colors)
    theme_minimal() + 
    theme(legend.title = element_blank())
  
  return (plot)
}

# -------------------------------------------------------------------------------------------------
# Main
# -------------------------------------------------------------------------------------------------
data <- read.csv(file = "results/results.csv", sep = "", header = TRUE)
data <- subset(data, select = c("acc", "spd", "imp_method", "classifier", "dataset"))
data <- add_extra_classifiers(data)
plot <- create_plot(data)
plot


