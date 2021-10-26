# in this file an example is shown on how to create plots similar to the "original" 
# Fairness and Missing data paper, note some warnings are shown but they are not important.
# two data sets accompany this example in dir datasets/toy: 
    # toy_plot_2 (for single classifier example)
    # toy_plot_3 (for multiple classifier example)

require(ggplot2)
require(ggrepel)
require(dplyr) 
library(readxl)

rm(list = ls()) # clear environment variables

# read in some toy data that was created by @Leightonvg
toy_data <- read_xlsx("datasets/toy/toy_plot_2.xlsx")
toy_data[]=lapply(toy_data,type.convert,as.is=TRUE) # convert numbers to numeric

# some settings for the general layout of the plot
Fideal = 0; Fymin = -0.1; Fymin2 = 0.1;Fymax = 0.1 #SPD

### create df to hold extra information for line to and from perfect and majority classifiers
classifiers <- unique(toy_data$classifier) # list of classifiers
n_clf <- length(classifiers) # number of different classifiers
datasets <- unique(toy_data$dataset) # list of datasets
n_datasets <- length(datasets) # number of different datasets
n_imp <-length(unique(toy_data$imp_method)) # number of imputation methods

# # init df with -1 to be added later to org data
# data_draw <- data.frame(X_f = rep(-1, times = 2*n_clf*n_datasets),  
#                         Y_f = rep(-1, times = 2*n_clf*n_datasets),
#                         X_l = rep(-1, times = 2*n_clf*n_datasets),
#                         Y_l = rep(-1, times = 2*n_clf*n_datasets))
# 
# # add some variables for which values are easily known by structure
# data_draw$grp <- rep(1:2, times = n_clf*n_datasets)
# data_draw$classifier <- rep(classifiers, times =2*n_datasets)
# datasets_rep <- c() # init with empty
# for (dataset in datasets){
#   datasets_rep <- c(datasets_rep, rep(dataset, times = 2*n_clf))
# }
# data_draw$dataset <- datasets_rep
# 
# # gather information for data_draw for each of the datasets/outcome variables
# for (j in 1:n_datasets){ 
#   for (i in 1:n_clf) {
#     #fill data_draw with values for perf class and maj class
#     data_draw[i + ((j-1)*n_clf*2), 1:4] <- c(toy_data[toy_data$dataset==datasets[j],]$maj_acc[1], 
#                                              toy_data[toy_data$dataset==datasets[j],]$maj_spd[1], 
#                                              toy_data[toy_data$dataset==datasets[j],]$perf_acc[1],
#                                              toy_data[toy_data$dataset==datasets[j],]$perf_spd[1])
#     
#     # obs = rows belonging to this classifier and dataset
#     obs <- toy_data[((j-1)*n_clf*n_imp + 1 + (i-1)*n_imp):((j-1)*n_clf*n_imp + n_imp + (i-1)*n_imp), ]
#     
#     # fill in data_draw with min and max spd's to which extra lines from perf and maj clf should be drawn
#     data_draw[i + n_clf + ((j-1)*n_clf*2), 1:4] <- c(obs$acc[which(obs$acc==min(obs$acc))],
#                                                      obs$spd[which(obs$acc==min(obs$acc))],
#                                                      obs$acc[which(obs$acc==max(obs$acc))],
#                                                      obs$spd[which(obs$acc==max(obs$acc))])
#   }
# }
# 
# # create extended version of data to combine datasets
# toy_data_extended <- toy_data
# for (var in colnames(data_draw)[1:5] ){
#   toy_data_extended[var] <- NA
# }
# 
# # create extended version of data_draw to combine datasets
# data_draw_extended <- data_draw
# vars <- c("acc", "spd", "imp_method", "perf_acc", "perf_spd", "maj_acc", "maj_spd")
# for (var in vars){
#   data_draw_extended[var] <- NA
# }
# 
# # combine the two datasets to 1, because ggplot facet_wrap can best handle only 1 dataset
# toy_data_extended <- rbind(toy_data_extended, data_draw_extended)
# toy_data_extended$datatype <- NA
# toy_data_extended$datatype[1:nrow(toy_data)] <- "regular"
# toy_data_extended$datatype[nrow(toy_data):(nrow(toy_data)+nrow(data_draw))] <- "extra"

#### start the visualization from here on

# define some colors that could be used in options
base_colors = c("#f98a92", "#7dc37e", "#78cdd1", "#96b1dc", "#d89ac5", "#c8b669")
colors = rep(base_colors[1:n_clf], n_clf)

# start creating the ggplot on extended dataset
plot <- ggplot(toy_data)+
  # first just some general layout, red areas, y=0 line etc.
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = Fymin, ymax = Fymax,   fill = "grey", alpha = 0.05, colour = "white")+
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.04, colour = "white")+
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = -Inf, ymax = Fymin,   fill = "lightcoral", alpha = 0.04, colour = "white") +
  geom_hline(yintercept=Fideal, linetype="dashed", color = "gray", size = 1.5) +
  annotate("text", x = -Inf + 0.1, y = Fideal, label = "Fair", angle = 90, hjust = 1.2, vjust = 1.4, colour = "darkgrey")+
  annotate("text", x = -Inf + 0.1, y = Fymin, label = "Bias", angle = 90, hjust = 1.2, vjust = 1.4, colour = "red")+ 
  annotate("text", x = -Inf + 0.1, y = Fymin2, label = "Bias", angle = 90, hjust = 0, vjust = 1.4, colour = "red") +
  
  # plot the perfect classifier
  geom_point(data = toy_data, aes(perf_acc, perf_spd), size = 8, shape = 11)+
  geom_point(aes(perf_acc, perf_spd), size = 2, colour = "white") +
  geom_point(aes(perf_acc, perf_spd), size = 1, colour = "black") +
  # geom_text_repel(aes(perf_acc , perf_spd, label = "Perfect classifier"), force = 10, size = 4, fontface = 'italic',
  #                 box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50')+

  # plot the majority classifier
  geom_point(data = toy_data, aes(maj_acc, maj_spd), size = 8, shape = 13)+
  geom_point(aes(maj_acc, maj_spd), size = 2, colour = "white") +
  geom_point(aes(maj_acc, maj_spd), size = 1, colour = "black") +
  # geom_text_repel(aes(maj_acc , maj_spd, label = "Majority classifier"), force = 10, size = 4, fontface = 'italic',
  #                 box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50')+
  
  # plot the accuracies and spd's for each imputation method and classifier
  geom_point(aes(acc, spd, colour = imp_method, shape = imp_method), size = 10, alpha = 0.6)+
  geom_point(aes(acc, spd), size = 2, colour = "white") +
  geom_point(aes(acc, spd), size = 1, colour = "Black") +
  # geom_line(aes(acc, spd, colour = classifier), size = 1.1)+ # the line connecting a classifiers results
  
  # the following plots the lines to the perf clf and from the maj clf
  # the if statements are reversed because otherwise ggplot cannot handle it
  # {if (n_clf > 1)geom_line(data = toy_data_extended[toy_data_extended$datatype=="extra",], 
  #                          aes(X_l, Y_l, group = grp, colour = classifier), size = 1.1, linetype = "dashed")} +
  # {if (n_clf > 1)geom_line(data = toy_data_extended[toy_data_extended$datatype=="extra",], 
  #                          aes(X_f, Y_f, group = grp, colour = classifier), size = 1.1)} +
  #     
  # 
  # {if (n_clf == 1)geom_line(data = toy_data_extended[toy_data_extended$datatype=="extra",], 
  #                        aes(X_l, Y_l, colour = classifier), size = 1.1, linetype = "dashed")} +
  # {if (n_clf == 1)geom_line(data = toy_data_extended[toy_data_extended$datatype=="extra",], 
  #                        aes(X_f, Y_f, colour = classifier), size = 1.1)} +

  facet_wrap(~dataset, ncol = 2)+
  
  # some settings for the plot
  labs(x="Accuracy", y = "SPD") +
  scale_shape_manual(values=c(15, 16, 17, 18, 20))+
  # scale_color_manual(values=colors)
  theme_minimal() + 
  theme(legend.title = element_blank())  

plot
