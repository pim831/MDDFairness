require(ggplot2)
require(ggrepel)
require(dplyr) 
library(xlsx)

rm(list = ls()) # clear environment variables

# read in some toy data that was created by @Leightonvg
toy_data <- read.xlsx("refactored_simulation/datasets/toy/toy_plot.xlsx", 
                        sheetIndex = 1, stringsAsFactors=FALSE)
toy_data[]=lapply(toy_data,type.convert,as.is=TRUE)

# some settings for the general layout of the plot
Fideal = 0; Fymin = -0.1; Fymin2 = 0.1;Fymax = 0.1 #SPD


#TODO INIT OF DATA DRAW MAKE GENERIC
# create new data frames to draw lines to perfect classifier and from maj class.
n_clf <- length(unique(toy_data$classifier)) # number of different classifiers
data_draw_last_lines = data.frame(X = rep(toy_data$perf_acc[1], times = n_clf),
                                  Y = rep(toy_data$perf_spd[1], times = n_clf), toy_data$perf_spd[1]) # add n times perf_class
data_draw_first_lines = data.frame(X = rep(toy_data$maj_acc[1], times = n_clf), 
                                   Y = rep(toy_data$maj_spd[1], times = n_clf)) # add n times maj_class

for (i in 1:n_clf){ # this loop gathers the observations for the last classifier
  obs <- unlist(c(toy_data[,1:n_clf]))[(1+ (i-1)*(nrow(toy_data)/n_clf)):(i*(nrow(toy_data)/n_clf))] # all observations for 1 classifier
  print(obs)
  index_max <- which(obs==max(obs)) # select the index for the max acc for that classifier
  index_min <- which(obs==min(obs)) # select the index for the min acc for that classifier
  print(index_min)
  obs_max_acc <- toy_data[(index_max + (i-1)*(nrow(toy_data)/n_clf)), 1:2] # select the obs corresponding to the max acc
  obs_min_acc <- toy_data[(index_min + (i-1)*(nrow(toy_data)/n_clf)), 1:2] # select the obs corresponding to the min acc
  data_draw_last_lines <- rbind(data_draw_last_lines, unlist(obs_max_acc)) # add to the df for drawing
  data_draw_first_lines <- rbind(data_draw_first_lines, unlist(obs_min_acc)) # add to the df for drawing
}
data_draw_last_lines$grp <- as.factor(rep(1:n_clf, times =2)) #add grouping variable to connect points in drawing
data_draw_first_lines$grp <- as.factor(rep(1:n_clf, times =2)) #add grouping variable to connect points in drawing

base_colors = c("#f98a92", "#7dc37e", "#78cdd1", "#96b1dc", "#d89ac5", "#c8b669")
colors = rep(base_colors[1:n_clf], n_clf)

plot <- ggplot(toy_data)+
  # first just some general layout
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = Fymin, ymax = Fymax,   fill = "grey", alpha = 0.05, colour = "white")+
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.04, colour = "white")+
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = -Inf, ymax = Fymin,   fill = "lightcoral", alpha = 0.04, colour = "white") +
  geom_hline(yintercept=Fideal, linetype="dashed", color = "gray", size = 1.5) +
  annotate("text", x = -Inf + 0.1, y = Fideal, label = "Fair", angle = 90, hjust = 1.2, vjust = 1.4, colour = "darkgrey")+
  annotate("text", x = -Inf + 0.1, y = Fymin, label = "Bias", angle = 90, hjust = 1.2, vjust = 1.4, colour = "red")+ 
  annotate("text", x = -Inf + 0.1, y = Fymin2, label = "Bias", angle = 90, hjust = 0, vjust = 1.4, colour = "red") +
  
  # plot the perfect classifier
  geom_point(data = toy_data, aes(perf_acc, perf_spd), size = 10, shape = 11)+
  geom_point(aes(perf_acc, perf_spd), size = 2, colour = "white") +
  # geom_text_repel(aes(perf_acc , perf_spd, label = "Perfect classifier"), force = 10, size = 4, fontface = 'italic',
  #                 box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50')+

  # plot the majority classifier
  geom_point(data = toy_data, aes(maj_acc, maj_spd), size = 10, shape = 13)+
  geom_point(aes(maj_acc, maj_spd), size = 2, colour = "white") +
  # geom_text_repel(aes(maj_acc , maj_spd, label = "Majority classifier"), force = 10, size = 4, fontface = 'italic',
  #                 box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50')+
  
  # plot the accuracies and spd's for each imputation method and classifier
  geom_point(aes(acc, spd, colour = classifier, shape = imp_method), size = 10, alpha = 0.6)+
  geom_point(aes(acc, spd), size = 2, colour = "white") +
  geom_point(aes(acc, spd), size = 1, colour = "Black") +
  geom_line(aes(acc, spd, colour = classifier), size = 1.1)+ # the line connecting a classifiers results
  
  # connect last classifier result to perfect classifier
  geom_line(data = data_draw_last_lines, aes(X, Y, group = grp, colour = grp), size = 1.1, linetype = "dashed") +
  
  # connect first classifier result to majority classifier
  geom_line(data = data_draw_first_lines, aes(X, Y, group = grp, colour = grp), size = 1.1) +
  
  # facet_wrap(~dataset, ncol = 2)+
  
  
  # some settings for the plot
  labs(x="Accuracy", y = "SPD") +
  scale_shape_manual(values=c(15, 16, 17, 18, 20))+
  scale_color_manual(values=colors)
  theme_minimal() + 
  theme(legend.title = element_blank())  

plot
