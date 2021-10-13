#-----------------------------------------------------------------------------
#
# This is R code for the following paper:
#
#  - 
#
# This code implements: 
#
# This code has been developed by
#   FERNANDO MARTINEZ-PLUMED, UNIVERSITAT POLITECNICA DE VALENCIA, SPAIN
#   fmartinez@dsic.upv.es
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    
#
# FUTURE FEATURES:
#
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
#---------------------------- CODE ORGANISATION ------------------------------
#-----------------------------------------------------------------------------



#-----------------------------------------------------------------------------
#----------------------- LIBRARIES AND I/O OPTIONS ---------------------------
#-----------------------------------------------------------------------------
#devtools::install_github(repo = "stefvanbuuren/mice")

.lib<- c("DescTools","digest", "plyr", "dplyr","mice","missForest","Hmisc", "ggplot2", "reshape2", "farff")
.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)

options("scipen"=1000000)
options( java.parameters = "-Xmx8g" )

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}
 PDFEPS <- 1 # 0 None, 1 PDF, 2 EPS
 PDFheight= 6 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
 PDFwidth= 9 # 7 by default

set.seed(288)




# Define Simple Imputation 
#-----------------------------------------------------------------------------

simpleImpute <- function(data, cols, func, name, file = "OK_datasets/"){
  dataImp <- data
  func2 <- ifelse(func=="random", func, get(func))
  path = paste0(file, name, "_", func, collapse = "")
  
  for(i in cols){
    print(i)
    if(class(dataImp[,colnames(dataImp)%in%i]) == "factor"){
      dataImp[,colnames(dataImp)%in%i] <- as.character(dataImp[,colnames(dataImp)%in%i])
      dataImp[,colnames(dataImp)%in%i] <-  impute(dataImp[,i], func2) 
      dataImp[,colnames(dataImp)%in%i] <- as.factor(dataImp[,colnames(dataImp)%in%i])
    }
    else{
      if (class(dataImp[,colnames(dataImp)%in%i]) == "numeric" | class(dataImp[,colnames(dataImp)%in%i]) == "integer"){
        func3 <- ifelse(func =="mode", get("mean"), func2)
        dataImp[,colnames(dataImp)%in%i] <-  impute(dataImp[,i], func3) 
      }else{
        dataImp[,colnames(dataImp)%in%i] <-  impute(dataImp[,i], func2) 
      }
      
    }
  }
  
  tmp <- rbind(data.frame(name="orig", att = cols[1], values = as.character(data[,colnames(data)%in%cols[1]])),
               data.frame(name="imputed", att = cols[1], values = as.character(dataImp[,colnames(dataImp)%in%cols[1]])))
  
  if (length(cols)>=2){
    for(i in 2:length(cols)){
      tmp <- rbind(tmp,
                   data.frame(name="orig", att = cols[i], values = as.character(data[,colnames(data)%in%cols[i]])),
                   data.frame(name="imputed", att = cols[i], values = as.character(dataImp[,colnames(dataImp)%in%cols[i]])))
    }
  }
  
  g <- ggplot(tmp, aes(values, fill = name)) + geom_bar(alpha = 0.5,position = "dodge") + facet_grid(. ~ att, scales = "free") +
    theme_light() + theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
    ggtitle(paste0(name,": ",func," Imputation", collase = ""))
  
  
  saveRDS(dataImp, file = paste0(path,".rds", collapse = ""))
  write.csv(dataImp, file = paste0(path,".csv", collapse = ""), row.names = FALSE)
  writeARFF(dataImp, path = paste0(path,".arff", collapse = ""))
  
  openPDFEPS(paste0(path,"_plot",collapse = ""), height= 8, width= 12)
  #print(g)
  # dev.off()
}

getSimpleImp <- function(data, cols, func, name, file){
  
  # Impute with Random values
  print("Random Imputation")
  set.seed(288)
  func = "random"
  simpleImpute(data, cols, func, name, file)
  
  # Impute with median
  print("Median Imputation")
  set.seed(288)
  func = "mode"
  simpleImpute(data, cols, func, name, file)
  
  # Impute with min
  print("Min Imputation")
  set.seed(288)
  func = "min"
  simpleImpute(data, cols, func, name, file)
  
  # Delete Columns
  print("Columns Imputation")
  path = paste0(file, name, "_columns", collapse = "")
  dataImp <- data[,colSums(is.na(data)) == 0] 
  #data[,colSums(is.na(data)) == 0] 
  saveRDS(dataImp, file = paste0(path,".rds", collapse = ""))
  write.csv(dataImp, file = paste0(path,".csv", collapse = ""), row.names = FALSE)
  
  # Original Data split by WithNA and WithoutNA
  ok <- complete.cases(data)
  
  print("NAs")
  dataNA <- data[!ok,]
  dataNA <- dataNA[,colSums(is.na(dataNA)) == 0] 
  path = paste0(file, name, "_orig_NAs", collapse = "")
  saveRDS(dataNA, file = paste0(path,".rds", collapse = ""))
  write.csv(dataNA, file = paste0(path,".csv", collapse = ""), row.names = FALSE)
  
  print("No NAs")
  dataNoNA <- data[ok,]
  path = paste0(file, name, "_orig_clean", collapse = "")
  saveRDS(dataNoNA, file = paste0(path,".rds", collapse = ""))
  write.csv(dataNoNA, file = paste0(path,".csv", collapse = ""), row.names = FALSE)
  
}



#-----------------------------------------------------------------------------
#---------------------------- LOAD DATASETS ----------------------------------
#-----------------------------------------------------------------------------


# First set your Working directory correctly!
# Easiest way to do this in RStudio: Click on Session/Set Working Directory/To Source file location

files <-list.files("datasets/orig", pattern = ".arff")
i = 1 
for(i in 1:length(files)){
  
  file = paste0("datasets/orig/", files[i])
  data = readARFF(file)
  print("mode/mean Imputation")
  set.seed(288)
  cols = colnames(data)[colSums(is.na(data)) > 0]
  name = strsplit(files[i], "\\.")[[1]][1]
  print(name)
  simpleImpute(data, cols, "mode", name, "datasets/")

}






# no clue as to what dataset these are exactly, probably the original datasets which can be found in /datasets, 
# but then under a different directory structure I guess?

# Adults
data <- readRDS("OK_datasets/Adult/Adult_orig.rds")
cols = colnames(data)[colSums(is.na(data)) > 0]
name= "Adult"
file = "OK_datasets/Adult/"
noCols = ""
# class <- data[,ncol(data)]
# data <- data[,-ncol(data)]
getSimpleImp(data, cols, func, name, file)

# Titanic Kaggle
data <- readRDS("OK_datasets/Titanic/TitanicKaggle.rds")
noCols <- c("name","ticket")
cols = colnames(data)[colSums(is.na(data)) > 0]
name= "TitanicKaggle"
file = "OK_datasets/Titanic/"
# class <- data[,2]
# data <- data[,-2]
getSimpleImp(data, cols, func, name, file)

# Irish
data <- readRDS("OK_datasets/Irish/irish.rds")
noCols <- ""
cols = colnames(data)[colSums(is.na(data)) > 0]
name= "Irish"
file = "OK_datasets/Irish/"
# class <- data[,4]
# data <- data[,-4]
getSimpleImp(data, cols, func, name, file)

# 
# #Recidivism 
data <- readRDS("OK_datasets/Recidivism/Recidivism_orig.rds")
noCols <- ""
cols = colnames(data)[colSums(is.na(data)) > 0]
name= "Recidivsm"
file = "OK_datasets/Recidivism/"
# class <- data[,ncol(data)]
# data <- data[,-ncol(data)]
getSimpleImp(data, cols, func, name, file)

#ViolentRecidivism
data <- readRDS("OK_datasets/ViolentRecidivism/ViolentRecidivism_orig.rds")
noCols <- ""
cols = colnames(data)[colSums(is.na(data)) > 0]
name= "ViolentRecidivsm"
file = "OK_datasets/ViolentRecidivism/"
# class <- data[,ncol(data)]
# data <- data[,-ncol(data)]
getSimpleImp(data, cols, func, name, file)


#SQF 
data <- readRDS("OK_datasets/SQF/sqf-2017_orig.rds")
noCols <- ""
cols = colnames(data)[colSums(is.na(data)) > 0]
name= "sqf-2017"
file = "OK_datasets/SQF/"
# class <- data[,ncol(data)]   
# data <- data[,-ncol(data)]   
getSimpleImp(data, cols, func, name, file)



#ViolentRecidivism <- readRDS("OK_datasets/ViolentRecidivism.rds")


#-----------------------------------------------------------------------------
#-------------------------- MISSING IMPUTATION -------------------------------
#-----------------------------------------------------------------------------

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



# Model-based Imputation 
#-----------------------------------------------------------------------------

MICEimpute <- function(data, cols, func, name, file = "", noCols = "", class){
  
  dataImp <- data
  set.seed(288)
  
  init = mice(data, maxit = 0)
  meth = init$method
  predM = init$predictorMatrix
  
  if (noCols[1] != ""){predM[,noCols] = 0}
  
  
  if(func != ""){
    path = paste0(file, name, "_MICE_", func, collapse = "")
    meth[cols] <- func
    print(paste0("MICE Imputation (",func,") for ",name,collapse=""))
    imputed = mice(data, method = meth, predictorMatrix = predM, m = 1, nnet.MaxNWts = 3000,  ridge=0.001)
  }else{
    path = paste0(file, name, "_MICE_Default", collapse = "")
    print(paste0("MICE Imputation (Default) for ",name,collapse=""))
    imputed = mice(data, method = meth, m = 1, predictorMatrix = predM, nnet.MaxNWts = 3000,  ridge=0.001)
  }
  
  imputed2 <- mice::complete(imputed) #first out of five dataframes created 
  
  saveRDS(cbind(imputed2,class), file = paste0(path,".rds", collapse = ""))
  write.csv(cbind(imputed2,class), file = paste0(path,".csv", collapse = ""), row.names = FALSE)
  
  # openPDFEPS(paste0(path,"_plot",collapse = ""), height= 8, width= 12)
  # for (i in cols){
  #   if(sum(is.na(dataImp[,i]))>10){
  #     form = as.formula(paste0(" ~ ",i,collapse = ""))
  #     plot <- densityplot(imputed, form)
  #     print(plot)
  #   }
  # }
  # dev.off()
  
  
  
}

memory.size(max = F)
memory.limit(size=NA)

set.seed(288)
print("MICE Default Imputation")
MICEimpute(data, cols, "", name, file, noCols, class)
set.seed(288)
print("Mice PMM Imputation")
MICEimpute(data, cols, "pmm", name, file, noCols, class)
set.seed(288)
print("MICE Sample Imputation")
MICEimpute(data, cols, "sample", name, file, noCols, class)
set.seed(288)
print("MICE Cart Imputation")
MICEimpute(data, cols, "cart", name, file, noCols, class)
set.seed(288)
# print("MICE MidasTouch Imputation")
# MICEimpute(data, cols, "midastouch", name, file, noCols, class)

# missForest
set.seed(288)
print("MISSFOREST Imputation")
path = paste0(file, name, "_missForest", collapse = "")
imputed2 <- missForest(data[,!(colnames(data) %in% noCols)], verbose = TRUE)
saveRDS(cbind(imputed2$ximp,class), file = paste0(path,".rds", collapse = ""))
write.csv(cbind(imputed2$ximp,class), file = paste0(path,".csv", collapse = ""), row.names = FALSE)


# New MissForest:
# doesn't work yet for the recidivism dataset as missforest does not work for factors with more than 53 levels, and recidivism has the factor c_charge_desc with 438 levels. thus we should aggregate those if we want to use missforest




set.seed(288)
print("MissForest Imputation")


files <-list.files("datasets/orig")

file = paste0("datasets/orig/", files[3])
result = readARFF(file)


# Write new recidivism datasets after having dropped missing values in c_charge_desc
j = 3
for(j in 3:4){
  recidivism = readARFF(paste0("datasets/orig/", files[j]))
  correct_recidivism = recidivism[!is.na(recidivism$c_charge_desc),]
  writeARFF(correct_recidivism, path = paste0("datasets/no-c_charge/", files[j]))
}


files <-list.files("datasets/no-c_charge")



i = 1 
for(i in 3:length(files)){
  # Read dataset location
  file = paste0("datasets/no-c_charge/", files[i])
  # Load dataset
  data = readARFF(file)
  # Remove c_charge_desc for recidivism
  if(i>=3 && i<=4){
    # Split off c_charge_desc from the dataset
    c_charge = subset(data, select = c(c_charge_desc))
    no_c_charge = subset(data, select = -c(c_charge_desc) )
    #perform imputation
    imputed_result = missForest(no_c_charge)$ximp
    
    
    # Add back c_charge_desc to the imputed dataset
    imputed_result$c_charge_desc = c_charge[,1]
  } else {
    # perform imputation
    imputed_result = missForest(data)$ximp
  }
  
  # Save imputation to disk
  names = strsplit(file, "/")
  name = lapply(names, tail, n=1L)
  result_path = paste0("datasets/missforest/", name)
  writeARFF(imputed_result, path = result_path)
}



#-----------------------------------------------------------------------------
#--------------------------- RESULTS & PLOTS ---------------------------------
#-----------------------------------------------------------------------------

# results2DF_old <- function(path){
#   
#   files <- list.files(path, pattern = "*.json")
#   
#   metricsSelected <- c("Mean Difference", "Statistical Parity Difference", "Disparate Impact")
#   colsSelected <- c("metric", "value", "description", "ideal", "message")
#   
#   listData <- list()
#   
#   for (i in 1:length(files)){
#     print(files[i])
#     file = paste0(path, files[i], collapse = "")
#     json_data <- fromJSON(file = file)
#     
#     json_df <- do.call("rbind.fill", lapply(json_data$protected_attributes[[1]]$results, as.data.frame))
#     json_df <- json_df[json_df$metric %in% metricsSelected,colsSelected]
#     json_df$attProtected <- json_data$protected_attributes[[1]]$name
#     json_df$ideal_value <- c(0.0,0.0,1.0)
#     
#     if(length(json_data$protected_attributes) > 1){
#       for (protAtt in 2:length(json_data$protected_attributes)){
#         
#         temp <- do.call("rbind.fill", lapply(json_data$protected_attributes[[protAtt]]$results, as.data.frame))
#         temp<- temp[temp$metric %in% metricsSelected,colsSelected]
#         temp$attProtected <- json_data$protected_attributes[[protAtt]]$name
#         temp$ideal_value <- c(0.0,0.0,1.0)
#         json_df <- rbind(json_df,temp)
#       }
#     }
#     
#     split <- strsplit(files[i],"[_]")
#     json_df$dataset <- split[[1]][1]
#     json_df$imputation <- paste(split[[1]][2], split[[1]][3], collapse = "_")
#     listData[[i]] <- json_df
#   }
#   
#   
#   all <- do.call("rbind", listData)
#   all<- select(all, dataset,  attProtected, metric, imputation, value, ideal_value, message, description, ideal)
#   all$imputation <- as.factor(all$imputation)
#   all$imputation <- factor(all$imputation,
#                            levels = c("orig dataset", "orig clean", "orig NAs", "columns dataset", "random dataset", "MICE sample", "min dataset",
#                                       "mode dataset", "MICE pmm", "MICE cart", "missForest dataset", "MICE Default"),
#                            labels = c("Orig", "Orig woNAs", "Orig NAs", "Cols", "Random", "Sample", "Min", 
#                                       "Mean/Mode",  "PMM", "CART", "RF", "Best"))
#   
#   all$metric <- factor(all$metric,
#                        levels = c("Mean Difference", "Statistical Parity Difference", "Disparate Impact", "Number Of Negatives", "Number Of Positives"),
#                        labels = c("MD", "SPD", "DI", "Num Neg", "Num Pos")) 
#   
#   saveRDS(all, paste0(path,"DF_Fairness_metrics.rds"))
#   
#   return(all)
#   
# }
# 
# results2DF <- function(path, atts){
#   
#   files <- list.files(path, pattern = "*.json")
#   
# 
#   
#   listData <- list()
# 
#   for (i in 1:length(files)){
# 
#     print(files[i])
#     file = paste0(path, files[i], collapse = "")
#     json_data <- fromJSON(file = file)
#     
#     # PREPROCESSING RESULTS -------------------------------------
#     
#     json_df <- do.call("rbind.fill", lapply(json_data$preprocessing_results[[atts[1]]], as.data.frame))
#     json_df$metric <- c("Mean Difference", "Statistical Parity Difference", "Num Negatives","Disparate Impact","Num Positives")
#     z <- melt(json_df, id.vars = c("metric"))
#     z <- z[complete.cases(z),]
#     z$attProtected <- atts[1]
#     z$phase <- "preprocessing"
#     z$technique <- "none"
#     z$partition <- "all"
#     colnames(z)[2] <- "metric2"
#     
#     
#     if(length(atts) > 1){
#       for (protAtt in 2:length(atts)){
#         
#         temp <- do.call("rbind.fill", lapply(json_data$preprocessing_results[[atts[1]]], as.data.frame))
#         temp$metric <- c("Mean Difference", "Statistical Parity Difference", "Num Negatives","Disparate Impact","Num Positives")
#         z2 <- melt(temp, id.vars = c("metric"))
#         z2 <- z2[complete.cases(z2),]
#         z2$attProtected <- atts[protAtt]
#         z2$phase <- "preprocessing"
#         colnames(z2)[2] <- "metric2"
#         z2$technique <- "none"
#         z2$partition <- "all"
#         z <- rbind(z,z2)
#         
#       }
#     }
#     
#     split <- strsplit(files[i],"[_]")
#     z$dataset <- split[[1]][1]
#     z$imputation <- paste(split[[1]][2], split[[1]][3], collapse = "_")
#     # listData[[i]] <- json_df
#     
#     
#     # INPROCESSING RESULTS --------------------------------------------------
#     
#     json_df2 <- do.call("rbind.fill", lapply(json_data$inprocessing_results[[atts[1]]], as.data.frame))
#     json_df2$technique <- c("DC_stratified","DC_uniform", "RF","LR","NN")
#     
#     t <- melt(json_df2, id.vars = c("technique"))
# 
#     mysplit <- function(x){strsplit(as.character(x),".", fixed = T)[[1]][1]}
#     mysplit2 <- function(x){strsplit(as.character(x),".", fixed = T)[[1]][2]}
#     mysplit3 <- function(x){strsplit(as.character(x),".", fixed = T)[[1]][3]}
#     
#     t$partition <-  sapply(t$variable, mysplit)
#     t$metric <-  sapply(t$variable, mysplit2)
#     t$metric2 <-  sapply(t$variable, mysplit3)
#     t$attProtected <- atts[1]
#     t <- select(t, technique, partition, attProtected, metric, metric2, value)
#     
#     if(length(atts) > 1){
#       for (protAtt in 2:length(atts)){
# 
#         
#         temp <- do.call("rbind.fill", lapply(json_data$inprocessing_results[[atts[protAtt]]], as.data.frame))
#         temp$technique <- c("DC_stratified","DC_uniform", "RF","LR","NN")
#         
#         t2 <- melt(temp, id.vars = c("technique"))
#         t2$partition <-  sapply(t2$variable, mysplit)
#         t2$metric <-  sapply(t2$variable, mysplit2)
#         t2$metric2 <-  sapply(t2$variable, mysplit3)
#         t2$attProtected <- atts[protAtt]
#         t2 <- select(t2, technique, partition, attProtected, metric, metric2, value)
#         t <- rbind(t,t2)
#         
#       }
#     
#       }
#     
#     t$phase <- "inprocessing"
#     t$dataset <- split[[1]][1]
#     t$imputation <- paste(split[[1]][2], split[[1]][3], collapse = "_")
#    
#     z <- select(z, dataset, attProtected, imputation, phase, technique, partition, metric, metric2, value)
#     t <- select(t, dataset, attProtected, imputation, phase, technique, partition, metric, metric2, value)
#     
#     all <- rbind(z,t)
#     listData[[i]] <- all
#     
# 
#   }
#   
#   
#   allData <- do.call("rbind", listData)
#   
#   allData$imputation <- as.factor(allData$imputation)
#   allData$imputation <- factor(allData$imputation,
#          levels = c("columns results.json", "min results.json", "mode results.json", "random results.json", 
#                     "MICE sample", "MICE pmm", "MICE cart", "missForest results.json", "MICE Default"),
#          labels = c("Cols", "Min", "Mean/Mode", "Random", 
#                     "Sample", "PMM", "CART", "RF", "Best"))
#  
#   
#   allData$metric <- as.factor(allData$metric)
#   allData$metric <- factor(allData$metric,
#                        levels = c("Mean Difference", "Statistical Parity Difference", "statistical_parity_difference", "Disparate Impact", 
#                                   "equal_opportunity_difference", "theil_index","best_balanced_acc", "avg_odds_diff_at_best_bal_acc",
#                                   "accuracy", "abs",
#                                   "Num Negatives", "Num Positives"),
#                        labels = c("MD", "SPD", "SPD", "DI", 
#                                   "EOD", "TI", "balACC", "OddsDiff",
#                                   "ACC", "ABS",
#                                   "Num Neg", "Num Pos")) 
# 
#   saveRDS(allData, paste0(path,"Preprocessing_Fairness_metrics.rds"))
#   return(allData)
#   
# }
# 
# plotMetrics <- function(df, path, h = 10, w = 14){
#   plot <- ggplot(df, aes(metric, value, fill = imputation)) + geom_bar(stat="identity",  position = "dodge", colour = "black") + 
#     # geom_point(aes(metric,ideal_value), colour = "red", size = 12, shape ="_") + 
#     geom_text(aes(metric, value, label = sprintf("%2.6f", value)), size = 2.5, position = position_dodge(width = 1)) +
#     facet_grid(. ~ attProtected ) + coord_flip() + theme_light() + 
#     theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
#     theme(strip.text.x = element_text(size = 8)) +  theme(legend.position = c(0.9, 0.3)) + ggtitle(df$dataset)
#   
#   openPDFEPS(paste0(path,"DF_Fairness_metrics_plot"), height= h, width= w)
#   print(plot)
#   dev.off()
#   
# }
# 
# latexMetrics <- function(df, att = ""){
# 
#   table <- select(df, dataset, attProtected, metric, imputation,  value, ideal_value)
#   table <- table[order(table$attProtected,table$metric, table$imputation),]
#   colnames(table) <- c("Dataset", "Att", "Metric","NA Imp.", "Value","Ideal")
#   table <- table[,-1]
#   levels(table$`NA Imputation`)
#   if(att == ""){
#     print(xtable(table, digits=c(0,0,0,0,4,2)), include.rownames=FALSE)
#   }else{
#     table <- filter(table, Att == att)
#     print(xtable(table, digits=c(0,0,0,0,4,2)), include.rownames=FALSE)
#   }
# 
# 
# }


# path <- "results/SEED_1/Fold_1/Adult/"
# adultData <- results2DF(path, atts = c("sex", "race"))
# 
# path <- "results/SEED_1/Fold_1/Titanic/"
# TitanicData <- results2DF(path, atts = c("pclass", "sex"))
# 
# path <- "results/SEED_1/Fold_1/Irish_1/"
# Irish1Data <- results2DF(path, atts = c("Sex"))
# 
# path <- "results/SEED_1/Fold_1/Irish_2/"
# Irish2Data <- results2DF(path, atts = c("Sex"))
# 
# 
# fm <- adultData
# 
# 
# fm$value <- as.numeric(fm$value)
# fm <- fm[!is.na(fm$value),]
# fm.pre <- filter(fm, phase == "preprocessing", metric %in% c("MD", "SPD", "DI"), metric2 == "value")
# fm.in <-  filter(fm, phase == "inprocessing", metric2 == "value")




# path <- "results/Adult/"
# all <- results2DF(path)
# plotMetrics(all, path, 8, 12)
# latexMetrics(all)
# latexMetrics(all, "sex")
# latexMetrics(all, "race")
# 
# 
# path <- "results/Titanic/"
# all <- results2DF(path)
# plotMetrics(all, path, 8, 12)
# latexMetrics(all)
# latexMetrics(all, "pclass")
# latexMetrics(all, "sex")
# 
# 
# 
# path <- "results/Irish_1/"
# all <- results2DF(path)
# all$dataset <- "Irish_female"
# plotMetrics(all, path, 8, 6)
# latexMetrics(all)
# 
# 
# path <- "results/Irish_2/"
# all <- results2DF(path)
# all$dataset <- "Irish_male"
# plotMetrics(all, path, 8, 6)
# latexMetrics(all)























# 
# 
# 
# library(ggplot2)
# gdURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderCountryColors.txt"
# countryColors <- read.delim(file = gdURL, as.is = 3) # protect color
# str(countryColors)
# jColors <- countryColors$colorº
# names(jColors) <- countryColors$color
# head(jColors)
# 
# 
# df$name <- gsub("_dataset_metrics_output","",df$name)
# 
# g <- ggplot(df, aes(metric, value)) + geom_bar(stat="identity", alpha = 0.5) + geom_point(aes(metric,ideal), colour = "red", size = 12, shape ="_") + 
#   geom_text(data=df,aes(x=metric,y=value,label=round(value,6)),vjust=0, size = 3.5) + 
#   facet_grid(attribute ~ name)  + theme_light() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
#   theme(strip.text.x = element_text(size = 8))
# 
# g2 <- ggplot(df, aes(metric, value, fill = name)) + geom_bar(stat="identity",  position = "dodge") +
#   facet_grid(. ~ attribute) + geom_text(aes(metric, value, label = sprintf("%2.6f", round(value,6))), size = 3.5, position = position_dodge(width = 1)) + 
#   coord_flip() + theme_light() + 
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
#   theme(strip.text.x = element_text(size = 8)) +  theme(legend.position = c(0.9, 0.1)) 
#   
# 
# openPDFEPS("df_FairMetrics_plot2", height= 10, width= 14)
# g2
# dev.off()
# 
# 
# openPDFEPS("df_FairMetrics_plot", height= 10, width= 20)
# g
# dev.off()
