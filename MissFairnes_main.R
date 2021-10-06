# -------------------------------------------------------------------------------------------------
# Packages
# -------------------------------------------------------------------------------------------------

options("scipen"=1000000)
options( java.parameters = "-Xmx6g" )

.lib<- c("caret", "plyr", "dplyr", "farff", "mice", "naivebayes", "rpart", "foreign", "C50", "ggplot2", "reshape2", "xlsx", "reshape2", 
         "caret", "rpart.plot", "xtable", "ggrepel", "e1071", "C50", "stats", "nnet", "randomForest")
.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)


source("runmethods.R")
source("libfair.R")

# set.seed(288)



# -------------------------------------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------------------------------------


openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}


#Converting csv to arrf for Weka

toARFF <- function(file){
  
  data=read.csv(file,header=TRUE)
  name = tools::file_path_sans_ext(file)
  write.arff(x=data ,file= paste0(name,".arff"))
  
}

# file = "datasets/Recidivism.csv"
# file = "datasets/Adult.csv"
# file = "datasets/TitanicKaggle.csv"
# toARFF(file)


# Percentage of missing data per attribute
pMiss <- function(x){sum(is.na(x))/length(x)*100}


# Plit missing Values
plotMissing <- function(data, title){
  title <- paste("Proportion of missingness", title)
  plot <- aggr(data, col=c('navyblue','orange'),
               numbers=TRUE, sortVars=TRUE,
               labels=names(data), cex.axis=.7,
               gap=3, ylab=c(title,"Pattern"))
  return(plot)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

removeBlankLevelsInDataFrame <- function(dataframe) {
  for (i in 1:ncol(dataframe)) {
    levels <- levels(dataframe[, i])
    if (!is.null(levels) && levels[1] == "") {
      levels(dataframe[,i])[1] = "?"
    }
  }
  dataframe
}

removeBlankLevelsInVector <- function(vector) {
  levels <- levels(vector)
  if (!is.null(levels) && levels[1] == "") {
    levels(vector)[1] = "?"
  }
  vector
}

# ML models 
runmodel<-function(met, train, id, nameParam = "class")
{
  model = NA
  ob<-as.formula(paste(nameParam," ~ .",sep=""))
  
  if (met=="rpart"){
    model<-  rpart(ob, data = train, method = 'class', control =rpart.control(cp = 0.0001))
  }
  
  if (met == "Maj"){
    model <- NA
  }
  
  #e1071
  if (met=="naive_bayes"){
    model <- naiveBayes(ob ,  data = train, na.action = na.omit)
  }
  
  #C50
  if (met=="C5.0Tree"){
    x = train[,-ncol(train)]
    y = train[, ncol(train)]
    trainX = removeBlankLevelsInDataFrame(x)
    trainY = removeBlankLevelsInVector(y)
    model <-  C5.0(trainX, trainY)
  }
  
  #stats
  if (met=="Logistic"){
    # model <- glm(ob, data=train, family=binomial(link='logit'))
    train_control <- trainControl(method = "none", classProbs = FALSE)
    model <- train(ob,data=train, method="glm",trControl=train_control, family="binomial", na.action = na.pass)
  } 
  
  #svmLinear
  if (met=="svmLinear"){ 
    # model <- nnet(ob ,data=train, size = 5)
    train_control <- trainControl(method = "none", classProbs = FALSE)
    model<-train(ob,data=train, trControl=train_control, method="svmLinear", trace = FALSE)
  }
  
  if (met=="nnet"){ 
    # model <- nnet(ob ,data=train, size = 5)
    if ("c_charge_desc" %in% colnames(train)){
      train <- select(train, -c_charge_desc)
    }
    train_control <- trainControl(method = "none", classProbs = FALSE)
    model<-train(ob, data = train, method = 'nnet', preProcess = c('center', 'scale'), trControl = train_control, tuneGrid=expand.grid(size=c(10), decay=c(0.1)))

  }
  
  if (met=="rf"){
    if ("c_charge_desc" %in% colnames(train)){
      train <- select(train, -c_charge_desc)
    }
    model <- randomForest(ob ,data=train)
  }
  
  return (model)
}


cm <- function(preds,mlabs, posClass)
{
  rescm<-list()
  posInd <- which(levels(preds) == posClass)
  negInd <- which(levels(preds) != posClass)
  
  pos<-levels(preds)[posInd]
  neg<-levels(preds)[negInd]
  rescm$TP<-sum(mlabs==pos&preds==pos, na.rm = TRUE)
  rescm$FP<-sum(mlabs==neg&preds==pos, na.rm = TRUE)
  rescm$TN<-sum(mlabs==neg&preds==neg, na.rm = TRUE)
  rescm$FN<-sum(mlabs==pos&preds==neg, na.rm = TRUE)
  rescm$N<-rescm$FN+rescm$FP+rescm$TN+rescm$TP
  rescm$acc<-(rescm$TP+rescm$TN)/length(preds)
  rescm$TPR<-rescm$TP/(rescm$TP+rescm$FN)
  rescm$FNR<-rescm$FN/(rescm$TP+rescm$FN)
  rescm$TNR<-rescm$TN/(rescm$TN+rescm$FP)
  rescm$FPR<-rescm$FP/(rescm$TN+rescm$FP)
  return(rescm)
}

#spd(rescmpriv,rescmnopriv)
spd <- function(cmp,cmnop)
{
  return(((cmp$TP+cmp$FP)/cmp$N)- ((cmnop$TP+cmnop$FP)/cmnop$N))
}



# fairness_results(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))

fairness_results <- function(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, posClass){
  # preds = preds[test[,WhatColPriv]==WhoPriv]
  # mlabs = test$class[test[,WhatColPriv]==WhoPriv]
  rescmpriv<-cm(preds[test[,WhatColPriv]==WhoPriv],test$class[test[,WhatColPriv]==WhoPriv], posClass)
  rescmnopriv<-cm(preds[test[,WhatColPriv]==WhoNoPriv],test$class[test[,WhatColPriv]==WhoNoPriv], posClass)
  res <- spd(rescmpriv,rescmnopriv)
  return(res)
}


myPredict <- function(met, model, test, train = NA){
  
  preds = NA
  
  if (met=="rpart"){ #Funciona
    preds<-predict(model, newdata = test ,type = 'class')
    preds <- as.factor(preds)
  }
  
  if (met == "Maj"){#Funciona
    preds<- rep(getmode(train$class), nrow(test))
    preds <- as.factor(preds)
  }
  
  if (met == "naive_bayes"){#Funciona
    preds<- predict(model, newdata = test ,type = 'class')
    preds <- as.factor(preds)
  }
  
  if (met == "C5.0Tree"){ #Falla
    preds<- predict(model, newdata = test ,type = 'class')
    preds <- as.factor(preds)
  }
  if (met == "Logistic"){#Falla
    # probs <- predict(model, newdata = test ,type = "response", se.fit = F)
    # preds <- ifelse(probs > 0.5, levels(test$class)[2], levels(test$class)[1])  
    # preds <- as.vector(preds)
    # preds <- as.factor(preds)
    # 
    preds <- predict(model, newdata=test)
    
  }
  
  if (met == "svmLinear"){#Funciona
    preds<- predict(model, newdata = test ,type = 'raw')
    preds <- as.factor(preds)
    
  }
  
  if(met == "nnet"){
    preds <- predict(model, newdata=test)
    
  }
  
  if (met == "rf"){#Falla
    preds<- predict(model, newdata = test ,type = 'class')
    preds <- as.factor(preds)
  }
  
  
  return(preds) 
}
  
# fairness_results(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))

fairness_results <- function(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, posClass){
  # preds = preds[test[,WhatColPriv]==WhoPriv]
  # mlabs = test$class[test[,WhatColPriv]==WhoPriv]
  rescmpriv<-cm(preds[test[,WhatColPriv]==WhoPriv],test$class[test[,WhatColPriv]==WhoPriv], posClass)
  rescmnopriv<-cm(preds[test[,WhatColPriv]==WhoNoPriv],test$class[test[,WhatColPriv]==WhoNoPriv], posClass)
  res <- spd(rescmpriv,rescmnopriv)
  return(res)
}


split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  
  
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    if (nchar(labs[i])  > 30) {
      labs[i] <- paste(substr(labs[i], 1, 30), "...", collapse = " ")
    }
    labs[i] <- paste(strwrap(labs[i], width = 15), collapse = "\n")
    
  }
  # print(labs)
  labs
}
  
print.rpart<- function(model, title){

  prp(model, extra = 6, 
      box.palette = "auto", 
      main = title,
      faclen = 2, 
      clip.facs = FALSE,
      split.fun = split.fun,
      varlen = 0,
      tweak = 1.2
      )
  
 }

# -------------------------------------------------------------------------------------------------
# Preprocessing Data
# -------------------------------------------------------------------------------------------------
# 
# # Recidivism
# Recidivism = readARFF("datasets/Recidivism.arff")
# sapply(Recidivism, pMiss)
# ncol(Recidivism)
# features_to_keep  = c('sex', 'age', 'age_cat', 'race',
#                       'juv_fel_count', 'juv_misd_count', 'juv_other_count', 'priors_count', 'days_b_screening_arrest',
#                       'c_days_from_compas', 'c_charge_degree', 'c_charge_desc',   #'decile_score.1', 'score_text',
#                       'two_year_recid') #, 'start', 'end', 'event') # From AIF360
# features_to_keep  %in% colnames(Recidivism)
# Recidivsm2 <- Recidivism[, colnames(Recidivism) %in% features_to_keep]
# colnames(Recidivsm2)[ncol(Recidivsm2)] <- "class"
# levels(Recidivsm2$race) <- c("No Caucasian", "No Caucasian", "Caucasian", "No Caucasian", "No Caucasian", "No Caucasian")
# write.arff(x=Recidivsm2 ,file= "datasets/RecidivismRace.arff")
# write.arff(x=Recidivsm2 ,file= "datasets/RecidivismSex.arff")


# Titanic
# Titanic <- readARFF("datasets/TitanicKaggle.arff")
# Titanic <- readARFF("datasets/titanic.arff")
# 
# sapply(Titanic, pMiss)
# 
# Titanic <- cbind(Titanic[,-2], Titanic[,2])
# colnames(Titanic)
# Titanic <- Titanic[,-c(2,7)] #Remov ticket
# 
# colnames(Titanic)[ncol(Titanic)] <- "class"
# Titanic$pclass <- as.factor(Titanic$pclass)
# levels(Titanic$pclass) <- c("1", "2-3", "2-3")
# write.arff(x=Titanic ,file= "datasets/TitanicKaggleClass.arff")
# write.arff(x=Titanic ,file= "datasets/TitanicKaggleSex.arff")


# # Adult
# Adult<- readARFF("datasets/adult.arff")
# sapply(Adult, pMiss)
# levels(Adult$race) <- c("White", "No White", "No White", "No White", "No White")
# write.arff(x=Adult ,file= "datasets/adultRace.arff")
# write.arff(x=Adult ,file= "datasets/adultSex.arff")


# -------------------------------------------------------------------------------------------------
# Variables
# -------------------------------------------------------------------------------------------------

numrep<-100

methods = c("rpart")
# methods = c("Maj")

datasets <- list.files("datasets/", pattern = "*.arff") 
datasetsIMPUTED <- list.files("datasets/imputed/", pattern = "*.arff") 


# Metadata: Protected attributes & (non-)privileged values
lnoprivs<-list()
lnamepos<-list()
lprivs<- list()

lprivs[["adult_Sex.arff"]]<-c("sex","Male")
lnoprivs[["adult_Sex.arff"]]<-c("sex","Female")
lnamepos[["adult_Sex.arff"]]<-">50K"

lprivs[["adult_Race.arff"]]<-c("race","White")
lnoprivs[["adult_Race.arff"]]<-c("race","No White")
lnamepos[["adult_Race.arff"]]<-">50K"


lprivs[["TitanicKaggle_Sex.arff"]]<-c("sex","female")
lnoprivs[["TitanicKaggle_Sex.arff"]]<-c("sex","male")
lnamepos[["TitanicKaggle_Sex.arff"]]<-"1"

lprivs[["TitanicKaggle_Class.arff"]]<-c("pclass","1")
lnoprivs[["TitanicKaggle_Class.arff"]]<-c("pclass","2-3")
lnamepos[["TitanicKaggle_Class.arff"]]<-"1"


lprivs[["Recidivism_Sex.arff"]]<-c("sex","Female")
lnoprivs[["Recidivism_Sex.arff"]]<-c("sex","Male")
lnamepos[["Recidivism_Sex.arff"]]<-"0"

lprivs[["Recidivism_Race.arff"]]<-c("race","Caucasian")
lnoprivs[["Recidivism_Race.arff"]]<-c("race","No Caucasian")
lnamepos[["Recidivism_Race.arff"]]<-"0"



 
# fitControl <-trainControl(## 5-fold CV
#   method = "none"
#   # number=5
# )


# -------------------------------------------------------------------------------------------------
# Main
# -------------------------------------------------------------------------------------------------

k<-0 # seed

AllResults <- data.frame(method = NA,
                         ds = NA,
                         rows = NA,
                         AllPosPriv = NA,
                         AllPosNoPriv = NA,
                         missings = NA,
                         MissPosPriv = NA,
                         MissPosNoPriv = NA,
                         noMissings = NA,
                         NMissPosPriv = NA,
                         NMissPosNoPriv = NA,
                         misCols = NA,
                         
                         All_Data_SPD = NA,
                         WM_Data_SPD = NA,
                         WOM_Data_SPD = NA,

                         
                         All_ACC = NA,
                         All_ACC_sd = NA,
                         All_SPD = NA,
                         All_SPD_sd = NA,
                         
                         All_ACC_imputed = NA,
                         All_ACC_sd_imputed = NA,
                         All_SPD_imputed = NA,
                         All_SPD_sd_imputed = NA,

                         
                         WM_ACC = NA,
                         WM_ACC_sd = NA,
                         WM_SPD = NA,
                         WM_SPD_sd = NA,
                         
                         WOM_ACC = NA,
                         WOM_ACC_sd = NA,
                         WOM_SPD = NA,
                         WOM_SPD_sd = NA,
                         
                         WOM_ACC_Sample = NA, #accwom_sample<-c()
                         WOM_ACC_Sample_sd = NA,
                         WOM_SPD_Sample = NA, #spdwom_sample<-c()
                         WOM_SPD_Sample_sd = NA,
                         
                         All_RemCols_ACC = NA,
                         All_RemCols_ACC_sd = NA,
                         All_RemCols_SPD = NA,
                         All_RemCols_SPD_sd = NA,
                         
                         WM_RemCols_ACC = NA,
                         WM_RemCols_ACC_sd = NA,
                         WM_RemCols_SPD = NA,
                         WM_RemCols_SPD_sd = NA,
                         
                         WOM_RemCols_ACC = NA,
                         WOM_RemCols_ACC_sd = NA,
                         WOM_RemCols_SPD = NA,
                         WOM_RemCols_SPD_sd = NA,
                         
                         WOM_RemCols_ACC_Sample = NA, #accwomc_sample<-c()
                         WOM_RemCols_ACC_Sample_sd = NA,
                         WOM_RemCols_SPD_Sample = NA, #spdwomc_sample<-c()
                         WOM_RemCols_SPD_Sample_sd = NA)

toPDF = FALSE
otherThanCART = FALSE

for (id in 1:length(datasets))
{

  k<-0 # seed
  
  datos <- readARFF(paste("datasets/",datasets[id],sep=""))
  datos$class <- as.factor(datos$class) 
  misind<-unique(which(is.na(datos), arr.ind=TRUE)[,1]) #indexs of rows with na
  miscols<-which(unlist(lapply(datos, function(x) any(is.na(x)))))
  
  datosImputed <- readARFF(paste("datasets/imputed/",datasetsIMPUTED[id],sep=""))
  datosImputed$class <- as.factor(datosImputed$class) 
  
  print(paste ("dataset:",datasets[id]))
  print(paste("Missings: ", length(misind)))
  print(paste("Cols with  Missings: ", length(miscols)))
  
  
  # Esta parte es sólo para estadisticas
  #datos<-datos[complete.cases(datos),]### remove missing rows
  # Asumimos que el estudio se hace en base al ultimo campo
  posParam<-length(datos[1,])
  nameParam<-names(datos)[posParam]
  dpv<-subset(datos,datos[,lprivs[[datasets[id]]][1]]==lprivs[[datasets[id]]][2])
  dnpv<-subset(datos,datos[,lnoprivs[[datasets[id]]][1]]==lnoprivs[[datasets[id]]][2])
  print(paste("registers:",nrow(datos)))
  
  AllPosPriv = sum(dpv[,nameParam]==lnamepos[[datasets[id]]][1])/nrow(dpv)
  AllPosNoPriv = sum(dnpv[,nameParam]==lnamepos[[datasets[id]]][1])/nrow(dnpv)
  print(paste ("total ratio pos priv: ", AllPosPriv))
  print(paste ("total ratio pos nopriv:", AllPosNoPriv))
  
  datoswm<-datos[misind,]
  dpv<-subset(datoswm,datoswm[,lprivs[[datasets[id]]][1]]==lprivs[[datasets[id]]][2])
  dnpv<-subset(datoswm,datoswm[,lnoprivs[[datasets[id]]][1]]==lnoprivs[[datasets[id]]][2])
  print(paste("registers:",nrow(datoswm)))
  
  MissPosPriv = sum(dpv[,nameParam]==lnamepos[[datasets[id]]][1])/nrow(dpv)
  MissPosNoPriv = sum(dpv[,nameParam]==lnamepos[[datasets[id]]][1])/nrow(dpv)
  print(paste ("missing: ratio pos priv: ", MissPosPriv))
  print(paste ("missing: ratio pos nopriv:", MissPosNoPriv))
  
  datoswom<-datos[-misind,]
  dpv<-subset(datoswom,datoswom[,lprivs[[datasets[id]]][1]]==lprivs[[datasets[id]]][2])
  dnpv<-subset(datoswom,datoswom[,lnoprivs[[datasets[id]]][1]]==lnoprivs[[datasets[id]]][2])
  print(paste("registers:",nrow(datoswom)))
  
  NMissPosPriv = sum(dpv[,nameParam]==lnamepos[[datasets[id]]][1])/nrow(dpv)
  NMissPosNoPriv = sum(dnpv[,nameParam]==lnamepos[[datasets[id]]][1])/nrow(dnpv)
  print(paste ("no missing: ratio pos priv: ", NMissPosPriv))
  print(paste ("no missing: ratio pos nopriv:", NMissPosNoPriv))
  
  
  
  # Fairness Measures DATASET
  
  WhatColPriv = lprivs[[datasets[id]]][1]
  WhoPriv = lprivs[[datasets[id]]][2]
  WhoNoPriv = lnoprivs[[datasets[id]]][2]
  
  
  rescmpriv_All<-cm(datos$class[datos[,WhatColPriv]==WhoPriv],datos$class[datos[,WhatColPriv]==WhoPriv], lnamepos[[datasets[id]]][1])
  rescmnopriv_All<-cm(datos$class[datos[,WhatColPriv]==WhoNoPriv],datos$class[datos[,WhatColPriv]==WhoNoPriv], lnamepos[[datasets[id]]][1])
  All_Data_SPD = spd(rescmpriv_All,rescmnopriv_All)
  
  rescmpriv_WM<-cm(datoswm$class[datoswm[,WhatColPriv]==WhoPriv],datoswm$class[datoswm[,WhatColPriv]==WhoPriv], lnamepos[[datasets[id]]][1])
  rescmnopriv_WM<-cm(datoswm$class[datoswm[,WhatColPriv]==WhoNoPriv],datoswm$class[datoswm[,WhatColPriv]==WhoNoPriv], lnamepos[[datasets[id]]][1])
  WM_Data_SPD = spd(rescmpriv_WM,rescmnopriv_WM)
  
  rescmpriv_WOM<-cm(datoswom$class[datoswom[,WhatColPriv]==WhoPriv],datoswom$class[datoswom[,WhatColPriv]==WhoPriv], lnamepos[[datasets[id]]][1])
  rescmnopriv_WOM<-cm(datoswom$class[datoswom[,WhatColPriv]==WhoNoPriv],datoswom$class[datoswom[,WhatColPriv]==WhoNoPriv], lnamepos[[datasets[id]]][1])
  WOM_Data_SPD_WOM = spd(rescmpriv_WOM,rescmnopriv_WOM)
  
  accAll<-c()
  spdAll<-c()
  accAll_imp<-c()
  spdAll_imp<-c()
  accwm<-c()
  accwom<-c()
  spdwm<-c()
  spdwom<-c()
  accwom_sample<-c()
  spdwom_sample<-c()
  
  accAllc<-c()
  spdAllc<-c()
  accwmc<-c()
  accwomc<-c()
  spdwmc<-c()
  spdwomc<-c()
  accwomc_sample<-c()
  spdwomc_sample<-c()
  
  
  for (im in 1:length(methods)){
    
    for (rep in 1:numrep)
    {
      k<-k+1
      print(paste("rep",rep,"-",numrep))
      set.seed(k)
      ldata<-list()
      
      #75 %Train
      indiceStrat<-createDataPartition(datos[,nameParam], p = 0.75,list=FALSE) ##STRATAFIED!!!
      train<-datos[indiceStrat,]
      test<-datos[-indiceStrat,]  
      completeTrain <- train
      
      
      trainImputed <- datosImputed[indiceStrat,]
      testImputed<-datosImputed[-indiceStrat,]  
      
      # separamos el train wm y wom
      #misind<-unique(which(is.na(train), arr.ind=TRUE)[,1]) #indexs of rows with na
      misind <- which(!complete.cases(train))
      trainWOM<-train[-misind,]
      trainWM<-train[misind,]
      
      met=methods[im]
      print(paste("method",met))
      restot<-list()
      
      nameDS = strsplit(datasets[id],".",fixed = T)[[1]][1]
      nameDS = strsplit(datasets[id],"_",fixed = T)[[1]][1]
      
      ## all ------------------------------------------------------------------------------------------------
      if (!otherThanCART){
        print("All")
        
        model <- runmodel(met, completeTrain, id)
        preds <- myPredict(met, model, test, completeTrain)
        
        if(toPDF){
          openPDFEPS(paste0("plots/",nameDS,"_all_",rep), height= 6, width= 8) 
          print.rpart(model, title = paste0("Dataset: ", nameDS, " (All Data)"))
          dev.off() }
        
        accAll<-c(accAll, sum((preds==test$class)/length(preds)))
        spdAll<-c(spdAll, fairness_results(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
      }
        
      ## all imputed ------------------------------------------------------------------------------------------------
      print("All where NAs are imputed with mode/mean")

      model <- runmodel(met, trainImputed, id)
      preds <- myPredict(met, model, testImputed, trainImputed)
      
      if(toPDF){
        openPDFEPS(paste0("plots/",nameDS,"_all_",rep), height= 6, width= 8) 
        print.rpart(model, title = paste0("Dataset: ", nameDS, " (All Data)"))
        dev.off() }
      
      accAll_imp<-c(accAll_imp, sum((preds==testImputed$class)/length(preds)))
      spdAll_imp<-c(spdAll_imp, fairness_results(preds, testImputed, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
      
      
      # With missing ------------------------------------------------------------------------------------------------
      if (!otherThanCART){
        print("With missing")
        
        model <- runmodel(met, trainWM, id)
        preds <- myPredict(met, model, test, trainWM)
        
        if(toPDF){
          openPDFEPS(paste0("plots/",nameDS,"_WM_",rep), height= 6, width= 8) 
          print.rpart(model, title = paste0("Dataset: ", nameDS, " (w Missing)"))
          dev.off()}
        
        accwm<-c(accwm,sum((preds==test$class)/length(preds)))
        spdwm<-c(spdwm, fairness_results(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
        
      }
     
       # without missing  --------------------------------------------------------------------------------------------
      print("without missing")

      model <- runmodel(met, trainWOM, id)
      preds<-myPredict(met, model, test, trainWOM)
      
      if(toPDF){
        openPDFEPS(paste0("plots/",nameDS,"_WOM_",rep), height= 6, width= 8) 
        print.rpart(model, title = paste0("Dataset: ", nameDS, " (w/o Missing)"))
        dev.off() }

      accwom<-c(accwom, sum((preds==test$class)/length(preds), na.rm = TRUE))
      spdwom<-c(spdwom, fairness_results(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
      
      # without missing (Sampling) --------------------------------------------------------------------------------------------
      if (!otherThanCART){
        print("without missing (sampling)")

        probsWM = table(trainWM$class)/nrow(trainWM)
        
        classWOM = trainWOM$class
        probsWOM = as.numeric(as.character(classWOM))
        probsWOM[which(classWOM == names(probsWM)[1])] <- probsWM[1]
        probsWOM[which(classWOM == names(probsWM)[2])] <- probsWM[2]
        
        if (id == 3 | id == 4){
          trainWOM_sample <- sample_n(trainWOM, nrow(trainWM), weight = probsWOM)
        }else{
          trainWOM_sample <- sample_n(trainWOM, nrow(trainWM))
        }
        
        model <- runmodel(met, trainWOM_sample, id)
        preds<-myPredict(met, model, test, trainWOM_sample)
        
        if(toPDF){
          openPDFEPS(paste0("plots/",nameDS,"_WOM_",rep), height= 6, width= 8) 
          print.rpart(model, title =  paste0("Dataset: ", nameDS, " (w/o Missing - sampling)"))
          dev.off() }
        
        accwom_sample<-c(accwom_sample,sum((preds==test$class)/length(preds)))
        spdwom_sample<-c(spdwom_sample, fairness_results(preds, test, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
        
      }
      
      ## all - Removing NA cols --------------------------------------------------------------------------------------
      if (!otherThanCART){
        
      print("All RemCols")
      
      completeTrain_cols <- completeTrain[,-(miscols)]
      test_cols<-test[,-(miscols)]
      
      model <- runmodel(met, completeTrain_cols, id)
      preds<-myPredict(met, model, test_cols, completeTrain_cols)
      
      if(toPDF){
        openPDFEPS(paste0("plots/",nameDS, "_all_RemCols_",rep), height= 6, width= 8) 
        print.rpart(model, title = paste0("Dataset: ", nameDS, " (All Data - Rem. NA Cols)"))
        dev.off() }

      accAllc<-c(accAllc,sum((preds==test_cols$class)/length(preds)))
      spdAllc<-c(spdAllc, fairness_results(preds, test_cols, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
      }
      
      
      # with missing - Removing NA cols  ----------------------------------------------------------------------------- 
      if (!otherThanCART){
        
        print("With Missings RemCols")
      
      trainWM_cols<-trainWM[,-(miscols)]

      # model<-  rpart(class~., data = trainWM_cols, method = 'class')
      model <- runmodel(met, trainWM_cols, id)
      preds<-myPredict(met, model, test_cols, trainWM_cols)
      
      if(toPDF){
        openPDFEPS(paste0("plots/",nameDS,"_WM_RemCols_",rep), height= 6, width= 8) 
        print.rpart(model,title = paste0("Dataset: ", nameDS, " (w Missings - Rem. NA Cols)"))
        dev.off() }

      accwmc<-c(accwmc,sum((preds==test_cols$class)/length(preds)))
      spdwmc<-c(spdwmc, fairness_results(preds, test_cols, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
      }
      
      # Without missing - Removing NA cols  --------------------------------------------------------------------------
      if (!otherThanCART){
        print("Without Missings RemCols")
        
        trainWOM_cols<-trainWOM[,-(miscols)]
        
        model <- runmodel(met, trainWOM_cols, id)
        preds<-myPredict(met, model, test_cols, trainWM_cols)
        
        if(toPDF){
          openPDFEPS(paste0("plots/",nameDS,"_WOM_RemCols_",rep), height= 6, width= 8) 
          print.rpart(model, title = paste0("Dataset: ", nameDS, " (w/o Missing - Rem. NA Cols)"))
          dev.off() }
        
        accwomc<-c(accwomc,sum((preds==test_cols$class)/length(preds)))
        spdwomc<-c(spdwomc, fairness_results(preds, test_cols, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
      }
      
      # Without missing - Removing NA cols (sampling) --------------------------------------------------------------------------
      if (!otherThanCART){
        print("Without Missings RemCols (sampling)")
        
        probsWM_sample = table(trainWM_cols$class)/nrow(trainWM_cols)
        classWOM = trainWOM_cols$class
        probsWOM = as.numeric(as.character(classWOM))
        probsWOM[which(classWOM == names(probsWM_sample)[1])] <- probsWM[1]
        probsWOM[which(classWOM == names(probsWM_sample)[2])] <- probsWM[2]
        
        if (id == 3 | id == 4){
          trainWOM_cols_sample <- sample_n(trainWOM_cols, nrow(trainWM_cols), weight = probsWOM)
        }else{
          trainWOM_cols_sample <- sample_n(trainWOM_cols, nrow(trainWM_cols))
        }
        
        
        # trainWOM_cols_sample <- sample_n(trainWOM_cols, nrow(trainWM_cols))
        
        model <- runmodel(met, trainWOM_cols_sample, id)
        preds<-myPredict(met, model, test_cols, trainWOM_cols_sample)
        
        if(toPDF){
          openPDFEPS(paste0("plots/",nameDS,"_WOM_RemCols_",rep), height= 6, width= 8) 
          print.rpart(model, title = paste0("Dataset: ", nameDS, " (w/o Missing - Rem. NA Cols - sampling)"))
          dev.off() }
        
        accwomc_sample<-c(accwomc_sample,sum((preds==test_cols$class)/length(preds)))
        spdwomc_sample<-c(spdwomc_sample, fairness_results(preds, test_cols, WhatColPriv, WhoPriv, WhoNoPriv, lnamepos[[datasets[id]]][1]))
      }
      
    }
    
    result <- c(
      method = met,
      ds = datasets[id],
      rows = nrow(datos),
      AllPosPriv = AllPosPriv,
      AllPosNoPriv = AllPosNoPriv,
      missings = nrow(datoswm),
      MissPosPriv = MissPosPriv,
      MissPosNoPriv = MissPosNoPriv,
      noMissings = nrow(datoswom),
      NMissPosPriv = NMissPosPriv,
      NMissPosNoPriv = NMissPosNoPriv,
      misCols = length(miscols),
      
      All_Data_SPD = All_Data_SPD,
      WM_Data_SPD = WM_Data_SPD,
      WOM_Data_SPD = WOM_Data_SPD_WOM,
      
      All_ACC = mean(accAll),
      All_ACC_sd = sd(accAll),
      All_SPD = mean(spdAll), ################
      All_SPD_sd = sd(spdAll),
      
      All_ACC_imputed = mean(accAll_imp),
      All_ACC_sd_imputed = sd(accAll_imp),
      All_SPD_imputed = mean(spdAll_imp), 
      All_SPD_sd_imputed = sd(spdAll_imp),
      
      WM_ACC = mean(accwm),
      WM_ACC_sd = sd(accwm),
      WM_SPD = mean(spdwm), ################
      WM_SPD_sd = sd(spdwm),
      
      WOM_ACC = mean(accwom),
      WOM_ACC_sd = sd(accwom),
      WOM_SPD = mean(spdwom), ################
      WOM_SPD_sd = sd(spdwom),
      
      WOM_ACC_Sample = mean(accwom_sample),
      WOM_ACC_Sample_sd = sd(accwom_sample),
      WOM_SPD_Sample = mean(spdwom_sample), ################
      WOM_SPD_Sample_sd = sd(spdwom_sample),
      
      
      All_RemCols_ACC = mean(accAllc),
      All_RemCols_ACC_sd = sd(accAllc),
      All_RemCols_SPD = mean(spdAllc), ################
      All_RemCols_SPD_sd = sd(spdAllc),
      
      WM_RemCols_ACC = mean(accwmc),
      WM_RemCols_ACC_sd = sd(accwmc),
      WM_RemCols_SPD = mean(spdwmc), ################
      WM_RemCols_SPD_sd = sd(spdwmc),
      
      WOM_RemCols_ACC = mean(accwomc),
      WOM_RemCols_ACC_sd = sd(accwomc),
      WOM_RemCols_SPD = mean(spdwomc), ################
      WOM_RemCols_SPD_sd = sd(spdwomc),
      
      WOM_RemCols_ACC_Sample = mean(accwomc_sample),
      WOM_RemCols_ACC_Sample_sd = sd(accwomc_sample),
      WOM_RemCols_SPD_Sample = mean(spdwomc_sample), ################
      WOM_RemCols_SPD_Sample_sd = sd(spdwomc_sample)
    )
    
    list4test_temp <- data.frame(method = met,
                                 ds = datasets[id],
                                 spdAll = spdAll,
                                 spdwm = spdwm,
                                 spdwom = spdwom,
                                 spdwom_sample = spdwom_sample,
                                 spdAllc = spdAllc, 
                                 spdwmc = spdwmc,
                                 spdwomc = spdwomc,
                                 spdwomc_sample = spdwomc_sample)
    saveRDS(list4test_temp, file = paste0("ANOVA_",datasets[id],".rds"))
    
    AllResults <- rbind(AllResults, result)
    
    
  }
  



}

# print(AllResults)
# AllResults[-1, c(2,22,26,30,38,42,46)]
AllResults <- AllResults[-1,]

if(otherThanCART){
  AllResults <- select(AllResults, method,ds, All_ACC, All_ACC_sd, All_SPD, All_SPD_sd, All_ACC_imputed, All_ACC_sd_imputed, All_SPD_imputed, All_SPD_sd_imputed, WOM_ACC, WOM_ACC_sd, WOM_SPD, WOM_SPD_sd)
}
saveRDS(AllResults, file = paste0("All_Results_3ds_FIN_",met,"_",rep,"_rep.rds"))
# saveRDS(AllResults, file = "All_Results_4ds_Mode_100rep.rds")



# met = "rpart";rep = 100

res <- readRDS(paste0("All_Results_3ds_FIN_",met,"_",rep,"_rep.rds"))
sapply(res, class)
res = colwise(type.convert)(res)
write.xlsx2(res, paste0("All_Results_3ds_FIN_",met,"_",rep,"_rep.xlsx"), sheetName = "Sheet1",
            col.names = T, row.names = F, append = F)



# -------------------------------------------------------------------------------------------------
# Plots
# -------------------------------------------------------------------------------------------------
# res <- readRDS("All_Results_4ds_rpart2_10rep.rds")
# resMaj <- readRDS("All_Results_4ds_Maj2_10rep.rds")
# resMaj <- resMaj[-1,]

sapply(res, class)
res = colwise(type.convert)(res)
res$name = unlist(strsplit(as.character(res$ds), ".", fixed =T))[c(1,3,5,7,9,11)]

# resMaj = colwise(type.convert)(resMaj)
# resMaj$name <- res$name



# All, WM, WoM faceted plot ---------------------------------------------------------------------------------------------

Fideal = 0; Fymin = -0.1; Fymin2 = 0.1;Fymax = 0.1 #SPD

plot2 <- ggplot(res) +

  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = Fymin, ymax = Fymax,   fill = "grey", alpha = 0.2, colour = "white") +
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.2, colour = "white") +
  #geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.2, colour = "white") +
  
  geom_hline(yintercept=Fideal, linetype="dashed", color = "gray", size = 1.5) +
  annotate("text", x = -Inf + 0.1, y = Fideal, label = "Fair", angle = 90, hjust = 1.2, vjust = 1.4, colour = "darkgrey" ) +
  annotate("text", x = -Inf + 0.1, y = Fymin, label = "Bias", angle = 90, hjust = 1.2, vjust = 1.4, colour = "red") +
  #annotate("text", x = -Inf + 0.1, y = Fymin2+0.1, label = "Bias", angle = 90, hjust = 0, vjust = 1.4, colour = "red") +

  
  geom_point(data = resMaj, aes(All_ACC , All_SPD, colour = name), size = 10, shape = 13) +
  # geom_text_repel(data = resMaj, aes(All_ACC , All_SPD, label = "Maj. class", colour = name), force = 10, size = 4, fontface = 'italic',
                  # box.padding = 0.35, point.padding = 0.5,
                  # segment.color = 'grey50') +
  
  
  geom_point(aes(All_ACC, All_SPD, colour = name, shape ="All"), size = 10, alpha = 0.6) +
  geom_point(aes(All_ACC, All_SPD), size = 2, colour = "white") +
  geom_point(aes(All_ACC, All_SPD), size = 1, colour = "Black") +
  
  geom_point(aes(All_ACC_imputed, All_SPD_imputed, colour = name, shape = "All_imputed"), size = 5, alpha = 0.6) +
  geom_point(aes(All_ACC_imputed, All_SPD_imputed), size =2, colour = "white") +
  geom_point(aes(All_ACC_imputed, All_SPD_imputed), size = 1, colour = "Black") +
  
  
  #geom_errorbar(aes(x = WM_ACC,  ymin=WM_SPD-WM_SPD_sd, ymax=WM_SPD+WM_SPD_sd), colour="black", width=.1) +
  geom_point(aes(WM_ACC, WM_SPD, colour = name, shape ="WM"), size = 10, alpha = 0.6) +
  geom_point(aes(WM_ACC, WM_SPD), size = 2, colour = "white") +
  geom_point(aes(WM_ACC, WM_SPD), size = 1, colour = "Black") +
  
  
  #geom_errorbar(aes(x = WOM_ACC,  ymin=WOM_SPD-WOM_SPD_sd, ymax=WOM_SPD+WOM_SPD_sd), colour="black", width=.1) +
  geom_point(aes(WOM_ACC, WOM_SPD, colour = name, shape = "WOM"), size = 10, alpha = 0.6) +
  geom_point(aes(WOM_ACC, WOM_SPD), size =2, colour = "white") +
  geom_point(aes(WOM_ACC, WOM_SPD), size = 1, colour = "Black") +
  
  
  geom_point(aes(WOM_ACC_Sample, WOM_SPD_Sample, colour = name, shape = "WOM_Sample"), size = 5, alpha = 0.6) +
  geom_point(aes(WOM_ACC_Sample, WOM_SPD_Sample), size =2, colour = "white") +
  geom_point(aes(WOM_ACC_Sample, WOM_SPD_Sample), size = 1, colour = "Black") +
  
  
  
  
  
  facet_wrap(name~., ncol = 2, scales = "free")+
  #geom_errorbar(aes(x = WM_RemCols_ACC,  ymin=WM_RemCols_SPD-WM_RemCols_SPD_sd, ymax=WM_RemCols_SPD+WM_RemCols_SPD_sd), colour="black", width=.1) +
  #geom_point(aes(WM_RemCols_ACC, WM_RemCols_SPD, colour = ds, shape = "WM_RemCols"), size = 14, alpha = 0.3) +
  
  #geom_errorbar(aes(x = WOM_RemCols_ACC,  ymin=WOM_RemCols_SPD-WOM_RemCols_SPD_sd, ymax=WOM_RemCols_SPD+WOM_RemCols_SPD_sd), colour="black", width=.1) +
  #geom_point(aes(WOM_RemCols_ACC, WOM_RemCols_SPD, colour = ds, shape = "WOM_RemCols"), size = 14, alpha = 0.3) +
  
  scale_shape_manual(values = c(18, 9, 17, 16, 19)) +
  scale_color_discrete(guide = FALSE) +
  labs(x="Accuracy", y = "SPD") +
  theme_minimal() + 
  theme(legend.title = element_blank())
  
  
  plot2

openPDFEPS(paste("Fairness_3ds_rpart_facet_100rep", sep=""), height = 10, width = 14)
plot(plot2)
dev.off()
  





# All, WM, WoM removing NA columns faceted plot ---------------------------------------------------------------------------------------------



Fideal = 0; Fymin = -0.1; Fymin2 = 0.1;Fymax = 0.1 #SPD

plot4 <- ggplot(res) +
  
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = Fymin, ymax = Fymax,   fill = "grey", alpha = 0.2, colour = "white") +
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = -Inf, ymax = Fymin,   fill = "lightcoral", alpha = 0.2, colour = "white") +
  #geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.2, colour = "white") +
  
  geom_hline(yintercept=Fideal, linetype="dashed", color = "gray", size = 1.5) +
  annotate("text", x = -Inf + 0.1, y = Fideal, label = "Fair", angle = 90, hjust = 1.2, vjust = 1.4, colour = "darkgrey" ) +
  annotate("text", x = -Inf + 0.1, y = Fymin, label = "Bias", angle = 90, hjust = 1.2, vjust = 1.4, colour = "red") +
  #annotate("text", x = -Inf + 0.1, y = Fymin2+0.1, label = "Bias", angle = 90, hjust = 0, vjust = 1.4, colour = "red") +
  
  geom_point(data = resMaj, aes(All_ACC , All_SPD, colour = name), size = 10, shape = 13) +
  geom_text_repel(data = resMaj, aes(All_ACC , All_SPD, label = "Maj. class", colour = name), force = 10, size = 4, fontface = 'italic',
                  box.padding = 0.35, point.padding = 0.5,
                  segment.color = 'grey50') +
  
  geom_point(aes(All_RemCols_ACC, All_RemCols_SPD, colour = name, shape ="All"), size = 10, alpha = 0.4) +
  geom_point(aes(All_RemCols_ACC, All_RemCols_SPD), size = 2, colour = "white") +
  geom_point(aes(All_RemCols_ACC, All_RemCols_SPD), size = 1, colour = "Black") +
  
  
  #geom_errorbar(aes(x = WM_ACC,  ymin=WM_SPD-WM_SPD_sd, ymax=WM_SPD+WM_SPD_sd), colour="black", width=.1) +
  geom_point(aes(WM_RemCols_ACC, WM_RemCols_SPD, colour = name, shape ="WM"), size = 10, alpha = 0.4) +
  geom_point(aes(WM_RemCols_ACC, WM_RemCols_SPD), size = 2, colour = "white") +
  geom_point(aes(WM_RemCols_ACC, WM_RemCols_SPD), size = 1, colour = "Black") +
  
  
  #geom_errorbar(aes(x = WOM_ACC,  ymin=WOM_SPD-WOM_SPD_sd, ymax=WOM_SPD+WOM_SPD_sd), colour="black", width=.1) +
  geom_point(aes(WOM_RemCols_ACC, WOM_RemCols_SPD, colour = name, shape = "WOM"), size = 10, alpha = 0.4) +
  geom_point(aes(WOM_RemCols_ACC, WOM_RemCols_SPD), size =2, colour = "white") +
  geom_point(aes(WOM_RemCols_ACC, WOM_RemCols_SPD), size = 1, colour = "Black") +
  
  geom_point(aes(WOM_ACC_Sample, WOM_SPD_Sample, colour = name, shape = "WOM_Sample"), size = 5, alpha = 0.4) +
  geom_point(aes(WOM_ACC_Sample, WOM_SPD_Sample), size =2, colour = "white") +
  geom_point(aes(WOM_ACC_Sample, WOM_SPD_Sample), size = 1, colour = "Black") +
  
  
  facet_wrap(name~., ncol = 2, scales = "free")+
  #geom_errorbar(aes(x = WM_RemCols_ACC,  ymin=WM_RemCols_SPD-WM_RemCols_SPD_sd, ymax=WM_RemCols_SPD+WM_RemCols_SPD_sd), colour="black", width=.1) +
  #geom_point(aes(WM_RemCols_ACC, WM_RemCols_SPD, colour = ds, shape = "WM_RemCols"), size = 14, alpha = 0.3) +
  
  #geom_errorbar(aes(x = WOM_RemCols_ACC,  ymin=WOM_RemCols_SPD-WOM_RemCols_SPD_sd, ymax=WOM_RemCols_SPD+WOM_RemCols_SPD_sd), colour="black", width=.1) +
  #geom_point(aes(WOM_RemCols_ACC, WOM_RemCols_SPD, colour = ds, shape = "WOM_RemCols"), size = 14, alpha = 0.3) +
  
  scale_shape_manual(values = c(16, 17, 18, 19)) +
  scale_color_discrete(guide = FALSE) +
  labs(x="Accuracy", y = "SPD") +
  theme_minimal() + 
  theme(legend.title = element_blank())


plot4

openPDFEPS(paste("Fairness_4ds_RemCols_rpartb_facet_100rep", sep=""), height = 10, width = 14)
plot(plot4)
dev.off()




# -------------------------------------------------------------------------------------------------
# Tables
# -------------------------------------------------------------------------------------------------

colnames(res)

library(dplyr)

dataDescription <- select(res, name, rows, missings, misCols, All_Data_SPD, WM_Data_SPD, WOM_Data_SPD)
xtable(dataDescription, digits = 4)

colnames(res)
modelling <- res[,c(48,16:31)]
xtable(modelling, digits = 4)

remCols <- res[,c(48,32:47)]
xtable(remCols, digits = 4)


# -------------------------------------------------------------------------------------------------
require(plyr)
require(xlsx)
All <- read.xlsx("All_Results_4ds_All_10_rep.xlsx", sheetName = "Sheet1")

require(ggplot2)
require(ggrepel)

colnames(All) <- make.unique(names(All))
All$name <- unlist(strsplit(as.character(All$ds), ".", fixed =T))[c(1,3,5,7,9,11)]
All = colwise(type.convert)(All)

require(dplyr) 

Fideal = 0; Fymin = -0.1; Fymin2 = 0.1;Fymax = 0.1 #SPD


All.f <- filter(All, method == "svmLinear")

ggplot(All.f)+
  
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = Fymin, ymax = Fymax,   fill = "grey", alpha = 0.2, colour = "white") +
  geom_rect(xmin = -Inf, xmax = +Inf,   ymin = -Inf, ymax = Fymin,   fill = "lightcoral", alpha = 0.2, colour = "white") +
  #geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.2, colour = "white") +
  
  geom_hline(yintercept=Fideal, linetype = "dashed", color = "gray", size = 1.5) +
  annotate("text", x = -Inf + 0.1, y = Fideal, label = "Fair", angle = 90, hjust = 1.2, vjust = 1.4, colour = "darkgrey" ) +
  annotate("text", x = -Inf + 0.1, y = Fymin, label = "Bias", angle = 90, hjust = 1.2, vjust = 1.4, colour = "red") +
  #annotate("text", x = -Inf + 0.1, y = Fymin2+0.1, label = "Bias", angle = 90, hjust = 0, vjust = 1.4, colour = "red") +
  
  #MAJ CLASS
  geom_point(aes(MajClass_ACC  , MajClass_SPD , colour = method), size = 7, shape = 13) +
  geom_text_repel(aes(MajClass_ACC , MajClass_SPD, label = "Maj. class", colour = method), force = 10, size = 4, fontface = 'italic',
                  box.padding = 0.35, point.padding = 0.5,
                  segment.color = 'grey50') +
  
  # Withouth MISSING
  geom_point(aes(WOM_ACC, WOM_SPD, colour = method, shape = "WOM"), size = 7, alpha = 0.8) +
  geom_point(aes(WOM_ACC, WOM_SPD), size =2, colour = "white") +
  geom_point(aes(WOM_ACC, WOM_SPD), size = 1, colour = "Black")+
  
  # All IMPUTED
  geom_point(aes(All_ACC_imputed, All_SPD_imputed, colour = method, shape = "All_imputed"), size = 7, alpha = 0.8) +
  geom_point(aes(All_ACC_imputed, All_SPD_imputed), size =2, colour = "white") +
  geom_point(aes(All_ACC_imputed, All_SPD_imputed), size = 1, colour = "Black") +
  
  
  # Perfect CLASS
  
  geom_point(aes(PerfectClass_ACC , PerfectClass_SPD, colour = method), size = 7, alpha = 0.8, shape = 8) +
  geom_text_repel(aes(PerfectClass_ACC , PerfectClass_SPD, label = "Perfect class", colour = method), force = 10, size = 4, fontface = 'italic',
                  box.padding = 0.35, point.padding = 0.5,
                  segment.color = 'grey50') +
  
  
  facet_wrap(name~., ncol = 2, scales = "free")+
  #geom_errorbar(aes(x = WM_RemCols_ACC,  ymin=WM_RemCols_SPD-WM_RemCols_SPD_sd, ymax=WM_RemCols_SPD+WM_RemCols_SPD_sd), colour="black", width=.1) +
  #geom_point(aes(WM_RemCols_ACC, WM_RemCols_SPD, colour = ds, shape = "WM_RemCols"), size = 14, alpha = 0.3) +
  
  #geom_errorbar(aes(x = WOM_RemCols_ACC,  ymin=WOM_RemCols_SPD-WOM_RemCols_SPD_sd, ymax=WOM_RemCols_SPD+WOM_RemCols_SPD_sd), colour="black", width=.1) +
  #geom_point(aes(WOM_RemCols_ACC, WOM_RemCols_SPD, colour = ds, shape = "WOM_RemCols"), size = 14, alpha = 0.3) +
  
  scale_shape_manual(values = c(15, 19)) +
  scale_color_discrete(guide = FALSE) +
  labs(x="Accuracy", y = "SPD") +
  theme_minimal() + 
  theme(legend.title = element_blank())



require(reshape2)
All.ACC.s <- select(All, method, name, MajClass_ACC,  WOM_ACC,  All_ACC_imputed,  PerfectClass_ACC)
All.SPD.s <- select(All, method, name,  MajClass_SPD,  WOM_SPD,  All_SPD_imputed,  PerfectClass_SPD)

All.ACC.m <- melt(All.ACC.s, id.vars = c("method","name"))
All.SPD.m <- melt(All.SPD.s, id.vars = c("method","name"))

All.m <-cbind(select(All.ACC.m, method, name, variable, value), select(All.SPD.m, variable, value))
colnames(All.m) <- c("method", "name", "var.ACC", "val.ACC", "var.SPD", "val.SPD")

temp <- All.m[All.m$name == "Recidivism_Race" | All.m$name == "Recidivism_Race",]$val.SPD * (-1)
All.m[All.m$name == "Recidivism_Race" | All.m$name == "Recidivism_Race", "val.SPD"] <- temp    

Fideal = 0; Fymin = -0.1; Fymin2 = 0.1;Fymax = 0.1 #SPD
# 
# geom_rect(xmin = -Inf, xmax = +Inf,   ymin = Fymin, ymax = Fymax,   fill = "grey", alpha = 0.2, colour = "white") +
#   geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.2, colour = "white") +
#   #geom_rect(xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.2, colour = "white") +
#   
#   geom_hline(yintercept=Fideal, linetype="dashed", color = "gray", size = 1.5) +
#   annotate("text", x = -Inf + 0.1, y = Fideal, label = "Fair", angle = 90, hjust = 1.2, vjust = 1.4, colour = "darkgrey" ) +
#   annotate("text", x = -Inf + 0.1, y = Fymin, label = "Bias", angle = 90, hjust = 1.2, vjust = 1.4, colour = "red") +
#   #annotate("text", x = -Inf + 0.1, y = Fymin2+0.1, label = "Bias", angle = 90, hjust = 0, vjust = 1.4, colour = "red") +
levels(All.m$method) <- c("LR", "NB", "NN", "RF", "DT", "SV")
levels(All.m$var.ACC) <- c("Majority","w/o Missings", "Missings Imputed","Perfect")

post = All.m[All.m$var.ACC == "Perfect" | All.m$var.ACC == "Missings Imputed",]
pre =  All.m[All.m$var.ACC != "Perfect",]

diag =  All.m[All.m$var.ACC == "Majority" | All.m$var.ACC == "Perfect",]

p <- ggplot(All.m, aes(val.ACC, val.SPD, colour = method, shape = var.ACC, group = method)) + 
  
  annotate("rect",xmin = -Inf, xmax = +Inf,   ymin = Fymin, ymax = Fymax,   fill = "grey", alpha = 0.2, colour = "white") +
  annotate("rect",xmin = -Inf, xmax = +Inf,   ymin = +Inf, ymax = Fymin2,   fill = "lightcoral", alpha = 0.2, colour = "white") +

  geom_hline(yintercept=Fideal, linetype = "dashed", color = "grey", size = 1.5) +
  annotate("text", x = -Inf + 0.1, y = Fideal, label = "Fair", angle = 90, hjust = 1.2, vjust = 1.4, colour = "darkgrey" ) +
  annotate("text", x = -Inf + 0.1, y = Fymin2, label = "Bias", angle = 90, hjust = -0.2, vjust = 1.4, colour = "red") +

  geom_point(size = 6, alpha = 0.7 ) +
  geom_point(size =1, colour = "white") +
  geom_line(data = pre, aes(val.ACC, val.SPD, colour = method, group = method), size = 1, alpha = 0.5) +
  geom_line(data = post, aes(val.ACC, val.SPD, colour = method, group = method), linetype = "dashed", size = 1, alpha = 0.5) +
  geom_line(data = diag, aes(val.ACC, val.SPD, group = method), linetype = "dotted", size = 0.9, alpha = 0.5) +
  
  
  
  facet_wrap(name~., ncol = 2, scales = "free")+
  scale_shape_manual(values = c(13,19, 15, 11)) +
  # scale_color_discrete(guide = FALSE) +
  labs(x="Accuracy", y = "SPD") +
  theme_minimal() + 
  theme(legend.title = element_blank())
p 

openPDFEPS(paste("Fairness_6tech_3ds_100rep", sep=""), height = 12, width = 16)
plot(p)
dev.off()

