
#Statistical Parity Difference (SPD) [-1,1]:
#(TPP+FPP)/(NP)-(TPU+FPU)/NU

spd<-function(cmp,cmnop)
{
  return(((cmp$TP+cmp$FP)/cmp$N)- ((cmnop$TP+cmnop$FP)/cmnop$N))
}


#Disparate Impact (DI) [0, +Inf)

#En vez de diferencia es division

#((TPP+FPP)/(NP))/((TPU+FPU)/NU)
di<-function(cmp,cmnop)
{
  return(((cmp$TP+cmp$FP)/cmp$N)/ ((cmnop$TP+cmnop$FP)/cmnop$N))
}


#Average odds difference (OddsDif) [-1,1]:
#((FPRU-FPRP)+(TPRU-TPRP))*0.5

#Donde FPRU=FPU/(FPU+TNU)....

OddsDif<-function(cmp,cmnop)
{
  return (((cmnop$FPR-cmp$FPR)+(cmnop$TPR-cmp$TPR))/2)
}

#Equal opportunity difference (EOD):[-1,1]

#This is the difference in true positive rates between unprivileged and privileged groups.

#TPRU-TPRP

eod<-function(cmp,cmnop)
{
  return (cmnop$TPR-cmp$TPR)
}



cm<-function(preds,mlabs)
{
  rescm<-list()
  pos<-levels(preds)[2]
  neg<-levels(preds)[1]
  rescm$TP<-sum(mlabs==pos&preds==pos)
  rescm$FP<-sum(mlabs==neg&preds==pos)
  rescm$TN<-sum(mlabs==neg&preds==neg)
  rescm$FN<-sum(mlabs==pos&preds==neg)
  rescm$N<-rescm$FN+rescm$FP+rescm$TN+rescm$TP
  rescm$acc<-(rescm$TP+rescm$TN)/length(preds)
  rescm$TPR<-rescm$TP/(rescm$TP+rescm$FN)
  rescm$FNR<-rescm$FN/(rescm$TP+rescm$FN)
  rescm$TNR<-rescm$TN/(rescm$TN+rescm$FP)
  rescm$FPR<-rescm$FP/(rescm$TN+rescm$FP)
  return(rescm)
}
