library(Hmisc)
library(plyr)
library(randomForest)

library(doMC)
library(foreach)
library(ROCR)
registerDoMC(20)

base="" #path to directory where summary.csv is

calculateRF10FCV<-function(data, selectedProperties, columnOrder,nIterations=10,nFolds=10) {
  currentData=data[,c(selectedProperties,"refProne")]
  foldSize=ceiling(nrow(currentData)/nFolds)
  results=data.frame()
  importances=data.frame()
  
  for(j in 1:nIterations) {
    print(paste("  iteration: ",j,sep=""))
    currentData=currentData[sample(1:nrow(currentData)),]
    tmpResults=data.frame()
    for(i in 1:nFolds) {
      #tmpResults = foreach(i=1:nFolds,.combine = rbind) %dopar% {
      start=(i-1)*foldSize+1
      end=min(ceiling(i*foldSize),nrow(currentData))
      
      testidx=start:end
      trainidx=setdiff(1:nrow(currentData),testidx)
      test=currentData[testidx,]
      train=currentData[trainidx,]
      ##PREDICTION
      refProne.rf=randomForest(refProne ~ ., data=train, importance=T, na.action=na.omit,ntree=500)
      x=test[,-which(colnames(test)=="refProne")] # 67
      
      predictions=predict(refProne.rf, x, type="response")
      test$predictions=predictions
      probs=predict(refProne.rf, x, type="prob")
      
      test$probFalse=probs[,1]
      test$probTrue=probs[,2]
      
      pred <- prediction(test$probTrue, test$refProne)
      
      
      idxCutoffF=which(performance(pred,"f")@y.values[[1]][performance(pred,"f")@y.values[[1]]!="NaN"]==max(performance(pred,"f")@y.values[[1]][performance(pred,"f")@y.values[[1]]!="NaN"]))
      
      acc=performance(pred,"acc")@y.values[[1]][performance(pred,"acc")@y.values[[1]]!="NaN"][idxCutoffF]
      prec=performance(pred,"prec")@y.values[[1]][performance(pred,"prec")@y.values[[1]]!="NaN"][idxCutoffF]
      rec=performance(pred,"rec")@y.values[[1]][performance(pred,"rec")@y.values[[1]]!="NaN"][idxCutoffF]
      f=performance(pred,"f")@y.values[[1]][performance(pred,"f")@y.values[[1]]!="NaN"][idxCutoffF]
      auc=performance(pred,"auc")@y.values[[1]]
      
      metrics=data.frame(iteration=j, fold=i,acc=acc,prec=prec,rec=rec,f=f,auc=auc)
      if(nrow(metrics)>1){
        metrics=metrics[1,]
      }
      
      imps=importance(refProne.rf)
      importances = as.data.frame(matrix(0,nrow = 1,ncol = length(columnOrder)))
      colnames(importances)=columnOrder
      gini=imps[,"MeanDecreaseGini"]
      importances[,names(gini)]=gini
      metrics=data.frame(metrics,importances)
      #return(metrics)
      tmpResults=rbind(tmpResults,metrics)
    }
    results=rbind(results,tmpResults)
  }
  return(results)
}



doAnalysis<-function(type="refProne") {
  
  data=read.csv(paste(base,"summary.csv",sep=""),sep = "\t")
  data=data[data$usageCount!=0,]
  refProne=rep(F,times=nrow(data))
  if(type=="refProne"){
    ##prepare for refProne
   
    refProneCopy=data$refCount
    data$refCount=NULL
    data$usageCount=NULL
    
    m=median(refProneCopy)
    refProne[refProneCopy>m]=T
    refProne=as.factor(refProne)
    #end prepare
  }else if(type=="refDense"){
    #prepare for refDensity
    refDensity=data$refCount/data$usageCount
    data$refCount=NULL
    data$usageCount=NULL
    
    m=median(refDensity)
    refProne[refDensity>m]=T
    refProne=as.factor(refProne)
    ##end prepare
  }else if(type =="postProne"){
    ##prepare for refProne
    
    postProneCopy=data$postCount
    data$postCount=NULL
    data$usageCount=NULL
    
    m=median(postProneCopy)
    refProne[postProneCopy>m]=T
    refProne=as.factor(refProne)
    #end prepare
  }
 
  
  
  
  data= data[,-c(1,2,3)]
  toRemove = c(  which(colnames(data)=="name"),   which(colnames(data)=="ADD"),   which(colnames(data)=="MODIFY"),   
                 which(colnames(data)=="DELETE"),   which(colnames(data)=="refUsage"),   which(colnames(data)=="changeCount") ,  
                 which(colnames(data)=="smellCount") ,   which(colnames(data)=="kind"))
  data = data[, -toRemove]
  
  
  cor=cor(data)
  write.table(cor,file=paste(base,"cor.csv",sep=""),sep=";",quote = T)
  
  clus = varclus(as.matrix(data), similarity=c("spearman"))
  #plot(clus)
  #abline(h = 1-0.64, col = "gray60")
  t =cutree(clus$hclust, h=(1-0.64))
  
  cluster = data.frame(property=names(t),group=unname(t), stringsAsFactors = FALSE)
  counts=count(cluster$group)
  
  groups=list()
  
  for(i in 1:max(cluster$group)) {
    groups[[i]]=cluster[cluster$group==i,"property"]
  }
  
  
  #1,3,49
  runs=matrix(nrow = length(groups[[1]])*length(groups[[3]])*length(groups[[49]]), ncol = length(groups))
  line=1
  for(i in groups[[1]]) {
    for(j in groups[[3]]) {
      for(k in groups[[49]]) {
        runs[line,1]=groups[[1]][which(groups[[1]]==i)]
        runs[line,2]=groups[[3]][which(groups[[3]]==j)]
        runs[line,3]=groups[[49]][which(groups[[49]]==k)]
        line=line+1
      } 
    } 
  }
  col=4
  for(g in groups[-c(1,3,49)]) {
    runs[,col]=unlist(g)
    col=col+1
  }
  
  columnOrder=colnames(data)
  
  totalMetrics = data.frame()
  data=cbind(data,refProne)
  
 
  i=1
  totalMetrics = foreach(nr=1:nrow(runs),.combine = rbind) %dopar% { 
    #for(nr in 1:nrow(runs)) {
    #for(nr in 1:2) {
    print(paste("RUN: ",nr,sep=""))
    run=runs[nr,]
    metrics=calculateRF10FCV(data=data, selectedProperties=as.character(unlist(run)), columnOrder = columnOrder, nIterations = 10, nFolds = 10)
    addMetrics=data.frame(featureSet=i,metrics)
    #addMetrics=addMetrics[,columnOrder]
    i=i+1
    #totalMetrics=rbind(totalMetrics,addMetrics)
    return(addMetrics)
  }
  
  write.table(totalMetrics,file=paste(base,"results.csv",sep=""),sep=";",quote = T)
  boxplot(totalMetrics[,4:8],ylim=c(0,1))
  
  #varimp plot without 0 elements (0 means that the variable has not been used for this model)
  plotdata=list() # TODO finish
  plotdatanames=list()
  line=1
  markers=c()
  for(i in 9:ncol(totalMetrics)) {
    plotdata[[line]]=totalMetrics[totalMetrics[,i]!=0,i]
    plotdatanames[[line]]=colnames(totalMetrics)[i]
    if(mean((totalMetrics[totalMetrics[,i]!=0,i]))>25) {
      markers=rbind(markers,line)
    }
    line=line+1
  }
  pdf(file=paste(base,"importances_",type,".pdf",sep=""),useDingbats = F,height = 10,width = 13)
  par(mar=c(20.4,4.1,4.1,2.1))
  boxplot(plotdata,names = plotdatanames, las=2)
  abline(v=markers)
  dev.off()
}

#doAnalysis(type="refProne")

#doAnalysis(type="refDense")

doAnalysis(type="postProne")
