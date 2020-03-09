
library(randomForest)
library(doMC)
library(foreach)
library(ROCR)
registerDoMC(7)
library(e1071)
library(Hmisc)
library(plyr)
library(caret)
library(mlbench)
library(pROC)
library(MASS)

pathToFile= "/path/to/summary.csv" # path to metrics file

ref=TRUE
refDensity = TRUE


# call startClassificationRefProne to classify refProne API classes 
startClassificationRefProne <- function(){
  # BINARY CLASSIFICATION
  b_resultsRF = computeModels("RF", ref, F, 2)
  b_resultsSVM = computeModels("SVM", ref, F, 2)
  b_resultsLR = computeModels("LR", ref, F, 2)
  
  # TERTIARY CLASSIFICAION
  t_resultsRF = computeModels("RF", T, F, 3)
  t_resultsSVM = computeModels("SVM", T, F, 3)
  t_resultsLR = computeModels("LR", T, F, 3)
  
  # plot performance
  par(mfrow=c(2,3))
  boxplot(b_resultsRF[,c(3:7)],main="RF for refCount",ylim=c(0,1))
  boxplot(b_resultsSVM[,c(3:7)],main="SVM for refCount",ylim=c(0,1))
  boxplot(b_resultsLR[,c(3:7)],main="LR for refCount",ylim=c(0,1)) 
  boxplot(t_resultsRF[,c(3:7)],main="Tertiary: RF for refCount",ylim=c(0,1))
  boxplot(t_resultsSVM[,c(3:7)],main="Tertiary: SVM for refCount",ylim=c(0,1))
  boxplot(t_resultsLR[,c(3:7)],main="Tertiary: LR for refCount",ylim=c(0,1))
  par(mfrow=c(1,1)) 
  
  # plot importance - binary
  plotImportance(b_resultsRF,"Binary: ref-proneness", 5 )
  # plot importance - tertiary
  plotImportance(t_resultsRF,"Tertiary: ref-proneness", 5 )
  
  
}

# startClassificationRefDense to classify reference-dense API classes
startClassificationRefDense <-function(){
  
  # BINARY CLASSIFICATION
   b_resultsRF_norm = computeModels("RF", ref, F,2,  refDensity)
  b_resultsSVM_norm = computeModels("SVM", ref,F, 2,  refDensity)
  b_resultsLR_norm = computeModels("LR", ref,F, 2,  refDensity)
  
  # TERTIARY CLASSIFICATION
  t_resultsRF_norm = computeModels("RF", ref, F,3,  refDensity)
  t_resultsSVM_norm = computeModels("SVM", ref,F, 3,  refDensity)
  t_resultsLR_norm = computeModels("LR", ref,F, 3,  refDensity)
  
  # plot performance 
  par(mfrow=c(2,3))
  boxplot(b_resultsRF_norm[,c(3:7)],main="RF for norm.refCount",ylim=c(0,1))
  boxplot(b_resultsSVM_norm[,c(3:7)],main="SVM for norm.refCount",ylim=c(0,1))
  boxplot(b_resultsLR_norm[,c(3:7)],main="LR for norm.refCount",ylim=c(0,1)) 
  boxplot(t_resultsRF_norm[,c(3:7)],main="Tertiary: RF for norm.refCount",ylim=c(0,1))
  boxplot(t_resultsSVM_norm[,c(3:7)],main="Tertiary: SVM for norm.refCount",ylim=c(0,1))
  boxplot(t_resultsLR_norm[,c(3:7)],main="Tertiary: LR for norm.refCount",ylim=c(0,1)) 
  par(mfrow=c(1,1)) 
  
  # plot importance - binary
  plotImportance(b_resultsRF_norm,"Binary: ref-dense",5 )
  # plot importance - tertiary
  plotImportance(t_resultsRF_norm,"Tertiary: ref-dense",5 )
}  


plotImportance <- function(data, title,val=7){
  par(mar=c(18,4,4,2))
  par(cex.lab=1.2)
  par(cex.axis=1.2)
  help <- data[apply(data, 2, median) >= val]
  boxplot(help[, -(1:2)],las=2)#,  cex.axis=0.8, cex.names=0.8)
  title(title, line= -1.3,cex.lab=1.6)
  title(ylab="MDG", line=2.2, cex.lab=1.2)
  par(mar=c(5, 4, 4, 2) + 0.1)
 # par(mfrow=c(1,1)) 
}
  
computeModels <- function(mlType="RF",ref=T,USAGE =T, classType=2, normalizeUsage=F, featureSelection=T){
  
  dataForRefCnt = getDataForModels(ref, USAGE, classType, normalizeUsage,mlType,featureSelection)
  
  nIterations=10
  
  nFolds=10
  count = 0
  foldSize=ceiling(nrow(dataForRefCnt)/nFolds)
  results=data.frame()
  importances=data.frame()
  
  for(j in 1:nIterations) {
    dataForRefCnt=dataForRefCnt[sample(1:nrow(dataForRefCnt)),]
    #i=1
    #tmpResults = data.frame() # uncomment if %dopar% does not work
    #for (i in 1:nFolds){
    # print(colnames(dataForRefCnt))
    tmpResults = foreach(i=1:nFolds,.combine = rbind) %dopar% {
      start=(i-1)*foldSize+1
      end=min(ceiling(i*foldSize),nrow(dataForRefCnt))
      
      testidx=start:end
      trainidx=setdiff(1:nrow(dataForRefCnt),testidx)
      test=dataForRefCnt[testidx,]
      train=dataForRefCnt[trainidx,]
      predictions = -1
      probs = -1
      ##PREDICTION
      #rf
      if(mlType == "RF"){
        print("RF")
        refProne.rf=randomForest(refProne ~ ., data=train, importance=T, na.action=na.omit,ntree=500)
        x=test[,-which(colnames(test)=="refProne")] # 67
        
        predictions=predict(refProne.rf, x, type="response")
        test$predictions=predictions
        probs=predict(refProne.rf, x, type="prob")
        
        
      }     
      
      #svm
      if(mlType == "SVM"){
        print("SVM")
        refProne.svm  <- svm(refProne ~ ., data = train, kernel = "radial", gamma = 0.1, cost = 10,probability=T) 
        x=test[,-which(colnames(test)=="refProne")]#67
        predictions=predict(refProne.svm, x)
        probs=predict(refProne.svm, x, probability=TRUE)
        probs=attr(probs, "probabilities")
        
      }
      
      lrSummary = -1
      #logReg
      if(mlType == "LR"){
        print("LR")
        x=test[,-which(colnames(test)=="refProne")]#66
        if(classType==2){
          refProne.lr <- glm(refProne ~ .,family=binomial(link='logit'),data=train)
          
          #test
          # summary(refProne.lr)
          # mod_fit <- train(refProne ~ ., data=train, method="polr", family=binomial(link='logit'))
          # varImp(mod_fit)
          
          
          predictions=predict(refProne.lr, x, type="response")
          
          test$predictions=F
          test$predictions[predictions>0.5]=T
          test$probFalse=1-predictions
          test$probTrue=predictions
        }else{
          refProne.lr <- polr(refProne ~ ., data = train, Hess=T)
          predictions = predict(refProne.lr, x, type = "class") 
          probs =   predict(refProne.lr, x, type = "probs")
          #print(refProne.lr$coefficients)
          print(length(refProne.lr$coefficients))
          lrSummary = refProne.lr$coefficients
          # tmpResults=cbind(tmpResults,refProne.lr$coefficients)
          #lrSummary=rbind(lrSummary,refProne.lr$coefficients)
          #colnames(lrSummary)= names(refProne.lr$coefficients)
        }
      }
      
      
      
      # METRICS
      if(classType==2){
        if(!mlType=="LR"){
          test$probFalse=probs[,1]
          test$probTrue=probs[,2]
        }
        pred <- prediction(test$probTrue, test$refProne)
        
        
        idxCutoffF=which(performance(pred,"f")@y.values[[1]][performance(pred,"f")@y.values[[1]]!="NaN"]==max(performance(pred,"f")@y.values[[1]][performance(pred,"f")@y.values[[1]]!="NaN"]))
        
        acc=performance(pred,"acc")@y.values[[1]][performance(pred,"acc")@y.values[[1]]!="NaN"][idxCutoffF]
        prec=performance(pred,"prec")@y.values[[1]][performance(pred,"prec")@y.values[[1]]!="NaN"][idxCutoffF]
        rec=performance(pred,"rec")@y.values[[1]][performance(pred,"rec")@y.values[[1]]!="NaN"][idxCutoffF]
        f=performance(pred,"f")@y.values[[1]][performance(pred,"f")@y.values[[1]]!="NaN"][idxCutoffF]
        auc=performance(pred,"auc")@y.values[[1]]
        
        metrics=data.frame(acc=acc,prec=prec,rec=rec,f=f,auc=auc)
        if(nrow(metrics)>1){
          metrics=metrics[1,]
        }
      }
      else{
        
        matrix = confusionMatrix(predictions, test$refProne)
        conf = matrix$table
        # accuracy
        accL= c()
        for(i in 1: length(conf[1,])){
          # accuracy = (tp+tn)/(tp+tn+fp+fn)
          accL[i] = (conf[i,i] + sum(conf[-i,-i])) / sum(conf)
          
        }
        acc = mean(accL)
        
        # precision, recall, and fscore for classes >2
        precL = c()
        recL = c()
        fL = c()
        for(i in 1: length(conf[1,])){
          precL[i]= (conf[i,i]/(sum(conf[i,])))
          recL[i]= (conf[i,i]/(sum(conf[,i])))
          fL[i] = 2*(precL[i]*recL[i])/(precL[i]+recL[i])
        }
        prec= mean(precL)
        rec= mean(recL)
        f = mean(fL)
        
        #auc
        roc = multiclass.roc(test$refProne, as.numeric(predictions), levels=base::levels(as.factor(test$refProne)) )
        auc = roc$auc
        
        metrics=data.frame(acc=acc,prec=prec,rec=rec,f=f,auc=auc)
      }
      
      
      if(mlType == "RF"){
        imps=importance(refProne.rf) #ONLY RF
        imps=as.data.frame(t(imps[,"MeanDecreaseGini"])) #MeanDecreaseAccuracy  #ONLY RF
        retList=cbind(iteration=j,fold=i,metrics,imps)  #ONLY RF
      } else if(mlType == "LR"){
        retList=cbind(iteration=j,fold=i,metrics, t(data.frame(lrSummary))) # ONLY FOR SVM and LR
      } else{
        retList=cbind(iteration=j,fold=i,metrics) # ONLY FOR SVM and LR
        
      }
      
      #tmpResults=rbind(tmpResults,retList) # uncomment if %dopar% does not work
      return(retList)
    }
    results=rbind(results,tmpResults)
  }
  return(results)
}


getData <- function(ref=T,USAGE =T, normalizeUsage=F, mlType){
  
  
  data=read.csv(file=pathToFile,sep="\t")
  toRemove = c(  which(colnames(data)=="name"),   which(colnames(data)=="ADD"),   which(colnames(data)=="MODIFY"),   
                 which(colnames(data)=="DELETE"),   which(colnames(data)=="refUsage"),   which(colnames(data)=="changeCount") ,  
                 which(colnames(data)=="smellCount") ,   which(colnames(data)=="kind"))
  data = data[, -toRemove]
  
  
  
  if(ref){
    data=data[,-which(colnames(data)=="postCount")] # remove postcount
    if(normalizeUsage){
      data$refCount = data$refCount/data$usageCount
      USAGE=F
      data = data[-which(c(data$refCount=="Inf")),]
    }
  }else{
    data=data[,-which(colnames(data)=="refCount")] # remove refcount
    if(normalizeUsage){
      data$postCount = data$postCount/data$usageCount
      USAGE=F
      data = data[-which(c(data$postCount=="Inf")),]
    }
    
  } 
  
  if(!USAGE){
    # print("no usage")
    data= data[,-which(colnames(data)=="usageCount")]
  }
  
  return(data)
}

getDataForModels <- function(refCount, USAGE, classType, normalizeUsage=F, mlType="RF", featureSelection = T){
  
  data = getData(refCount, USAGE, normalizeUsage, mlType)
  if(featureSelection==T){
    print("select features")
    selectedFeatures=selectFeatures(refCount, USAGE, normalizeUsage, mlType)
    sub = data[,which(names(data) %in%  c(selectedFeatures, "refCount", "postCount"))]
    data = sub
  }
  
  
  # binary classification
  if(classType == 2){
    data =  splitBinary(data, refCount, USAGE)
  } else if(classType == 3){
    # tertiary classification
    data =  splitTertiary(data, refCount, USAGE)
  } else if(classType == 5){
    # quintary classification
    data =  splitQuintary(data, refCount, USAGE)
  } else{
    if(!refCount){
      data$refCount = data$postCount
    }
    data$postCount = NULL
    print("regression")
  }
  return(data)
  
}

splitBinary <- function(data, refCount, USAGE){
  
  if(refCount){
    m = -1
    m=median(data$refCount)
    data$refProne=F
    data$refProne[data$refCount>m]=T
    data$refProne=as.factor(data$refProne)
    data$refCount=NULL
  }else{
    m=-1
    m=median(data$postCount)
    data$refProne=F
    data$refProne[data$postCount>m]=T
    data$refProne=as.factor(data$refProne)
    data$postCount=NULL
  }
  return(data)
}

splitTertiary <- function(data, refCount, USAGE){
  if(refCount){
    
    t1 = quantile(data$refCount,  probs = c(1/3))[1]
    t2= quantile(data$refCount,  probs = c(2/3))[1]
    
    data$refProne="LOW"
    data$refProne[data$refCount>t1]="MEDIUM"
    data$refProne[data$refCount>t2]="HIGH"
    data$refProne=as.factor(data$refProne)
    data$refCount=NULL
  }else{
    
    t1 = quantile(data$postCount,  probs = c(1/3))[1]
    t2= quantile(data$refCount,  probs = c(2/3))[1]
    
    data$refProne="LOW"
    data$refProne[data$postCount>t1]="MEDIUM"
    data$refProne[data$postCount>t2]="HIGH"
    data$refProne=as.factor(data$refProne)
    data$postCount=NULL
  }
  return(data)
}

splitQuintary <- function(data, refCount, USAGE){
  
  if(refCount){
    
    q1 = quantile(data$refCount,  probs = c(1/5))[1]
    q2= quantile(data$refCount,  probs = c(2/5))[1]
    q3 = quantile(data$refCount,  probs = c(3/5))[1]
    q4= quantile(data$refCount,  probs = c(4/5))[1]
    
    data$refProne="VERY_LOW"
    data$refProne[data$refCount>q1]="LOW"
    data$refProne[data$refCount>q2]="MEDIUM"
    data$refProne[data$refCount>q3]="HIGH"
    data$refProne[data$refCount>q4]="VERY_HIGH"
    data$refProne=as.factor(data$refProne)
    data$refCount=NULL
  }else{
    
    q1 = quantile(data$refCount,  probs = c(1/5))[1]
    q2= quantile(data$refCount,  probs = c(2/5))[1]
    q3 = quantile(data$refCount,  probs = c(3/5))[1]
    q4= quantile(data$refCount,  probs = c(4/5))[1]
    
    data$refProne="VERY_LOW"
    data$refProne[data$postCount>q1]="LOW"
    data$refProne[data$postCount>q2]="MEDIUM"
    data$refProne[data$postCount>q3]="HIGH"
    data$refProne[data$postCount>q4]="VERY_HIGH"
    data$refProne=as.factor(data$refProne)
  }
  return(data)
  
  
}

prepareCorrelation<-function(data, refCount=T){
  corr = cor(data, method="spearman")
  if(refCount){
    countCorr= corr[which(colnames(corr)=="refCount"),]
  } else{
    countCorr= corr[which(colnames(corr)=="postCount"),]
  }
  countCorrDF = data.frame(names(countCorr), unname(countCorr), stringsAsFactors = FALSE)
  names(countCorrDF)=c("property","correlation")
  return(countCorrDF)
}

clusterVariables <-function(data){
  #remove refCount or postCount
  data = data[,-1]
  clus = varclus(as.matrix(data), similarity=c("spearman"))
  #plot(clus)
  #abline(h = 1-0.64, col = "gray60")
  t =cutree(clus$hclust, h=(1-0.64))
  dt = data.frame(names(t),unname(t), stringsAsFactors = FALSE)
  names(dt)=c("property","group")
  return(dt)
}

selectFeatures <- function(refCount=T, USAGE=T, normalizeUsage=F, mlType="RF"){
  clustdata = getData(refCount,USAGE, normalizeUsage, mlType)
  
  # prepare correlation to refcount or postcount
  countCorrDF = prepareCorrelation(clustdata,refCount)
  
  # cluster variables
  clustDF = clusterVariables(clustdata) 
  
  # feature selection
  selectedFeatures = c()
  
  for(i in 1:max(clustDF$group)){
    x=clustDF[clustDF$group==i,"property"]
    #print(x)
    
    if(length(x)>1){
      # select maxcorr to target variable and add to featurelist
      sub = subset(countCorrDF,countCorrDF$property %in%  x)
      #print(sub)
      maxCorr= sub[sub$correlation == max(sub$correlation),"property"]
      #print(maxCorr)
      selectedFeatures=c(selectedFeatures, maxCorr)
    }else{
      # add to featurelist
      selectedFeatures=c(selectedFeatures, x)
    }
    
  }
  
  return(selectedFeatures)
  
}


startClassificationRefProne()
startClassificationRefDense()

