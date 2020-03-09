
# reads the output of all iterative runs for refprone classification and prints the importance in a pdf

pathToDirectory="/path/to/Directory/"

checkIterativeResultsRefProne<- function(pathToDirectory="~/"){

  results_refProne=read.csv(file=paste(pathToDirectory,"results_refprone.csv", sep=""),sep=";")
  checkResults(results_refProne, "refProne")
}

checkIterativeResultsPostProne<- function(pathToDirectory="~/"){

  results_postProne=read.csv(file=paste(pathToDirectory,"results_postprone.csv", sep=""),sep=";")
  checkResults(results_postProne, "postProne")
}

checkIterativeResultsRefDense<- function(pathToDirectory="~/"){

  results_density=read.csv(file=paste(pathToDirectory,"results_density.csv", sep=""),sep=";")
  checkResults(results_density, "refDense")
}

checkResults <- function(data, type, marker=25){

  perfDensity= data
  perf.df  = data.frame()
  
  for(i in 1:54){
    #summary(perfDensity[1:100,])[3,]
    summary(perfDensity[((i-1)*100)+1:i*100,])[3,1]
    run =  perfDensity[(((i-1)*100)+1):(i*100),]
    acc = median(run[,1])
    prec = median(run[,2])
    rec = median(run[,3])
    f = median(run[,4])
    auc = median(run[,5])
    
    perf.df = rbind(perf.df, c(acc, prec, rec, f, auc))
    
  }
  colnames(perf.df)=c("acc", "prec", "rec", "f", "auc")
  
  totalMetrics = perfDensity
  #varimp plot without 0 elements (0 means that the variable has not been used for this model)
  plotdata=list() 
  plotdatanames=list()
  line=1
  markers=c()
  for(i in 9:ncol(totalMetrics)) {
    plotdata[[line]]=totalMetrics[totalMetrics[,i]!=0,i]
    plotdatanames[[line]]=colnames(totalMetrics)[i]
    mmean=mean((totalMetrics[totalMetrics[,i]!=0,i]))
    if(mmean>marker) {
      markers=rbind(markers,line)
    }
    line=line+1
  }
  
  names(plotdata) <- plotdatanames
  pdf(file=paste("~/importances_iterativ_", type,".pdf",sep=""),useDingbats = F,height = 10,width = 13)
  par(mar=c(20.4,4.1,4.1,2.1))
  boxplot(plotdata, las=2)
  title(main=paste("Importances for all runs for ", type,". Markers set by mean >", marker, sep=""))
  abline(v=markers)
  dev.off()
  
  
  
}
checkIterativeResultsRefProne(pathToDirectory)
checkIterativeResultsRefDense(pathToDirectory)
checkIterativeResultsPostProne(pathToDirectory)

