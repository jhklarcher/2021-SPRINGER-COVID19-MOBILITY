Decomposition_VMD<-function(data,nmodes)
{
  library(vmd)
  
  vtrain <- vmd(data[,1],DC=FALSE,tol=1e-3,K=nmodes)
  
  decomposed_data<- as.data.frame(vtrain)
  
  colnames(decomposed_data)<-c("X","Signal",paste("MODE",1:nmodes,sep=""),"Magg")
  
  Modes<-decomposed_data[,3:6]
  return(Modes)
}

