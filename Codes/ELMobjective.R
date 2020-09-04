obj_fit<- function(x) {
  
  funcoes           <-c('sig','sin','radbas','hardlim','hardlims','satlins','tansig','tribas','relu','purelin')
  tipo_inicializacao<-c('normal_gaussian','uniform_positive','uniform_negative')
  
  if(1<=x[1] & x[1]<=1000 & 1<=x[2] & 1<=x[3])
  {
    
    par<-ELMpar(x)
    
    set.seed(1234)
    mod <- train(X_train,Y_train,
                 method = ELMmodell,
                 trControl = fitControl,
                 preProcess = c("center","scale"),
                 tuneGrid = data.frame(nhid         = as.numeric(par[1]), 
                                       actfun       = as.character(par[2]),
                                       init_weights = as.character(par[3])),
                 verbose = FALSE)
    
    getTrainPerf(mod)[, "TrainRMSE"]
  }
  else
  {
    100000000000000000
  }
}