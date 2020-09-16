rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(1234)
#Save several directories
BaseDir       <- getwd()
CodesDir      <- paste(BaseDir, "Codes", sep="/")
FiguresDir    <- paste(BaseDir, "Figures", sep="/")
ResultsDir    <- paste(BaseDir, "Results", sep="/")
DataDir       <- paste(BaseDir, "Data",sep="/")

#Load Packages
setwd(CodesDir)
source("checkpackages.R")

packages<-c("quantmod","hht","forecast","xlsx",
            "cowplot","Metrics","caret","elmNNRcpp","tcltk","TTR",
            "foreach","iterators","doParallel","lmtest","fbasics")
sapply(packages,packs)

source("Metricas.R")
source("elm_caret.R")

library(extrafont)
#loadfonts(device="postscript")
loadfonts(device = "win", quiet = TRUE)

windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)

#Checa quantos n?cleos existem
ncl<-detectCores();ncl

#Registra os clusters a serem utilizados
cl <- makeCluster(ncl-1);registerDoParallel(cl)
############################################################################

setwd(DataDir)

dados_all     <-read.table(file="covid19-mobility-data.csv",header=TRUE,sep=";",dec=".")
dados_states  <-split(dados_all,dados_all$state)

#Correlation Matrix

dadoscor<-dados_all[,c("new_confirmed","mob1","mob2","mob3","mob4","mob5","mob6")]

colnames(dadoscor)<-c("$Y","$X[1]","$X[2]","$X[3]","$X[4]","$X[5]","$X[6]")
rownames(dadoscor)<-c("$Y","$X[1]","$X[2]","$X[3]","$X[4]","$X[5]","$X[6]")

M <- cor(dadoscor)
M
par(family="Times New Roman")
x11()
corrplot(M, method = "circle",tl.col = "black",tl.cex=1.5,tl.srt = 60, cl.pos = "b")

X11()
corrplot.mixed(M,upper.col = colorRampPalette(c("black","white","gray"))(200)
               ,lower.col = "black",tl.col = "black")

#Descriptive Measures

#Statistics

descriptives<-lapply(dados_states,function(x){apply(x[,c('cum_confirmed','new_confirmed')],2,basicStats)})
names_desc<-names(descriptives)
desc_summary<-list()
for(i in 1:length(descriptives))
  desc_summary[[i]]<-do.call(cbind.data.frame,descriptives[[i]])

names(desc_summary)<-names(dados_states)
desc_final<-do.call(cbind.data.frame,desc_summary)

setwd(ResultsDir)
write.xlsx(desc_final,file="Descriptive_Measures.xlsx")

#Descriptives for mobility

descriptives<-lapply(dados_states,function(x){apply(x[,c('mob1','mob2','mob3','mob4','mob5','mob6')],2,basicStats)})
names_desc<-names(descriptives)
desc_summary<-list()
for(i in 1:length(descriptives))
  desc_summary[[i]]<-do.call(cbind.data.frame,descriptives[[i]])

names(desc_summary)<-names(dados_states)
desc_final<-do.call(cbind.data.frame,desc_summary)

setwd(ResultsDir)
write.xlsx(desc_final,file="Descriptive_Measures_Mobility.xlsx")
##########################Decomposition######################################
Decomposed_data<-list()

for(state in 1:length(dados_states))
{
  set.seed(1234)
  data<-as.vector(dados_states[[state]][,"new_confirmed"])
  
  trials_eemd      <- 100;  
  nimf_eemd        <- 5;  
  noise.amp_eemd   <- 0.25;

  trials.dir <- "testtrain"
  
  #set.seed(1234)
  #EEMD decomposition
  EEMD(data, time(data), noise.amp_eemd, trials_eemd, nimf_eemd, trials.dir = trials.dir,verbose=FALSE)
  result<- EEMDCompile(trials.dir, trials_eemd, nimf_eemd)
  
  data_final_eemd<-as.data.frame(cbind(result$averaged.imfs,result$averaged.residue)) 
  colnames(data_final_eemd)<-c(paste("IMF",1:(dim(data_final_eemd)[2]-1),sep=""),"Residual")
 
  
  Decomposed_data[[state]]<-data.frame(dados_states[[state]][,c(2:3,6,8:13)],data_final_eemd)
  cat('Decomposition for',names(dados_states)[state],'done! \n')
}

names(Decomposed_data)<-names(dados_states)

#Components plots

#Remove PA state
#Decomposed_data<-Decomposed_data[names(Decomposed_data)[c(1:6,8:10)]]

#Save the datasets
setwd(DataDir)
saveRDS(Decomposed_data,"Decomposed_data.rds")


setwd(FiguresDir)
for(state in 1:length(Decomposed_data))
{
  Dataset<-names(Decomposed_data)[state]
  Data   <-Decomposed_data[[state]][,c(2,3,10:15)]
  nameeps<-paste("Components_",Dataset,".eps",sep="")
  
  #Set in one table
  legends <- c("IMF[1]","IMF[2]","IMF[3]","IMF[4]","IMF[5]","Residual")
  
  components_EEMD<-data.frame(Obs=unlist(Data[,-c(1,2)]),
                               Samples=rep(c(1:dim(Data)[1]),times=length(legends)),
                               Comp=rep(legends,each=dim(Data)[1]))
  
  #p2<-ACF(dados_sea[[d]][,"TOTALDEMAND"])  
  
  p3<-
    {
      x11()
      ggplot(components_EEMD, aes(x=Samples, y = Obs)) + 
        geom_line() +
        #facet_wrap(~Comp, scales = "free",nrow=4,labeller = label_parsed)+
        facet_grid(scales="free",rows = vars(Comp),labeller = label_parsed,switch="both")+
        xlab("Samples") +  ylab("")+
        theme_bw() +
        theme(
          axis.text=element_text(size=17),
          axis.text.y = element_text(size=17),
          strip.text.x = element_text(size = 17),
          strip.text = element_text(size = 17),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          #legend.position = "bottom",
          legend.position=c(0.3, 0.9),  
          legend.background = element_rect(fill="transparent",colour=NA),
          legend.text = element_text(size=17),
          text = element_text(family = "Times New Roman", size = 25),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.grid.minor = element_blank()) 
    }
  
  ggsave(nameeps, device=cairo_ps,width = 10,height = 6,dpi = 1200)
}


#Modeling
setwd(DataDir)
Decomposed<-list(EEMD=readRDS("Decomposed_data.rds"))

#Date in Date format

for(i in 1:length(Decomposed[[1]]))
{
  n                 <-dim(Decomposed[[1]][[i]])[1]
  Decomposed[[1]][[i]][,'date']<-as.Date(seq(Sys.Date()-10-(n-1), by = "day", length.out = n))
}



models<-c("ELMmodell","svmLinear2","xgbLinear")

number<-seq(1,length(models),1)
combs <-expand.grid(number,number,number,number,number,number)

#Find the approaches that considers same models for all components
r<-c()
for(l in 1:length(models))
  r[l]<- rownames(combs)[combs$Var1 == l & combs$Var2 == l &   combs$Var3 == l &
                         combs$Var4 == l & combs$Var5 == l &   combs$Var6 == l ]

#Homogeneous Ensemble Learning
#combs<-combs[r,]

#Forecasting Horizons
FH<-c(7,14)
setwd(CodesDir)

for(decomp in 1:length(Decomposed)) #Varia??o do n?mero de m?todos de decomposi??o
{  
  Data_Decomp<-Decomposed[[decomp]]
  MDecomp    <-ifelse(decomp==1,"EEMD","None")
  
  for(s in 1:length(Data_Decomp)) #Varia??o pro n?mero de Datasets
  {
    Dataset<-names(Data_Decomp)[s]
    
    #Horizon
    Inputs               <-Data_Decomp[[s]][,c(4:9)]
    Lags_Inputs          <-lags(data.frame(Inputs), n = 14)
    colnames(Lags_Inputs)<- c(colnames(Inputs),as.vector(outer(colnames(Inputs), paste0("Lags",c(1:14),sep=""), FUN = "paste0")))
    
    Lags_InputsH1<-c(colnames(Lags_Inputs)[43:48])   #Exogenous variables for H=7
    Lags_InputsH2<-c(colnames(Lags_Inputs)[85:90])   #Exogenous variables for H=14

    Output     <-Data_Decomp[[s]][,"new_confirmed"]  #Non-Decomposed output
    data_dec   <-Data_Decomp[[s]][,c("IMF1","IMF2","IMF3","IMF4","IMF5","Residual")]      #Decomposed output according to EEMD or MODWT
    data_dec1  <-list()                #Receive components of each decomposed output
    
    #cat("\014")
    #Objetos para salvar os resultados
    {Models <-list();    
      Params <-list();  
      PTROSA <-list(); PTRDSA    <-list();
      PTEOSA <-list(); PTEDSA    <-list();  
      
      POSA   <-matrix(nrow=dim(combs)[1]+length(models)+1, ncol=11)
      PDSA   <-matrix(nrow=dim(combs)[1]+length(models)+1, ncol=11)
      k      <-1;
      
      colnames(POSA)     <-c(rep(c("RMSE","R2","MAE","SMAPE","RRMSE"),times=2),"Model")
      colnames(PDSA)     <-c(rep(c("RMSE","R2","MAE","SMAPE","RRMSE"),times=2),"Model")

      thetas          <-matrix(nrow=dim(combs)[1],ncol=dim(data_dec)[2],byrow=FALSE) 
      
      Importances       <-list()
    }
    
    for(i in 1:dim(data_dec)[2]) #Varia??o pro n?mero de componentes
    {
      Lags_Outputs<-lags(data.frame(data_dec[,i]), n = 14)                   #Lags for output
      Lags_Outputs<-Lags_Outputs[,1:8]
      colnames(Lags_Outputs)<-c(names(data_dec)[i],paste("Lag",1:7,sep="")) #Lags' names for ouput
      
      for(Horizonte in 1:2)
      {
        Horizon<-FH[Horizonte]
        
        if(Horizon==7)   #7-steps-ahead
        {
          data_dec1[[i]]<-data.frame(Lags_Outputs)
          #----------------------Divis?o em treinamento e teste---------------------#
          n      <-dim(data_dec1[[i]])[1]
          cut    <-n-14
          
          fitControl<- trainControl(method         = "timeslice",
                                    initialWindow  = floor(cut*0.9), 
                                    horizon        = 7,               
                                    fixedWindow    = TRUE,
                                    allowParallel  = TRUE,
                                    savePredictions="final")
          
          train  <-data.frame(data_dec1[[i]][(1:cut),],Lags_Inputs[(1:cut),Lags_InputsH1]);
          Y_train<-train[,1];X_train<-train[,-1]
          test   <-tail(data.frame(data_dec1[[i]],Lags_Inputs[,Lags_InputsH1]),n-cut)
          Y_test <-test[,1]; X_test <-test[,-1]
          
          #Objects
          {
            #---------------------------OSA----------------------------------------------#
            PTRosa  <-matrix(nrow=cut,ncol=length(models));    #Recebe predi??es OSA das componentes
            PTEosa  <-matrix(nrow=n-cut,ncol=length(models));  #Recebe predi??es OSA das componentes
            
            PTrOSA <-matrix(nrow=cut,ncol=dim(combs)[1]+length(models)+1);     #Recebe predi??es OSA Combinadas + ARIMA model
            ETrOSA <-matrix(nrow=cut ,ncol=dim(combs)[1]+length(models)+1);    #Recebe erros OSA Combinadas     + ARIMA model
            PTeOSA <-matrix(nrow=n-cut ,ncol=dim(combs)[1]+length(models)+1);  #Recebe predi??es OSA Combinadas + ARIMA model
            ETeOSA <-matrix(nrow=n-cut  ,ncol=dim(combs)[1]+length(models)+1); #Recebe erros OSA Combinadas     + ARIMA model
            
            PTRFOSA   <-matrix(nrow=cut,ncol=2*length(models)+1);              #Recebe os Objetos finais OSA
            ETRFOSA   <-matrix(nrow=cut,ncol=2*length(models)+1);
            
            PTEFOSA   <-matrix(nrow=n-cut,ncol=2*length(models)+1);
            ETEFOSA   <-matrix(nrow=n-cut,ncol=2*length(models)+1);
          }
          
          
          for(m in 1:length(models))
          {
            X_trainm<-train[,-1]; X_testm <-test[,-1]
            
            if(m==1)
            {
              set.seed(1234)
              Models[[k]]<-train(X_trainm,Y_train,
                                 method=ELMmodell,
                                 preProcess = c("center","scale"),
                                 tuneLength= 3,
                                 trControl = fitControl,verbose=FALSE)
            }
            else
            {
              #-----------------------Treinamento----------------------------------------#
              set.seed(1234)
              Models[[k]]<-train(X_trainm,as.numeric(Y_train),
                                 method=models[m],
                                 preProcess = c("center","scale"),
                                 tuneLength = 3,
                                 trControl = fitControl,verbose=FALSE,importance=TRUE)
            }
            
            
            Lag1<-match("Lag1",colnames(X_test));Lag2<-match("Lag2",colnames(X_test))
            Lag3<-match("Lag3",colnames(X_test));Lag4<-match("Lag4",colnames(X_test))
            Lag5<-match("Lag5",colnames(X_test));Lag6<-match("Lag6",colnames(X_test))
            Lag7<-match("Lag7",colnames(X_test))
            #-----------------------Save Hyperparameters--------------------------------#
            Params[[k]]<-Models[[k]]$bestTune
            Importances[[k]]<-varImp(Models[[k]], scale = FALSE)
            
            
            #--------------------------Predictions 7-steps---------------------------------#
            
            #Train
            for(p in 1:cut)
            {
              if(p%%Horizon !=1)
              {
                PTRosa[p,m]<-as.numeric(predict(Models[[k]],X_trainm[p,]))
                X_trainm[p+1,Lag1]<-PTRosa[p,m]
                X_trainm[p+2,Lag2]<-PTRosa[p,m]
                X_trainm[p+3,Lag3]<-PTRosa[p,m]
                X_trainm[p+4,Lag4]<-PTRosa[p,m]
                X_trainm[p+5,Lag5]<-PTRosa[p,m]
                X_trainm[p+6,Lag6]<-PTRosa[p,m]
              }
              else
              {
                X_trainm[p:(n-cut),]<-X_train[p:(n-cut),]
                PTRosa[p,m]       <-as.numeric(predict(Models[[k]],X_trainm[p,]))
                X_trainm[p+1,Lag1]<-PTRosa[p,m]
                X_trainm[p+2,Lag2]<-PTRosa[p,m]
                X_trainm[p+3,Lag3]<-PTRosa[p,m]
                X_trainm[p+4,Lag4]<-PTRosa[p,m]
                X_trainm[p+5,Lag5]<-PTRosa[p,m]
                X_trainm[p+6,Lag6]<-PTRosa[p,m]
              }
            }
            #Test
            for(p in 1:(n-cut))
            {
              if(p%%Horizon !=1)
              {
                PTEosa[p,m]<-as.numeric(predict(Models[[k]],X_testm[p,]))
                X_testm[p+1,Lag1]<-PTEosa[p,m]
                X_testm[p+2,Lag2]<-PTEosa[p,m]
                X_testm[p+3,Lag3]<-PTEosa[p,m]
                X_testm[p+4,Lag4]<-PTEosa[p,m]
                X_testm[p+5,Lag5]<-PTEosa[p,m]
                X_testm[p+6,Lag6]<-PTEosa[p,m]
              }
              else
              {
                X_testm[p:(n-cut),]<-X_test[p:(n-cut),]
                PTEosa[p,m]       <-as.numeric(predict(Models[[k]],X_testm[p,]))
                X_testm[p+1,Lag1]<-PTEosa[p,m]
                X_testm[p+2,Lag2]<-PTEosa[p,m]
                X_testm[p+3,Lag3]<-PTEosa[p,m]
                X_testm[p+4,Lag4]<-PTEosa[p,m]
                X_testm[p+5,Lag5]<-PTEosa[p,m]
                X_testm[p+6,Lag6]<-PTEosa[p,m]
              }
            }
            
            k<-k+1
            
            cat("State:",Dataset,"Component:",i,"Horizon:",Horizon,"Model:",models[m],"\n")
            
          }
        }
        else             #14-steps-ahead
        {
          data_dec1[[i]]<-data.frame(Lags_Outputs)
          #----------------------Divis?o em treinamento e teste---------------------#
          n      <-dim(data_dec1[[i]])[1]
          cut    <-n-14
          
          fitControl<- trainControl(method         = "timeslice",
                                    initialWindow  = floor(cut*0.9), #69% 
                                    horizon        = 14,              #3% 
                                    fixedWindow    = TRUE,
                                    allowParallel  = TRUE,
                                    savePredictions="final")
          
          
          train  <-data.frame(data_dec1[[i]][(1:cut),],Lags_Inputs[(1:cut),Lags_InputsH2]);
          Y_train<-train[,1];X_train<-train[,-1]
          test   <-tail(data.frame(data_dec1[[i]],Lags_Inputs[,Lags_InputsH2]),n-cut)
          Y_test <-test[,1]; X_test <-test[,-1]
          
          #Objects
          {
            #-------------------------DSA------------------------------------------------#
            PTRdsa  <-matrix(nrow=cut,ncol=length(models));    #Recebe predi??es DSA das componentes
            PTEdsa  <-matrix(nrow=n-cut,ncol=length(models));  #Recebe predi??es DSA das componentes
            
            PTrDSA <-matrix(nrow=cut,ncol=dim(combs)[1]+length(models)+1);     #Recebe predi??es DSA Combinadas 
            ETrDSA <-matrix(nrow=cut ,ncol=dim(combs)[1]+length(models)+1);    #Recebe erros DSA Combinadas 
            PTeDSA <-matrix(nrow=n-cut ,ncol=dim(combs)[1]+length(models)+1);  #Recebe predi??es DSA Combinadas 
            ETeDSA <-matrix(nrow=n-cut  ,ncol=dim(combs)[1]+length(models)+1); #Recebe erros DSA Combinadas
            
            PTRFDSA   <-matrix(nrow=cut,ncol=2*length(models)+1);              #Recebe os Objetos finais DSA
            ETRFDSA   <-matrix(nrow=cut,ncol=2*length(models)+1);
            
            PTEFDSA   <-matrix(nrow=n-cut,ncol=2*length(models)+1);
            ETEFDSA   <-matrix(nrow=n-cut,ncol=2*length(models)+1);
            
          }
          
          for(m in 1:length(models))
          {
            X_trainm<-train[,-1]; X_testm <-test[,-1]
            

            if(m==1)
            {
              set.seed(1234)
              Models[[k]]<-train(X_trainm,Y_train,
                                 method=ELMmodell,
                                 preProcess = c("center","scale"),
                                 tuneLength= 3,
                                 trControl = fitControl,verbose=FALSE)
            }
            else
            {
              #-----------------------Treinamento----------------------------------------#
              set.seed(1234)
              Models[[k]]<-train(X_trainm,as.numeric(Y_train),
                                 method=models[m],
                                 preProcess = c("center","scale"),
                                 tuneLength = 3,
                                 trControl = fitControl,verbose=FALSE,importance=TRUE)
            }
            
            #-----------------------Save Hyperparameters--------------------------------#
            Params[[k]]<-Models[[k]]$bestTune
            Importances[[k]]<-varImp(Models[[k]], scale = FALSE)
            
            
            #--------------------------Predictions 14-steps---------------------------------#
            
            #Train
            for(p in 1:cut)
            {
              if(p%%Horizon !=1)
              {
                PTRdsa[p,m]<-as.numeric(predict(Models[[k]],X_trainm[p,]))
                X_trainm[p+1,Lag1]<-PTRdsa[p,m]
                X_trainm[p+2,Lag2]<-PTRdsa[p,m]
                X_trainm[p+3,Lag3]<-PTRdsa[p,m]
                X_trainm[p+4,Lag4]<-PTRdsa[p,m]
                X_trainm[p+5,Lag5]<-PTRdsa[p,m]
                X_trainm[p+6,Lag6]<-PTRdsa[p,m]
                X_trainm[p+7,Lag7]<-PTRdsa[p,m]
                
              }
              else
              {
                X_trainm[p:(n-cut),]<-X_train[p:(n-cut),]
                PTRdsa[p,m]       <-as.numeric(predict(Models[[k]],X_trainm[p,]))
                X_trainm[p+1,Lag1]<-PTRdsa[p,m]
                X_trainm[p+2,Lag2]<-PTRdsa[p,m]
                X_trainm[p+3,Lag3]<-PTRdsa[p,m]
                X_trainm[p+4,Lag4]<-PTRdsa[p,m]
                X_trainm[p+5,Lag5]<-PTRdsa[p,m]
                X_trainm[p+6,Lag6]<-PTRdsa[p,m]
                X_trainm[p+7,Lag7]<-PTRdsa[p,m]
                
              }
            }
            #Test
            for(p in 1:(n-cut))
            {
              if(p%%Horizon !=1)
              {
                PTEdsa[p,m]<-as.numeric(predict(Models[[k]],X_testm[p,]))
                X_testm[p+1,Lag1]<-PTEdsa[p,m]
                X_testm[p+2,Lag2]<-PTEdsa[p,m]
                X_testm[p+3,Lag3]<-PTEdsa[p,m]
                X_testm[p+4,Lag4]<-PTEdsa[p,m]
                X_testm[p+5,Lag5]<-PTEdsa[p,m]
                X_testm[p+6,Lag6]<-PTEdsa[p,m]
                X_testm[p+7,Lag7]<-PTEdsa[p,m]
                
              }
              else
              {
                X_testm[p:(n-cut),]<-X_test[p:(n-cut),]
                PTEdsa[p,m]       <-as.numeric(predict(Models[[k]],X_testm[p,]))
                X_testm[p+1,Lag1]<-PTEdsa[p,m]
                X_testm[p+2,Lag2]<-PTEdsa[p,m]
                X_testm[p+3,Lag3]<-PTEdsa[p,m]
                X_testm[p+4,Lag4]<-PTEdsa[p,m]
                X_testm[p+5,Lag5]<-PTEdsa[p,m]
                X_testm[p+6,Lag6]<-PTEdsa[p,m]
                X_testm[p+7,Lag7]<-PTEdsa[p,m]
                
              }
            }
            
            k<-k+1
            cat("State:",Dataset,"Component:",i,"Horizon:",Horizon,"Model:",models[m],"\n")
            
          }
        }
       
      }
      
      PTROSA[[i]]<-PTRosa[1:cut,];PTEOSA[[i]]<-PTEosa[1:(n-cut),];
      PTRDSA[[i]]<-PTRdsa[1:cut,];PTEDSA[[i]]<-PTEdsa[1:(n-cut),];
    }
    
    #----------------------------Combina??o-----------------------------------#
    for(c in 1:dim(combs)[1]) #Varia??o pro n?mero de combina??es
    {
      
      ##############################################################################   
      thetas[c,]<-rep(1,times=dim(data_dec)[2])
      
      ###################Predictions for combined model##########################
      #----------------------------OSA-----------------------------------#
      PTrOSA[1:cut,c]<-PTROSA[[1]][1:cut,combs[c,1]]*thetas[c,1]+ #Comp 1
        PTROSA[[2]][1:cut,combs[c,2]]*thetas[c,2]+ #Comp 2
        PTROSA[[3]][1:cut,combs[c,3]]*thetas[c,3]+ #Comp 3
        PTROSA[[4]][1:cut,combs[c,4]]*thetas[c,4]+  #Comp 4
        PTROSA[[5]][1:cut,combs[c,5]]*thetas[c,5]+  #Comp 5
        PTROSA[[6]][1:cut,combs[c,6]]*thetas[c,6]
        
      PTeOSA[1:(n-cut),c]<-PTEOSA[[1]][1:(n-cut),combs[c,1]]*thetas[c,1]+
        PTEOSA[[2]][1:(n-cut),combs[c,2]]*thetas[c,2]+
        PTEOSA[[3]][1:(n-cut),combs[c,3]]*thetas[c,3]+
        PTEOSA[[4]][1:(n-cut),combs[c,4]]*thetas[c,4]+
        PTEOSA[[5]][1:(n-cut),combs[c,5]]*thetas[c,5]+
        PTEOSA[[6]][1:(n-cut),combs[c,6]]*thetas[c,6]
      
      #---------------------------DSA-------------------------------------#
      PTrDSA[1:cut,c]<-PTRDSA[[1]][1:cut,combs[c,1]]*thetas[c,1]+
        PTRDSA[[2]][1:cut,combs[c,2]]*thetas[c,2]+
        PTRDSA[[3]][1:cut,combs[c,3]]*thetas[c,3]+
        PTRDSA[[4]][1:cut,combs[c,4]]*thetas[c,4]+
        PTRDSA[[5]][1:cut,combs[c,5]]*thetas[c,5]+
        PTRDSA[[6]][1:cut,combs[c,6]]*thetas[c,6]
      
      PTeDSA[1:(n-cut),c]<-PTEDSA[[1]][1:(n-cut),combs[c,1]]*thetas[c,1]+
        PTEDSA[[2]][1:(n-cut),combs[c,2]]*thetas[c,2]+
        PTEDSA[[3]][1:(n-cut),combs[c,3]]*thetas[c,3]+
        PTEDSA[[4]][1:(n-cut),combs[c,4]]*thetas[c,4]+
        PTEDSA[[5]][1:(n-cut),combs[c,5]]*thetas[c,5]+
        PTEDSA[[6]][1:(n-cut),combs[c,6]]*thetas[c,6]
     
      ######################################Errors############################
      #----------------------------OSA-----------------------------------#
      ETrOSA[,c]<-as.vector(Output[15:(cut+14)] -PTrOSA[1:cut,c])
      ETeOSA[,c]<-as.vector(Output[(cut+15):length(Output)]-PTeOSA[1:(n-cut),c])
      #----------------------------DSA-----------------------------------#
      ETrDSA[,c]<-as.vector(Output[15:(cut+14)] -PTrDSA[1:cut,c])
      ETeDSA[,c]<-as.vector(Output[(cut+15):length(Output)]-PTeDSA[1:(n-cut),c])
      
         
      ######################################################################
      ###################Performance Measures###############################
      #----------------------------OSA-----------------------------------#
      
      POSA[c,1:4]<-PM(Output[15:(cut+14)]  ,PTrOSA[1:cut,c])
      POSA[c,6:9]<-PM(Output[(cut+15):length(Output)],PTeOSA[1:(n-cut),c])
      POSA[c,5]  <-POSA[c,1]/mean(Output[15:(cut+14)])
      POSA[c,10] <-POSA[c,6]/mean(Output[(cut+15):length(Output)])
      POSA[c,11]  <-c
      #----------------------------DSA-----------------------------------#
      PDSA[c,1:4]<-PM(Output[15:(cut+14)]  ,PTrDSA[1:cut,c])
      PDSA[c,6:9]<-PM(Output[(cut+15):length(Output)],PTeDSA[1:(n-cut),c])
      PDSA[c,5]  <-PDSA[c,1]/mean(Output[15:(cut+14)])
      PDSA[c,10] <-PDSA[c,6]/mean(Output[(cut+15):length(Output)])
      PDSA[c,11] <-c
      
        }
    
    #------------------------Modelo sem decomposi??o-----------------------------------------#
    {
      Lags_Outputs<-lags(Data_Decomp[[s]][,"new_confirmed"], n = 14)                   #Lags for output
      Lags_Outputs<-Lags_Outputs[,1:8]
      colnames(Lags_Outputs)<-c("Confirmed",paste("Lag",1:7,sep="")) #Lags' names for ouput
      
      for(Horizonte in 1:2)
      {
        Horizon<-FH[Horizonte]
        
        if(Horizon==7)
        {
          train  <-data.frame(Lags_Outputs[(1:cut),],Lags_Inputs[(1:cut),Lags_InputsH1]);
          Y_train<-train[,1];X_train<-train[,-1]
          test   <-tail(data.frame(Lags_Outputs,Lags_Inputs[,Lags_InputsH1]),n-cut)
          Y_test <-test[,1]; X_test <-test[,-1]
          
          for(m in 1:(length(models)))
          {
            X_trainm<-train[,-1]; X_testm <-test[,-1]
            
            if(m==1)
            {
              set.seed(1234)
              Models[[k]]<-train(X_trainm,Y_train,
                                 method=ELMmodell,
                                 preProcess = c("center","scale"),
                                 tuneLength= 3,
                                 trControl = fitControl,verbose=FALSE)
            }
            else 
            {
              #-----------------------Treinamento----------------------------------------#
              set.seed(1234)
              Models[[k]]<-train(X_trainm,as.numeric(Y_train),
                                 method=models[m],
                                 preProcess = c("center","scale"),
                                 tuneLength = 3,
                                 trControl = fitControl,verbose=FALSE,importance=TRUE)
            }
            
            #-----------------------Save Hyperparameters--------------------------------#
            Params[[k]]<-Models[[k]]$bestTune
            Importances[[k]]<-varImp(Models[[k]], scale = FALSE)
            #-----------------------Predictions OSA-------------------------------------#
            
            for(p in 1:cut)
            {
              if(p%%Horizon !=1)
              {
                PTrOSA[p,c+m]<-as.numeric(predict(Models[[k]],X_trainm[p,]))
                X_trainm[p+1,Lag1]<-PTrOSA[p,c+m]
                X_trainm[p+2,Lag2]<-PTrOSA[p,c+m]
                X_trainm[p+3,Lag3]<-PTrOSA[p,c+m]
                X_trainm[p+4,Lag4]<-PTrOSA[p,c+m]
                X_trainm[p+5,Lag5]<-PTrOSA[p,c+m]
                X_trainm[p+6,Lag6]<-PTrOSA[p,c+m]
              }
              else
              {
                X_trainm[p:(n-cut),]<-X_train[p:(n-cut),]
                PTrOSA[p,c+m]       <-as.numeric(predict(Models[[k]],X_trainm[p,]))
                X_trainm[p+1,Lag1]<-PTrOSA[p,c+m]
                X_trainm[p+2,Lag2]<-PTrOSA[p,c+m]
                X_trainm[p+3,Lag3]<-PTrOSA[p,c+m]
                X_trainm[p+4,Lag4]<-PTrOSA[p,c+m]
                X_trainm[p+5,Lag5]<-PTrOSA[p,c+m]
                X_trainm[p+6,Lag6]<-PTrOSA[p,c+m]
              }
            }
            #Test
            for(p in 1:(n-cut))
            {
              if(p%%Horizon !=1)
              {
                PTeOSA[p,c+m]<-as.numeric(predict(Models[[k]],X_testm[p,]))
                X_testm[p+1,Lag1]<-PTeOSA[p,c+m]
                X_testm[p+2,Lag2]<-PTeOSA[p,c+m]
                X_testm[p+3,Lag3]<-PTeOSA[p,c+m]
                X_testm[p+4,Lag4]<-PTeOSA[p,c+m]
                X_testm[p+5,Lag5]<-PTeOSA[p,c+m]
                X_testm[p+6,Lag6]<-PTeOSA[p,c+m]
              }
              else
              {
                X_testm[p:(n-cut),]<-X_test[p:(n-cut),]
                PTeOSA[p,c+m]       <-as.numeric(predict(Models[[k]],X_testm[p,]))
                X_testm[p+1,Lag1]<-PTeOSA[p,c+m]
                X_testm[p+2,Lag2]<-PTeOSA[p,c+m]
                X_testm[p+3,Lag3]<-PTeOSA[p,c+m]
                X_testm[p+4,Lag4]<-PTeOSA[p,c+m]
                X_testm[p+5,Lag5]<-PTeOSA[p,c+m]
                X_testm[p+6,Lag6]<-PTeOSA[p,c+m]
              }
            }
            
            k<-k+1
            cat("State",Dataset,"Horizon:",Horizon,"Model:",models[m],"\n")
          }
        }
        else 
        {
          train  <-data.frame(Lags_Outputs[(1:cut),],Lags_Inputs[(1:cut),Lags_InputsH2]);
          Y_train<-train[,1];X_train<-train[,-1]
          test   <-tail(data.frame(Lags_Outputs,Lags_Inputs[,Lags_InputsH2]),n-cut)
          Y_test <-test[,1]; X_test <-test[,-1]
          
          for(m in 1:length(models))
          {
            X_trainm<-train[,-1]; X_testm <-test[,-1]
            
            if(m==1)
            {
              set.seed(1234)
              Models[[k]]<-train(X_trainm,Y_train,
                                 method=ELMmodell,
                                 preProcess = c("center","scale"),
                                 tuneLength= 3,
                                 trControl = fitControl,verbose=FALSE)
            }
            else
            {
              #-----------------------Treinamento----------------------------------------#
              set.seed(1234)
              Models[[k]]<-train(X_trainm,as.numeric(Y_train),
                                 method=models[m],
                                 preProcess = c("center","scale"),
                                 tuneLength = 3,
                                 trControl = fitControl,verbose=FALSE,importance=TRUE)
            }
            
            #-----------------------Save Hyperparameters--------------------------------#
            Params[[k]]<-Models[[k]]$bestTune
            Importances[[k]]<-varImp(Models[[k]], scale = FALSE)
            
            #------------------------Predictions 14-steps-ahead------------------------------------#
            
            for(p in 1:cut)
            {
              if(p%%Horizon !=1)
              {
                PTrDSA[p,c+m]<-as.numeric(predict(Models[[k]],X_trainm[p,]))
                X_trainm[p+1,Lag1]<-PTrDSA[p,c+m]
                X_trainm[p+2,Lag2]<-PTrDSA[p,c+m]
                X_trainm[p+3,Lag3]<-PTrDSA[p,c+m]
                X_trainm[p+4,Lag4]<-PTrDSA[p,c+m]
                X_trainm[p+5,Lag5]<-PTrDSA[p,c+m]
                X_trainm[p+6,Lag6]<-PTrDSA[p,c+m]
              }
              else
              {
                X_trainm[p:(n-cut),]<-X_train[p:(n-cut),]
                PTrDSA[p,c+m]       <-as.numeric(predict(Models[[k]],X_trainm[p,]))
                X_trainm[p+1,Lag1]<-PTrDSA[p,c+m]
                X_trainm[p+2,Lag2]<-PTrDSA[p,c+m]
                X_trainm[p+3,Lag3]<-PTrDSA[p,c+m]
                X_trainm[p+4,Lag4]<-PTrDSA[p,c+m]
                X_trainm[p+5,Lag5]<-PTrDSA[p,c+m]
                X_trainm[p+6,Lag6]<-PTrDSA[p,c+m]
              }
            }
            #Test
            for(p in 1:(n-cut))
            {
              if(p%%Horizon !=1)
              {
                PTeDSA[p,c+m]<-as.numeric(predict(Models[[k]],X_testm[p,]))
                X_testm[p+1,Lag1]<-PTeDSA[p,c+m]
                X_testm[p+2,Lag2]<-PTeDSA[p,c+m]
                X_testm[p+3,Lag3]<-PTeDSA[p,c+m]
                X_testm[p+4,Lag4]<-PTeDSA[p,c+m]
                X_testm[p+5,Lag5]<-PTeDSA[p,c+m]
                X_testm[p+6,Lag6]<-PTeDSA[p,c+m]
              }
              else
              {
                X_testm[p:(n-cut),]<-X_test[p:(n-cut),]
                PTeDSA[p,c+m]       <-as.numeric(predict(Models[[k]],X_testm[p,]))
                X_testm[p+1,Lag1]<-PTeDSA[p,c+m]
                X_testm[p+2,Lag2]<-PTeDSA[p,c+m]
                X_testm[p+3,Lag3]<-PTeDSA[p,c+m]
                X_testm[p+4,Lag4]<-PTeDSA[p,c+m]
                X_testm[p+5,Lag5]<-PTeDSA[p,c+m]
                X_testm[p+6,Lag6]<-PTeDSA[p,c+m]
              }
            }
            
            k<-k+1
            cat("State:",Dataset,"Horizon:",Horizon,"Model:",models[m],"\n")
            
          }
        }
       
      }
      
      #ARIMA model
      
      Arimas        <-auto.arima(Y_train)
      predictions_arima  <-forecast::forecast(Arimas, h=14)
      PTrOSA[,c+m+1]    <-round(c(Arimas$fitted))
      PTrDSA[,c+m+1]    <-round(c(Arimas$fitted))
      
      PTeOSA[,c+m+1]    <-rep(round(c(predictions_arima$mean[1:7])),times=2)
      PTeDSA[,c+m+1]    <-round(c(predictions_arima$mean[1:14]))
    }
    
    
    for(m in 1:(length(models)+1))
    {
      ###################################Performance#####################################
      {
        #----------------------------------OSA---------------------------------------#
        
        ETrOSA[,c+m]<-as.vector(Output[15:(cut+14)] -PTrOSA[1:cut,c+m])
        ETeOSA[,c+m]<-as.vector(Output[(cut+15):length(Output)]-PTeOSA[1:(n-cut),c+m])
        #----------------------------DSA-----------------------------------#
        ETrDSA[,c+m]<-as.vector(Output[15:(cut+14)] -PTrDSA[1:cut,c+m])
        ETeDSA[,c+m]<-as.vector(Output[(cut+15):length(Output)]-PTeDSA[1:(n-cut),c+m])
        
  
          #--------------------Performance Measures---------------------#
         POSA[c+m,1:4]<-PM(Output[15:(cut+14)]  ,PTrOSA[1:cut,c+m]) #O 14 se refere ao n?merode lags adotados
         POSA[c+m,6:9]<-PM(Output[(cut+15):length(Output)],PTeOSA[1:(n-cut),c+m])
         POSA[c+m,5]  <-POSA[c+m,1]/mean(Output[15:(cut+14)])
         POSA[c+m,10] <-POSA[c+m,6]/mean(Output[(cut+15):length(Output)])
         POSA[c+m,11]  <-c+m
          
         PDSA[c+m,1:4]<-PM(Output[15:(cut+14)]  ,PTrDSA[1:cut,c+m]) #O 14 se refere ao n?merode lags adotados
         PDSA[c+m,6:9]<-PM(Output[(cut+15):length(Output)],PTeDSA[1:(n-cut),c+m])
         PDSA[c+m,5]  <-PDSA[c+m,1]/mean(Output[15:(cut+14)])
         PDSA[c+m,10] <-PDSA[c+m,6]/mean(Output[(cut+15):length(Output)])
         PDSA[c+m,11]  <-c+m
  
      }
      
    }
    
    
    ######################################################################## 
    #Final Objects
    
    #Find the minimum RMSE
    msOSA<-POSA[which.min(POSA[,6]),11]
    MFOSA<-c(combs[POSA[which.min(POSA[,6]),11],])
    msDSA<-PDSA[which.min(PDSA[,6]),11]
    MFDSA<-c(combs[PDSA[which.min(PDSA[,6]),11],])
       #---------------------------------Final Remarks------------------------------# 
    {
      linesOSA  <-c(as.numeric(r),POSA[which.min(POSA[,6]),11],seq(c+1,c+length(models)+1,1))
      PTRFOSA   <-PTrOSA[,linesOSA];  PTEFOSA   <-PTeOSA[,linesOSA]
      ETRFOSA   <-ETrOSA[,linesOSA];  ETEFOSA   <-ETeOSA[,linesOSA];  PFOSA     <-POSA[linesOSA,]

      colnames(PTRFOSA)<-row.names(PFOSA);    colnames(PTEFOSA)<-row.names(PFOSA)
      colnames(ETEFOSA)<-row.names(PFOSA);    colnames(ETRFOSA)<-row.names(PFOSA)

      names        <-c(paste(MDecomp,models,sep="--"))
      row.names(PFOSA)<-c(names,paste(MDecomp,"Proposed",sep="--"),models,"ARIMA")
      PFSOSA    <-PFOSA[order(PFOSA[,6],decreasing=FALSE),]

      linesDSA  <-c(as.numeric(r),PDSA[which.min(PDSA[,6]),11],seq(c+1,c+length(models)+1,1))
      PTRFDSA   <-PTrDSA[,linesDSA];  PTEFDSA   <-PTeDSA[,linesDSA]
      ETRFDSA   <-ETrDSA[,linesDSA];  ETEFDSA   <-ETeDSA[,linesDSA];  PFDSA     <-PDSA[linesDSA,]

      colnames(PTRFDSA)<-row.names(PFDSA);    colnames(PTEFDSA)<-row.names(PFDSA)
      colnames(ETEFDSA)<-row.names(PFDSA);    colnames(ETRFDSA)<-row.names(PFDSA)

      row.names(PFDSA)<-c(names,paste(MDecomp,"Proposed",sep="--"),models,"ARIMA")
      PFSDSA    <-PFDSA[order(PFDSA[,6],decreasing=FALSE),]
      
    }
    
    Stability<-do.call(cbind.data.frame,list(Std_OSA=apply(ETEFOSA,2,sd),
                    Std_DSA=apply(ETEFDSA,2,sd)))
    row.names(Stability)<-row.names(ETEFOSA)
  
    
  #-----------------------------Plots--------------------------------#
    #Set Language
    Sys.setenv("LANGUAGE"="En")
    Sys.setlocale("LC_ALL", "English")
    
    PREDS_long<-data.frame(Predicted   = c(PTEFOSA[,which(linesOSA %in% PFSOSA[1,11])[1]],
                                           PTEFDSA[,which(linesDSA %in% PFSDSA[1,11])[1]],
                                           Output[(cut+15):length(Output)]),
                           Data     =rep(c("7-Days-ahead","14-Days-ahead","Observed"),each=dim(PTEFOSA)[1]),
                           Date     =rep(tail(Data_Decomp[[s]][,"date"],14),times=3))
    
    #Plot predicted versus observed
    
    PREDS_long%>%
      ggplot(aes(x = Date, y = Predicted, colour=Data)) + 
      geom_line(aes(linetype=Data,colour=Data),size=1) +
      scale_color_manual(values=c("#FF0000","#00FF00","#000000")) + 
      scale_linetype_manual(values=c("dashed","dotdash","longdash"))+
      xlab("Date") +  ylab("COVID-19 Incidence")+
      theme_bw() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="transparent",colour=NA),
        legend.text = element_text(size=16),
        text = element_text(family = "Times New Roman", size = 16),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.grid.minor = element_blank())+
    scale_x_date(date_labels = "%b %d", date_breaks = "2 days")
       
   
     #Save eps
    setwd(FiguresDir)
    ggsave(paste0(Dataset,"-PO.eps",sep=""), device=cairo_ps,width = 12,height = 6.75,dpi = 1200)
    
    #Save pdf
    ggsave(paste0(Dataset,"-PO.pdf",sep=""),device = cairo_pdf, width = 9,height = 6.75,units = "in",
      dpi = 300) 
   
  #------------------Resultado---------------------------------------#
    cat("Results",Dataset,"Decomposition",MDecomp,"\n")
    print(list("H=7"=as.data.frame(PFSOSA[,6:11]),
               "H=14"=as.data.frame(PFSDSA[,6:11])))
    
    Results7SA<-list(PTrOSA,PTeOSA,PTRFOSA,PTEFOSA,ETRFOSA,ETEFOSA,Params,Models,data_dec,PFSOSA,POSA,Importances,PTROSA,PTEOSA,Stability)
    Results14SA<-list(PTrDSA,PTeDSA,PTRFDSA,PTEFDSA,ETRFDSA,ETEFDSA,Params,Models,data_dec,PFSDSA,PDSA,Importances,PTRDSA,PTEDSA)

    formOSA   <-paste("Results_",Dataset,"_",MDecomp,"_",Sys.Date(),"_7SA.RData", sep='')
    formDSA   <-paste("Results_",Dataset,"_",MDecomp,"_",Sys.Date(),"_14SA.RData", sep='')

    setwd(ResultsDir)
    save(Results7SA,file=formOSA)
    save(Results14SA,file=formDSA)

    
    measures_name<-paste("Metrics_",Dataset,"_",MDecomp,"_",Sys.Date(),".xlsx", sep='')
    
    Measures<-do.call(rbind.data.frame,
                      list("H=7"=as.data.frame(PFSOSA[,6:11]),
                           "H=14"=as.data.frame(PFSDSA[,6:11])))
    write.xlsx(Measures,measures_name)
  }
}

#######################Plots#################################
setwd(ResultsDir)

#Load RData from out of dropbox
list_files <- list.files(pattern = ".RData")

importances_ind  <-list()
importances<-list()
stabilities<-list()
var_names  <-c("Y[t-1]","Y[t-2]","Y[t-3]","Y[t-4]","Y[t-5]","Y[t-6]","Y[t-7]",
                "X[1]","X[2]","X[3]","X[4]","X[5]","X[6]")
for(i in 1:20)
{
  Results        <-get(load(list_files[i]))
  
  if(i%%2 == 0)
  {
    stabilities[[k]]<-Results[[15]]
  }
  
  Models          <-Results[[8]]
  k<-1
  for(j in 1:length(Models))
  {
    inf                 <-varImp(Models[[j]], scale = FALSE)$importance
    df                  <-data.frame(Variables=row.names(inf),Import=inf[,1])
    importances_ind[[k]]<-df[order(df$Variables), ]
    k<-k+1 
  }
  aux                   <-do.call(cbind.data.frame, importances_ind)
  excluir               <-c("Variables")
  aux1                  <-aux[,!(names(aux) %in% excluir), drop = FALSE]
  row.names(aux1)       <-var_names
  #aux1$Average          <-apply(aux1,1,mean)
  #aux1$Std              <-apply(aux1,1,sd)
  importances[[i]]       <-aux1
}

importances_final<-do.call(cbind.data.frame, importances)
importances_final$Average<-apply(importances_final[,1:840],1,mean)
importances_final$Std    <-apply(importances_final[,1:840],1,sd)/sqrt(840)

#Average and errors for each input

data<-data.frame(importances_final[,c("Average","Std")],variables=row.names(importances_final))

data %>%
  ggplot(aes(reorder(variables, Average), Average)) +
  geom_col(aes(fill = Average), color = "black") +
  geom_errorbar(
    aes(ymin = Average - Std, ymax = Average + Std),
    width = .2,
    position = position_dodge(.9)
  ) +
  ylab("Average Importance") + xlab("Variable") +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.direction = "vertical",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    legend.text = element_text(size = 15),
    legend.position = "none",
    legend.background = element_rect(
      fill = "transparent",
      linetype = "solid",
      colour = 'black'
    )
  ) +
  scale_fill_gradient2(low = "yellow",
                       high = "red") +
  coord_flip()  +
  scale_x_discrete(labels = rev(
    c(
      expression(Y[t - 1]),
      expression(Y[t - 2]),
      expression(Y[t - 3]),
      expression(Y[t - 4]),
      expression(Y[t - 7]),
      expression(Y[t - 5]),
      expression(Y[t - 6]),
      expression(X[4]),
      expression(X[1]),
      expression(X[5]),
      expression(X[3]),
      expression(X[2]),
      expression(X[6])
    )
  ))

# data %>% 
# ggplot(aes(reorder(variables, Average), Average)) + 
#   geom_col(aes(fill = Average),color="black") + 
#   geom_errorbar(aes(ymin=Average-Std, ymax=Average+Std), width=.2,
#                 position=position_dodge(.9)) +
#   ylab("Average Importance")+xlab("Variable")+
#   theme_bw() +
#   theme(text = element_text(family = "Times New Roman"),
#         legend.direction = "vertical",
#         axis.text=element_text(size=15),
#         axis.title=element_text(size=20),
#         axis.text.x = element_text(angle = 45, vjust = 0.5),
#         legend.text=element_text(size=15),
#         legend.position = "none",
#         legend.background = element_rect(fill="transparent",linetype="solid",colour='black'))+
#   scale_fill_gradient2(low = "yellow", 
#                        high = "red") + 
#   # scale_x_discrete(labels = rev(c(expression(Y[t-1]),
#   # expression(Y[t-2]),expression(Y[t-3]), expression(Y[t-4]),
#   # expression(Y[t-5]),expression(Y[t-6]),expression(Y[t-7]),
#   # expression(X[1]),expression(X[2]),expression(X[3]),
#   # expression(X[4]),expression(X[5]),expression(X[6]))))+
#   coord_flip() 

#Save eps
setwd(FiguresDir)
ggsave(paste0("Feature_Importance.eps",sep=""), device=cairo_ps,width = 12,height = 6.75,dpi = 1200)

#Save pdf
ggsave(paste0("Feature_Importance.pdf",sep=""),device = cairo_pdf, width = 9,height = 6.75,units = "in",
       dpi = 300) 


setwd(ResultsDir)
load("Results_MA_EEMD_2020-09-14_7SA.RData")

load("Results_MA_EEMD_2020-09-14_14SA.RData")
