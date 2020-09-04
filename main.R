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

packages<-c("quantmod","hht","forecast","metaheuristicOpt",
            "cowplot","Metrics","caret","elmNNRcpp","tcltk","TTR",
            "foreach","iterators","doParallel","lmtest")
sapply(packages,packs)

source("Metricas.R")
source("elm_caret.R")

library(extrafont)
#loadfonts(device="postscript")
loadfonts(device = "win", quiet = TRUE)

windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggplot2)
library(Cairo)

#Checa quantos núcleos existem
ncl<-detectCores();ncl

#Registra os clusters a serem utilizados
cl <- makeCluster(ncl-1);registerDoParallel(cl)
############################################################################

setwd(DataDir)

dados_all     <-read.table(file="covid19-mobility-data.csv",header=TRUE,sep=";",dec=".")
dados_states  <-split(dados_all,dados_all$state)

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
  
  set.seed(1234)
  #EEMD decomposition
  EEMD(data, time(data), noise.amp_eemd, trials_eemd, nimf_eemd, trials.dir = trials.dir,verbose=FALSE)
  result<- EEMDCompile(trials.dir, trials_eemd, nimf_eemd)
  
  data_final_eemd<-as.data.frame(cbind(result$averaged.imfs,result$averaged.residue)) 
  colnames(data_final_eemd)<-c(paste("IMF",1:(dim(data_final_eemd)[2]-1),sep=""),"Residual")
 
  
  Decomposed_data[[state]]<-data.frame(dados_states[[state]][,c(2:3,6,12:18)],data_final_eemd)
  cat('Decomposition for',names(dados_states)[state],'done! \n')
}

names(Decomposed_data)<-names(dados_states)

#Components plots

#Remove PA state
Decomposed_data<-Decomposed_data[names(Decomposed_data)[c(1:6,8:10)]]
setwd(FiguresDir)
for(state in 1:length(Decomposed_data))
{
  Dataset<-names(Decomposed_data)[state]
  Data   <-Decomposed_data[[state]][,c(2,3,11:16)]
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
          axis.text=element_text(size=20),
          axis.text.y = element_text(size=20),
          strip.text.x = element_text(size = 20),
          strip.text = element_text(size = 20),
          legend.direction = "horizontal",
          legend.title = element_blank(),
          #legend.position = "bottom",
          legend.position=c(0.3, 0.9),  
          legend.background = element_rect(fill="transparent",colour=NA),
          legend.text = element_text(size=20),
          text = element_text(family = "Times New Roman", size = 25),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.grid.minor = element_blank()) 
    }
  
  ggsave(nameeps, device=cairo_ps,width = 10,height = 6,dpi = 1200)
}

#Moving Average of Mobility Data

apply(Decomposed_data[[1]][,4:9],2,function(x){SMA(x,n=7,na.rm=TRUE)})
