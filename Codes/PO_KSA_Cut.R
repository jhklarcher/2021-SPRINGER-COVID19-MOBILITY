PO_kSA_Cut<-function(OBS,OSA,DSA,TSA,DimTest)
{
  # OBS<-floor(dtype[[2]][4:309,1])
  # OSA<-c(R1SA[[3]][,5],R1SA[[4]][,5])
  # DSA<-c(R2SA[[3]][1:214,5],R2SA[[4]][1:92,5])
  # TSA<-c(R3SA[[3]][1:214,5],R3SA[[4]][1:92,5])
  
  all<-data.frame(OBS,OSA,DSA,TSA)
  
  Long<-data.frame(as.vector(unlist(all)),
                   rep(c("Observed","1-hour-ahead","12-hours-ahead","24-hours-ahead"),each=DimTest),
                   rep(c(rep(c("Test"),each=DimTest)),times=4),
                   rep(seq(1,DimTest,1),times=4))
  
  colnames(Long)<-c("Price","Models","Set","Date")
  
  Long$Models <- factor(Long$Models, levels = c("Observed","1-hour-ahead",
                                                "12-hours-ahead",
                                                "24-hours-ahead"))
  
  
  ggplot(Long, aes(x = Date, y = Price, colour=Models)) + 
    geom_line(aes(linetype=Models,colour=Models),size=1) +
    scale_color_manual(values=c("#0033FF","#000000","#FF0000","#00FF00")) + 
    scale_linetype_manual(values=c("solid", "solid","solid","solid"))+
    xlab("Samples (30 minutes)") +  ylab("Load (MW)")+
    theme_bw(base_size = 30)+
    theme(#legend.position = "bottom", 
      legend.direction = "vertical",
      legend.title = element_blank(),
      legend.position = c(0.55, 0.85),
      legend.background = element_rect(fill="transparent",colour=NA),
      legend.text = element_text(size=30),
      axis.text=element_text(size=30),
      text=element_text(family="Times New Roman"),
      axis.title=element_text(size=30))#+
    #geom_vline(aes(xintercept=as.numeric(Date[DimTrain])))+
    #annotate("text", x = Long$Date[0.5*DimTrain], y = min(round(Long[,1],2)), label = "Training ",size=8)+
    #annotate("text", x = Long$Date[0.9*(DimTrain+DimTest)], y = min(round(Long[,1],2)), label = "Test",size=8)
}

