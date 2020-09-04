PO_kSA<-function(OBS,OSA,DSA,TSA)
{
   #OBS<-dtype[[1]][4:288,1]
   #OSA<-c(C1SA[[3]][,5],C1SA[[4]][,5])
   #DSA<-c(C2SA[[3]][1:200,5],C2SA[[4]][1:85,5])
   #TSA<-c(C3SA[[3]][1:200,4],C3SA[[4]][1:85,5])
 
  Predicted<-data.frame(OBS,OSA,OBS,DSA,OBS,TSA)
  
  
  Long<-data.frame(as.vector(unlist(Predicted)),
                   rep(c("Observed","Predicted","Observed","Predicted","Observed","Predicted"),each=285),
                   rep(c("One-month","Two-months","Three-months"),each=2*285),
                   rep(c(rep(c("Training"),each=200),rep(c("Test"),each=85)),times=6),
                   rep(seq(from=as.Date("1996-04-01"), to=as.Date("2019-12-01"), by="month"),times=6))
  
 colnames(Long)<-c("Price","Type","Horizon","Set","Date")
 
 Long$Horizon <- factor(Long$Horizon, levels = c("One-month","Two-months","Three-months"))
 
 
 ggplot(Long, aes(Date, Price,colour=Type)) +
   geom_line(aes(colour=Type),size=1) +ylab("Electricity price (R$/MWh)")+xlab("Year")+
   #theme_bw(base_size = 30)+
   scale_color_manual(values=c("#0000CC","#000000"),labels = c("Predicted","Observed"))+
   facet_grid(rows = vars(Horizon))+
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
     panel.grid.minor = element_blank()) +
   geom_vline(aes(xintercept=as.numeric(Date[214])))+
   annotate("text", x = Long$Date[138], y = 0.25*max(round(Long[,1],2)), label = "Training ",size=10,family="Times New Roman")+
   annotate("text", x = Long$Date[268], y = 0.25*max(round(Long[,1],2)), label = "Test",size=10,family="Times New Roman")+
   scale_x_date(date_labels = "%Y")
}

