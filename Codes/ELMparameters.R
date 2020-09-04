ELMpar<-function(p)
{
  nhid  <-round(as.numeric(p[1]),0)
  
  actfun<-ifelse(p[2] >= 10, funcoes[10],
                 ifelse(p[2] >= 9 ,funcoes[9],
                        ifelse(p[2]>= 8 ,funcoes[8],
                               ifelse(p[2] >= 7 ,funcoes[7],
                                      ifelse(p[2] >= 6 ,funcoes[6],
                                             ifelse(p[2] >= 5 ,funcoes[5],
                                                    ifelse(p[2] >= 4 ,funcoes[4],
                                                           ifelse(p[2] >= 3 ,funcoes[3],
                                                                  ifelse(p[2]>= 2 ,funcoes[2],funcoes[1]))))))))) 
  
  
  init_weights<-ifelse(as.numeric(p[3])>=3,tipo_inicializacao[1],
                       ifelse(as.numeric(p[3])>= 2 ,tipo_inicializacao[2],tipo_inicializacao[3])) 
  
  parameters<-c(nhid,actfun,init_weights)
  parameters
  }
