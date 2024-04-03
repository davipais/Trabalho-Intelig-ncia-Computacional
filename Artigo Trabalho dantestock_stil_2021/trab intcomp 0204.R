tweets_stocks$sent=tweets_stocks$TRU
  
base =tweets_stocks[,c(1:11)]
base1=base[(base$NEUTRAL>-1),]

base2=base1[,c(2:6)]
base2$sent=base2$TRU+base2$JOY-base2$DIS-base2$SAD
base2$sentFator=rep(0, length(base2[,1]))
for(i in 1:length(base2[,1])){
  base2$sentFator[i]=ifelse(base2$sent[i]==0, "Neutro",
         ifelse(base2$sent[i]>0,"Positivo", "Negativo"))
}

baseFinal=base2[,c(1,7)]
