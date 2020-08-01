install.packages('aplot')
library(ggplot2)
library(aplot)
df<-read.csv('rscu.txt',header=F,stringsAsFactors = F)
p1<-ggplot(df,aes(fill=as.character(V4),x=V2,y=V3))+
  geom_bar(position = "stack",stat="identity")+
  theme_bw()+scale_y_continuous(expand=c(0,0),
                                limits = c(0,6.2))+
  theme(legend.position = "none")+labs(y="RSCU",x="")
  #geom_text(aes(label=V1),position=position_stack(vjust=0.5))
p1
p2<-ggplot(df,aes(x=V2,y=V4))+
  geom_label(aes(label=V1,fill=as.character(V4)),
             size=2)+
  labs(x="",y="")+ylim(3.4,6.1)+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
p1%>%
  insert_bottom(p2,height = 0.3)