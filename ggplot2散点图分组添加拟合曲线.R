df<-read.csv("line_regression.csv",header=T,stringsAsFactors = F)
head(df)
dim(df)
library(ggplot2)
ggplot(df,aes(x=A,y=B))+
  geom_point(aes(color=D))+
  geom_smooth(aes(color=D),method = "lm",se=F)+
  annotate("text",x=2.5,y=45,
           label="atop(Y==3*X+20,R^2==0.9)",parse=T)+
  annotate("text",x=7.5,y=10,
           label="atop(Y==3*X+5,R^2==0.9)",parse=T)+
  theme_bw()+
  scale_color_manual(values=c("#00B2FF","orange"))+
  theme(legend.title = element_blank())+
  labs(x="time",y="value")