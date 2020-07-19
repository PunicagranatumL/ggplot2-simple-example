x<- 1:100
y1<- 3*x + 20 + rnorm(100,sd=1)
y2<- 3*x + 40 + rnorm(100,sd=1)
df<-data.frame(A=c(x,x),B=c(y1,y2),D=c(rep("A",100),rep("B",100)))
head(df)
library(ggplot2)
ggplot(df,aes(x=A,y=B,group=D))+
  geom_point(aes(color=D))+
  geom_line(aes(color=D))+
  geom_smooth(aes(color=D),method = "lm", se=T, 
              formula = y ~ x)+
  annotate("text",x=75,y=150,label="atop(Y==3*X+20,R^2==0.9)",
           parse=T,color="#00B2FF")+
  annotate("text",x=25,y=250,label="atop(Y==3*X+40,R^2==0.9)",
           parse=T,color="orange")+
  scale_color_manual(values=c("#00B2FF","orange"))+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(x="time",y="value")
