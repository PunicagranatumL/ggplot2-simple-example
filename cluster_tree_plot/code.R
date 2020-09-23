raw<-read.csv("hclust_practice_1.csv",header=T,row.names = 1)
head(raw)
hc<-hclust(dist(raw[,1:4]),method="average")
#install.packages("ggdendro")
library(ggdendro)
df<-dendro_data(hc,type="rectangle")
df1<-df$segments
df2<-df$labels
library(ggplot2)
ggplot()+
  geom_segment(data=df1,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_text(data=df2,aes(x=x,y=y-0.3,label=label),
            angle=90,size=3)+
  geom_point(data=df2,aes(x=x,y=y))+
  scale_y_continuous(expand=c(0.1,0))