library(xml2)
library(rvest)
library(reshape2)
library(ggplot2)
library(dplyr)

page<-read_html("../../new1.txt")
Jokic<-html_table(page,fill=T)
Jokic[[9]]
list(Jokic)
df1<-Jokic[[9]]
colnames(df1)
df1_1<-df1[1:5,]%>%
  select(Season,GS,MIN,FGA,"FG%")
df1_1

df1_1<-melt(df1_1)
head(df1_1)
ggplot(df1_1,aes(x=Season,y=value,color=variable))+
  geom_point(size=5)+
  geom_line(aes(group=1))+
  facet_wrap(~variable,scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="",y="")
head(df1)
df1_2<-df1[1:5,]%>%
  select(Season,PTS,REB,AST,TOV,BLK)
df1_2<-melt(df1_2)
ggplot(df1_2,aes(x=Season,y=value,color=variable))+
  geom_point(size=5)+
  geom_line(aes(group=1))+
  facet_wrap(~variable,scales = "free",ncol=3,nrow=2)+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=60,vjust=0.5))+
  labs(x="",y="")

df2<-read.csv("../../new2.txt",header=F,
              stringsAsFactors = F)
df2_1<-df2%>%
  select(V3,V9,V11,V13,V14,V24,V29)
colnames(df2_1)<-c("Game","w_or_l","MIN","FGA","FG%","AST","PTS")
head(df2_1)
df2_1$w_or_l<-stringr::str_sub(df2_1$w_or_l,1,1)
df2_1$MIN<-stringr::str_sub(df2_1$MIN,1,2)
df2_1$MIN<-as.numeric(df2_1$MIN)
head(df2_1)
df2_1<-melt(df2_1,ids=c("Game",'w_or_l'))
head(df2_1)
ggplot(df2_1,aes(x=Game,y=value,color=w_or_l))+
  geom_point(size=5)+
  geom_line(aes(group=1))+
  facet_grid(variable~.,scales = "free")+
  theme_bw()+
  theme(
        legend.title = element_blank(),
        axis.text.x = element_text(angle=60,vjust=0.5))+
  labs(x="",y="")

list(Jokic)
df3<-Jokic[[7]]
df3
colnames(df3)[1]<-"Game"
colnames(df3)
df3_1<-df3[1:2,]%>%
  select(Game,MPG,PPG,RPG,APG,BPG,TPG,"FG%","3P%","FT%")
df3_2<-melt(df3_1)
df3_2
df3_2$Game<-stringr::str_replace(df3_2$Game,"2019-20 ","")
ggplot(df3_2,aes(x=Game,y=value,fill=Game))+
  geom_bar(stat="identity",width=0.5)+
  facet_wrap(~variable,scales = "free")+
  theme_minimal()+labs(x="",y="")+
  theme(legend.position = "top",
        legend.title = element_blank())