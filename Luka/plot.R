Rookiestats<-read.csv("Rookie season stats.csv")
Rookiestats
dim(Rookiestats)
colnames(Rookiestats)[c(2, 12, 13, 14, 15, 16, 17, 18, 19, 22)] <- c("Rookie Season", "FG%", "3P", "3PA", 
                                                                     "3P%", "2P", "2PA", "2P%", "eFG%", "FT%")  
Rookiestats
library(ggplot2)
library(tidyquant)
col<-matrix(palette_dark())[,1][1:7]
ggplot(data=Rookiestats, aes(x=reorder(Player, -PTS), y=PTS)) +
  geom_bar(aes(fill=Player), stat="identity", color="black", show.legend=FALSE) +
  geom_label(aes(label=PTS)) +
  scale_fill_manual(values=col) +
  labs(title="NBA Rookie stats comparisons", 
       subtitle="How many points did they score in their first season?",
       x="Player", y="Points Per Game") +
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.background=element_blank(), 
        axis.line=element_line(colour="black"),
        axis.title.x=element_blank()) +
  ylim(0, 40) 
library(magick)
library(grid)
image <- image_read("jordan.jpg") 
grid.raster(image, x=0.143, y=0.77, height=0.2)
image <- image_read("doncic.jpg") 
grid.raster(image, x=0.27, y=0.64, height=0.2)
image <- image_read("james.jpg") 
grid.raster(image, x=0.4, y=0.64, height=0.2)
image <- image_read("durant.jpg") 
grid.raster(image, x=0.53, y=0.64, height=0.2)
image <- image_read("curry.jpg") 
grid.raster(image, x=0.655, y=0.58, height=0.2)
image <- image_read("harden.jpg") 
grid.raster(image, x=0.785, y=0.42, height=0.2)
image <- image_read("bryant.jpg") 
grid.raster(image, x=0.915, y=0.38, height=0.2)


#篮板
ggplot(data=Rookiestats, aes(x=reorder(Player, -TRB), y=TRB)) +
  geom_bar(aes(fill=Player), stat="identity", color="black", show.legend=FALSE) +
  geom_label(aes(label=TRB)) +
  scale_fill_manual(values=col)+
  labs(title="NBA Rookie stats comparisons", 
       subtitle="How many rebounds did they get in their first season?",
       x="Player", y="Rebounds Per Game") +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_blank(), axis.line=element_line(colour="black"),
        axis.title.x=element_blank()) +
  ylim(0, 15) 
image <- image_read("doncic.jpg") 
grid.raster(image, x=0.143, y=0.61, height=0.2)
image <- image_read("jordan.jpg") 
grid.raster(image, x=0.27, y=0.57, height=0.2)
image <- image_read("james.jpg") 
grid.raster(image, x=0.4, y=0.52, height=0.2)
image <- image_read("curry.jpg") 
grid.raster(image, x=0.53, y=0.47, height=0.2)
image <- image_read("durant.jpg") 
grid.raster(image, x=0.655, y=0.46, height=0.2)
image <- image_read("harden.jpg") 
grid.raster(image, x=0.785, y=0.39, height=0.2)
image <- image_read("bryant.jpg") 
grid.raster(image, x=0.915, y=0.33, height=0.2)


ggplot(data=Rookiestats, aes(x=reorder(Player, -AST), y=AST)) +
  geom_bar(aes(fill=Player), stat="identity", color="black", show.legend=FALSE) +
  geom_label(aes(label=AST)) +
  scale_fill_manual(values=col) +
  labs(title="NBA Rookie stats comparisons", 
       subtitle="How many assists did they make in their first season?",
       x="Player", y="Assists Per Game") +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_blank(), axis.line=element_line(colour="black"),
        axis.title.x=element_blank()) +
  ylim(0, 9) 
image <- image_read("james.jpg") 
grid.raster(image, x=0.15, y=0.72, height=0.2)
image <- image_read("doncic.jpg") 
grid.raster(image, x=0.27, y=0.72, height=0.2)
image <- image_read("jordan.jpg") 
grid.raster(image, x=0.4, y=0.72, height=0.2)
image <- image_read("curry.jpg") 
grid.raster(image, x=0.53, y=0.72, height=0.2)
image <- image_read("durant.jpg") 
grid.raster(image, x=0.655, y=0.44, height=0.2)
image <- image_read("harden.jpg") 
grid.raster(image, x=0.785, y=0.39, height=0.2)
image <- image_read("bryant.jpg") 
grid.raster(image, x=0.915, y=0.34, height=0.2)

nbaTwoSeason<-read.csv("18191920.csv")
nbaTwoSeason
nbaTwoSeason1<-nbaTwoSeason[,c(-2,-3,-4,-5,-31)]
fd<-nbaTwoSeason1[,c(1,7,10,13,14,17)]
colnames(fd)<-str_replace_all(colnames(fd),'X','')
colnames(fd)<-str_replace_all(colnames(fd),'\\.','%')
colnames(fd)
library(reshape2)
fd<-melt(fd)
fd
ggplot(fd,aes(x=Season,y=value,group=variable))+
  geom_line()+
  geom_point(size=5,color="red")+
  facet_wrap(~variable,nrow=1)+
  labs(x="",y="")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=60,vjust=0.5))

nbaTwoSeason1
df1<-nbaTwoSeason1[,c(1,2)]
df1<-melt(df1)
df1
p1<-ggplot(df1,aes(x=Season,y=value,group=variable))+
  geom_line()+
  geom_point(size=5,color="red")+
  labs(x="",y="")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=60,vjust=0.5))
df2<-nbaTwoSeason1[,c(1,2,4)]
df2$MP<-df2$MP/df2$G
df2<-melt(df2)
df2
p2<-ggplot(df2,aes(x=Season,y=value,group=variable))+
  geom_line()+
  geom_point(size=5,color="red")+
  facet_wrap(~variable,nrow=2)+
  labs(x="",y="")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=60,vjust=0.5))
p2
df3<-nbaTwoSeason1[,c(1,2,20:26)]
df4<-df3[,3:9]/df3$G
df4$Season<-df3$Season
df4
df4<-melt(df4)
p3<-ggplot(df4,aes(x=Season,y=value,group=variable))+
  geom_line()+
  geom_point(size=5,color="red")+
  facet_wrap(~variable,nrow=2)+
  labs(x="",y="")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=60,vjust=0.5))
ggpubr::ggarrange(p2,p3,ncol=2,nrow=1,widths = c(1,4))

df<-read.csv("nwe.csv",stringsAsFactors = F)
head(df)
dim(df)
df1<-df%>%
  select(c("Date","PTS","AST","TRB"))
head(df1)
df2<-df1[-c(26:29,48:54,57,61,73),]
df2$PTS<-as.numeric(df2$PTS)
df2$AST<-as.numeric(df2$AST)
df2$TRB<-as.numeric(df2$TRB)
dim(df2)
df3<-melt(df2)
df3
ggplot(df3,aes(x=Date, y=value, color=variable, group=variable)) +
  geom_line(show.legend=FALSE) +
  geom_point(show.legend=FALSE) +
  facet_grid(variable ~ ., scales="free") +
  geom_rect(aes(xmin=0, xmax=54.5, ymin=-Inf, ymax=Inf), 
            fill="darkseagreen1", alpha=0.01, show.legend=FALSE) +
  geom_rect(aes(xmin=54.5, xmax=61.5, ymin=-Inf, ymax=Inf), 
            fill="sandybrown", alpha=0.01, show.legend=FALSE) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5,size=5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(title="Luka Doncic stats - (2019-20)",
       subtitle="得分, 助攻 , 篮板
                                                                   复赛前                                                        复赛后")
head(df3)
