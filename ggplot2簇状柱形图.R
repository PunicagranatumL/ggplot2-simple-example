#读入数据
getwd()
df<-read.csv("ggplot2_grouped_bar_plot/Grouped_bar_plot_data.csv",
             header=T,stringsAsFactors = F)
df
#加载
#install.packages("ggplot2")
library(ggplot2)
#最基本的簇状柱形图
ggplot(df,aes(x=Stage,y=value,fill=cultivar))+
  geom_bar(stat="identity",position="dodge")

#x轴分组按照自己想要的顺序
df$Stage<-factor(df$Stage,
                 levels = c("young_fruit","developing_fruit","mature_fruit"))

ggplot(df,aes(x=Stage,y=value,fill=cultivar))+
  geom_bar(stat="identity",position = "dodge")

#更改间距
ggplot(df,aes(x=Stage,y=value,fill=cultivar))+
  geom_bar(stat="identity",position = "dodge",width=0.5)

#手动填充颜色
ggplot(df,aes(x=Stage,y=value,fill=cultivar))+
  geom_bar(stat="identity",position = "dodge",width=0.5)+
  scale_fill_manual(values = c("#D55E00", "#0072B2"))

#移除灰色背景
ggplot(df,aes(x=Stage,y=value,fill=cultivar))+
  geom_bar(stat="identity",position = "dodge",width=0.5)+
  scale_fill_manual(values = c("#D55E00", "#0072B2"))+
  theme_bw()

#柱子贴底
ggplot(df,aes(x=Stage,y=value,fill=cultivar))+
  geom_bar(stat="identity",position = "dodge",width=0.5)+
  scale_fill_manual(values = c("#D55E00", "#0072B2"))+
  theme_bw()+
  scale_y_continuous(expand=c(0,0),limits=c(0,25))

#添加误差线
ggplot(df,aes(x=Stage,y=value,fill=cultivar))+
  geom_bar(stat="identity",position = "dodge",width=0.5)+
  scale_fill_manual(values = c("#D55E00", "#0072B2"))+
  theme_bw()+
  scale_y_continuous(expand=c(0,0),limits=c(0,25))+
  labs(x="",y="")+
  geom_errorbar(aes(ymin=value-sd,ymax=value+sd),width=0.2,
                position = position_dodge(0.5))
