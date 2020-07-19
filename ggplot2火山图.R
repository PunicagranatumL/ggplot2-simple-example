DEGs<-read.csv("volcano_plot_example_data.csv",header=T,stringsAsFactors = F)
dim(DEGs)
df<-DEGs[-log10(DEGs$padj)>25,]
dim(df)
library(ggplot2)
library(ggrepel)
ggplot(DEGs,aes(x=log2FoldChange,y=-log10(padj)))+
  geom_point(aes(color=change),size=2.5,alpha=1,na.rm = T)+
  geom_hline(yintercept =-log10(0.05),color="#990000",
             linetype="dashed")+
  geom_vline(xintercept = -2,color="#990000",linetype="dashed")+
  geom_vline(xintercept = 2,color="#990000",linetype="dashed")+
  theme_bw(base_size = 14)+
  scale_color_manual(values=c("red","#00B2FF","orange"))+
  xlab(expression(log[2]("FoldChange")))+
  ylab(expression(-log[10]("padj")))+
  theme(legend.title = element_blank())+
  ggtitle(label = "Volcano Plot", 
          subtitle = "Colored by fold-change direction")+
  geom_label_repel(data=df,aes(x=log2FoldChange,
                          y=-log10(padj),
                          label=name,fill=change),
                   color="white",size=5)+
  scale_fill_manual(values=c("red","orange"))+
  guides(fill=F)
