library(ggplot2)
df= xlsx::read.xlsx("GO.xlsx",1)
df$GO<-as.factor(df$GO)
GO_term_order=factor(as.integer(rownames(df)),labels=df$Description)
ggplot(data=df, aes(x=GO_term_order,y=Count, fill=GO)) + geom_bar(stat="identity", width=0.8) + coord_flip() +  xlab("GO term") + ylab("Num of Genes") + theme_bw()
COLS <- c("#66C3A5", "#8DA1CB", "#FD8D62")

#不等距
ggplot(data=df, aes(x=GO_term_order,y=Count, fill=GO)) +
  geom_bar(stat="identity", width=0.8)  + 
  scale_fill_manual(values = COLS) + theme_bw()  +
  xlab("GO term") + ylab("Num of Genes") + labs(title = "The Most Enriched GO Terms")+ facet_wrap(vars(GO), strip.position = "bottom", scales = "free_x")+ theme(axis.line=element_line(color="black"),axis.text.x=element_text(angle = 80,vjust = 1, hjust = 1),plot.title = element_text(hjust = 0.5),strip.placement = "outside",panel.spacing = unit(0, "lines"),panel.border=element_blank(),strip.background=element_blank(),strip.switch.pad.wrap=unit(1,"cm"))

#等距1
ggplot(data=df, aes(x=GO_term_order,y=Count, fill=GO)) +
  geom_bar(stat="identity", width=0.8)  + 
  scale_fill_manual(values = COLS) + theme_bw()  +
  xlab("GO term") + ylab("Num of Genes") + labs(title = "The Most Enriched GO Terms")+ facet_grid(cols=vars(GO), scales = "free_x",space = "free",switch = "x")+ theme(axis.line=element_line(color="black"),axis.text.x=element_text(angle = 80,vjust = 1, hjust = 1),plot.title = element_text(hjust = 0.5),strip.placement = "outside",panel.spacing = unit(0, "lines"),panel.border=element_blank(),strip.background=element_blank() )

#等距2
ggplot(data=df, aes(x=GO_term_order,y=Count, fill=GO)) +
  geom_bar(stat="identity", width=0.8)  + 
  scale_fill_manual(values = COLS) + theme_bw()  +
  xlab("GO term") + ylab("Num of Genes") + labs(title = "The Most Enriched GO Terms")+ facet_grid(cols=vars(GO), scales = "free_x",space = "free",switch = "x")+ theme(axis.line=element_line(color="black"),axis.text.x=element_text(angle = 80,vjust = 1, hjust = 1),plot.title = element_text(hjust = 0.5),panel.spacing = unit(0, "lines"))
