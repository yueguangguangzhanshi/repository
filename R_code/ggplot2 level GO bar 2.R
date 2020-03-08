diffnum=15
opt<-data.frame(rotation=70,txt.size=2)
rot<-as.numeric(opt$rotation)
txt.size<-as.numeric(opt$txt.size)
ppi=200
group<-unique(df$GO)
col6<-c("#ff69b4","#7ec0ee","#669933")
indat<-df[,c("Description","GO","Count")]
class<-as.vector(indat[,2])
func<-as.vector(indat[,1])
bardat<-as.matrix(indat[,-c(1:2)])
#set labels colour
col=col6
names(col)<-group
text.len<-max(nchar(as.matrix(indat)[,1]),50)*1.2
temp<-100*bardat/rep(diffnum,each=nrow(bardat))
width =(7+length(bardat))*0.5*ppi
height =text.len*0.09*sin(rot*pi/180)*txt.size*ppi+2000
space<-c(0,0.6)
percent<-log10(temp)+1
percent[percent<0]<-0
png(filename = "GO_Classification_Bar_Plot.png",width =width,height =height,res = ppi,units = "px")
par(lwd = 2,cex=1,cex.lab =txt.size,cex.axis = txt.size,font.lab = 1,font.axis = 1,mar = c(txt.size*text.len*0.095*sin(rot*pi/180)/par("csi")+2,text.len*0.3,5,ceiling(nchar(max(diffnum))/2.5)+8),mgp=c(4,0.5,0))
bar=barplot(t(percent),col=col[class],beside = T,border =NA,axes=F,names.arg = rep(NA,length(percent)),ylim=c(0,3), ylab="Percentage of Proteins",space =space)
mtext(side = 4,line =ceiling(nchar(max(diffnum))/3)+2,text = "Number of Proteins" ,cex=txt.size,font = 1)
mtext(side = 3,line=1,text = paste("GO Classification Plot") ,cex=txt.size,font = 1)
box(bty="u")
bar<-apply(bar,2,mean)
axis(side = 1,labels =F,tck=0)
axis(side = 2,at=c(0,1,2,3),lab=c(0,1,10,100),cex=txt.size,tck=-0.005,font=1,las=2)
for(i in 1:length(func)){
  g<-ifelse(indat[i,2]=="BP",1,ifelse(indat[i,2]=="MF",2,ifelse(indat[i,2]=="CC",3,1)))
  text(x=bar[i]-0.6, y=-0.1,labels=func[i], adj=1, srt=rot, xpd=TRUE,tcl=-0.02,cex=txt.size,font = 1,col=col6[g])
}
yl1<-c(0,1,10,100)*diffnum[1]/100
yl1[yl1<1]<-0
axis(side = 4,at=c(0,1,2,3),tck=-0.005,labels = F,lwd=2)
axis(side = 4,at=c(0,1,2,3),lab=floor(yl1),cex=txt.size,tck=1,col="black",las=2,xpd=T,font = 1,lwd=2)
xdis<-max(strwidth(indat[max(which.max(nchar(func))),1])*1.1,strwidth("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))*cos(rot*pi/180)*txt.size*200/ppi*1.2
yf<-tan(rot*pi/180)/strwidth("a")*strwidth("a","in")/strheight("a","in")*strheight("a")
ydis<-xdis*yf
xgroup<-c("Biological Process","Molecular Function","Cellular Component")
for(i in 1:length(group)){
  lab<-indat[,2]==group[i]
  l<-func[lab]
  x1<-bar[lab][1]-(strwidth(l[1])+5)*cos(rot*pi/180)*txt.size
  y1<--yf*(strwidth(l[1])+4.5)*cos(rot*pi/180)*txt.size
  x2<-tail(bar[lab],1)-(strwidth(tail(l,1))+5)*cos(rot*pi/180)*txt.size
  y2<--yf*(strwidth(tail(l,1))+4.5)*cos(rot*pi/180)*txt.size
  segments(x0 = x1,y0 = y1-0.06,x1 =bar[lab][1]-xdis ,y1=-ydis-0.06,xpd=T,lwd=2)
  segments(x0=tail(bar[lab],1)-xdis,y0=-ydis-0.06,x1 =bar[lab][1]-xdis,y1=-ydis-0.06,xpd=T,lwd=2)
  segments(x0=tail(bar[lab],1)-xdis,y0=-ydis-0.06,x1 =x2,y1=y2-0.06,xpd=T,lwd=2) 
  text(labels = xgroup[i],col=col6[i],x =mean(range(bar[lab]))-xdis,y=-ydis-0.3,xpd=T,cex=txt.size*1,adj =c(0.6,1),font = 1)
}
dev.off()
