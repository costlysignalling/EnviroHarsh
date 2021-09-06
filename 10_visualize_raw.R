source("01_data_prepare.R")
load("posterior_model.RData")


table(dat_list_big$fCond,d$fCond)
d<-dat_list_big

colcond<-c("#808080","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")
colchar<-c("#009696","#960096","#969600")


vio<-function(x,v,col,wb=0.2,wv=1.2,coll="#00000080"){
  dens<-density(v,adjust=1.5)
  polygon(c(-dens$y,rev(dens$y))*wv+x,c(dens$x,rev(dens$x)),col=paste(col,"80",sep=""),border=NA)
  qs<-quantile(v,probs=c(0.025,0.25,0.5,0.75,0.975))
  segments(rep(x,2),qs[c(1,4)],rep(x,2),qs[c(2,5)],col=coll,lwd=1.2)
  rect(x-wb/2,qs[2],x+wb/2,qs[4],border=coll,col=col,lwd=1.2)
  lines(x+c(-wb/2,wb/2),rep(qs[3],2),lwd=2)
}

dumb<-function(x,v1,v2,col=1,bg=0){
  m1<-mean(v1)
  m2<-mean(v2)
  
  lines(x,c(m1,m2),lwd=2,col=col)
  points(x,c(m1,m2),lwd=2,col=col,bg=bg,pch=21)
}

#Make the plot

tiff("FigureRaw2.tif",width=23,height=10,units="cm",res=600,compression="lzw")
off<-2.5

layout(matrix(c(1,2,3,4),nrow=1),widths=c(1,1,1,0.5))
par(mar=c(5.5,3.5,1,1),mgp=c(2,0.6,0))
plot(NULL,xlim=c(0.4,7.6),ylim=c(1,7),yaxs="i",xaxs="i",xlab="",ylab="",xaxt="n")
axis(1,at=rep(c(0,1,2),each=2)*off+rep(c(1:2),times=3),labels=rep(c("Fem","Mas"),times=3),tck=0,las=2)
par(mgp=c(4,3,0))
axis(1,at=1.5,labels=labcond[1],tck=0,col.axis=colcond[1],font.axis=2)
axis(1,at=1.5+off,labels=labcond[2],tck=0,col.axis=colcond[2],font.axis=2)
axis(1,at=1.5+off*2,labels=labcond[3],tck=0,col.axis=colcond[3],font.axis=2)

rect(-1,-1,10,10,border=NA,col="#E0F0F0")
abline(h=2:6,col=0)
abline(h=seq(0.5,6.5,1),col=0,lty=2)

mtext("Attractiveness ratings",side=2,line=2,col=colchar[1],font=2)

vio(1,d$Attr_fem[d$fCond==1],col=colcond[1])
vio(2,d$Attr_mas[d$fCond==1],col=colcond[1])
dumb(x=c(1,2),d$Attr_fem[d$fCond==1],d$Attr_mas[d$fCond==1])

vio(1+off,d$Attr_fem[d$fCond==2],col=colcond[2])
vio(2+off,d$Attr_mas[d$fCond==2],col=colcond[2])
dumb(x=c(1,2)+off,d$Attr_fem[d$fCond==2],d$Attr_mas[d$fCond==2])

vio(1+off*2,d$Attr_fem[d$fCond==3],col=colcond[3])
vio(2+off*2,d$Attr_mas[d$fCond==3],col=colcond[3])
dumb(x=c(1,2)+off*2,d$Attr_fem[d$fCond==3],d$Attr_mas[d$fCond==3])

box()


#Formidability
par(mar=c(5.5,3.5,1,1),mgp=c(2,0.6,0))
plot(NULL,xlim=c(0.4,7.6),ylim=c(1,7),yaxs="i",xaxs="i",xlab="",ylab="",xaxt="n")
axis(1,at=rep(c(0,1,2),each=2)*off+rep(c(1:2),times=3),labels=rep(c("Fem","Mas"),times=3),tck=0,las=2)
par(mgp=c(4,3,0))
axis(1,at=1.5,labels=labcond[1],tck=0,col.axis=colcond[1],font.axis=2)
axis(1,at=1.5+off,labels=labcond[2],tck=0,col.axis=colcond[2],font.axis=2)
axis(1,at=1.5+off*2,labels=labcond[3],tck=0,col.axis=colcond[3],font.axis=2)

rect(-1,-1,10,10,border=NA,col="#F0E0F0")
abline(h=2:6,col=0)
abline(h=seq(0.5,6.5,1),col=0,lty=2)

mtext("Formidability ratings",side=2,line=2,col=colchar[2],font=2)

vio(1,d$Form_fem[d$fCond==1],col=colcond[1])
vio(2,d$Form_mas[d$fCond==1],col=colcond[1])
dumb(x=c(1,2),d$Form_fem[d$fCond==1],d$Form_mas[d$fCond==1])

vio(1+off,d$Form_fem[d$fCond==2],col=colcond[2])
vio(2+off,d$Form_mas[d$fCond==2],col=colcond[2])
dumb(x=c(1,2)+off,d$Form_fem[d$fCond==2],d$Form_mas[d$fCond==2])

vio(1+off*2,d$Form_fem[d$fCond==3],col=colcond[3])
vio(2+off*2,d$Form_mas[d$fCond==3],col=colcond[3])
dumb(x=c(1,2)+off*2,d$Form_fem[d$fCond==3],d$Form_mas[d$fCond==3])

box()


#Healthiness
par(mar=c(5.5,3.5,1,1),mgp=c(2,0.6,0))
plot(NULL,xlim=c(0.4,7.6),ylim=c(1,7),yaxs="i",xaxs="i",xlab="",ylab="",xaxt="n")
axis(1,at=rep(c(0,1,2),each=2)*off+rep(c(1:2),times=3),labels=rep(c("Fem","Mas"),times=3),tck=0,las=2)

par(mgp=c(4,3,0))
axis(1,at=1.5,labels=labcond[1],tck=0,col.axis=colcond[1],font.axis=2)
axis(1,at=1.5+off,labels=labcond[2],tck=0,col.axis=colcond[2],font.axis=2)
axis(1,at=1.5+off*2,labels=labcond[3],tck=0,col.axis=colcond[3],font.axis=2)

rect(-1,-1,10,10,border=NA,col="#F0F0E0")
abline(h=2:6,col=0)
abline(h=seq(0.5,6.5,1),col=0,lty=2)

mtext("Healthiness ratings",side=2,line=2,col=colchar[3],font=2)

vio(1,d$Heal_fem[d$fCond==1],col=colcond[1])
vio(2,d$Heal_mas[d$fCond==1],col=colcond[1])
dumb(x=c(1,2),d$Heal_fem[d$fCond==1],d$Heal_mas[d$fCond==1])

vio(1+off,d$Heal_fem[d$fCond==2],col=colcond[2])
vio(2+off,d$Heal_mas[d$fCond==2],col=colcond[2])
dumb(x=c(1,2)+off,d$Heal_fem[d$fCond==2],d$Heal_mas[d$fCond==2])

vio(1+off*2,d$Heal_fem[d$fCond==3],col=colcond[3])
vio(2+off*2,d$Heal_mas[d$fCond==3],col=colcond[3])
dumb(x=c(1,2)+off*2,d$Heal_fem[d$fCond==3],d$Heal_mas[d$fCond==3])

box()


par(mar=c(5.5,3.5,1,1),mgp=c(2,0.6,0))
plot(NULL,xlim=c(0,3),ylim=c(0,1),yaxs="i",xaxs="i",xlab="",ylab="",xaxt="n",yaxt="n")
mtext("Masculinized face selected",side=2,line=2,col=1,font=2)

at<-seq(0,1,0.25)
axis(2,at=at)

scol<-c("#000000","#0088CC")

mean(d$Selmas[d$fCond==1])
mean(d$Selmas[d$fCond==2])
mean(d$Selmas[d$fCond==3])

proprect<-function(x,m){
rect(0+x,0,1+x,m,border=NA,col=scol[1])
rect(0+x,m,1+x,1,border=NA,col=scol[2])
}

proprect(0,mean(d$Selmas[d$fCond==1]))
proprect(1,mean(d$Selmas[d$fCond==2]))
proprect(2,mean(d$Selmas[d$fCond==3]))

abline(v=c(1,2),col=0)
abline(h=at[-c(1,length(at))],col="#E0E0E0",lty=2)

axis(1,at=0.5,labels=labcond[1],tck=0,col.axis=colcond[1],font.axis=2,las=2)
axis(1,at=1.5,labels=labcond[2],tck=0,col.axis=colcond[2],font.axis=2,las=2)
axis(1,at=2.5,labels=labcond[3],tck=0,col.axis=colcond[3],font.axis=2,las=2)

text(0.5+c(0,1,2),0.1,"Mas",font=2,col=0,srt=90)
text(0.5+c(0,1,2),0.9,"Fem",font=2,col=0,srt=90)

box()
dev.off()


#Numeric table into supplements
summar<-function(v){
  paste(format(round(mean(v),2),nsmall=2)," (",format(round(sd(v),2),nsmall=2),")",sep="")
}


suptab<-rbind(
  cbind(
    summar(d$Attr_fem[d$fCond==1]),
    summar(d$Attr_mas[d$fCond==1]),
    summar(d$Attr_fem[d$fCond==2]),
    summar(d$Attr_mas[d$fCond==2]),
    summar(d$Attr_fem[d$fCond==3]),
    summar(d$Attr_mas[d$fCond==3])
  ),
  cbind(
    summar(d$Form_fem[d$fCond==1]),
    summar(d$Form_mas[d$fCond==1]),
    summar(d$Form_fem[d$fCond==2]),
    summar(d$Form_mas[d$fCond==2]),
    summar(d$Form_fem[d$fCond==3]),
    summar(d$Form_mas[d$fCond==3])
  ),
  cbind(
    summar(d$Heal_fem[d$fCond==1]),
    summar(d$Heal_mas[d$fCond==1]),
    summar(d$Heal_fem[d$fCond==2]),
    summar(d$Heal_mas[d$fCond==2]),
    summar(d$Heal_fem[d$fCond==3]),
    summar(d$Heal_mas[d$fCond==3])
  ),
  cbind(
    format(round((1-mean(d$Selmas[d$fCond==1]))*100,2),nsmall=2),
    format(round((mean(d$Selmas[d$fCond==1]))*100,2),nsmall=2),
    format(round((1-mean(d$Selmas[d$fCond==2]))*100,2),nsmall=2),
    format(round((mean(d$Selmas[d$fCond==2]))*100,2),nsmall=2),
    format(round((1-mean(d$Selmas[d$fCond==3]))*100,2),nsmall=2),
    format(round((mean(d$Selmas[d$fCond==3]))*100,2),nsmall=2)
  )
)


rownames(suptab)<-c("Attractiveness ratings","Formidability ratings","Healthiness ratings","Selected as a more suitable long-term partner (%)")

suptab<-rbind(
  rep(c("Feminized","Masculinized"),times=3),
  suptab)

colnames(suptab)<-c("Control","","Pathogen","","Scarcity","")

write.table(suptab,"SupplementTable.txt",sep="\t",col.names=NA)

