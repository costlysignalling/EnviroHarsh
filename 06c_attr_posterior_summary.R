library(rethinking)
library(pracma)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

load("posterior_attr.RData")

post<-extract.samples(m.attr)

(n<-nrow(d))
(s<-nrow(post[[1]]))

str(dat_list_attr)

str(post)

rat.vals<-c(1:7)


median(c(d$Heal_mas,d$Heal_fem))
median(c(d$Form_mas,d$Form_fem))

Cond<-1
probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*0+post$bH[s,Cond]*sum(post$preddelta_H[s,1:3]),post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsFmin1<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*1+post$bH[s,Cond]*sum(post$preddelta_H[s,1:3]),post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsFmax1<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*sum(post$preddelta_F[s,1:3])+post$bH[s,Cond]*0,post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsHmin1<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*sum(post$preddelta_F[s,1:3])+post$bH[s,Cond]*1,post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsHmax1<-colSums(probs*rat.vals)

Cond<-2
probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*0+post$bH[s,Cond]*sum(post$preddelta_H[s,1:3]),post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsFmin2<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*1+post$bH[s,Cond]*sum(post$preddelta_H[s,1:3]),post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsFmax2<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*sum(post$preddelta_F[s,1:3])+post$bH[s,Cond]*0,post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsHmin2<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*sum(post$preddelta_F[s,1:3])+post$bH[s,Cond]*1,post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsHmax2<-colSums(probs*rat.vals)

Cond<-3
probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*0+post$bH[s,Cond]*sum(post$preddelta_H[s,1:3]),post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsFmin3<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*1+post$bH[s,Cond]*sum(post$preddelta_H[s,1:3]),post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsFmax3<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*sum(post$preddelta_F[s,1:3])+post$bH[s,Cond]*0,post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsHmin3<-colSums(probs*rat.vals)

probs<-sapply(1:s,function(s){pordlogit(1:7,post$bF[s,Cond]*sum(post$preddelta_F[s,1:3])+post$bH[s,Cond]*1,post$cutpointsA[s,])})
probs<-apply(rbind(0,probs),2,diff)
expsHmax3<-colSums(probs*rat.vals)


exmin<-expsFmin1
exmax<-expsFmax1


meanCont<-function(exmin,exmax,xc=0.5,wid=0.2,col="#009696",lwd=1.5,length=0.05){

ci1<-PI(exmin,prob=0.89)
ci2<-PI(exmax,prob=0.89)

arrows(xc-0.5*wid,ci1[1],xc-0.5*wid,ci1[2],angle=90,code=3,length = length,lwd=lwd,col=col)
arrows(xc+0.5*wid,ci2[1],xc+0.5*wid,ci2[2],angle=90,code=3,length = length,lwd=lwd,col=col)
segments(xc-0.5*wid,mean(exmin),xc+0.5*wid,mean(exmax),lwd=lwd,col=col)
points(c(xc+wid*c(-0.5,0.5)),c(mean(exmin),mean(exmax)),col="white",pch=16)
points(c(xc+wid*c(-0.5,0.5)),c(mean(exmin),mean(exmax)),col=col,lwd=lwd)
axis(1,at=c(xc+wid*c(-0.5,0.5)),labels = c(1,7))
     
}


labcond<-c("Control","Pathogen","Scarcity")
colcond<-c("#333333","#FF6432","#3232FF")

xcoords<-c(0.05,0.15,0.25)
wid<-0.05
lwd<-1.8

png("Figure16.png",width=12,height=9,units="cm",res=600)

par(mgp=c(2,0.6,0),mar=c(4,4,2,1))
plot(NULL,xlim=c(0,0.7),ylim=c(1,7),ylab="Mean attractiveness",xlab="",xaxt="n",bty="n")

abline(h=c(2:6),col="#D0D0D0")

meanCont(expsFmin1,expsFmax1,wid=wid,xc=xcoords[1],lwd=lwd,col=colcond[1])
meanCont(expsFmin2,expsFmax2,wid=wid,xc=xcoords[2],lwd=lwd,col=colcond[2])
meanCont(expsFmin3,expsFmax3,wid=wid,xc=xcoords[3],lwd=lwd,col=colcond[3])
text(xcoords[2],-0.4,"Formidability",xpd=T)
text(xcoords[2],-1,"Healthiness = 4",xpd=T,col="#505050")


xcoords<-xcoords+0.4

meanCont(expsHmin1,expsHmax1,wid=wid,xc=xcoords[1],lwd=lwd,col=colcond[1])
meanCont(expsHmin2,expsHmax2,wid=wid,xc=xcoords[2],lwd=lwd,col=colcond[2])
meanCont(expsHmin3,expsHmax3,wid=wid,xc=xcoords[3],lwd=lwd,col=colcond[3])
text(xcoords[2],-0.4,"Healthiness",xpd=T)
text(xcoords[2],-1,"Formidability = 4",xpd=T,col="#505050")

text(c(0.15,0.35,0.55),7,labels = labcond,col=colcond,font=2,xpd=T)
text(0.35,7.7,labels = "Condition",col=1,font=1,xpd=T)

dev.off()


mean(post$bF[,1]-post$bF[,2])
PI(post$bF[,1]-post$bF[,2],prob=0.89)

mean(post$bF[,3]-post$bF[,2])
PI(post$bF[,3]-post$bF[,2],prob=0.89)




mean(post$bH[,1]-post$bH[,2])
PI(post$bH[,1]-post$bH[,2],prob=0.89)

mean(post$bH[,3]-post$bH[,2])
PI(post$bH[,3]-post$bH[,2],prob=0.89)



