library(rethinking)
library(vioplot)
source("visualization_functions.R")

memory.limit(size=2000000)

load("posterior_model.RData")
source("01_data_prepare.R")

load("posterior_pilot.RData")

post<-extract.samples(m.big)
postp<-extract.samples(m.pilot)


dall<-data.frame(fCond=c(dat_list_big$fCondU,dat_list_pilot$fCondU),PP=c(d.unique$PP,dp$PP),RS=c(d.unique$RS,dp$RS))

summary(dall$fCond)

dall$PPs<-scale(dall$PP)
dall$RSs<-scale(dall$RS)

centerPPa<-attributes(dall$PPs)[[2]]
sdPPa<-attributes(dall$PPs)[[3]]

centerRSa<-attributes(dall$RSs)[[2]]
sdRSa<-attributes(dall$RSs)[[3]]

descalePPa<-function(x){(x*sdPPa+centerPPa)/18}
descaleRSa<-function(x){(x*sdRSa+centerRSa)/16}

summary(as.factor(dall$fCond))

#Posterior evaluation

dat_list_all <- list(
  fCondU = as.integer(dall$fCond),
  
  PPsU=as.numeric(dall$PPs),
  RSsU=as.numeric(dall$RSs)
)

set.seed(42)
m.all <- ulam(
  alist(
    #model of PP and RS scales based on condition
    PPsU ~ dnorm( mu_PPs , sigma_PPs ) ,
    RSsU ~ dnorm( mu_RSs , sigma_RSs ) ,
    
    mu_PPs<-a_PPs[fCondU],
    mu_RSs<-a_RSs[fCondU],
    
    a_PPs[fCondU] ~ dnorm( 0 , 1 ),
    a_RSs[fCondU] ~ dnorm( 0 , 1 ),
    
    sigma_PPs~ dexp(1),
    sigma_RSs~ dexp(1)
    
  ) , data=dat_list_all , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

precis(m.all,depth=2)
posta<-extract.samples(m.all)


#Start plotting simple plots for the full sample only
colcond<-c("#808080","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05


png("Figure11.png",h=9,w=15,units="cm",res=600)

par(oma=c(2,4,0,0))

par(mfrow=c(1,3))

ylim<-c(1,7)

par(mgp=c(2,0.6,0),mar=c(3,0,2,0.5))
vioplot(descalePPp(dat_list_pilot$PPsU)~dat_list_pilot$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim)
mpost<-postp$a_PPs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descalePPp(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descalePPp(CI[1]),i+hwd,descalePPp(CI[2]),border=1,col=1)
  points(i,descalePPp(mean(mpost[,i])),col="white",pch=16)
  points(i,descalePPp(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descalePPp(0),lwd=1,col="#DD002280")
title("Pilot",adj=0)

mtext("Perceived Pathogen prevalence",2,outer=T,line=2)
mtext("Condition",1,outer=T)


vioplot(descalePP(dat_list_big$PPsU)~dat_list_big$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-post$a_PPs
axis(1,1:3,labcond)

for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descalePP(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descalePP(CI[1]),i+hwd,descalePP(CI[2]),border=1,col=1)
  points(i,descalePP(mean(mpost[,i])),col="white",pch=16)
  points(i,descalePP(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descalePP(0),lwd=1,col="#DD002280")
title("Experiment",adj=0)

vioplot(descalePPa(dat_list_all$PPsU)~dat_list_all$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n",xaxt="r")
axis(1,1:3,labcond)
mpost<-posta$a_PPs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descalePPa(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descalePPa(CI[1]),i+hwd,descalePPa(CI[2]),border=1,col=1)
  points(i,descalePPa(mean(mpost[,i])),col="white",pch=16)
  points(i,descalePPa(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descalePPa(0),lwd=1,col="#DD002280")
title("Both samples",adj=0)

dev.off()


png("Figure12.png",h=9,w=15,units="cm",res=600)

par(oma=c(2,4,0,0))

par(mfrow=c(1,3))

ylim<-c(2,7)

par(mgp=c(2,0.6,0),mar=c(3,0,2,0.5))
vioplot(descaleRSp(dat_list_pilot$RSsU)~dat_list_pilot$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim)
mpost<-postp$a_RSs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleRSp(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleRSp(CI[1]),i+hwd,descaleRSp(CI[2]),border=1,col=1)
  points(i,descaleRSp(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleRSp(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleRSp(0),lwd=1,col="#DD002280")
title("Pilot",adj=0)

mtext("Perceived Resource Scarcity",2,outer=T,line=2)
mtext("Condition",1,outer=T)


vioplot(descaleRS(dat_list_big$RSsU)~dat_list_big$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-post$a_RSs
axis(1,1:3,labcond)

for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleRS(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleRS(CI[1]),i+hwd,descaleRS(CI[2]),border=1,col=1)
  points(i,descaleRS(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleRS(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleRS(0),lwd=1,col="#DD002280")
title("Experiment",adj=0)

vioplot(descaleRSa(dat_list_all$RSsU)~dat_list_all$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n",xaxt="r")
axis(1,1:3,labcond)
mpost<-posta$a_RSs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleRSa(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleRSa(CI[1]),i+hwd,descaleRSa(CI[2]),border=1,col=1)
  points(i,descaleRSa(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleRSa(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleRSa(0),lwd=1,col="#DD002280")
title("Both samples",adj=0)

dev.off()



colcond<-c("#808080","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05


png("Figure13.png",h=13,w=15,units="cm",res=600)

par(oma=c(4,4,4,0))

par(mfrow=c(2,3))

ylim<-c(2,7)


#Pathogen

par(mgp=c(2,0.6,0),mar=c(0.5,0,0.5,0.5))

vioplot(descalePPp(dat_list_pilot$PPsU)~dat_list_pilot$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-postp$a_PPs
axis(2,2:7)
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descalePPp(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descalePPp(CI[1]),i+hwd,descalePPp(CI[2]),border=1,col=1)
  points(i,descalePPp(mean(mpost[,i])),col="white",pch=16)
  points(i,descalePPp(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descalePPp(0),lwd=1,col="#DD002280")
title("Pilot",adj=0,outer = T,line=0)

mtext("Perceived Pathogen prevalence",2,outer=T,line=2,adj=0.95,cex=0.8)

vioplot(descalePP(dat_list_big$PPsU)~dat_list_big$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-post$a_PPs
#axis(1,1:3,labcond)

for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descalePP(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descalePP(CI[1]),i+hwd,descalePP(CI[2]),border=1,col=1)
  points(i,descalePP(mean(mpost[,i])),col="white",pch=16)
  points(i,descalePP(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descalePP(0),lwd=1,col="#DD002280")
title("Experiment",adj=0.385,outer = T,line=0)

vioplot(descalePPa(dat_list_all$PPsU)~dat_list_all$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n",xaxt="r")
#axis(1,1:3,labcond)
mpost<-posta$a_PPs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descalePPa(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descalePPa(CI[1]),i+hwd,descalePPa(CI[2]),border=1,col=1)
  points(i,descalePPa(mean(mpost[,i])),col="white",pch=16)
  points(i,descalePPa(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descalePPa(0),lwd=1,col="#DD002280")
title("Both samples",adj=0.795,outer=T,line=0)





#Resource

vioplot(descaleRSp(dat_list_pilot$RSsU)~dat_list_pilot$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim)
mpost<-postp$a_RSs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleRSp(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleRSp(CI[1]),i+hwd,descaleRSp(CI[2]),border=1,col=1)
  points(i,descaleRSp(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleRSp(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleRSp(0),lwd=1,col="#DD002280")

mtext("Perceived Resource Scarcity",2,outer=T,line=2,adj=0.05,cex=0.8)
mtext("Condition",1,outer=T,line = 1.5)

vioplot(descaleRS(dat_list_big$RSsU)~dat_list_big$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-post$a_RSs
axis(1,1:3,labcond)

for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleRS(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleRS(CI[1]),i+hwd,descaleRS(CI[2]),border=1,col=1)
  points(i,descaleRS(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleRS(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleRS(0),lwd=1,col="#DD002280")

vioplot(descaleRSa(dat_list_all$RSsU)~dat_list_all$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n",xaxt="r")
axis(1,1:3,labcond)
mpost<-posta$a_RSs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleRSa(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleRSa(CI[1]),i+hwd,descaleRSa(CI[2]),border=1,col=1)
  points(i,descaleRSa(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleRSa(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleRSa(0),lwd=1,col="#DD002280")

dev.off()

#The model of PP and RS scale pilot
apply(postp$a_PPs,2,mean)
apply(postp$a_PPs,2,PI,prob=0.89)

mean(postp$a_PPs[,1]-postp$a_PPs[,2])
PI(postp$a_PPs[,1]-postp$a_PPs[,2],prob=0.89)

mean(postp$a_PPs[,1]-postp$a_PPs[,3])
PI(postp$a_PPs[,1]-postp$a_PPs[,3],prob=0.89)


apply(postp$a_RSs,2,mean)
apply(postp$a_RSs,2,PI,prob=0.89)

mean(postp$a_RSs[,1]-postp$a_RSs[,2])
PI(postp$a_RSs[,1]-postp$a_RSs[,2],prob=0.89)

mean(postp$a_RSs[,1]-postp$a_RSs[,3])
PI(postp$a_RSs[,1]-postp$a_RSs[,3],prob=0.89)

#Standard deviations
mean(postp$sigma_PPs)
PI(postp$sigma_PPs,prob=0.89)

mean(postp$sigma_RSs)
PI(postp$sigma_RSs,prob=0.89)



#The model of PP and RS scale
apply(post$a_PPs,2,mean)
apply(post$a_PPs,2,PI,prob=0.89)

mean(post$a_PPs[,1]-post$a_PPs[,2])
PI(post$a_PPs[,1]-post$a_PPs[,2],prob=0.89)

mean(post$a_PPs[,1]-post$a_PPs[,3])
PI(post$a_PPs[,1]-post$a_PPs[,3],prob=0.89)


apply(post$a_RSs,2,mean)
apply(post$a_RSs,2,PI,prob=0.89)

mean(post$a_RSs[,1]-post$a_RSs[,2])
PI(post$a_RSs[,1]-post$a_RSs[,2],prob=0.89)

mean(post$a_RSs[,1]-post$a_RSs[,3])
PI(post$a_RSs[,1]-post$a_RSs[,3],prob=0.89)

#Standard deviations
mean(post$sigma_PPs)
PI(post$sigma_PPs,prob=0.89)

mean(post$sigma_RSs)
PI(post$sigma_RSs,prob=0.89)


#The model of PP and RS scale total
apply(posta$a_PPs,2,mean)
apply(posta$a_PPs,2,PI,prob=0.89)

mean(posta$a_PPs[,1]-posta$a_PPs[,2])
PI(posta$a_PPs[,1]-posta$a_PPs[,2],prob=0.89)

mean(posta$a_PPs[,1]-posta$a_PPs[,3])
PI(posta$a_PPs[,1]-posta$a_PPs[,3],prob=0.89)


apply(posta$a_RSs,2,mean)
apply(posta$a_RSs,2,PI,prob=0.89)

mean(posta$a_RSs[,1]-posta$a_RSs[,2])
PI(posta$a_RSs[,1]-posta$a_RSs[,2],prob=0.89)

mean(posta$a_RSs[,1]-posta$a_RSs[,3])
PI(posta$a_RSs[,1]-posta$a_RSs[,3],prob=0.89)

#Standard deviations
mean(posta$sigma_PPs)
PI(posta$sigma_PPs,prob=0.89)

mean(posta$sigma_RSs)
PI(posta$sigma_RSs,prob=0.89)


save.image(file="scales.RData")


mean(descalePPp(postp$a_PPs[,2])-descalePPp(postp$a_PPs[,1]))
PI(descalePPp(postp$a_PPs[,2])-descalePPp(postp$a_PPs[,1]),prob=0.89)

mean(descalePPp(postp$a_PPs[,2])-descalePPp(postp$a_PPs[,3]))
PI(descalePPp(postp$a_PPs[,2])-descalePPp(postp$a_PPs[,3]),prob=0.89)



mean(post$a_PPs[,2]-post$a_PPs[,1])
PI(post$a_PPs[,2]-post$a_PPs[,1],prob=0.89)

mean(descalePP(post$a_PPs[,1])-descalePP(post$a_PPs[,2]))
PI(descalePP(post$a_PPs[,1])-descalePP(post$a_PPs[,2]),prob=0.89)


mean(post$a_PPs[,1]-post$a_PPs[,3])
PI(post$a_PPs[,1]-post$a_PPs[,3],prob=0.89)

mean(descalePP(post$a_PPs[,1])-descalePP(post$a_PPs[,3]))
PI(descalePP(post$a_PPs[,1])-descalePP(post$a_PPs[,3]),prob=0.89)


mean(descalePP(post$a_PPs[,2])-descalePP(post$a_PPs[,3]))
PI(descalePP(post$a_PPs[,2])-descalePP(post$a_PPs[,3]),prob=0.89)


mean(descalePPa(posta$a_PPs[,2])-descalePPa(posta$a_PPs[,1]))
PI(descalePPa(posta$a_PPs[,2])-descalePPa(posta$a_PPs[,1]),prob=0.89)

mean(descalePPa(posta$a_PPs[,2])-descalePPa(posta$a_PPs[,3]))
PI(descalePPa(posta$a_PPs[,2])-descalePPa(posta$a_PPs[,3]),prob=0.89)



#resource scarcity
mean(descaleRSp(postp$a_RSs[,1])-descaleRSp(postp$a_RSs[,3]))
PI(descaleRSp(postp$a_RSs[,1])-descaleRSp(postp$a_RSs[,3]),prob=0.89)

mean(descaleRSp(postp$a_RSs[,2])-descaleRSp(postp$a_RSs[,3]))
PI(descaleRSp(postp$a_RSs[,2])-descaleRSp(postp$a_RSs[,3]),prob=0.89)


mean(post$a_RSs[,1]-post$a_RSs[,2])
PI(post$a_RSs[,1]-post$a_RSs[,2])


mean(descaleRS(post$a_RSs[,1])-descaleRS(post$a_RSs[,2]))
PI(descaleRS(post$a_RSs[,1])-descaleRS(post$a_RSs[,2]),prob=0.89)

mean(descaleRS(post$a_RSs[,1])-descaleRS(post$a_RSs[,3]))
PI(descaleRS(post$a_RSs[,1])-descaleRS(post$a_RSs[,3]),prob=0.89)


mean(descaleRSa(posta$a_RSs[,1])-descaleRSa(posta$a_RSs[,2]))
PI(descaleRSa(posta$a_RSs[,1])-descaleRSa(posta$a_RSs[,2]),prob=0.89)

mean(descaleRSa(posta$a_RSs[,1])-descaleRSa(posta$a_RSs[,3]))
PI(descaleRSa(posta$a_RSs[,1])-descaleRSa(posta$a_RSs[,3]),prob=0.89)


#Differnence between pilot and experiment
mean(rowMeans(descaleRS(post$a_RSs))-rowMeans(descaleRSp(postp$a_RSs)))
PI(rowMeans(descaleRS(post$a_RSs))-rowMeans(descaleRSp(postp$a_RSs)),prob = 0.89)



