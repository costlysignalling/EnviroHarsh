library(rethinking)
library(vioplot)

dp <- read.csv("pilot_data.csv", sep = ",",stringsAsFactors = F)

(nam<-names(dp))

nrow(dp)

#Check how many answers got people right

summary(as.factor(dp$PPVAS1)) # 1 is correct
summary(as.factor(dp$PPVAS2)) # 2 is correct

summary(as.factor(dp$RSVAS1)) # 2 is correct
summary(as.factor(dp$RSVAS2)) # 1 is correct

summary(as.factor(dp$SPVAS1)) # 2 is correct
summary(as.factor(dp$SPVAS2)) # 1 is correct

# Sum correct answers
dp$VAScorrect<-ifelse(dp$PPVAS1==1,1,0)+ifelse(dp$PPVAS2==2,1,0)+ifelse(dp$RSVAS1==2,1,0)+ifelse(dp$RSVAS2==1,1,0)+ifelse(dp$SPVAS1==2,1,0)+ifelse(dp$SPVAS2==1,1,0)

#Exclude wrong answers or not
#d<-d[dp$VAScorrect==2,]
dp<-dp[dp$VAScorrect>0,]

nrow(dp)

#We included only heterosexual women to the study. Homosexuals, bisexuals and other were excluded.
summary(as.factor(dp$SexOrient))

#exclude homosexuals or not
dp<-dp[dp$SexOrient==1,]
nrow(dp)

#fCond
dp$fCond<-ifelse(dp$Cond==3,"Control",ifelse(dp$Cond==1,"Pathogen",ifelse(dp$Cond==2,"Scarcity",NA)))
dp$fCond<-as.factor(dp$fCond)

cbind(dp$Cond,as.character(dp$fCond))

summary(dp$fCond)

# Calculate VIS scale for the pilot data
dp$VIS<-dp$VIS01+dp$VIS02+dp$VIS03
dp$VISs<-scale(dp$VIS)

centerVISp<-attributes(dp$VISs)[[2]]
sdVISp<-attributes(dp$VISs)[[3]]

descaleVISp<-function(x){(x*sdVISp+centerVISp)/3}


source("01_data_prepare.R")

summary(dw$fCond)

dw$VIS<-dw$VIS01+dw$VIS02+dw$VIS03
dw$VISs<-scale(dw$VIS)

centerVIS<-attributes(dw$VISs)[[2]]
sdVIS<-attributes(dw$VISs)[[3]]

descaleVIS<-function(x){(x*sdVIS+centerVIS)/3}

#dboth datasetes combines
da<-data.frame(fCond=c(dp$fCond,dw$fCond),VIS=c(dp$VIS,dw$VIS))
da$VISs<-scale(da$VIS)

centerVISa<-attributes(da$VISs)[[2]]
sdVISa<-attributes(da$VISs)[[3]]

descaleVISa<-function(x){(x*sdVISa+centerVISa)/3}


dat_vis_pilot <- list(
  fCond = as.integer(dp$fCond),
  VISs=as.numeric(dp$VISs)
)

set.seed(42)
m.vis.p <- ulam(
  alist(
    VISs ~ dnorm( mu_VISs , sigma_VISs ) ,
    mu_VISs<-a_VISs[fCond],
    
    a_VISs[fCond] ~ dnorm( 0 , 1 ),
    sigma_VISs~ dexp(1)
    
  ) , data=dat_vis_pilot , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))


dat_vis <- list(
  fCond = as.integer(dw$fCond),
  VISs=as.numeric(dw$VISs)
)

set.seed(42)
m.vis <- ulam(
  alist(
    VISs ~ dnorm( mu_VISs , sigma_VISs ) ,
    mu_VISs<-a_VISs[fCond],
    
    a_VISs[fCond] ~ dnorm( 0 , 1 ),
    sigma_VISs~ dexp(1)
    
  ) , data=dat_vis , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))


dat_vis_all <- list(
  fCond = as.integer(da$fCond),
  VISs=as.numeric(da$VISs)
)

set.seed(42)
m.vis.a <- ulam(
  alist(
    VISs ~ dnorm( mu_VISs , sigma_VISs ) ,
    mu_VISs<-a_VISs[fCond],
    
    a_VISs[fCond] ~ dnorm( 0 , 1 ),
    sigma_VISs~ dexp(1)
    
  ) , data=dat_vis_all , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

save.image("VIS.Rdata")

precis(m.vis.p,depth=2)
precis(m.vis,depth=2)
precis(m.vis.a,depth=2)

vpostp<-extract.samples(m.vis.p)
vpost<-extract.samples(m.vis)
vposta<-extract.samples(m.vis.a)

#pilot
mean(vpostp$a_VISs[,1]-vpostp$a_VISs[,2])
PI(vpostp$a_VISs[,1]-vpostp$a_VISs[,2],prob=0.89)

mean(vpostp$a_VISs[,1]-vpostp$a_VISs[,3])
PI(vpostp$a_VISs[,1]-vpostp$a_VISs[,3],prob=0.89)

#experiment
mean(vpost$a_VISs[,1]-vpost$a_VISs[,2])
PI(vpost$a_VISs[,1]-vpost$a_VISs[,2],prob=0.89)

mean(vpost$a_VISs[,1]-vpost$a_VISs[,3])
PI(vpost$a_VISs[,1]-vpost$a_VISs[,3],prob=0.89)

#both
mean(vposta$a_VISs[,1]-vposta$a_VISs[,2])
PI(vposta$a_VISs[,1]-vposta$a_VISs[,2],prob=0.89)

mean(vposta$a_VISs[,1]-vposta$a_VISs[,3])
PI(vposta$a_VISs[,1]-vposta$a_VISs[,3],prob=0.89)

#On the original scale
#pilot
mean(descaleVISp(vpostp$a_VISs[,2])-descaleVISp(vpostp$a_VISs[,1]))
PI(descaleVISp(vpostp$a_VISs[,2])-descaleVISp(vpostp$a_VISs[,1]),prob=0.89)

mean(descaleVISp(vpostp$a_VISs[,3])-descaleVISp(vpostp$a_VISs[,1]))
PI(descaleVISp(vpostp$a_VISs[,3])-descaleVISp(vpostp$a_VISs[,1]),prob=0.89)


#experiment
mean(descaleVIS(vpost$a_VISs[,2])-descaleVIS(vpost$a_VISs[,1]))
PI(descaleVIS(vpost$a_VISs[,2])-descaleVIS(vpost$a_VISs[,1]),prob=0.89)

mean(descaleVIS(vpost$a_VISs[,3])-descaleVIS(vpost$a_VISs[,1]))
PI(descaleVIS(vpost$a_VISs[,3])-descaleVIS(vpost$a_VISs[,1]),prob=0.89)

#both
mean(descaleVISa(vposta$a_VISs[,2])-descaleVISa(vposta$a_VISs[,1]))
PI(descaleVISa(vposta$a_VISs[,2])-descaleVISa(vposta$a_VISs[,1]),prob=0.89)

mean(descaleVISa(vposta$a_VISs[,3])-descaleVISa(vposta$a_VISs[,1]))
PI(descaleVISa(vposta$a_VISs[,3])-descaleVISa(vposta$a_VISs[,1]),prob=0.89)


#Start plotting simple plots for the full sample only
colcond<-c("#808080","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05

png("Figure14.png",h=7,w=15,units="cm",res=600)

par(oma=c(2,4,0,0))

par(mfrow=c(1,3))

ylim<-c(2,7)

par(mgp=c(2,0.6,0),mar=c(3,0,2,0.5))
vioplot(descaleVISp(dat_vis_pilot$VISs)~dat_vis_pilot$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim)
mpost<-vpostp$a_VISs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleVISp(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleVISp(CI[1]),i+hwd,descaleVISp(CI[2]),border=1,col=1)
  points(i,descaleVISp(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleVISp(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleVISp(0),lwd=1,col="#DD002280")
title("Pilot",adj=0)

mtext("Video Impact Score",2,outer=T,line=2)
mtext("Condition",1,outer=T)


vioplot(descaleVIS(dat_vis$VISs)~dat_vis$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-vpost$a_VISs
axis(1,1:3,labcond)

for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleVIS(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleVIS(CI[1]),i+hwd,descaleVIS(CI[2]),border=1,col=1)
  points(i,descaleVIS(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleVIS(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleVIS(0),lwd=1,col="#DD002280")
title("Experiment",adj=0)

vioplot(descaleVISa(dat_vis_all$VISs)~dat_vis_all$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n",xaxt="r")
axis(1,1:3,labcond)
mpost<-vposta$a_VISs
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,descaleVISa(polY),col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,descaleVISa(CI[1]),i+hwd,descaleVISa(CI[2]),border=1,col=1)
  points(i,descaleVISa(mean(mpost[,i])),col="white",pch=16)
  points(i,descaleVISa(mean(mpost[,i])),col="black",pch=1)
  
}
abline(h=descaleVISa(0),lwd=1,col="#DD002280")
title("Both samples",adj=0)

dev.off()


