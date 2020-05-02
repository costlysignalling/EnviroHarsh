library(rethinking)
library(psych)
library(vioplot)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

revcode<-function(v,maxi=max(v),mini=min(v)){v*(-1)+(maxi+mini)}

revcol<-function(d,c,maxi=7,mini=1){
  for(i in c){
    d[,i]<-revcode(d[,i],maxi=maxi,mini=mini)
  }
  return(d)
}

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


#condition in the pilot study
dp$fCond<-ifelse(dp$Cond==3,"Control",ifelse(dp$Cond==1,"Pathogen",ifelse(dp$Cond==2,"Scarcity",NA)))
dp$fCond<-as.factor(dp$fCond)

# Calculate Enviromental Harshness scales

PPdat<-dp[,substr(nam,1,2)=="PP"&substr(nam,3,3)!="V"&substrRight(nam,6)!="revcod"&substrRight(nam,3)!="AVE"]
names(PPdat)[c(3,5,12,13,15,16,18)]
pPPdat<-revcol(PPdat,c(3,5,12,13,15,16,18))

RSdat<-dp[,substr(nam,1,2)=="RS"&substr(nam,3,3)!="V"&substrRight(nam,6)!="revcod"&substrRight(nam,3)!="AVE"]
names(RSdat)[c(1,2,3,5,11,12,16)]
pRSdat<-revcol(RSdat,c(1,2,3,5,11,12,16))

# Check that there are no missing values
sum(is.na(as.matrix(pPPdat)))
sum(is.na(as.matrix(pRSdat)))


#The same prceudre for experiemntal data
source("01_data_prepare.R")

nrow(dw)
names(dw)


PPdat<-dw[,substr(names(dw),1,2)=="PP"&substr(names(dw),3,3)!="V"&nchar(names(dw))==4]
names(PPdat)[c(3,5,12,13,15,16,18)]
wPPdat<-revcol(PPdat,c(3,5,12,13,15,16,18))

RSdat<-dw[,substr(names(dw),1,2)=="RS"&substr(names(dw),3,3)!="V"&nchar(names(dw))==4]
names(RSdat)[c(1,2,3,5,11,12,16)]
wRSdat<-revcol(RSdat,c(1,2,3,5,11,12,16))


#Both together
bPPdat<-rbind(pPPdat,wPPdat)
bRSdat<-rbind(pRSdat,wRSdat)


#Factor analysis
PPitems<-read.table("PPitems.txt",sep="\t",header=T,stringsAsFactors = F)
RSitems<-read.table("RSitems.txt",sep="\t",header=T,stringsAsFactors = F)

w<-17

png("parallel_FA.png",width=w,height=(w/2)*3,res=600,units="cm")
par(mfrow=c(3,2))
fa.parallel(pPPdat, main="Pathogen prevalence scale items (Pilot)")
fa.parallel(pRSdat, main="Resource scarcity scale items (Pilot)")

fa.parallel(wPPdat, main="Pathogen prevalence scale items (Experiment)")
fa.parallel(wRSdat, main="Resource scarcity scale items (Experiment)")

fa.parallel(bPPdat, main="Pathogen prevalence scale items (Both samples)")
fa.parallel(bRSdat, main="Resource scarcity scale items (Both samples)")
dev.off()


#start of the factorization
pPPfact<-fa(pPPdat,2,rotate="promax",scores="tenBerge",fm="ml")
pRSfact<-fa(pRSdat,3,rotate="promax",scores="tenBerge",fm="ml")

wPPfact<-fa(wPPdat,2,rotate="promax",scores="tenBerge",fm="ml")
wRSfact<-fa(wRSdat,3,rotate="promax",scores="tenBerge",fm="ml")

bPPfact<-fa(bPPdat,2,rotate="promax",scores="tenBerge",fm="ml")
bRSfact<-fa(bRSdat,3,rotate="promax",scores="tenBerge",fm="ml")

#PP loadings
resPPp<-cbind(rownames(pPPfact$loadings),PPitems$short,round(pPPfact$loadings,2))
resPPp<-data.frame(resPPp)

resPPw<-cbind(rownames(wPPfact$loadings),PPitems$short,round(wPPfact$loadings,2))
resPPw<-data.frame(resPPw)

resPPb<-cbind(rownames(bPPfact$loadings),PPitems$short,round(bPPfact$loadings,2))
resPPb<-data.frame(resPPb)

resPPp
resPPw
resPPb

resPP<-cbind(resPPp,resPPw[,3:4],resPPb[,3:4])

names(resPP)<-c("ID","Item","Perceived infectability p","Germ aversion p","Perceived infectability e","Germ aversion e","Perceived infectability b","Germ aversion b")
write.table(resPP,"PP_loadings.txt",sep="\t",row.names = F)

pPPfact$r.scores
wPPfact$r.scores
bPPfact$r.scores


#RS loadings
resRSp<-cbind(rownames(pRSfact$loadings),RSitems$short,round(pRSfact$loadings,2))
resRSp<-data.frame(resRSp)

resRSw<-cbind(rownames(wRSfact$loadings),RSitems$short,round(wRSfact$loadings,2))
resRSw<-data.frame(resRSw)

resRSb<-cbind(rownames(bRSfact$loadings),RSitems$short,round(bRSfact$loadings,2))
resRSb<-data.frame(resRSb)

resRSp
resRSw
resRSb

resRS<-cbind(resRSp,resRSw[,3:5],resRSb[,3:5])

names(resRS)<-c("ID","Item","Worry about future p","Act against eco-crisis p","Planetary resources limited p","Worry about future w","Act against eco-crisis w","Planetary resources limited w","Worry about future b","Act against eco-crisis b","Planetary resources limited b")
write.table(resRS,"RS_loadings.txt",sep="\t",row.names = F)

pRSfact$r.scores
wRSfact$r.scores
bRSfact$r.scores


#datasets compilation

dat_list_pilot <- list(
  fCond = as.integer(dp$fCond),

  PP_PI=as.numeric(pPPfact$scores[,1]),
  PP_GA=as.numeric(pPPfact$scores[,2]),
  
  RS_WF=as.numeric(pRSfact$scores[,1]), #"Worry about Future"
  RS_AE=as.numeric(pRSfact$scores[,2]), #"Act against Eco crisis"
  RS_PL=as.numeric(pRSfact$scores[,3])  #"Planetary resources perceived as Limited"
)

dat_list_exp <- list(
  fCond = as.integer(dw$fCond),
  
  PP_PI=as.numeric(wPPfact$scores[,1]),
  PP_GA=as.numeric(wPPfact$scores[,2]),
  
  RS_WF=as.numeric(wRSfact$scores[,1]),
  RS_AE=as.numeric(wRSfact$scores[,2]),
  RS_PL=as.numeric(wRSfact$scores[,3])
)

dat_list_all <- list(
  fCond = c(as.integer(dp$fCond),as.integer(dw$fCond)),
  
  PP_PI=as.numeric(bPPfact$scores[,1]),
  PP_GA=as.numeric(bPPfact$scores[,2]),
  
  RS_WF=as.numeric(bRSfact$scores[,1]),
  RS_AE=as.numeric(bRSfact$scores[,2]),
  RS_PL=as.numeric(bRSfact$scores[,3])
)

#Posterior evaluation

#Perceived infectability
set.seed(42)
m.PP_PI.p <- ulam(
  alist(
    PP_PI ~ dnorm( mu_PP_PI , sigma_PP_PI ) ,
    mu_PP_PI<-a_PP_PI[fCond],
    
    a_PP_PI[fCond] ~ dnorm( 0 , 1 ),
    sigma_PP_PI~ dexp(1)
    
  ) , data=dat_list_pilot , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.PP_PI.e <- ulam(
  alist(
    PP_PI ~ dnorm( mu_PP_PI , sigma_PP_PI ) ,
    mu_PP_PI<-a_PP_PI[fCond],
    
    a_PP_PI[fCond] ~ dnorm( 0 , 1 ),
    sigma_PP_PI~ dexp(1)
    
  ) , data=dat_list_exp , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.PP_PI.b <- ulam(
  alist(
    PP_PI ~ dnorm( mu_PP_PI , sigma_PP_PI ) ,
    mu_PP_PI<-a_PP_PI[fCond],
    
    a_PP_PI[fCond] ~ dnorm( 0 , 1 ),
    sigma_PP_PI~ dexp(1)
    
  ) , data=dat_list_all , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

#Germ aversion
set.seed(42)
m.PP_GA.p <- ulam(
  alist(
    PP_GA ~ dnorm( mu_PP_GA , sigma_PP_GA ) ,
    mu_PP_GA<-a_PP_GA[fCond],
    
    a_PP_GA[fCond] ~ dnorm( 0 , 1 ),
    sigma_PP_GA~ dexp(1)
    
  ) , data=dat_list_pilot , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.PP_GA.e <- ulam(
  alist(
    PP_GA ~ dnorm( mu_PP_GA , sigma_PP_GA ) ,
    mu_PP_GA<-a_PP_GA[fCond],
    
    a_PP_GA[fCond] ~ dnorm( 0 , 1 ),
    sigma_PP_GA~ dexp(1)
    
  ) , data=dat_list_exp , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.PP_GA.b <- ulam(
  alist(
    PP_GA ~ dnorm( mu_PP_GA , sigma_PP_GA ) ,
    mu_PP_GA<-a_PP_GA[fCond],
    
    a_PP_GA[fCond] ~ dnorm( 0 , 1 ),
    sigma_PP_GA~ dexp(1)
    
  ) , data=dat_list_all , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))


#Resource Scarcity
#Worry about Future
set.seed(42)
m.RS_WF.p <- ulam(
  alist(
    RS_WF ~ dnorm( mu_RS_WF , sigma_RS_WF ) ,
    mu_RS_WF<-a_RS_WF[fCond],
    
    a_RS_WF[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_WF~ dexp(1)
    
  ) , data=dat_list_pilot , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.RS_WF.e <- ulam(
  alist(
    RS_WF ~ dnorm( mu_RS_WF , sigma_RS_WF ) ,
    mu_RS_WF<-a_RS_WF[fCond],
    
    a_RS_WF[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_WF~ dexp(1)
    
  ) , data=dat_list_exp , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.RS_WF.b <- ulam(
  alist(
    RS_WF ~ dnorm( mu_RS_WF , sigma_RS_WF ) ,
    mu_RS_WF<-a_RS_WF[fCond],
    
    a_RS_WF[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_WF~ dexp(1)
    
  ) , data=dat_list_all , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

#Act angainst Eco crisis
set.seed(42)
m.RS_AE.p <- ulam(
  alist(
    RS_AE ~ dnorm( mu_RS_AE , sigma_RS_AE ) ,
    mu_RS_AE<-a_RS_AE[fCond],
    
    a_RS_AE[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_AE~ dexp(1)
    
  ) , data=dat_list_pilot , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.RS_AE.e <- ulam(
  alist(
    RS_AE ~ dnorm( mu_RS_AE , sigma_RS_AE ) ,
    mu_RS_AE<-a_RS_AE[fCond],
    
    a_RS_AE[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_AE~ dexp(1)
    
  ) , data=dat_list_exp , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.RS_AE.b <- ulam(
  alist(
    RS_AE ~ dnorm( mu_RS_AE , sigma_RS_AE ) ,
    mu_RS_AE<-a_RS_AE[fCond],
    
    a_RS_AE[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_AE~ dexp(1)
    
  ) , data=dat_list_all , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

#Planetary resorces perceived as Limited
set.seed(42)
m.RS_PL.p <- ulam(
  alist(
    RS_PL ~ dnorm( mu_RS_PL , sigma_RS_PL ) ,
    mu_RS_PL<-a_RS_PL[fCond],
    
    a_RS_PL[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_PL~ dexp(1)
    
  ) , data=dat_list_pilot , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.RS_PL.e <- ulam(
  alist(
    RS_PL ~ dnorm( mu_RS_PL , sigma_RS_PL ) ,
    mu_RS_PL<-a_RS_PL[fCond],
    
    a_RS_PL[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_PL~ dexp(1)
    
  ) , data=dat_list_exp , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

set.seed(42)
m.RS_PL.b <- ulam(
  alist(
    RS_PL ~ dnorm( mu_RS_PL , sigma_RS_PL ) ,
    mu_RS_PL<-a_RS_PL[fCond],
    
    a_RS_PL[fCond] ~ dnorm( 0 , 1 ),
    sigma_RS_PL~ dexp(1)
    
  ) , data=dat_list_all , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

#Sampled posteriors
postPP_PIp<-extract.samples(m.PP_PI.p)
postPP_PIe<-extract.samples(m.PP_PI.e)
postPP_PIb<-extract.samples(m.PP_PI.b)

postPP_GAp<-extract.samples(m.PP_GA.p)
postPP_GAe<-extract.samples(m.PP_GA.e)
postPP_GAb<-extract.samples(m.PP_GA.b)

#RS
postRS_WFp<-extract.samples(m.RS_WF.p)
postRS_WFe<-extract.samples(m.RS_WF.e)
postRS_WFb<-extract.samples(m.RS_WF.b)

postRS_AEp<-extract.samples(m.RS_AE.p)
postRS_AEe<-extract.samples(m.RS_AE.e)
postRS_AEb<-extract.samples(m.RS_AE.b)

postRS_PLp<-extract.samples(m.RS_PL.p)
postRS_PLe<-extract.samples(m.RS_PL.e)
postRS_PLb<-extract.samples(m.RS_PL.b)

save.image("factors_posterior.Rdata")

#Start plotting simple plots for the full sample only
colcond<-c("#808080","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05


#PP factors
png("FigurePP1.png",h=13,w=15,units="cm",res=600)

par(oma=c(4,4,4,0))

par(mfrow=c(2,3))

ylim<-c(-2.5,2.5)

par(mgp=c(2,0.6,0),mar=c(0.5,0,0.5,0.5))

vioplot(dat_list_pilot$PP_PI~dat_list_pilot$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(2,-3:3)
mpost<-postPP_PIp$a_PP_PI
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")
title("Pilot",adj=0,outer = T,line=0)


mtext("Perceived infectability",2,outer=T,line=2,adj=0.815,cex=0.8)
#mtext("Condition",1,outer=T)

vioplot(dat_list_exp$PP_PI~dat_list_exp$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-postPP_PIe$a_PP_PI
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")
title("Experiment",adj=0.385,outer = T,line=0)


vioplot(dat_list_all$PP_PI~dat_list_all$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-postPP_PIb$a_PP_PI
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")
title("Both samples",adj=0.795,outer=T,line=0)


#germ aversion
vioplot(dat_list_pilot$PP_GA~dat_list_pilot$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(1,1:3,labcond)
axis(2,-3:3)
mpost<-postPP_GAp$a_PP_GA
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")

mtext("Germ aversion",2,outer=T,line=2,adj=0.185,cex=0.8)

mtext("Condition",1,outer=T,line = 1.5)

vioplot(dat_list_exp$PP_GA~dat_list_exp$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(1,1:3,labcond)
mpost<-postPP_GAe$a_PP_GA
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")

vioplot(dat_list_all$PP_GA~dat_list_all$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(1,1:3,labcond)
mpost<-postPP_GAb$a_PP_GA
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")

dev.off()


colcond<-c("#808080","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05

png("FigureRS1.png",h=19.5,w=15,units="cm",res=600)

par(oma=c(4,4,4,0))

par(mfrow=c(3,3))

ylim<-c(-2.5,2.5)

par(mgp=c(2,0.6,0),mar=c(0.5,0,0.5,0.5))

#Worry about own future
vioplot(dat_list_pilot$RS_WF~dat_list_pilot$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(2,-3:3)
mpost<-postRS_WFp$a_RS_WF
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")
title("Pilot",adj=0,outer = T,line=0)
mtext("Worry about own future",2,outer=T,line=2,adj=0.93,cex=0.8)

vioplot(dat_list_exp$RS_WF~dat_list_exp$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-postRS_WFe$a_RS_WF
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")
title("Experiment",adj=0.385,outer = T,line=0)


vioplot(dat_list_all$RS_WF~dat_list_all$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-postRS_WFb$a_RS_WF
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")
title("Both samples",adj=0.795,outer=T,line=0)


#Urge to act against eco-crisis
vioplot(dat_list_pilot$RS_AE~dat_list_pilot$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(2,-3:3)
mpost<-postRS_AEp$a_RS_AE
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")
mtext("Urge to act against eco-crisis",2,outer=T,line=2,adj=0.5,cex=0.8)

vioplot(dat_list_exp$RS_AE~dat_list_exp$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-postRS_AEe$a_RS_AE
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")

vioplot(dat_list_all$RS_AE~dat_list_all$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
mpost<-postRS_AEb$a_RS_AE
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")


#Perceived resource finiteness
vioplot(dat_list_pilot$RS_PL~dat_list_pilot$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(1,1:3,labcond)
axis(2,-3:3)
mpost<-postRS_PLp$a_RS_PL
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")

mtext("Perceived resource finiteness",2,outer=T,line=2,adj=0.05,cex=0.8)

mtext("Condition",1,outer=T,line = 1.5)

vioplot(dat_list_exp$RS_PL~dat_list_exp$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(1,1:3,labcond)
mpost<-postRS_PLe$a_RS_PL
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")

vioplot(dat_list_all$RS_PL~dat_list_all$fCond,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="",xlab="",border=F,ylim=ylim,yaxt="n")
axis(1,1:3,labcond)
mpost<-postRS_PLb$a_RS_PL
for(i in 1:ncol(mpost)){
  dens<-density(mpost[,i])
  polX<-c(dens$y,rev(-dens$y))
  polY<-c(dens$x,rev(dens$x))
  polygon(polX*wscaler+i,polY,col=colcond[i],border=col.pol,lwd=lwd.pol)
  CI<-PI(mpost[,i],prob=0.89)
  hwd=0.01
  rect(i-hwd,CI[1],i+hwd,CI[2],border=1,col=1)
  points(i,mean(mpost[,i]),col="white",pch=16)
  points(i,mean(mpost[,i]),col="black",pch=1)
  
}
abline(h=0,lwd=1,col="#DD002280")


dev.off()



