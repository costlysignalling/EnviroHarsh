library(rethinking)

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

dp<-read.table("data_pilot.txt",sep="\t",header=T,stringsAsFactors = F)

(nam<-names(dp))

nrow(dp)
summary(as.factor(dp$Sex))

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

summary(as.factor(dp$Sex))

mean(dp$Age)
sd(dp$Age)


# Calculate Enviromental Harshness scales

PPdat<-dp[,substr(nam,1,2)=="PP"&substr(nam,3,3)!="V"&substrRight(nam,6)!="revcod"&substrRight(nam,3)!="AVE"]
names(PPdat)[c(3,5,12,13,15,16,18)]
PPdat<-revcol(PPdat,c(3,5,12,13,15,16,18))

RSdat<-dp[,substr(nam,1,2)=="RS"&substr(nam,3,3)!="V"&substrRight(nam,6)!="revcod"&substrRight(nam,3)!="AVE"]
names(RSdat)[c(1,2,3,5,11,12,16)]
RSdat<-revcol(RSdat,c(1,2,3,5,11,12,16))

# Check that there are no missing values
sum(is.na(as.matrix(PPdat)))
sum(is.na(as.matrix(RSdat)))

psych::alpha(PPdat)$total$raw_alpha
psych::alpha(RSdat)$total$raw_alpha


dp$PP<-rowSums(PPdat)
dp$RS<-rowSums(RSdat)

dp$fCond<-ifelse(dp$Cond==3,"Control",ifelse(dp$Cond==1,"Pathogen",ifelse(dp$Cond==2,"Scarcity",NA)))
dp$fCond<-as.factor(dp$fCond)

cbind(dp$Cond,as.character(dp$fCond))

summary(dp$fCond)

dp$PPs<-scale(dp$PP)
dp$RSs<-scale(dp$RS)

centerPPp<-attributes(dp$PPs)[[2]]
sdPPp<-attributes(dp$PPs)[[3]]

centerRSp<-attributes(dp$RSs)[[2]]
sdRSp<-attributes(dp$RSs)[[3]]

descalePPp<-function(x){(x*sdPPp+centerPPp)/18}
descaleRSp<-function(x){(x*sdRSp+centerRSp)/16}

summary(as.factor(dp$fCond))

#Posterior evaluation

dat_list_pilot <- list(
  fCondU = as.integer(dp$fCond),

  PPsU=as.numeric(dp$PPs),
  RSsU=as.numeric(dp$RSs)
)

set.seed(42)
m.pilot <- ulam(
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
    
  ) , data=dat_list_pilot , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))


precis(m.pilot,depth=2)

save.image(file = "posterior_pilot.RData")

postp<-extract.samples(m.pilot)

library(vioplot)

colcond<-c("#333333","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05

png("Figure9.png",h=11,w=9,units="cm",res=600)

par(mgp=c(2,0.6,0),mar=c(3.2,3.2,2,1))
vioplot(descalePPp(dat_list_pilot$PPsU)~dat_list_pilot$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="Perceived Pathogen prevalence",xlab="Condition",border=F)
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
abline(h=descalePPp(0),lwd=1.5,col="#00000080")
dev.off()


colcond<-c("#555555","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05

png("Figure10.png",h=11,w=9,units="cm",res=600)

par(mgp=c(2,0.6,0),mar=c(3.2,3.2,2,1))
vioplot(descaleRSp(dat_list_pilot$RSsU)~dat_list_pilot$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="Perceived Resource scarcity",xlab="Condition",border=F)
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
abline(h=descaleRSp(0),lwd=1.5,col="#00000080")
dev.off()

