library(rethinking)
source("visualization_functions.R")

#options(digits = 2)

memory.limit(size=2000000)

load("posterior_model.RData")
precis(m.big,depth=2,omit=c("zf","zr","zfA","zrA","ziA","zfF","zrF","ziF","zfH","zrH","ziH"))

#Extract samples and link to create CI
post<-extract.samples(m.big)
linked<-link( m.big )

#Calculate how many data points and samples can we work with
(n<-nrow(d))
(s<-nrow(post[[1]]))

#In total
sum(d$Selmas)/length(d$Selmas)

mean(inv_logit(post$ac[,1]))
PI(inv_logit(post$ac[,1]))


mean(inv_logit(post$ac[,2]))
PI(inv_logit(post$ac[,2]))


mean(inv_logit(post$ac[,3]))
PI(inv_logit(post$ac[,3]))


mean(post$ac[,1]-post$ac[,2])
PI(post$ac[,1]-post$ac[,2])

mean(post$ac[,1]-post$ac[,3])
PI(post$ac[,1]-post$ac[,3])

mean(post$ac[,3]-post$ac[,2])
PI(post$ac[,3]-post$ac[,2])

str(post)
mean(inv_logit(0.5*post$bs))
mean(inv_logit(-0.5*post$bs))

PI(inv_logit(0.5*post$bs))
PI(inv_logit(-0.5*post$bs))


#Counterfactual plot comparing the effect of condition and laterality
#Condition 1, Mas left
dat_list_sim1<-dat_list_big
dat_list_sim1$PPs<-rep(0,n)
dat_list_sim1$RSs<-rep(0,n)
dat_list_sim1$Attr_dif<-rep(0,n)
dat_list_sim1$Form_dif<-rep(0,n)
dat_list_sim1$Heal_dif<-rep(0,n)

dat_list_sim1$fCond<-rep(1,n)
dat_list_sim1$Masright<-rep(-0.5,n)

#Condition 1, Mas right
dat_list_sim2<-dat_list_sim1

dat_list_sim2$fCond<-rep(1,n)
dat_list_sim2$Masright<-rep(0.5,n)


#Condition 2, Mas left
dat_list_sim3<-dat_list_sim1

dat_list_sim3$fCond<-rep(2,n)
dat_list_sim3$Masright<-rep(-0.5,n)


#Condition 2, Mas right
dat_list_sim4<-dat_list_sim1

dat_list_sim4$fCond<-rep(2,n)
dat_list_sim4$Masright<-rep(0.5,n)


#Condition 3, Mas left
dat_list_sim5<-dat_list_sim1

dat_list_sim5$fCond<-rep(3,n)
dat_list_sim5$Masright<-rep(-0.5,n)


#Condition 3, Mas right
dat_list_sim6<-dat_list_sim1

dat_list_sim6$fCond<-rep(3,n)
dat_list_sim6$Masright<-rep(0.5,n)

#Link to simulated data - it takes a while
linked1<-link( m.big ,data=dat_list_sim1)
linked2<-link( m.big ,data=dat_list_sim2)

linked3<-link( m.big ,data=dat_list_sim3)
linked4<-link( m.big ,data=dat_list_sim4)

linked5<-link( m.big ,data=dat_list_sim5)
linked6<-link( m.big ,data=dat_list_sim6)



png("Figure1.png",width=11,height=7,units="cm",res=600)

par(mar=c(1,4,1,0.1),mgp=c(2,0.8,0))
plot( NULL , type="n" , xlab="Rated characteristics" , ylab="Selection probability" ,
      xlim=c(0,0.57) , ylim=c(-0.2,1.2) , xaxp=c(0,1,1) , yaxp=c(0,1,2) ,xaxt="n",bty="n")

labcond<-c("Control","Pathogen","Scarcity")
colcond<-c("#333333","#FF6432","#3232FF")

xcoords<-c(0.1,0.3,0.5)
wid<-0.07
gap<-0.01
drawProbs(linked1,linked2,xc=xcoords[1],wid=wid,gap=gap)
drawProbs(linked3,linked4,xc=xcoords[2],wid=wid,gap=gap)
drawProbs(linked5,linked6,xc=xcoords[3],wid=wid,gap=gap)
abline(h=0.5,col="#DD0022",lty=2)

text(0,c(0.25,0.75),labels = c("mas.","fem."),col=c("#000000","#0088CC"),font=2,srt=90)
text(xcoords,1.1,labels=labcond,col=colcond,font=2)

text(xcoords[2],1.225,"Condition",xpd=T)
text(xcoords[2],-0.230,"Side (masculinized)",xpd=T)

dev.off()

#It is good to keep the RAM clean
rm(list=c("dat_list_sim1","linked1",
          "dat_list_sim2","linked2",
          "dat_list_sim3","linked3",
          "dat_list_sim4","linked4",
          "dat_list_sim5","linked5",
          "dat_list_sim6","linked6"))

#PP and RS
mean(post$bPP-post$bRS)
PI(post$bPP-post$bRS)

dat_list_sim7<-dat_list_big
dat_list_sim7$Attr_dif<-rep(0,n)
dat_list_sim7$Form_dif<-rep(0,n)
dat_list_sim7$Heal_dif<-rep(0,n)
dat_list_sim7$fCond<-rep(1,n)
dat_list_sim7$Masright<-rep(0,n)

# PPs -2 RSs 0
dat_list_sim7$PPs<-rep(-2,n)
dat_list_sim7$RSs<-rep(0,n)

# PPs 0 RSs 0
dat_list_sim8<-dat_list_sim7

dat_list_sim8$PPs<-rep(0,n)
dat_list_sim8$RSs<-rep(0,n)

# PPs 2 RSs 0
dat_list_sim9<-dat_list_sim7

dat_list_sim9$PPs<-rep(2,n)
dat_list_sim9$RSs<-rep(0,n)

# PPs 0 RSs -2
dat_list_sim10<-dat_list_sim7

dat_list_sim10$PPs<-rep(0,n)
dat_list_sim10$RSs<-rep(-2,n)

# PPs 0 RSs 2
dat_list_sim11<-dat_list_sim7

dat_list_sim11$PPs<-rep(0,n)
dat_list_sim11$RSs<-rep(2,n)


linked7<-link( m.big ,data=dat_list_sim7)
linked8<-link( m.big ,data=dat_list_sim8)
linked9<-link( m.big ,data=dat_list_sim9)

linked10<-link( m.big ,data=dat_list_sim10)
#linked8 here again
linked11<-link( m.big ,data=dat_list_sim11)

png("Figure2.png",width=11,height=7,units="cm",res=600)

par(mar=c(1,4,1,0.1),mgp=c(2,0.8,0))
plot( NULL , type="n" , xlab="Rated characteristics" , ylab="Selection probability" ,
      xlim=c(0,0.57) , ylim=c(-0.2,1.2) , xaxp=c(0,1,1) , yaxp=c(0,1,2) ,xaxt="n",bty="n")

labscale<-c("Pathogen prevalence","Resource scarcity")
colscale<-c("#FF6432","#3232FF")

xcoords<-c(0.15,0.45)
wid<-0.07
gap<-0.01

drawProbs3(linked7,linked8,linked9,xc=xcoords[1],wid=wid,gap=gap)
drawProbs3(linked10,linked8,linked11,xc=xcoords[2],wid=wid,gap=gap)

abline(h=0.5,col="#DD0022",lty=2)

text(0,c(0.25,0.75),labels = c("mas.","fem."),col=c("#000000","#0088CC"),font=2,srt=90)
text(xcoords,1.1,labels=labscale,col=colscale,font=2)

text(mean(xcoords),1.225,"Scale",xpd=T)

dev.off()

rm(list=c("dat_list_sim7","linked7",
          "dat_list_sim8","linked8",
          "dat_list_sim9","linked9",
          "dat_list_sim10","linked10",
          "dat_list_sim11","linked11"))


#Differences
mean(post$bAd[,1])
PI(post$bAd[,1],prob=0.89)

mean(post$bAd[,1]-post$bAd[,2])
PI(post$bAd[,1]-post$bAd[,2],prob=0.89)

mean(post$bAd[,1]-post$bAd[,3])
PI(post$bAd[,1]-post$bAd[,3],prob=0.89)


apply(post$bFd,2,mean)
apply(post$bFd,2,PI,prob=0.89)

apply(post$bHd,2,mean)
apply(post$bHd,2,PI,prob=0.89)


#differences
dat_list_sim12<-dat_list_big
dat_list_sim12$Masright<-rep(0,n)
dat_list_sim12$PPs<-rep(0,n)
dat_list_sim12$RSs<-rep(0,n)

dat_list_sim12$fCond<-rep(1,n)
dat_list_sim12$Attr_dif<-rep(-1,n)
dat_list_sim12$Form_dif<-rep(0,n)
dat_list_sim12$Heal_dif<-rep(0,n)

dat_list_sim13<-dat_list_sim12
dat_list_sim13$Attr_dif<-rep(1,n)


dat_list_sim14<-dat_list_sim12
dat_list_sim14$fCond<-rep(2,n)

dat_list_sim15<-dat_list_sim13
dat_list_sim15$fCond<-rep(2,n)

dat_list_sim16<-dat_list_sim12
dat_list_sim16$fCond<-rep(3,n)

dat_list_sim17<-dat_list_sim13
dat_list_sim17$fCond<-rep(3,n)

#Difference in Formidability
dat_list_sim18<-dat_list_sim12
dat_list_sim18$Attr_dif<-rep(0,n)
dat_list_sim18$Form_dif<-rep(-1,n)
dat_list_sim18$Heal_dif<-rep(0,n)

dat_list_sim19<-dat_list_sim18
dat_list_sim19$Form_dif<-rep(1,n)

dat_list_sim20<-dat_list_sim18
dat_list_sim20$fCond<-rep(2,n)

dat_list_sim21<-dat_list_sim19
dat_list_sim21$fCond<-rep(2,n)

dat_list_sim22<-dat_list_sim18
dat_list_sim22$fCond<-rep(3,n)

dat_list_sim23<-dat_list_sim19
dat_list_sim23$fCond<-rep(3,n)

#Difference in Healthiness
dat_list_sim24<-dat_list_sim12
dat_list_sim24$Attr_dif<-rep(0,n)
dat_list_sim24$Form_dif<-rep(0,n)
dat_list_sim24$Heal_dif<-rep(-1,n)

dat_list_sim25<-dat_list_sim24
dat_list_sim25$Heal_dif<-rep(1,n)

dat_list_sim26<-dat_list_sim24
dat_list_sim26$fCond<-rep(2,n)

dat_list_sim27<-dat_list_sim25
dat_list_sim27$fCond<-rep(2,n)

dat_list_sim28<-dat_list_sim24
dat_list_sim28$fCond<-rep(3,n)

dat_list_sim29<-dat_list_sim25
dat_list_sim29$fCond<-rep(3,n)

str(dat_list_sim12)

#Attr
linked12<-link( m.big ,data=dat_list_sim12)
linked13<-link( m.big ,data=dat_list_sim13)

linked14<-link( m.big ,data=dat_list_sim14)
linked15<-link( m.big ,data=dat_list_sim15)

linked16<-link( m.big ,data=dat_list_sim16)
linked17<-link( m.big ,data=dat_list_sim17)

#Form
linked18<-link( m.big ,data=dat_list_sim18)
linked19<-link( m.big ,data=dat_list_sim19)

linked20<-link( m.big ,data=dat_list_sim20)
linked21<-link( m.big ,data=dat_list_sim21)

linked22<-link( m.big ,data=dat_list_sim22)
linked23<-link( m.big ,data=dat_list_sim23)

#Heal
linked24<-link( m.big ,data=dat_list_sim24)
linked25<-link( m.big ,data=dat_list_sim25)

linked26<-link( m.big ,data=dat_list_sim26)
linked27<-link( m.big ,data=dat_list_sim27)

linked28<-link( m.big ,data=dat_list_sim28)
linked29<-link( m.big ,data=dat_list_sim29)


col.char<-c("#009696","#960096","#969600")

png("Figure3.png",width=11,height=12,units="cm",res=600)

par(mar=c(1,4,1,0.1),mgp=c(2,0.8,0))
plot( NULL , type="n" , xlab="" , ylab="Selection probability" ,
      xlim=c(0,0.57) , ylim=c(-3,1.2) , xaxp=c(0,1,1) , yaxp=c(0,1,2) ,xaxt="n", bty="n")

labcond<-c("Control","Pathogen","Scarcity")
colcond<-c("#333333","#FF6432","#3232FF")

xcoords<-c(0.1,0.3,0.5)
ycoords<-c(0,-1.4,-2.8)

ofs<-0
ofs2<--0.14

wid<-0.07
gap<-0.01
labs=c(-1,1)
drawProbs(linked12,linked13,xc=xcoords[1],wid=wid,gap=gap,labels = labs,col.text = col.char[1],yb=ycoords[1],ofs=ofs)
drawProbs(linked14,linked15,xc=xcoords[2],wid=wid,gap=gap,labels = labs,col.text = col.char[1],yb=ycoords[1],ofs=ofs)
drawProbs(linked16,linked17,xc=xcoords[3],wid=wid,gap=gap,labels = labs,col.text = col.char[1],yb=ycoords[1],ofs=ofs)

text(0,ycoords[1]+ofs2,"Attr.",xpd=T,col=col.char[1],font=2)

abline(h=0.5,col="#DD0022",lty=2)
text(0,c(0.25,0.75),labels = c("mas.","fem."),col=c("#000000","#0088CC"),font=2,srt=90)

drawProbs(linked18,linked19,xc=xcoords[1],wid=wid,gap=gap,labels = labs,col.text = col.char[2],yb=ycoords[2],ofs=ofs)
drawProbs(linked20,linked21,xc=xcoords[2],wid=wid,gap=gap,labels = labs,col.text = col.char[2],yb=ycoords[2],ofs=ofs)
drawProbs(linked22,linked23,xc=xcoords[3],wid=wid,gap=gap,labels = labs,col.text = col.char[2],yb=ycoords[2],ofs=ofs)

text(0,ycoords[2]+ofs2,"Form.",xpd=T,col=col.char[2],font=2)

abline(h=0.5+ycoords[2],col="#DD0022",lty=2)
text(0,c(0.25,0.75)+ycoords[2],labels = c("mas.","fem."),col=c("#000000","#0088CC"),font=2,srt=90)

axis(2,at=c(0,0.5,1)+ycoords[2],labels=c(0,0.5,1))


drawProbs(linked24,linked25,xc=xcoords[1],wid=wid,gap=gap,labels = labs,col.text = col.char[3],yb=ycoords[3],ofs=ofs)
drawProbs(linked26,linked27,xc=xcoords[2],wid=wid,gap=gap,labels = labs,col.text = col.char[3],yb=ycoords[3],ofs=ofs)
drawProbs(linked28,linked29,xc=xcoords[3],wid=wid,gap=gap,labels = labs,col.text = col.char[3],yb=ycoords[3],ofs=ofs)

text(0,ycoords[3]+ofs2,"Heal.",xpd=T,col=col.char[3],font=2)

abline(h=0.5+ycoords[3],col="#DD0022",lty=2)
text(0,c(0.25,0.75)+ycoords[3],labels = c("mas.","fem."),col=c("#000000","#0088CC"),font=2,srt=90)

axis(2,at=c(0,0.5,1)+ycoords[3],labels=c(0,0.5,1))


text(xcoords,1.1,labels=labcond,col=colcond,font=2)

text(xcoords[2],1.3,"Condition",xpd=T)
text(xcoords[2],-3.2,"Difference between mas. and fem. ratings" ,xpd=T)

dev.off()

rm(list=c("dat_list_sim12","linked12",
          "dat_list_sim13","linked13",
          "dat_list_sim14","linked14",
          "dat_list_sim15","linked15",
          "dat_list_sim16","linked16",
          "dat_list_sim17","linked17",
          "dat_list_sim18","linked18",
          "dat_list_sim19","linked19",
          "dat_list_sim20","linked20",
          "dat_list_sim21","linked21",
          "dat_list_sim22","linked22",
          "dat_list_sim23","linked23",
          "dat_list_sim24","linked24",
          "dat_list_sim25","linked25",
          "dat_list_sim26","linked26",
          "dat_list_sim27","linked27",
          "dat_list_sim28","linked28",
          "dat_list_sim29","linked29"))


#Varying intercepts
mean(post$sigma_f)
PI(post$sigma_f,prob=0.89)

mean(post$sigma_r)
PI(post$sigma_r,prob=0.89)

cor(post$sigma_f,post$sigma_r)

mean(post$sigma_f-post$sigma_r)
PI(post$sigma_f-post$sigma_r,prob=0.89)

#how the prob
pr<-linked$p
pr2<-1-pr

str(pr)
str(pr2)
comp<-cbind(c(pr),c(pr2))

guess<-ifelse(pr>=0.5,1,0)
str(guess)
is<-d$Selmas
hit<-apply(guess,1,function(guess){guess==is})
str(hit)

#Success rate per sample
hitrat<-colSums(hit)/n

#Success rate per data point
hitratI<-rowSums(hit)/s

plotcol<-"#FF6600"

#Success rate per individual
png("Figure4A.png",width=24,height=12,units="cm",res=600)
par(mfrow=c(1,2),mar=c(4,4,2,1),oma=c(0,0,2,0))
hist(hitratI,col=plotcol,xlab="Success rate",main="Success rate per data point",breaks=9)
plot(density(hitrat),main="Success rate per posterior sample",bty="n",xlim=c(0.4,0.7))
polygon(density(hitrat),col=plotcol)
mtext("Full model with varying intercepts", side=3, line=0.3, adj=0.5, cex=1.5,font=2, col=plotcol, outer=TRUE)  
dev.off()


#Prediction precision per individual
summary(as.factor(cut(hitratI,10)))/n

#Per posterior sample
mean(hitrat)
PI(hitrat,prob=0.89)


#Naive model without varying intercepts
preds<-sapply(1:s,function(i){
logitp<- with(post,ac[i,d$fCond] + 
  bAd[i,d$fCond]*d$Attr_dif + bFd[i,d$fCond]*d$Form_dif + bHd[i,d$fCond]*d$Heal_dif + 
  bPP[i]*d$PPs + bRS[i]*d$RSs + 
  bs[i]*d$Masright )
return(inv_logit(logitp))})

#preds is not transposed relative to prevous pr, to i will convert it to the same rotation
pr<-t(preds)
pr2<-1-pr

str(pr)
str(pr2)
comp<-cbind(c(pr),c(pr2))

guess<-ifelse(pr>=0.5,1,0)
str(guess)
is<-d$Selmas
hit<-apply(guess,1,function(guess){guess==is})
str(hit)

#Success rate per sample
hitrat<-colSums(hit)/n

#Success rate per data point
hitratI<-rowSums(hit)/s

plotcol<-"#00AAFF"

#Success rate per individual
png("Figure4B.png",width=24,height=12,units="cm",res=600)
par(mfrow=c(1,2),mar=c(4,4,2,1),oma=c(0,0,2,0))
hist(hitratI,col=plotcol,xlab="Success rate",main="Success rate per data point",breaks=9)
plot(density(hitrat),main="Success rate per posterior sample",bty="n",xlim=c(0.4,0.7))
polygon(density(hitrat),col=plotcol)
mtext("Naive model without varying intercepts", side=3, line=0.3, adj=0.5, cex=1.5,font=2, col=plotcol, outer=TRUE)  
dev.off()

#Prediction precision per individual
summary(as.factor(cut(hitratI,10)))/n

#Per posterior sample
mean(hitrat)
PI(hitrat,prob=0.89)


#Predicton of rating

#simulated data for mas/fem ratings and difference between them - only few vectors are changed
dat_list_sim30<-dat_list_big
dat_list_sim30$PPs<-rep(0,n)
dat_list_sim30$RSs<-rep(0,n)
dat_list_sim30$fCond<-rep(1,n)

linked30<-link( m.big ,data=dat_list_sim30)
sampled.dif30<-sample_dif(m.big,post,dat_list_sim30,linked30)
summed30<-summRatings(sampled.dif30)

#Pathogen condition
dat_list_sim31<-dat_list_sim30
dat_list_sim31$fCond<-rep(2,n)

linked31<-link( m.big ,data=dat_list_sim31)
sampled.dif31<-sample_dif(m.big,post,dat_list_sim31,linked31)
summed31<-summRatings(sampled.dif31)

#RS condition
dat_list_sim32<-dat_list_sim30
dat_list_sim32$fCond<-rep(3,n)

linked32<-link( m.big ,data=dat_list_sim32)
sampled.dif32<-sample_dif(m.big,post,dat_list_sim32,linked32)
summed32<-summRatings(sampled.dif32)

#save.image("dif_summaries.RData")
#load("dif_summaries.RData")


#Differences in regression slopes Attractiveness
mean(post$acA_mas[,1]-post$acA_fem[,1])
PI(post$acA_mas[,1]-post$acA_fem[,1],prob=0.89)

mean(post$acA_mas[,2]-post$acA_fem[,2])
PI(post$acA_mas[,2]-post$acA_fem[,2],prob=0.89)

mean(post$acA_mas[,3]-post$acA_fem[,3])
PI(post$acA_mas[,3]-post$acA_fem[,3],prob=0.89)

#The differeces between slope differences do not really difer from teh simle differences between posterior characteristics 
c1<-(post$acA_mas[,1]-post$acA_fem[,1])
c2<-(post$acA_mas[,2]-post$acA_fem[,2])
c3<-(post$acA_mas[,3]-post$acA_fem[,3])

mean(c1-c2)
PI(c1-c2,prob=0.89)

mean(c1-c3)
PI(c1-c3,prob=0.89)

mean(c2-c3)
PI(c1-c3,prob=0.89)

#expected differences
mean(summed30$summA$meandifA)
PI(summed30$summA$meandifA)

mean(summed31$summA$meandifA)
PI(summed31$summA$meandifA)

mean(summed32$summA$meandifA)
PI(summed32$summA$meandifA)

round(summed30$summA$rdifA1*100,1)
round(summed30$summA$rdifA2*100,1)

#Extract mean ratings...
mean(summed30$summA$meanmasA)
PI(summed30$summA$meanmasA,prob=0.89)
mean(summed30$summA$meanfemA)
PI(summed30$summA$meanfemA,prob=0.89)

mean(summed31$summA$meanmasA)
PI(summed31$summA$meanmasA,prob=0.89)
mean(summed31$summA$meanfemA)
PI(summed31$summA$meanfemA,prob=0.89)

mean(summed32$summA$meanmasA)
PI(summed32$summA$meanmasA,prob=0.89)
mean(summed32$summA$meanfemA)
PI(summed32$summA$meanfemA,prob=0.89)

#varying intercepts
mean(post$sigma_fA)
PI(post$sigma_fA,prob=0.89)

mean(post$sigma_rA)
PI(post$sigma_rA,prob=0.89)

mean(post$sigma_iA)
PI(post$sigma_iA,prob=0.89)


#Formidability
#Slope difference

mean(post$acF_mas[,1]-post$acF_fem[,1])
PI(post$acF_mas[,1]-post$acF_fem[,1],prob=0.89)

mean(post$acF_mas[,2]-post$acF_fem[,2])
PI(post$acF_mas[,2]-post$acF_fem[,2],prob=0.89)

mean(post$acF_mas[,3]-post$acF_fem[,3])
PI(post$acF_mas[,3]-post$acF_fem[,3],prob=0.89)

c1<-(post$acF_mas[,1]-post$acF_fem[,1])
c2<-(post$acF_mas[,2]-post$acF_fem[,2])
c3<-(post$acF_mas[,3]-post$acF_fem[,3])

mean(c1-c2)
PI(c1-c2,prob=0.89)

mean(c1-c3)
PI(c1-c3,prob=0.89)

mean(c2-c3)
PI(c1-c3,prob=0.89)

#prediction of the differences between mas and fem
mean(summed30$summF$meandifF)
PI(summed30$summF$meandifF)

mean(summed31$summF$meandifF)
PI(summed31$summF$meandifF)

mean(summed32$summF$meandifF)
PI(summed32$summF$meandifF)

round(summed30$summF$rdifF1*100,1)
round(summed30$summF$rdifF2*100,1)

round(summed31$summF$rdifF1*100,1)
round(summed31$summF$rdifF2*100,1)

round(summed32$summF$rdifF1*100,1)
round(summed32$summF$rdifF2*100,1)


#Extract mean ratings...
mean(summed30$summF$meanmasF)
PI(summed30$summF$meanmasF,prob=0.89)
mean(summed30$summF$meanfemF)
PI(summed30$summF$meanfemF,prob=0.89)

mean(summed31$summF$meanmasF)
PI(summed31$summF$meanmasF,prob=0.89)
mean(summed31$summF$meanfemF)
PI(summed31$summF$meanfemF,prob=0.89)

mean(summed32$summF$meanmasF)
PI(summed32$summF$meanmasF,prob=0.89)
mean(summed32$summF$meanfemF)
PI(summed32$summF$meanfemF,prob=0.89)

#Varying intercepts
mean(post$sigma_fF)
PI(post$sigma_fF,prob=0.89)

mean(post$sigma_rF)
PI(post$sigma_rF,prob=0.89)

mean(post$sigma_iF)
PI(post$sigma_iF,prob=0.89)



#Healthiness
#Slope difference

mean(post$acH_mas[,1]-post$acH_fem[,1])
PI(post$acH_mas[,1]-post$acH_fem[,1],prob=0.89)

mean(post$acH_mas[,2]-post$acH_fem[,2])
PI(post$acH_mas[,2]-post$acH_fem[,2],prob=0.89)

mean(post$acH_mas[,3]-post$acH_fem[,3])
PI(post$acH_mas[,3]-post$acH_fem[,3],prob=0.89)

c1<-(post$acH_mas[,1]-post$acH_fem[,1])
c2<-(post$acH_mas[,2]-post$acH_fem[,2])
c3<-(post$acH_mas[,3]-post$acH_fem[,3])

mean(c1-c2)
PI(c1-c2,prob=0.89)

mean(c1-c3)
PI(c1-c3,prob=0.89)

mean(c2-c3)
PI(c1-c3,prob=0.89)

#prediction of the differences between mas and fem
mean(summed30$summH$meandifH)
PI(summed30$summH$meandifH)

mean(summed31$summH$meandifH)
PI(summed31$summH$meandifH)

mean(summed32$summH$meandifH)
PI(summed32$summH$meandifH)

round(summed30$summH$rdifH1*100,1)
round(summed30$summH$rdifH2*100,1)

round(summed31$summH$rdifH1*100,1)
round(summed31$summH$rdifH2*100,1)

round(summed32$summH$rdifH1*100,1)
round(summed32$summH$rdifH2*100,1)

#Extract mean ratings...
mean(summed30$summH$meanmasH)
PI(summed30$summH$meanmasH,prob=0.89)
mean(summed30$summH$meanfemH)
PI(summed30$summH$meanfemH,prob=0.89)

mean(summed31$summH$meanmasH)
PI(summed31$summH$meanmasH,prob=0.89)
mean(summed31$summH$meanfemH)
PI(summed31$summH$meanfemH,prob=0.89)

mean(summed32$summH$meanmasH)
PI(summed32$summH$meanmasH,prob=0.89)
mean(summed32$summH$meanfemH)
PI(summed32$summH$meanfemH,prob=0.89)

#Varying intercepts
mean(post$sigma_fH)
PI(post$sigma_fH,prob=0.89)

mean(post$sigma_rH)
PI(post$sigma_rH,prob=0.89)

mean(post$sigma_iH)
PI(post$sigma_iH,prob=0.89)



#Plotting
coldifs<-c("#009696","#960096","#969600")
labrats<-c("Attractiveness","Formidability","Healthiness")

labcond<-c("Control","Pathogen","Scarcity")
colcond<-c("#333333","#FF6432","#3232FF")

xcoords<-c(0.1,0.3,0.5)
ycoords<-c(0,-1.7,-3.4)

png("Figure5.png",width=11,height=14,units="cm",res=600)

par(mar=c(1.5,4,0.5,1),mgp=c(2,0.8,0))
plot( NULL , type="n" , xlab="Rated characteristics" , ylab="Cummulative probability of rating" ,
      xlim=c(0,0.57) , ylim=c(-3.85,1.35) , xaxp=c(0,1,1) , yaxp=c(0,1,2) ,xaxt="n",bty="n")

#text(xcoords,1.2,labels=labrats,col=coldifs,font=2)

wid<-0.07
gap<-0.01

wid2<-0.030

ofs<--0.07
ofs2<--0.075
ofs3<--0.3

cex1<-0.8
cex2<-0.7
cex3<-0.8
cex4<-1

lowdif<--0.45

coltextdif<-"#707070"

text(xcoords,1.15,labels=labcond,col=colcond,font=2)
text(xcoords[2],1.38,"Condition",xpd=T)

axis(2,at=c(0,0.5,1)+ycoords[2],labels=c(0,0.5,1))
axis(2,at=c(0,0.5,1)+ycoords[3],labels=c(0,0.5,1))

text(0.0,seq(0.05,1-0.05,l=7)+ycoords[1],labels = 1:7,col=rev(rainbow(7)),font=2,cex=cex3)
text(0.0,seq(0.05,1-0.05,l=7)+ycoords[2],labels = 1:7,col=rev(rainbow(7)),font=2,cex=cex3)
text(0.0,seq(0.05,1-0.05,l=7)+ycoords[3],labels = 1:7,col=rev(rainbow(7)),font=2,cex=cex3)

text(-0.02,ycoords[1]+ofs3,"Attr.",xpd=T,col=coldifs[1],font=2,cex=cex4)
text(-0.02,ycoords[2]+ofs3,"Form.",xpd=T,col=coldifs[2],font=2,cex=cex4)
text(-0.02,ycoords[3]+ofs3,"Heal.",xpd=T,col=coldifs[3],font=2,cex=cex4)


drawRatings(summed30$summA,xc=xcoords[1],yb=ycoords[1],col.text = coldifs[1],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed30$summF,xc=xcoords[1],yb=ycoords[2],col.text = coldifs[2],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed30$summH,xc=xcoords[1],yb=ycoords[3],col.text = coldifs[3],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)

drawDiff(summed30$summA,xc=xcoords[1],yb=ycoords[1]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[1],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed30$summF,xc=xcoords[1],yb=ycoords[2]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[2],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed30$summH,xc=xcoords[1],yb=ycoords[3]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[3],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)


drawRatings(summed31$summA,xc=xcoords[2],yb=ycoords[1],col.text = coldifs[1],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed31$summF,xc=xcoords[2],yb=ycoords[2],col.text = coldifs[2],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed31$summH,xc=xcoords[2],yb=ycoords[3],col.text = coldifs[3],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)

drawDiff(summed31$summA,xc=xcoords[2],yb=ycoords[1]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[1],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed31$summF,xc=xcoords[2],yb=ycoords[2]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[2],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed31$summH,xc=xcoords[2],yb=ycoords[3]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[3],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)


drawRatings(summed32$summA,xc=xcoords[3],yb=ycoords[1],col.text = coldifs[1],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed32$summF,xc=xcoords[3],yb=ycoords[2],col.text = coldifs[2],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed32$summH,xc=xcoords[3],yb=ycoords[3],col.text = coldifs[3],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)

drawDiff(summed32$summA,xc=xcoords[3],yb=ycoords[1]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[1],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed32$summF,xc=xcoords[3],yb=ycoords[2]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[2],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed32$summH,xc=xcoords[3],yb=ycoords[3]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[3],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)

mtext("Expected distribution of ratings and differences",1)

dev.off()

rm(list=c("dat_list_sim30","linked30","sampled.dif30","summed30",
          "dat_list_sim31","linked31","sampled.dif31","summed31",
          "dat_list_sim32","linked32","sampled.dif32","summed32"))


#simulated data for differences between PP and RS
#Extreme PP sets
dat_list_sim33<-dat_list_big
dat_list_sim33$fCond<-rep(1,n)
dat_list_sim33$PPs<-rep(-2,n)
dat_list_sim33$RSs<-rep(0,n)

linked33<-link( m.big ,data=dat_list_sim33)
sampled.dif33<-sample_dif(m.big,post,dat_list_sim33,linked33)
summed33<-summRatings(sampled.dif33)

dat_list_sim34<-dat_list_sim33
dat_list_sim34$PPs<-rep(2,n)

linked34<-link( m.big ,data=dat_list_sim34)
sampled.dif34<-sample_dif(m.big,post,dat_list_sim34,linked34)
summed34<-summRatings(sampled.dif34)


#Extreme RS sets
dat_list_sim35<-dat_list_sim33
dat_list_sim35$PPs<-rep(0,n)
dat_list_sim35$RSs<-rep(-2,n)

linked35<-link( m.big ,data=dat_list_sim35)
sampled.dif35<-sample_dif(m.big,post,dat_list_sim35,linked35)
summed35<-summRatings(sampled.dif35)

dat_list_sim36<-dat_list_sim35
dat_list_sim36$RSs<-rep(2,n)

linked36<-link( m.big ,data=dat_list_sim36)
sampled.dif36<-sample_dif(m.big,post,dat_list_sim36,linked36)
summed36<-summRatings(sampled.dif36)

#save.image("dif_summaries2.RData")
#load("dif_summaries2.RData")

#Influence PP on attractiveness
mean(post$bPPA_mas)
PI(post$bPPA_mas,prob=0.89)

mean(post$bPPA_fem)
PI(post$bPPA_fem,prob=0.89)

mean(post$bPPA_mas-post$bPPA_fem)
PI(post$bPPA_mas-post$bPPA_fem,prob=0.89)

#Extract mean ratings...
mean(summed33$summA$meanmasA)
PI(summed33$summA$meanmasA,prob=0.89)
mean(summed33$summA$meanfemA)
PI(summed33$summA$meanfemA,prob=0.89)

mean(summed34$summA$meanmasA)
PI(summed34$summA$meanmasA,prob=0.89)
mean(summed34$summA$meanfemA)
PI(summed34$summA$meanfemA,prob=0.89)

#expected differences
mean(summed33$summA$meandifA)
PI(summed33$summA$meandifA)

mean(summed34$summA$meandifA)
PI(summed34$summA$meandifA)

mean(summed33$summA$meandifA-summed34$summA$meandifA)
PI(summed33$summA$meandifA-summed34$summA$meandifA)

#RS
mean(post$bRSA_mas)
PI(post$bRSA_mas,prob=0.89)

mean(post$bRSA_fem)
PI(post$bRSA_fem,prob=0.89)

mean(post$bRSA_mas-post$bRSA_fem)
PI(post$bRSA_mas-post$bRSA_fem,prob=0.89)

#Extract mean ratings...
mean(summed35$summA$meanmasA)
PI(summed35$summA$meanmasA,prob=0.89)
mean(summed35$summA$meanfemA)
PI(summed35$summA$meanfemA,prob=0.89)

mean(summed36$summA$meanmasA)
PI(summed36$summA$meanmasA,prob=0.89)
mean(summed36$summA$meanfemA)
PI(summed36$summA$meanfemA,prob=0.89)

#expected differences
mean(summed35$summA$meandifA)
PI(summed35$summA$meandifA)

mean(summed36$summA$meandifA)
PI(summed36$summA$meandifA)

mean(summed35$summA$meandifA-summed36$summA$meandifA)
PI(summed35$summA$meandifA-summed36$summA$meandifA)



#Influence PP on formidability
mean(post$bPPF_mas)
PI(post$bPPF_mas,prob=0.89)

mean(post$bPPF_fem)
PI(post$bPPF_fem,prob=0.89)

mean(post$bPPF_mas-post$bPPF_fem)
PI(post$bPPF_mas-post$bPPF_fem,prob=0.89)

#Extract mean ratings...
mean(summed33$summF$meanmasF)
PI(summed33$summF$meanmasF,prob=0.89)
mean(summed33$summF$meanfemF)
PI(summed33$summF$meanfemF,prob=0.89)

mean(summed34$summF$meanmasF)
PI(summed34$summF$meanmasF,prob=0.89)
mean(summed34$summF$meanfemF)
PI(summed34$summF$meanfemF,prob=0.89)

#expected differences
mean(summed33$summF$meandifF)
PI(summed33$summF$meandifF)

mean(summed34$summF$meandifF)
PI(summed34$summF$meandifF)

mean(summed33$summF$meandifF-summed34$summF$meandifF)
PI(summed33$summF$meandifF-summed34$summF$meandifF)

#RS
mean(post$bRSF_mas)
PI(post$bRSF_mas,prob=0.89)

mean(post$bRSF_fem)
PI(post$bRSF_fem,prob=0.89)

mean(post$bRSF_mas-post$bRSF_fem)
PI(post$bRSF_mas-post$bRSF_fem,prob=0.89)

#Extract mean ratings...
mean(summed35$summF$meanmasF)
PI(summed35$summF$meanmasF,prob=0.89)
mean(summed35$summF$meanfemF)
PI(summed35$summF$meanfemF,prob=0.89)

mean(summed36$summF$meanmasF)
PI(summed36$summF$meanmasF,prob=0.89)
mean(summed36$summF$meanfemF)
PI(summed36$summF$meanfemF,prob=0.89)

#expected differences
mean(summed35$summF$meandifF)
PI(summed35$summF$meandifF)

mean(summed36$summF$meandifF)
PI(summed36$summF$meandifF)

mean(summed35$summF$meandifF-summed36$summF$meandifF)
PI(summed35$summF$meandifF-summed36$summF$meandifF)



#Influence PP on healthiness
mean(post$bPPH_mas)
PI(post$bPPH_mas,prob=0.89)

mean(post$bPPH_fem)
PI(post$bPPH_fem,prob=0.89)

mean(post$bPPH_mas-post$bPPH_fem)
PI(post$bPPH_mas-post$bPPH_fem,prob=0.89)

#Extract mean ratings...
mean(summed33$summH$meanmasH)
PI(summed33$summH$meanmasH,prob=0.89)
mean(summed33$summH$meanfemH)
PI(summed33$summH$meanfemH,prob=0.89)

mean(summed34$summH$meanmasH)
PI(summed34$summH$meanmasH,prob=0.89)
mean(summed34$summH$meanfemH)
PI(summed34$summH$meanfemH,prob=0.89)

#expected differences
mean(summed33$summH$meandifH)
PI(summed33$summH$meandifH)

mean(summed34$summH$meandifH)
PI(summed34$summH$meandifH)

mean(summed33$summH$meandifH-summed34$summH$meandifH)
PI(summed33$summH$meandifH-summed34$summH$meandifH)

#RS
mean(post$bRSH_mas)
PI(post$bRSH_mas,prob=0.89)

mean(post$bRSH_fem)
PI(post$bRSH_fem,prob=0.89)

mean(post$bRSH_mas-post$bRSH_fem)
PI(post$bRSH_mas-post$bRSH_fem,prob=0.89)

#Extract mean ratings...
mean(summed35$summH$meanmasH)
PI(summed35$summH$meanmasH,prob=0.89)
mean(summed35$summH$meanfemH)
PI(summed35$summH$meanfemH,prob=0.89)

mean(summed36$summH$meanmasH)
PI(summed36$summH$meanmasH,prob=0.89)
mean(summed36$summH$meanfemH)
PI(summed36$summH$meanfemH,prob=0.89)

#expected differences
mean(summed35$summH$meandifH)
PI(summed35$summH$meandifH)

mean(summed36$summH$meandifH)
PI(summed36$summH$meandifH)

mean(summed35$summH$meandifH-summed36$summH$meandifH)
PI(summed35$summH$meandifH-summed36$summH$meandifH)




#Plotting
coldifs<-c("#009696","#960096","#969600")
labrats<-c("Attractiveness","Formidability","Healthiness")

labcond<-c("Pathogen prevalence","Resource scarcity")
colcond<-c("#FF6432","#3232FF")

xcoords<-c(0.1,0.27,0.47,0.64)
ycoords<-c(0,-1.7,-3.4)

png("Figure6.png",width=13,height=14,units="cm",res=600)

par(mar=c(1.5,4,0.5,1),mgp=c(2,0.8,0))
plot( NULL , type="n" , xlab="Rated characteristics" , ylab="Cummulative probability of rating" ,
      xlim=c(0,0.70) , ylim=c(-3.85,1.35) , xaxp=c(0,1,1) , yaxp=c(0,1,2) ,xaxt="n",bty="n")

#text(xcoords,1.2,labels=labrats,col=coldifs,font=2)

wid<-0.07
gap<-0.01

wid2<-0.030

ofs<--0.07
ofs2<--0.075
ofs3<--0.3

cex1<-0.8
cex2<-0.7
cex3<-0.8
cex4<-1

lowdif<--0.45

coltextdif<-"#707070"

text(c(mean(xcoords[1:2]),mean(xcoords[3:4])),1.30,labels=labcond,col=colcond,font=2)
text(mean(xcoords[2:3]),1.46,"Scale",xpd=T)

text(xcoords,1.12,labels=c("-2SD","+2SD"),col=paste(rep(colcond,each=2),"FF",sep=""),font=2,cex=0.9)


axis(2,at=c(0,0.5,1)+ycoords[2],labels=c(0,0.5,1))
axis(2,at=c(0,0.5,1)+ycoords[3],labels=c(0,0.5,1))

text(0.0,seq(0.05,1-0.05,l=7)+ycoords[1],labels = 1:7,col=rev(rainbow(7)),font=2,cex=cex3)
text(0.0,seq(0.05,1-0.05,l=7)+ycoords[2],labels = 1:7,col=rev(rainbow(7)),font=2,cex=cex3)
text(0.0,seq(0.05,1-0.05,l=7)+ycoords[3],labels = 1:7,col=rev(rainbow(7)),font=2,cex=cex3)

text(-0.02,ycoords[1]+ofs3,"Attr.",xpd=T,col=coldifs[1],font=2,cex=cex4)
text(-0.02,ycoords[2]+ofs3,"Form.",xpd=T,col=coldifs[2],font=2,cex=cex4)
text(-0.02,ycoords[3]+ofs3,"Heal.",xpd=T,col=coldifs[3],font=2,cex=cex4)


drawRatings(summed33$summA,xc=xcoords[1],yb=ycoords[1],col.text = coldifs[1],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed33$summF,xc=xcoords[1],yb=ycoords[2],col.text = coldifs[2],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed33$summH,xc=xcoords[1],yb=ycoords[3],col.text = coldifs[3],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)

drawDiff(summed33$summA,xc=xcoords[1],yb=ycoords[1]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[1],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed33$summF,xc=xcoords[1],yb=ycoords[2]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[2],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed33$summH,xc=xcoords[1],yb=ycoords[3]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[3],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)


drawRatings(summed34$summA,xc=xcoords[2],yb=ycoords[1],col.text = coldifs[1],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed34$summF,xc=xcoords[2],yb=ycoords[2],col.text = coldifs[2],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed34$summH,xc=xcoords[2],yb=ycoords[3],col.text = coldifs[3],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)

drawDiff(summed34$summA,xc=xcoords[2],yb=ycoords[1]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[1],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed34$summF,xc=xcoords[2],yb=ycoords[2]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[2],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed34$summH,xc=xcoords[2],yb=ycoords[3]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[3],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)


drawRatings(summed35$summA,xc=xcoords[3],yb=ycoords[1],col.text = coldifs[1],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed35$summF,xc=xcoords[3],yb=ycoords[2],col.text = coldifs[2],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed35$summH,xc=xcoords[3],yb=ycoords[3],col.text = coldifs[3],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)

drawDiff(summed35$summA,xc=xcoords[3],yb=ycoords[1]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[1],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed35$summF,xc=xcoords[3],yb=ycoords[2]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[2],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed35$summH,xc=xcoords[3],yb=ycoords[3]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[3],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)


drawRatings(summed36$summA,xc=xcoords[4],yb=ycoords[1],col.text = coldifs[1],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed36$summF,xc=xcoords[4],yb=ycoords[2],col.text = coldifs[2],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)
drawRatings(summed36$summH,xc=xcoords[4],yb=ycoords[3],col.text = coldifs[3],wid=wid,gap=gap,ofs=ofs,cex.text = cex1)

drawDiff(summed36$summA,xc=xcoords[4],yb=ycoords[1]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[1],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed36$summF,xc=xcoords[4],yb=ycoords[2]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[2],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)
drawDiff(summed36$summH,xc=xcoords[4],yb=ycoords[3]+lowdif,relhei = 0.50,maxabs = 2,ofs=ofs2,col=coldifs[3],wid=wid2,draw.mean = F,cex.text = cex2,col.text=coltextdif)

mtext("Expected distribution of ratings and differences",1)

dev.off()

rm(list=c("dat_list_sim33","linked33","sampled.dif33","summed33",
          "dat_list_sim34","linked34","sampled.dif34","summed34",
          "dat_list_sim35","linked35","sampled.dif35","summed35",
          "dat_list_sim36","linked36","sampled.dif36","summed36"))


#The model of PP and RS scale
apply(post$a_PPs,2,mean)
apply(post$a_PPs,2,PI,prob=0.89)

mean(post$a_PPs[,1]-post$a_PPs[,2])
PI(post$a_PPs[,1]-post$a_PPs[,2],prob=0.89)

mean(post$a_PPs[,1]-post$a_PPs[,3])
PI(post$a_PPs[,1]-post$a_PPs[,3],prob=0.89)


mean(post$a_RSs[,1]-post$a_RSs[,2])
PI(post$a_RSs[,1]-post$a_RSs[,2],prob=0.89)

mean(post$a_RSs[,1]-post$a_RSs[,3])
PI(post$a_RSs[,1]-post$a_RSs[,3],prob=0.89)

#Standard deviations
mean(post$sigma_PPs)
PI(post$sigma_PPs,prob=0.89)

mean(post$sigma_RSs)
PI(post$sigma_RSs,prob=0.89)

#Plot it
library(vioplot)
source("01_data_prepare.R")

colcond<-c("#333333","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05

png("Figure7.png",h=11,w=9,units="cm",res=600)

par(mgp=c(2,0.6,0),mar=c(3.2,3.2,2,1))
vioplot(descalePP(dat_list_big$PPsU)~dat_list_big$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="Perceived Pathogen prevalence",xlab="Condition",border=F)
mpost<-post$a_PPs
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
abline(h=descalePP(0),lwd=1.5,col="#00000080")
dev.off()


colcond<-c("#333333","#FF6432","#3232FF")
labcond<-c("Control","Pathogen","Scarcity")

col.pol<-"#000000"
lwd.pol<-1

wscaler<-0.05

png("Figure8.png",h=11,w=9,units="cm",res=600)

par(mgp=c(2,0.6,0),mar=c(3.2,3.2,2,1))
vioplot(descaleRS(dat_list_big$RSsU)~dat_list_big$fCondU,drawRect=F,col=paste(colcond,80,sep=""),names=labcond,ylab="Perceived Resource scarcity",xlab="Condition",border=F)
mpost<-post$a_RSs
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
abline(h=descaleRS(0),lwd=1.5,col="#00000080")
dev.off()

