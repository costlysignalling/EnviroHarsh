source("01_data_prepare.R")
source("visualization_functions.R")

library(rethinking)
library(pracma)

dat_list <- list(
  Selmas = d$Selmas,
  Masright=d$Masright,
  fCond = as.integer(d$fCond),
  face_id = as.integer(d$face_id),
  rate_id = as.integer(d$rate_id)
  )

set.seed(42)
m <- ulam(
  alist(
    
    Selmas ~ dbinom( 1 , p ) ,
    
    logit(p) <- ac[fCond] + bs*Masright + 
      zf[face_id]*sigma_f + zr[rate_id]*sigma_r ,
   
    ac[fCond] ~ dnorm( 0 , 0.5 ),
    bs ~ dnorm( 0 , 0.5 ),
    
    ## adaptive priors
    zf[face_id] ~ dnorm( 0 , 1 ),
    zr[rate_id] ~ dnorm( 0 , 1 ),
    
    ## hyper-priors
    sigma_f ~ dexp(1),
    sigma_r ~ dexp(1)
    
  ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))


precis(m,depth=2,omit=c("zf","zr"))

save.image(file = "total_effect_posterior.RData")


#Start visualization
post<-extract.samples(m)

describe<-read.table("params_minimal.txt",sep="\t",header = T)

hex<-describe$hex[match(unique(describe$col),describe$col)]
cols<-describe$col[match(unique(describe$col),describe$col)]

alpha<-"80"

describe$hex<-paste("#",hex[match(describe$col,cols)],alpha,sep="")

names(describe)
sum(describe$plot)
de<-describe[describe$plot==T,]

de<-de[order(de$block),]

de$n<-1:nrow(de)
de$ofs[is.na(de$ofs)]<-0

de$y<-de$n+de$block-1+cumsum(de$ofs)

#extract only the important columns
tos<-unique(as.character(de$nam))
tos<-tos[tos!=""]

cs<-sapply(tos,function(i){eval(parse(text=paste("ncol(post$",i,")",sep="")))})
cs[is.na(cs)]<-1
cs
#first element of each set
cumsum(cs)-cs+1

eval(parse(text=(
  paste("posts<-cbind(",paste(paste("post$",tos,sep=""),collapse=","),")"))))

#constant to add on both sides of the y axis to expand the plotting region a bit
const1<-1
#constant that is there for an extra space for axis
const2<-1.8

db<-lapply(1:max(de$block),function(i){de[de$block==i,]})
ploth<-sapply(1:length(db),function(i){max(db[[i]]$y)-min(db[[i]]$y)})
rath<-c(const2,ploth+2*const1+const2)
rath

rat<-c(1,1)

w1<-7

png("posterior_total.png",width=(w1/rat[1])*sum(rat),height=8,units="cm",res=600)
layout(matrix(rep(1:(length(rath)*2),rep(rat,length(rath))),nr=length(rath),byrow=T),
       widths=rat,heights=rath)

axes<-list(seq(-0.5,0.5,l=5),
           seq(0,1,l=5))

xlims<-lapply(1:length(axes),function(i){c(min(axes[[i]])-diff(range(axes[[i]]))*0.1,max(axes[[i]])+diff(range(axes[[i]]))*0.1)})

par(mar=rep(0.05,4),cex=0.9)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Parameters",pos=4,font=2)

plot(NULL,ylim=c(const2,0),xlim=c(-1,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.8,"Posterior distributions",font=2)

for(block in 1:length(db)){
  
  dei<-db[[block]]
  
  plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
  text(0,dei$y,dei$dependent,pos=4)
  text(0.33,dei$y,"~",pos=4)
  text(0.375,dei$y,paste(dei$predictor,dei$condition),pos=4)
  
  text(0,max(dei$y)+const1+const2-0.5,ifelse(block==1,"Hyperparameters",ifelse(block==2,"","Parameters")),pos=4,font=2,xpd=T)
  
  plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlim=xlims[[block]],bty="n")
  
  col.grid<-"#808080"
  lwd.grid<-1.5
  abline(h=dei$y,col=col.grid,lty=1,lwd=lwd.grid)
  
  #draw own axis
  lwd.ax<-1.5
  col.ax<-"#202020"
  
  tic<-0.35
  ofs<-0.65
  segments(axes[[block]],max(dei$y)+const1,axes[[block]],max(dei$y)+tic+const1,lwd=lwd.ax,col=col.ax)
  text(axes[[block]],max(dei$y)+tic+const1+ofs,labels=axes[[block]],col=col.ax,cex=0.9,font=2)
  lines(range(axes[[block]]),rep(max(dei$y)+const1,2),lwd=lwd.ax,col=col.ax)
  
  lwd.v<-1.5
  segments(axes[[block]],min(dei$y)-const1,axes[[block]],max(dei$y)+const1,lwd=lwd.v,col=col.ax,lty=3)
  segments(0,min(dei$y)-const1,0,max(dei$y)+const1,lwd=lwd.v,col=col.ax,lty=1)
  
  #Draw density polygons
  lwd.pol<-1.5
  col.pol<-col.grid
  
  #density areas are scaled within each block.
  area<-0.2*diff(xlims[[block]])
  
  for(i in 1:nrow(dei)){
    thispost<-posts[,dei$n[i]]
    dens<-density(thispost)
    polX<-c(dens$x,rev(dens$x))
    polY<-c(dens$y,rev(-dens$y))
    ar1<-abs(polyarea(polX,polY))
    perc<-area/ar1
    polygon(polX,polY*perc+dei$y[i],col=dei$hex[i],border=col.pol,lwd=lwd.pol)
    #CI<-PI(thispost,prob = 0.89)
    #lines(CI,rep(dei$y[i],2),lwd=3,col="#000000")
    points(mean(thispost),dei$y[i],pch=16,cex=1,col="#FFFFFF")
    points(mean(thispost),dei$y[i],pch=1,cex=1,col="#000000")
    
  }
  
  rect(xlims[[block]][1],min(dei$y)-const1,xlims[[block]][2],max(dei$y)+const1,lwd=lwd.ax,border=col.ax,xpd=T)
  
}

dev.off()

precis(m,depth=2,omit=c("zf","zr"))

linked<-link( m )

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
dat_list_sim1<-dat_list
dat_list_sim1$fCond<-rep(1,n)
dat_list_sim1$Masright<-rep(-0.5,n)

#Condition 1, Mas right
dat_list_sim2<-dat_list_sim1
dat_list_sim2$Masright<-rep(0.5,n)

#Condition 2, Mas left
dat_list_sim3<-dat_list_sim1
dat_list_sim3$fCond<-rep(2,n)

#Condition 2, Mas right
dat_list_sim4<-dat_list_sim2
dat_list_sim4$fCond<-rep(2,n)

#Condition 3, Mas left
dat_list_sim5<-dat_list_sim1
dat_list_sim5$fCond<-rep(3,n)

#Condition 3, Mas right
dat_list_sim6<-dat_list_sim2
dat_list_sim6$fCond<-rep(3,n)

#Link to simulated data - it takes a while
linked1<-link( m ,data=dat_list_sim1)
linked2<-link( m ,data=dat_list_sim2)

linked3<-link( m ,data=dat_list_sim3)
linked4<-link( m ,data=dat_list_sim4)

linked5<-link( m ,data=dat_list_sim5)
linked6<-link( m ,data=dat_list_sim6)

linked1<-list(p=linked1)
linked2<-list(p=linked2)
linked3<-list(p=linked3)
linked4<-list(p=linked4)
linked5<-list(p=linked5)
linked6<-list(p=linked6)


png("Figure15.png",width=11,height=7,units="cm",res=600)

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



