library(rethinking)
library(pracma)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

load("posterior_attr.RData")
post<-extract.samples(m.attr)

str(post)

# nam<-names(post)
# write.table(data.frame(nam=nam),"attr.nam.txt",sep="\t",row.names=F)

describe<-read.table("params_attr.txt",sep="\t",header = T)

hex<-describe$hex[match(unique(describe$col),describe$col)]
cols<-describe$col[match(unique(describe$col),describe$col)]

alpha<-"80"

describe$hex<-paste("#",hex[match(describe$col,cols)],alpha,sep="")

names(describe)
sum(describe$plot,na.rm=T)
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

png("posterior_attr.png",width=(w1/rat[1])*sum(rat),height=10,units="cm",res=600)
layout(matrix(rep(1:(length(rath)*2),rep(rat,length(rath))),nr=length(rath),byrow=T),
       widths=rat,heights=rath)

axes<-list(seq(-1,4,l=5),
           seq(0,10,l=5))

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
  text(0.35,dei$y,"~",pos=4)
  text(0.385,dei$y,paste(dei$predictor,dei$condition),pos=4)
  
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
  
  #density areas are scaled within each block - change to constant in the product to scale
  area<-0.1*diff(xlims[[block]])
  
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


