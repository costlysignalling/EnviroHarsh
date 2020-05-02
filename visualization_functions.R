
# Function that creates transparent colours with a specified alpha levels
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}



#Function that draws comparison of two sets of posterior probabilities from their linked objects
drawProbs<-function(linked1,linked2,xc=0.5,yb=0,wid=0.1,gap=0.05,hei=1,cols=c("#000000","#0088CC"),alp=0.25,labels=c("Left","Right"),cex.text=1,ofs=0,col.text="#808080"){
  
  xl<-xc-gap/2-wid
  xr<-xc-gap/2
  
  xl2<-xc+gap/2
  xr2<-xc+gap/2+wid
  
  
  #set colors and their inetrmediate shapes as a matrix
  colmat<-sapply(1:length(cols),function(i){
    cfunc<-colorRampPalette(c(cols[i],"white"))
    return(rev(cfunc(5)[c(1,3)]))
  })
  
  avp1<-apply(linked1$p,1,mean)
  med1<-median(avp1)
  ci1<-PI(avp1,prob=0.89)
  
  avp2<-apply(linked2$p,1,mean)
  med2<-median(avp2)
  ci2<-PI(avp2,prob=0.89)
  
  rect(rep(xl,2),c(0,med1)*hei+yb,rep(xr,2),c(med1,1)*hei+yb,col=colmat[1,],border=NA)
  rect(rep(xl,2),c(0,ci1[2])*hei+yb,rep(xr,2),c(ci1[1],1)*hei+yb,col=colmat[2,],border=NA)
  
  rect(rep(xl2,2),c(0,med2)*hei+yb,rep(xr2,2),c(med2,1)*hei+yb,col=colmat[1,],border=NA)
  rect(rep(xl2,2),c(0,ci2[2])*hei+yb,rep(xr2,2),c(ci2[1],1)*hei+yb,col=colmat[2,],border=NA)
  
  for(i in 1:2){
    polygon(c(rep(xr,2),rep(xl2,2)),c(c(0,med1,1)[i:(i+1)],rev(c(0,med2,1)[i:(i+1)]))*hei+yb,col=addalpha(colmat[2,i],alp),border=NA)
    polygon(c(rep(xr,2),rep(xl2,2)),c(c(c(0,ci1[2])[i],c(ci1[1],1)[i]),rev(c(c(0,ci2[2])[i],c(ci2[1],1)[i])))*hei+yb,col=addalpha(colmat[2,i],alp),border=NA)
  }
  
  text(c(mean(c(xl,xr)),mean(c(xl2,xr2))),c(yb,yb)-ofs,labels=labels,pos=1,cex=cex.text,col=col.text)
}


#Function that draws comparison of two sets of posterior probabilities from their linked objects
drawProbs3<-function(linked1,linked2,linked3,xc=0.5,yb=0,wid=0.1,gap=0.05,hei=1,cols=c("#000000","#0088CC"),alp=0.25,labels=c("-2SD","0","+2SD"),cex.text=1,ofs=0,col.text="#808080"){
  
  xl<-xc-gap-1.5*wid
  xr<-xc-gap-0.5*wid
  
  xl2<-xc-0.5*wid
  xr2<-xc+0.5*wid
  
  xl3<-xc+gap+0.5*wid
  xr3<-xc+gap+1.5*wid
  
  
  #set colors and their inetrmediate shapes as a matrix
  colmat<-sapply(1:length(cols),function(i){
    cfunc<-colorRampPalette(c(cols[i],"white"))
    return(rev(cfunc(5)[c(1,3)]))
  })
  
  avp1<-apply(linked1$p,1,mean)
  med1<-median(avp1)
  ci1<-PI(avp1,prob=0.89)
  
  avp2<-apply(linked2$p,1,mean)
  med2<-median(avp2)
  ci2<-PI(avp2,prob=0.89)
  
  avp3<-apply(linked3$p,1,mean)
  med3<-median(avp3)
  ci3<-PI(avp3,prob=0.89)
  
  rect(rep(xl,2),c(0,med1)*hei+yb,rep(xr,2),c(med1,1)*hei+yb,col=colmat[1,],border=NA)
  rect(rep(xl,2),c(0,ci1[2])*hei+yb,rep(xr,2),c(ci1[1],1)*hei+yb,col=colmat[2,],border=NA)
  
  rect(rep(xl2,2),c(0,med2)*hei+yb,rep(xr2,2),c(med2,1)*hei+yb,col=colmat[1,],border=NA)
  rect(rep(xl2,2),c(0,ci2[2])*hei+yb,rep(xr2,2),c(ci2[1],1)*hei+yb,col=colmat[2,],border=NA)
  
  rect(rep(xl3,2),c(0,med3)*hei+yb,rep(xr3,2),c(med3,1)*hei+yb,col=colmat[1,],border=NA)
  rect(rep(xl3,2),c(0,ci3[2])*hei+yb,rep(xr3,2),c(ci3[1],1)*hei+yb,col=colmat[2,],border=NA)
  
  for(i in 1:2){
    polygon(c(rep(xr,2),rep(xl2,2)),c(c(0,med1,1)[i:(i+1)],rev(c(0,med2,1)[i:(i+1)]))*hei+yb,col=addalpha(colmat[2,i],alp),border=NA)
    polygon(c(rep(xr,2),rep(xl2,2)),c(c(c(0,ci1[2])[i],c(ci1[1],1)[i]),rev(c(c(0,ci2[2])[i],c(ci2[1],1)[i])))*hei+yb,col=addalpha(colmat[2,i],alp),border=NA)
  }
  
  for(i in 1:2){
    polygon(c(rep(xr2,2),rep(xl3,2)),c(c(0,med2,1)[i:(i+1)],rev(c(0,med3,1)[i:(i+1)]))*hei+yb,col=addalpha(colmat[2,i],alp),border=NA)
    polygon(c(rep(xr2,2),rep(xl3,2)),c(c(c(0,ci2[2])[i],c(ci2[1],1)[i]),rev(c(c(0,ci3[2])[i],c(ci3[1],1)[i])))*hei+yb,col=addalpha(colmat[2,i],alp),border=NA)
  }
  
  text(c(mean(c(xl,xr)),mean(c(xl2,xr2)),mean(c(xl3,xr3))),c(yb,yb,yb)-ofs,labels=labels,pos=1,cex=cex.text,col=col.text)
}


#Function that returns expected distribution of differences
difDist<-function(s,matscore,linked,trait="A"){
  text<-paste("masrat<-pordlogit( 1:7 , linked$phi",trait,"_mas[s,] , post$cutpoints",trait,"[s,])",sep="")
  eval(parse(text=text))
  
  text<-paste("femrat<-pordlogit( 1:7 , linked$phi",trait,"_fem[s,] , post$cutpoints",trait,"[s,])",sep="")
  eval(parse(text=text))
  
  #distributions of ratings
  masdist<-masrat-cbind(rep(0,n),masrat[,1:6])
  femdist<-femrat-cbind(rep(0,n),femrat[,1:6])
  
  matans<-sapply(1:n,function(i){masdist[i,]%*%t(femdist[i,])})
  matdif<-matrix(rowMeans(matans),nrow=7)
  
  difdist<-tapply(matdif,as.factor(matscore),sum)
  return(difdist)
}


#functions that sample the posterior distribution and generates N attractiveness, formidability and healthiness ratings of feminized and masculinized faces and the expected differences between them (1 for each sample)
sample_dif<-function(m.big,post,dat_list_sim,linked){
  
  #matrix of differences
  masscore<-matrix(rep(1:7,each=7),ncol=7,byrow=T)
  femscore<-matrix(rep(1:7,each=7),ncol=7)
  matscore<-masscore-femscore
  
  difdistA<-sapply(1:s,difDist,matscore=matscore,linked=linked,trait="A")
  difdistF<-sapply(1:s,difDist,matscore=matscore,linked=linked,trait="F")
  difdistH<-sapply(1:s,difDist,matscore=matscore,linked=linked,trait="H")
  
  masA<-sapply(1:nrow(linked[[1]]),function(s){apply(pordlogit( 1:7 , linked$phiA_mas[s,] , post$cutpointsA[s,] ),2,mean)})
  femA<-sapply(1:nrow(linked[[1]]),function(s){apply(pordlogit( 1:7 , linked$phiA_fem[s,] , post$cutpointsA[s,] ),2,mean)})
  
  masF<-sapply(1:nrow(linked[[1]]),function(s){apply(pordlogit( 1:7 , linked$phiF_mas[s,] , post$cutpointsF[s,] ),2,mean)})
  femF<-sapply(1:nrow(linked[[1]]),function(s){apply(pordlogit( 1:7 , linked$phiF_fem[s,] , post$cutpointsF[s,] ),2,mean)})
  
  masH<-sapply(1:nrow(linked[[1]]),function(s){apply(pordlogit( 1:7 , linked$phiH_mas[s,] , post$cutpointsH[s,] ),2,mean)})
  femH<-sapply(1:nrow(linked[[1]]),function(s){apply(pordlogit( 1:7 , linked$phiH_fem[s,] , post$cutpointsH[s,] ),2,mean)})
  
  return(list(Attr=list(masA=masA,femA=femA,difdistA=difdistA),
              Form=list(masF=masF,femF=femF,difdistF=difdistF),
              Heal=list(masH=masH,femH=femH,difdistH=difdistH)))
}

#Function that summs the rating and difference posterior into medians and CI
summRatings<-function(samp){
  
  rmasA1<-apply(samp[[1]]$masA,1,median)
  rmasA2<-apply(samp[[1]]$masA,1,PI,prob=0.89)
  
  rfemA1<-apply(samp[[1]]$femA,1,median)
  rfemA2<-apply(samp[[1]]$femA,1,PI,prob=0.89)
  
  rdifA1<-apply(samp[[1]]$difdistA,1,median)
  rdifA2<-apply(samp[[1]]$difdistA,1,PI,prob=0.89)

  dif.vals<-as.numeric(dimnames(samp[[1]]$difdistA)[[1]])
  meandifA<-apply(samp[[1]]$difdistA,2,function(x){(sum(x*dif.vals))})
  
  rat.vals<-c(1:7)
  meanmasA<-apply(apply(rbind(0,samp[[1]]$masA),2,diff),2,function(x){(sum(x*rat.vals))})
  meanfemA<-apply(apply(rbind(0,samp[[1]]$femA),2,diff),2,function(x){(sum(x*rat.vals))})
  
  summA<-list(rmasA1=rmasA1,rmasA2=rmasA2,
              rfemA1=rfemA1,rfemA2=rfemA2,
              rdifA1=rdifA1,rdifA2=rdifA2,
              meandifA=meandifA,meanmasA=meanmasA,meanfemA=meanfemA)
  
  
  rmasF1<-apply(samp[[2]]$masF,1,median)
  rmasF2<-apply(samp[[2]]$masF,1,PI,prob=0.89)
  
  rfemF1<-apply(samp[[2]]$femF,1,median)
  rfemF2<-apply(samp[[2]]$femF,1,PI,prob=0.89)
  
  rdifF1<-apply(samp[[2]]$difdistF,1,median)
  rdifF2<-apply(samp[[2]]$difdistF,1,PI,prob=0.89)
  
  dif.vals<-as.numeric(dimnames(samp[[2]]$difdistF)[[1]])
  meandifF<-apply(samp[[2]]$difdistF,2,function(x){(sum(x*dif.vals))})
  
  meanmasF<-apply(apply(rbind(0,samp[[2]]$masF),2,diff),2,function(x){(sum(x*rat.vals))})
  meanfemF<-apply(apply(rbind(0,samp[[2]]$femF),2,diff),2,function(x){(sum(x*rat.vals))})
  
  summF<-list(rmasF1=rmasF1,rmasF2=rmasF2,
              rfemF1=rfemF1,rfemF2=rfemF2,
              rdifF1=rdifF1,rdifF2=rdifF2,
              meandifF=meandifF,meanmasF=meanmasF,meanfemF=meanfemF)
  
  
  rmasH1<-apply(samp[[3]]$masH,1,median)
  rmasH2<-apply(samp[[3]]$masH,1,PI,prob=0.89)
  
  rfemH1<-apply(samp[[3]]$femH,1,median)
  rfemH2<-apply(samp[[3]]$femH,1,PI,prob=0.89)
  
  rdifH1<-apply(samp[[3]]$difdistH,1,median)
  rdifH2<-apply(samp[[3]]$difdistH,1,PI,prob=0.89)
  
  dif.vals<-as.numeric(dimnames(samp[[3]]$difdistH)[[1]])
  meandifH<-apply(samp[[3]]$difdistH,2,function(x){(sum(x*dif.vals))})
  
  meanmasH<-apply(apply(rbind(0,samp[[3]]$masH),2,diff),2,function(x){(sum(x*rat.vals))})
  meanfemH<-apply(apply(rbind(0,samp[[3]]$femH),2,diff),2,function(x){(sum(x*rat.vals))})
  
  summH<-list(rmasH1=rmasH1,rmasH2=rmasH2,
              rfemH1=rfemH1,rfemH2=rfemH2,
              rdifH1=rdifH1,rdifH2=rdifH2,
              meandifH=meandifH,meanmasH=meanmasH,meanfemH=meanfemH)
  
  return(list(summA=summA,summF=summF,summH=summH))
}


#plot expected distributions with posterior predictions of 89 and 97 CI
drawRatings<-function(summ,xc=0.5,yb=0,wid=0.1,gap=0.05,hei=1,cols=substr(rev(rainbow(7)),1,7),alp=0.25,labels=c("Fem","Mas"),cex.text=1,ofs=0,col.text="#808080"){
  
  xl<-xc-gap/2-wid
  xr<-xc-gap/2
  
  xl2<-xc+gap/2
  xr2<-xc+gap/2+wid
  
  
  #set colors and their inetrmediate shapes as a matrix
  colmat<-sapply(1:length(cols),function(i){
    cfunc<-colorRampPalette(c(cols[i],"white"))
    return(rev(cfunc(5)[1:3]))
  })
  
  rfem1<-summ[[3]]
  rfem2<-summ[[4]]
  
  rmas1<-summ[[1]]
  rmas2<-summ[[2]]
  
  rect(rep(xl,7),c(0,rfem1[-length(rfem1)])*hei+yb,rep(xr,7),rfem1*hei+yb,col=colmat[1,],border=NA)
  rect(rep(xl,7),c(0,rfem2[2,][-length(rfem1)])*hei+yb,rep(xr,7),rfem2[1,]*hei+yb,col=colmat[2,],border=NA)
  
  rect(rep(xl2,7),c(0,rmas1[-length(rmas1)])*hei+yb,rep(xr2,7),rmas1*hei+yb,col=colmat[1,],border=NA)
  rect(rep(xl2,7),c(0,rmas2[2,][-length(rmas1)])*hei+yb,rep(xr2,7),rmas2[1,]*hei+yb,col=colmat[2,],border=NA)
  
  for(i in 1:7){
    polygon(c(rep(xr,2),rep(xl2,2)),c(c(0,rfem1)[i:(i+1)],rev(c(0,rmas1)[i:(i+1)]))*hei+yb,col=addalpha(colmat[2,i],alp),border=NA)
    polygon(c(rep(xr,2),rep(xl2,2)),c(c(c(0,rfem2[2,][-length(rfem1)])[i],rfem2[1,][i]),rev(c(c(0,rmas2[2,][-length(rmas1)])[i],rmas2[1,][i])))*hei+yb,col=addalpha(colmat[2,i],alp),border=NA)
  }
  
  text(c(mean(c(xl,xr)),mean(c(xl2,xr2))),c(yb,yb)-ofs,labels=labels,pos=1,cex=cex.text,col = col.text)
}


#plot histogram of differences
drawDiff<-function(summ,wid=0.05,xc=0.5,yb=0,relhei=2,maxabs=3,col="#008888",col.text="#808080",border=NA,alp=0.25,cex.text=1,ofs=0,div=200,low=0.3,border2=1,rectb=-0.03,rectt=-0.17,draw.mean=T){
  
  rdif1<-summ[[5]]
  rdif2<-summ[[6]]
  
  nr<-length(rdif1)
  med<-median(1:nr)
  
  nur<-c((med-maxabs):(med+maxabs))
  
  vals<-c(1:nr-med)[nur]
  cs<-xc+vals*wid
  xl<-xc+vals*wid-wid/2
  xr<-xc+vals*wid+wid/2
  
  rect(xl,yb,xr,yb+rdif1[nur]*relhei,col=col,border=border)
  #arrows(cs,yb+rdif1[nur]*relhei,cs,yb+rdif2[1,nur]*relhei,angle=90,length=0.05,col="#FFFFFF80")
  #arrows(cs,yb+rdif1[nur]*relhei,cs,yb+rdif2[2,nur]*relhei,angle=90,length=0.05,col=col)
  rect(xl,yb+rdif1[nur]*relhei,xr,yb+rdif2[1,nur]*relhei,col=addalpha("#FFFFFF",alp),border=border)
  rect(xl,yb+rdif1[nur]*relhei,xr,yb+rdif2[2,nur]*relhei,col=addalpha(col,alp),border=border)
  
  if(draw.mean==T){
    
    means<-summ[[7]]
    dens<-density(means)
    
    lines(c(xc+median(means)*wid,xc+median(means)*wid),c(yb,yb-low),lty=2,col=col)
    polygon(c(dens$x*wid+xc,rev(dens$x*wid+xc)),c(dens$y/div-low+yb,rev(-dens$y/div-low+yb)),col=col,border=border2)
    
    lines(c(xc,xc),c(yb,yb-low),lty=2,col="#808080")
    
    rect(min(xl),yb+ofs+rectb,max(xr),yb+ofs+rectt,col="#FFFFFF",border=NA)
  }
  
  text(cs,yb-ofs,vals,cex=cex.text,pos=1,col=col.text)
}

