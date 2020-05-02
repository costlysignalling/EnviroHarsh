start<-Sys.time()

library(rethinking)
source("01_data_prepare.R")

summary(d)

dat_list_attr <- list(
  fCond = as.integer(d$fCond),
  face_id = as.integer(d$face_id),
  rate_id = as.integer(d$rate_id),
  inte_id = as.integer(as.factor(paste(d$face_id,d$rate_id))),
  
  Attr_mas=d$Attr_mas,
  Attr_fem=d$Attr_fem,
  Form_mas=as.integer(d$Form_mas),
  Form_fem=as.integer(d$Form_fem),
  Heal_mas=as.integer(d$Heal_mas),
  Heal_fem=as.integer(d$Heal_fem), #this is as E in the rethinking book for ordered categorical predictor
  alphaF = rep( 2 , 6 ), #delta prior
  alphaH = rep( 2 , 6 )  #delta prior
)

set.seed(42)
m.attr <- ulam(
  alist(
    ###Attractivness model
    Attr_mas ~ ordered_logistic( phiA_mas , cutpointsA ),
    Attr_fem ~ ordered_logistic( phiA_fem , cutpointsA ),
    
    phiA_mas <- 0.5*sexC[fCond]+bF[fCond]*sum( delta_F[1:Form_mas] ) + bH[fCond]*sum( delta_H[1:Heal_mas] ) + 
      zfA[face_id]*sigma_fA + zrA[rate_id]*sigma_rA + ziA[inte_id]*sigma_iA,
    
    phiA_fem <- -0.5*sexC[fCond]+bF[fCond]*sum( delta_F[1:Form_fem] ) + bH[fCond]*sum( delta_H[1:Heal_fem] ) + 
      zfA[face_id]*sigma_fA + zrA[rate_id]*sigma_rA + ziA[inte_id]*sigma_iA,
    
    sexC[fCond] ~ dnorm(0,1),
    
    bF[fCond] ~ dnorm( 0 , 1 ),
    bH[fCond] ~ dnorm( 0 , 1 ),
    
    ## adaptive priors
    zfA[face_id] ~ dnorm( 0 , 1 ),
    zrA[rate_id] ~ dnorm( 0 , 1 ),
    ziA[inte_id] ~ dnorm( 0 , 1 ),
    
    ## hyper-priors
    sigma_fA ~ dexp(0.5),
    sigma_rA ~ dexp(0.5),
    sigma_iA ~ dexp(0.5),
    
    cutpointsA ~ dnorm( 0 , 1.5 ),
    
    vector[7]: delta_F <<- append_row( 0 , predelta_F ),
    simplex[6]: predelta_F ~ dirichlet( alphaF ),
    vector[7]: delta_H <<- append_row( 0 , predelta_H ),
    simplex[6]: predelta_H ~ dirichlet( alphaH )
    
  ) , data=dat_list_attr , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))


precis(m.attr,depth=2,omit=c("zf","zr","zfA","zrA","ziA","zfF","zrF","ziF","zfH","zrH","ziH"))

save.image(file = "posterior_attr.RData")

start-Sys.time()

