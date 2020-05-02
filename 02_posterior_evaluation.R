start<-Sys.time()

library(rethinking)
source("01_data_prepare.R")

summary(d)

#create a data of unique per-rater values from the data in long format
d.unique<-d[match(unique(d$rate_id),d$rate_id),]

set.seed(42)
dat_list_big <- list(
  Selmas = d$Selmas,
  Masright=d$Masright,
  fCond = as.integer(d$fCond),
  face_id = as.integer(d$face_id),
  rate_id = as.integer(d$rate_id),
  inte_id = as.integer(as.factor(paste(d$face_id,d$rate_id))),
  
  PPs=as.numeric(d$PPs),
  RSs=as.numeric(d$RSs),
  Attr_dif=d$Attr_dif,
  Form_dif=d$Form_dif,
  Heal_dif=d$Heal_dif,
  
  Attr_mas=d$Attr_mas,
  Attr_fem=d$Attr_fem,
  Form_mas=d$Form_mas,
  Form_fem=d$Form_fem,
  Heal_mas=d$Heal_mas,
  Heal_fem=d$Heal_fem,
  
  fCondU = as.integer(as.factor(d.unique$fCond)),
  PPsU = as.numeric(d.unique$PPs),
  RSsU = as.numeric(d.unique$RSs)
  )

set.seed(42)

m.big <- ulam(
  alist(
    
    Selmas ~ dbinom( 1 , p ) ,
    
    logit(p) <- ac[fCond] + 
      bAd[fCond]*Attr_dif + bFd[fCond]*Form_dif + bHd[fCond]*Heal_dif + 
      bPP*PPs + bRS*RSs + 
      bs*Masright + 
      zf[face_id]*sigma_f + zr[rate_id]*sigma_r ,
   
    ac[fCond] ~ dnorm( 0 , 1.5 ),
    
    bAd[fCond] ~ dnorm( 0 , 0.5 ),
    bFd[fCond] ~ dnorm( 0 , 0.5 ),
    bHd[fCond] ~ dnorm( 0 , 0.5 ),
    
    bPP ~ dnorm( 0 , 0.5 ),
    bRS ~ dnorm( 0 , 0.5 ),
    bs ~ dnorm( 0 , 0.5 ),
    
    ## adaptive priors
    zf[face_id] ~ dnorm( 0 , 1 ),
    zr[rate_id] ~ dnorm( 0 , 1 ),
    
    ## hyper-priors
    sigma_f ~ dexp(0.5),
    sigma_r ~ dexp(0.5),
    
    
    ###Attractivness model
    Attr_mas ~ dordlogit( phiA_mas , cutpointsA ),
    Attr_fem ~ dordlogit( phiA_fem , cutpointsA ),
    
    phiA_mas <- acA_mas[fCond] + bPPA_mas*PPs + bRSA_mas*RSs + 
      zfA[face_id]*sigma_fA + zrA[rate_id]*sigma_rA + ziA[inte_id]*sigma_iA,
    
    phiA_fem <- acA_fem[fCond] + bPPA_fem*PPs + bRSA_fem*RSs + 
      zfA[face_id]*sigma_fA + zrA[rate_id]*sigma_rA + ziA[inte_id]*sigma_iA,
    
    acA_mas[fCond] ~ dnorm( 0 , 0.5 ),
    acA_fem[fCond] ~ dnorm( 0 , 0.5 ),
    
    bPPA_mas ~ dnorm( 0 , 0.5 ),
    bPPA_fem ~ dnorm( 0 , 0.5 ),
    bRSA_mas ~ dnorm( 0 , 0.5 ),
    bRSA_fem ~ dnorm( 0 , 0.5 ),
    
    ## adaptive priors
    zfA[face_id] ~ dnorm( 0 , 1 ),
    zrA[rate_id] ~ dnorm( 0 , 1 ),
    ziA[inte_id] ~ dnorm( 0 , 1 ),
    
    ## hyper-priors
    sigma_fA ~ dexp(0.5),
    sigma_rA ~ dexp(0.5),
    sigma_iA ~ dexp(0.5),
    
    cutpointsA ~ dnorm( 0 , 1.5 ),
    
    
    ###Formidability model
    Form_mas ~ dordlogit( phiF_mas , cutpointsF ),
    Form_fem ~ dordlogit( phiF_fem , cutpointsF ),
    
    phiF_mas <- acF_mas[fCond] + bPPF_mas*PPs + bRSF_mas*RSs + 
      zfF[face_id]*sigma_fF + zrF[rate_id]*sigma_rF + ziF[inte_id]*sigma_iF,
    
    phiF_fem <- acF_fem[fCond] + bPPF_fem*PPs + bRSF_fem*RSs + 
      zfF[face_id]*sigma_fF + zrF[rate_id]*sigma_rF + ziF[inte_id]*sigma_iF,
    
    acF_mas[fCond] ~ dnorm( 0 , 0.5 ),
    acF_fem[fCond] ~ dnorm( 0 , 0.5 ),
    
    bPPF_mas ~ dnorm( 0 , 0.5 ),
    bPPF_fem ~ dnorm( 0 , 0.5 ),
    bRSF_mas ~ dnorm( 0 , 0.5 ),
    bRSF_fem ~ dnorm( 0 , 0.5 ),
    
    ## adaptive priors
    zfF[face_id] ~ dnorm( 0 , 1 ),
    zrF[rate_id] ~ dnorm( 0 , 1 ),
    ziF[inte_id] ~ dnorm( 0 , 1 ),
    
    ## hyper-priors
    sigma_fF ~ dexp(0.5),
    sigma_rF ~ dexp(0.5),
    sigma_iF ~ dexp(0.5),
    
    cutpointsF ~ dnorm( 0 , 1.5 ),
    
    
    ###Healthiness model
    Heal_mas ~ dordlogit( phiH_mas , cutpointsH ),
    Heal_fem ~ dordlogit( phiH_fem , cutpointsH ),
    
    phiH_mas <- acH_mas[fCond] + bPPH_mas*PPs + bRSH_mas*RSs + 
      zfH[face_id]*sigma_fH + zrH[rate_id]*sigma_rH + ziH[inte_id]*sigma_iH,
    
    phiH_fem <- acH_fem[fCond] + bPPH_fem*PPs + bRSH_fem*RSs + 
      zfH[face_id]*sigma_fH + zrH[rate_id]*sigma_rH + ziH[inte_id]*sigma_iH,
    
    acH_mas[fCond] ~ dnorm( 0 , 0.5 ),
    acH_fem[fCond] ~ dnorm( 0 , 0.5 ),
    
    bPPH_mas ~ dnorm( 0 , 0.5 ),
    bPPH_fem ~ dnorm( 0 , 0.5 ),
    bRSH_mas ~ dnorm( 0 , 0.5 ),
    bRSH_fem ~ dnorm( 0 , 0.5 ),
    
    ## adaptive priors
    zfH[face_id] ~ dnorm( 0 , 1 ),
    zrH[rate_id] ~ dnorm( 0 , 1 ),
    ziH[inte_id] ~ dnorm( 0 , 1 ),
    
    ## hyper-priors
    sigma_fH ~ dexp(0.5),
    sigma_rH ~ dexp(0.5),
    sigma_iH ~ dexp(0.5),
    
    cutpointsH ~ dnorm( 0 , 1.5 ),
    
    #model of PP and RS scales based on condition
    PPsU ~ dnorm( mu_PPs , sigma_PPs ) ,
    RSsU ~ dnorm( mu_RSs , sigma_RSs ) ,
    
    mu_PPs<-a_PPs[fCondU],
    mu_RSs<-a_RSs[fCondU],
    
    a_PPs[fCondU] ~ dnorm( 0 , 1 ),
    a_RSs[fCondU] ~ dnorm( 0 , 1 ),
    
    sigma_PPs~ dexp(1),
    sigma_RSs~ dexp(1)
    
  ) , data=dat_list_big , chains=4 , cores=4 , log_lik=TRUE ,iter = 5000,control=list(max_treedepth=15,adapt_delta=0.99))

precis(m.big,depth=2,omit=c("zf","zr","zfA","zrA","ziA","zfF","zrF","ziF","zfH","zrH","ziH"))

save.image(file = "posterior_model.RData")

start-Sys.time()

