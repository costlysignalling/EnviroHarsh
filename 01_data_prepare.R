#Function that returns last n characters of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#function that reverse-codes a vector
revcode<-function(v,maxi=max(v),mini=min(v)){v*(-1)+(maxi+mini)}

#function that reverse-codes several columns (c) of a data.frame/matrix (d)
revcol<-function(d,c,maxi=7,mini=1){
  for(i in c){
    d[,i]<-revcode(d[,i],maxi=maxi,mini=mini)
  }
  return(d)
}

#Load the raw data
d<-read.table("rawdata.txt",sep="\t",header=T,stringsAsFactors = F)
(nam<-names(d))

nrow(d)

#Eliminate problematic raters (describe)
d<-d[is.na(match(d$Subject,c("150","159"))),]
nrow(d)

#Check how many answers got people right
summary(as.factor(d$PPVAS1)) # 1 is correct
summary(as.factor(d$PPVAS2)) # 2 is correct

summary(as.factor(d$RSVAS1)) # 2 is correct
summary(as.factor(d$RSVAS2)) # 1 is correct

summary(as.factor(d$SPVAS1)) # 2 is correct
summary(as.factor(d$SPVAS2)) # 1 is correct

# Sum correct answers
d$VAScorrect<-ifelse(d$PPVAS1==1,1,0)+ifelse(d$PPVAS2==2,1,0)+ifelse(d$RSVAS1==2,1,0)+ifelse(d$RSVAS2==1,1,0)+ifelse(d$SPVAS1==2,1,0)+ifelse(d$SPVAS2==1,1,0)

#I do not exclude people with wrong answers in the analysis, this can be changed using the code lines below
#d<-d[d$VAScorrect==2,] #Take only people that answered both questions right
d<-d[d$VAScorrect>0,]  #Exclude only peoply that anwered both questions wrong

nrow(d)

#We included only heterosexual women to the study. Homosexuals, bisexuals and other were excluded.
summary(as.factor(d$SexOrient))

d<-d[d$SexOrient==1,]
nrow(d)

# Calculate Enviromental Harshness scales

PPdat<-d[,substr(nam,1,2)=="PP"&substr(nam,3,3)!="V"]
names(PPdat)[c(3,5,12,13,15,16,18)]
PPdat<-revcol(PPdat,c(3,5,12,13,15,16,18))

RSdat<-d[,substr(nam,1,2)=="RS"&substr(nam,3,3)!="V"]
names(RSdat)[c(1,2,3,5,11,12,16)]
RSdat<-revcol(RSdat,c(1,2,3,5,11,12,16))

# Check that there are no missing values
sum(is.na(as.matrix(PPdat)))
sum(is.na(as.matrix(RSdat)))

d$PP<-rowSums(PPdat)
d$RS<-rowSums(RSdat)

Rdat<-d[,substr(nam,1,5)=="Recog"]
Rdat<-revcol(Rdat,1:12,maxi=2,mini=1)-1
d$RecTot<-rowSums(Rdat)

summary(as.factor(d$RecTot))
d<-d[d$RecTot<5,]

nrow(d)

dw<-d

# Transform the data into a long format where one set of ratings and selection between masculinized and femininized version of the face is a unit of analysis.

Attr_fem<-unlist(d[,substr(nam,1,4)=="Attr"&substrRight(nam,3)=="fem"])
Attr_mas<-unlist(d[,substr(nam,1,4)=="Attr"&substrRight(nam,3)=="mas"])

Form_fem<-unlist(d[,substr(nam,1,4)=="Form"&substrRight(nam,3)=="fem"])
Form_mas<-unlist(d[,substr(nam,1,4)=="Form"&substrRight(nam,3)=="mas"])

Heal_fem<-unlist(d[,substr(nam,1,4)=="Heal"&substrRight(nam,3)=="fem"])
Heal_mas<-unlist(d[,substr(nam,1,4)=="Heal"&substrRight(nam,3)=="mas"])

face_id<-substr(names(Attr_fem),6,7)
face_n<-length(unique(face_id))

rate_id<-rep(formatC(d$Subject,width=3,flag=0),times=face_n)
rate_n<-length(unique(rate_id))

Select<-unlist(d[,substr(nam,1,9)=="Mate_Pair"])
Cond<-rep(d$Cond,times=face_n)

Recog<-unlist(d[,substr(nam,1,5)=="Recog"])

Age<-rep(d$Age,times=face_n)
RelationshipStatus<-rep(d$RelationshipStatus,times=face_n)

# What to do wih these questions Sum the scores? 
VIS01<-rep(d$VIS01,times=face_n)
VIS02<-rep(d$VIS02,times=face_n)
VIS03<-rep(d$VIS03,times=face_n)

MateValue_Body<-rep(d$MateValue_Body,times=face_n)
MateValue_Face<-rep(d$MateValue_Face,times=face_n)

PP<-rep(d$PP,times=face_n)
RS<-rep(d$RS,times=face_n)

VAS<-rep(d$VAScorrect,times=face_n)

nd<-data.frame(rate_id,
               face_id,
               Cond,
               Attr_fem,Attr_mas,Form_fem,Form_mas,Heal_fem,Heal_mas,
               Select,
               Recog,
               Age,
               RelationshipStatus,
               MateValue_Body,
               MateValue_Face,
               PP,
               RS,
               VAS,
               VIS01,VIS02,VIS03
)

d<-nd

d$Attr_dif<-d$Attr_mas-d$Attr_fem
d$Form_dif<-d$Form_mas-d$Form_fem
d$Heal_dif<-d$Heal_mas-d$Heal_fem

hist(d$Attr_dif)
summary(d$Attr_dif)

hist(d$Form_dif)
summary(d$Form_dif)

hist(d$Heal_dif)
summary(d$Heal_dif)

names(d)

#Variable indicating whether masculinized face was selected (original select variable contains information wheteher the face on the right was selected)
d$Selmas<-ifelse(d$Cond%%2==0,ifelse(d$Select==1,1,ifelse(d$Select==2,0,NA)),ifelse(d$Select==1,0,ifelse(d$Select==2,1,NA)))
cbind(d$Cond,d$Select,d$Selmas)

#Create the variable "Masculinized face on the right" from the original condition variable 1-6, odd numbers: masculinezed faces were presented on the right side of the screen, even numbers: masculinized faces were presented on the left 
d$Masright<-ifelse(d$Cond%%2==0,-0.5,0.5)
cbind(d$Cond,d$Select,d$Selmas)

#Create the manipulation condition variable from the original condition variable 1-2 control condition, 3-4 Pathogen prevalence priming message, 5-6 Resource scarcity priming message-
d$fCond<-ifelse(d$Cond<3,"Control",ifelse(d$Cond<5,"Pathogen",ifelse(d$Cond<7,"Scarcity",NA)))
d$fCond<-as.factor(d$fCond)

dw$fCond<-ifelse(dw$Cond<3,"Control",ifelse(dw$Cond<5,"Pathogen",ifelse(dw$Cond<7,"Scarcity",NA)))
dw$fCond<-as.factor(dw$fCond)

summary(dw$fCond)

#Check if it worked
#cbind(d$Cond,as.character(d$fCond))

#summary

#Standardize
d$PPs<-scale(d$PP)
d$RSs<-scale(d$RS)

centerPP<-attributes(d$PPs)[[2]]
sdPP<-attributes(d$PPs)[[3]]

centerRS<-attributes(d$RSs)[[2]]
sdRS<-attributes(d$RSs)[[3]]

descalePP<-function(x){(x*sdPP+centerPP)/18}
descaleRS<-function(x){(x*sdRS+centerRS)/16}

nrow(d)
summary(as.factor(d$Recog))

#If you wish, you can remove lines where the target was recognized by the rater
d<-d[d$Recog!=1,]
nrow(d)

nrow(dw)
mean(as.numeric(dw$Age[dw$Age!="HH"]))
sd(as.numeric(dw$Age[dw$Age!="HH"]))

