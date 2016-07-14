#Are fluctuations in personality states more than just fluctuations in affect? 
##Robert E. Wilson, Renee J. Thompson, & Simine Vazire
# note: code does not include loops or functions to make each step explicit.

#clear workspace
rm(list=ls(all=TRUE))
library(lme4)

data1 <- read.csv("~/Desktop/RR ESM within person 2.16.15 long all.csv", sep=',', header = TRUE)
attach(data1)

#centering our IVs
##PA
myMeans <- with(data1, tapply(na_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean on PA
data1$na_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$na_esm_PC <- data1$na_esm-data1$na_esm_i ##person centered
data1$na_esm_GMC <- data1$na_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##NA
myMeans <- with(data1, tapply(pa_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean on NA
data1$pa_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$pa_esm_PC <- data1$pa_esm-data1$pa_esm_i ##person centered
data1$pa_esm_GMC <- data1$pa_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi1_esm
myMeans <- with(data1, tapply(tipi1_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi1_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi1_esm_PC <- data1$tipi1_esm-data1$tipi1_esm_i ##person centered
data1$tipi1_esm_GMC <- data1$tipi1_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi2_esm
data1$tipi2r_esm <-(6-data1$tipi2_esm)
myMeans <- with(data1, tapply(tipi2r_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi2r_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi2r_esm_PC <- data1$tipi2r_esm-data1$tipi2r_esm_i ##person centered
data1$tipi2r_esm_GMC <- data1$tipi2r_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi3_esm
myMeans <- with(data1, tapply(tipi3_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi3_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi3_esm_PC <- data1$tipi3_esm-data1$tipi3_esm_i ##person centered
data1$tipi3_esm_GMC <- data1$tipi3_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi4_esm
data1$tipi4r_esm <-(6-data1$tipi4_esm)
myMeans <- with(data1, tapply(tipi4r_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi4r_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi4r_esm_PC <- data1$tipi4r_esm-data1$tipi4r_esm_i ##person centered
data1$tipi4r_esm_GMC <- data1$tipi4r_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi5_esm
myMeans <- with(data1, tapply(tipi5_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi5_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi5_esm_PC <- data1$tipi5_esm-data1$tipi5_esm_i ##person centered
data1$tipi5_esm_GMC <- data1$tipi5_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi6_esm
data1$tipi6r_esm <-(6-data1$tipi6_esm)
myMeans <- with(data1, tapply(tipi6r_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi6r_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi6r_esm_PC <- data1$tipi6r_esm-data1$tipi6r_esm_i ##person centered
data1$tipi6r_esm_GMC <- data1$tipi6r_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi7_esm
myMeans <- with(data1, tapply(tipi7_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi7_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi7_esm_PC <- data1$tipi7_esm-data1$tipi7_esm_i ##person centered
data1$tipi7_esm_GMC <- data1$tipi7_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi8_esm
data1$tipi8r_esm <-(6-data1$tipi8_esm)
myMeans <- with(data1, tapply(tipi8r_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi8r_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi8r_esm_PC <- data1$tipi8r_esm-data1$tipi8r_esm_i ##person centered
data1$tipi8r_esm_GMC <- data1$tipi8r_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi9_esm
myMeans <- with(data1, tapply(tipi9_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi9_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi9_esm_PC <- data1$tipi9_esm-data1$tipi9_esm_i ##person centered
data1$tipi9_esm_GMC <- data1$tipi9_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#tipi10_esm
data1$tipi10r_esm <-(6-data1$tipi10_esm)
myMeans <- with(data1, tapply(tipi10r_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$tipi10r_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$tipi10r_esm_PC <- data1$tipi10r_esm-data1$tipi10r_esm_i ##person centered
data1$tipi10r_esm_GMC <- data1$tipi10r_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##Work-Fun
data1$work_esm <-(6-data1$fun_esm)
myMeans <- with(data1, tapply(work_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$work_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$work_esm_PC <- data1$work_esm-data1$work_esm_i ##person centered
data1$work_esm_GMC <- data1$work_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##Like
myMeans <- with(data1, tapply(like_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$like_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$like_esm_PC <- data1$like_esm-data1$like_esm_i ##person centered
data1$like_esm_GMC <- data1$like_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##Want
myMeans <- with(data1, tapply(want_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$want_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$want_esm_PC <- data1$want_esm-data1$want_esm_i ##person centered
data1$want_esm_GMC <- data1$want_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##Impression
myMeans <- with(data1, tapply(careimpression_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$careimpression_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$careimpression_esm_PC <- data1$careimpression_esm-data1$careimpression_esm_i ##person centered
data1$careimpression_esm_GMC <- data1$careimpression_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##common
myMeans <- with(data1, tapply(common_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$common_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$common_esm_PC <- data1$common_esm-data1$common_esm_i ##person centered
data1$common_esm_GMC <- data1$common_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##constrain
myMeans <- with(data1, tapply(constrain_esm, ourID, mean, na.rm=TRUE)) ##getting each person's mean 
data1$constrain_esm_i <-myMeans[match(data1$ourID, rownames(myMeans))] ##putting variable back into data
data1$constrain_esm_PC <- data1$constrain_esm-data1$constrain_esm_i ##person centered
data1$constrain_esm_GMC <- data1$constrain_esm_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 

#Table 1
##internal consistency
#item-level correlations for the Big Five
##ext and it's flip
lmer(tipi1_esm~1+tipi6r_esm_PC+ (1+tipi6r_esm_PC|ourID), data = data1) # .63
##neu and it's flip
lmer(tipi9_esm~1+tipi4r_esm_PC+ (1+tipi4r_esm_PC|ourID), data = data1) # .71
##agr and it's flip
lmer(tipi7_esm~1+tipi2r_esm_PC+ (1+tipi2r_esm_PC|ourID), data = data1) # .19
##con and it's flip
lmer(tipi3_esm~1+tipi8r_esm_PC+ (1+tipi8r_esm_PC|ourID), data = data1) # .41 
##ope and it's flip
lmer(tipi5_esm~1+tipi10r_esm_PC+ (1+tipi10r_esm_PC|ourID), data = data1) # .29

#ICCs
lmer(ext_esm~ 1+ (1|ourID), data = data1) # .26
0.3551/(1.0017+0.3551) 
lmer(neu_esm~ 1+ (1|ourID), data = data1) # .40
0.4832/(0.7278+0.4832) = .40
lmer(agr_esm~ 1+ (1|ourID), data = data1) # .39
0.4017/(0.6384+0.4017) = .39
lmer(con_esm~ 1+ (1|ourID), data = data1) # .36
0.4073/(0.7209+0.4073) = .36
lmer(ope_esm~ 1+ (1|ourID), data = data1) # .36
0.4068/(0.7367+0.4068) = .36
lmer(pa_esm~ 1+ (1|ourID), data = data1) # .33
0.4103/(0.8515+0.4103) = .33
lmer(na_esm~ 1+ (1|ourID), data = data1) # .37 
0.4824/(0.8323+0.4824) = .37 
#crunch data from long to short so we can get means for Table 1
ext_esm_mean <-aggregate(ext_esm~ourID, data=data1,FUN=function(x) c(mean=mean(x)))    
neu_esm_mean <-aggregate(neu_esm~ourID, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(ext_esm_mean, neu_esm_mean, by.x = "ourID", by.y = "ourID", all=TRUE)
agr_esm_mean <-aggregate(agr_esm~ourID, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, agr_esm_mean, by.x = "ourID", by.y = "ourID", all=TRUE)
con_esm_mean <-aggregate(con_esm~ourID, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, con_esm_mean, by.x = "ourID", by.y = "ourID", all=TRUE)
ope_esm_mean <-aggregate(ope_esm~ourID, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, ope_esm_mean, by.x = "ourID", by.y = "ourID", all=TRUE)
pa_esm_mean <-aggregate(pa_esm~ourID, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, pa_esm_mean, by.x = "ourID", by.y = "ourID", all=TRUE)
na_esm_mean <-aggregate(na_esm~ourID, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, na_esm_mean, by.x = "ourID", by.y = "ourID", all=TRUE)
small <- as.data.frame(small)
#getting means 
mean(small$ext_esm, na.rm=TRUE)
mean(small$neu_esm, na.rm=TRUE)
mean(small$agr_esm, na.rm=TRUE)
mean(small$con_esm, na.rm=TRUE)
mean(small$ope_esm, na.rm=TRUE)
mean(small$pa_esm, na.rm=TRUE)
mean(small$na_esm, na.rm=TRUE)
#END Table 1

#Table 2
#mlm with PA as IV
rw1<-lmer(ext_esm~1+pa_esm_PC+ (1+pa_esm_PC|ourID), data = data1)
rw2<-lmer(neu_esm~1+pa_esm_PC+ (1+pa_esm_PC|ourID), data = data1)
rw3<-lmer(agr_esm~1+pa_esm_PC+ (1+pa_esm_PC|ourID), data = data1)
rw4<-lmer(con_esm~1+pa_esm_PC+ (1+pa_esm_PC|ourID), data = data1)
rw5<-lmer(ope_esm~1+pa_esm_PC+ (1+pa_esm_PC|ourID), data = data1)
##NA
rw6<-lmer(ext_esm~1+na_esm_PC+ (1+na_esm_PC|ourID), data = data1)
rw7<-lmer(neu_esm~1+na_esm_PC+ (1+na_esm_PC|ourID), data = data1)
rw8<-lmer(agr_esm~1+na_esm_PC+ (1+na_esm_PC|ourID), data = data1)
rw9<-lmer(con_esm~1+na_esm_PC+ (1+na_esm_PC|ourID), data = data1)
rw10<-lmer(ope_esm~1+na_esm_PC+ (1+na_esm_PC|ourID), data = data1)
#PA&NA predicting Big5
rw11<-lmer(ext_esm~1+pa_esm_PC+na_esm_PC+ (1+na_esm_PC+pa_esm_PC|ourID), data = data1)
rw12<-lmer(neu_esm~1+pa_esm_PC+na_esm_PC+ (1+na_esm_PC+pa_esm_PC|ourID), data = data1)
rw13<-lmer(agr_esm~1+pa_esm_PC+na_esm_PC+ (1+na_esm_PC+pa_esm_PC|ourID), data = data1)
rw14<-lmer(con_esm~1+pa_esm_PC+na_esm_PC+ (1+na_esm_PC+pa_esm_PC|ourID), data = data1)
rw15<-lmer(ope_esm~1+pa_esm_PC+na_esm_PC+ (1+na_esm_PC+pa_esm_PC|ourID), data = data1)
#Table 2, getting Rm and Rc
library(MuMIn)
r.squaredGLMM(rw1)
r.squaredGLMM(rw2)
r.squaredGLMM(rw3)
r.squaredGLMM(rw4)
r.squaredGLMM(rw5)
r.squaredGLMM(rw6)
r.squaredGLMM(rw7)
r.squaredGLMM(rw8)
r.squaredGLMM(rw9)
r.squaredGLMM(rw10)
r.squaredGLMM(rw11)
r.squaredGLMM(rw12)
r.squaredGLMM(rw13)
r.squaredGLMM(rw14)
r.squaredGLMM(rw15)
#END Table 2

# Table 3
lmer(pa_esm~1+work_esm_PC+ (1+work_esm_PC|ourID), data = data1) #worked predicting PA
lmer(na_esm~1+work_esm_PC+ (1+work_esm_PC|ourID), data = data1) #worked predicting NA
lmer(pa_esm~1+like_esm_PC+ (1+like_esm_PC|ourID), data = data1) #liking predicting PA
lmer(na_esm~1+like_esm_PC+ (1+like_esm_PC|ourID), data = data1) #liking predicting NA
lmer(pa_esm~1+want_esm_PC+ (1+want_esm_PC|ourID), data = data1) #want predicting PA
lmer(na_esm~1+want_esm_PC+ (1+want_esm_PC|ourID), data = data1) #want predicting NA
lmer(pa_esm~1+careimpression_esm_PC+ (1+careimpression_esm_PC|ourID), data = data1) #impression predicting PA
lmer(na_esm~1+careimpression_esm_PC+ (1+careimpression_esm_PC|ourID), data = data1) #impression predicting NA
lmer(pa_esm~1+common_esm_PC+ (1+common_esm_PC|ourID), data = data1) #common predicting PA
lmer(na_esm~1+common_esm_PC+ (1+common_esm_PC|ourID), data = data1) #common predicting NA
lmer(pa_esm~1+constrain_esm_PC+ (1+constrain_esm_PC|ourID), data = data1) #constrain predicting PA
lmer(na_esm~1+constrain_esm_PC+ (1+constrain_esm_PC|ourID), data = data1) #constrain predicting NA
#END Table 3

#Table 4
lp1<-lmer(ext_esm~1+work_esm_PC+ (1+work_esm_PC|ourID), data = data1) 
lp2<-lmer(neu_esm~1+work_esm_PC+ (1+work_esm_PC|ourID), data = data1) 
lp3<-lmer(agr_esm~1+work_esm_PC+ (1+work_esm_PC|ourID), data = data1) 
lp4<-lmer(con_esm~1+work_esm_PC+ (1+work_esm_PC|ourID), data = data1) 
lp5<-lmer(ope_esm~1+work_esm_PC+ (1+work_esm_PC|ourID), data = data1) 
lp6<-lmer(ext_esm~1+pa_esm_PC+na_esm_PC+ work_esm_PC+ (1+pa_esm_PC+na_esm_PC+work_esm_PC|ourID), data = data1) 
lp7<-lmer(neu_esm~1+pa_esm_PC+na_esm_PC+ work_esm_PC+ (1+pa_esm_PC+na_esm_PC+work_esm_PC|ourID), data = data1) 
lp8<-lmer(agr_esm~1+pa_esm_PC+na_esm_PC+ work_esm_PC+ (1+pa_esm_PC+na_esm_PC+work_esm_PC|ourID), data = data1) 
lp9<-lmer(con_esm~1+pa_esm_PC+na_esm_PC+ work_esm_PC+ (1+pa_esm_PC+na_esm_PC+work_esm_PC|ourID), data = data1) 
lp10<-lmer(ope_esm~1+pa_esm_PC+na_esm_PC+ work_esm_PC+ (1+pa_esm_PC+na_esm_PC+work_esm_PC|ourID), data = data1) 
#Table 4, getting Rm and Rc
r.squaredGLMM(lp1)
r.squaredGLMM(lp2)
r.squaredGLMM(lp3)
r.squaredGLMM(lp4)
r.squaredGLMM(lp5)
r.squaredGLMM(lp6)
r.squaredGLMM(lp7)
r.squaredGLMM(lp8)
r.squaredGLMM(lp9)
r.squaredGLMM(lp10)
#END Table 4

#Table 5
wm1<-lmer(agr_esm~1+ like_esm_PC+ (1+like_esm_PC|ourID), data = data1) #like
wm2<-lmer(agr_esm~1+ want_esm_PC+ (1+want_esm_PC|ourID), data = data1) #want
wm4<-lmer(agr_esm~1+ careimpression_esm_PC+ (1+careimpression_esm_PC|ourID), data = data1) #impression
wm5<-lmer(agr_esm~1+pa_esm_PC+na_esm_PC+ like_esm_PC+ (1+pa_esm_PC+na_esm_PC+like_esm_PC|ourID), data = data1) 
wm6<-lmer(agr_esm~1+pa_esm_PC+na_esm_PC+ want_esm_PC+ (1+pa_esm_PC+na_esm_PC+want_esm_PC|ourID), data = data1) 
wm8<-lmer(agr_esm~1+pa_esm_PC+na_esm_PC+ careimpression_esm_PC+ (1+pa_esm_PC+na_esm_PC+careimpression_esm_PC|ourID), data = data1) 
#Table 5, getting Rm and Rc
library(MuMIn)
r.squaredGLMM(wm1)
r.squaredGLMM(wm2)
r.squaredGLMM(wm4)
r.squaredGLMM(wm5)
r.squaredGLMM(wm6)
r.squaredGLMM(wm8)
#END Table 5

#Table 6
gd1<-lmer(ope_esm~1+ common_esm_PC+ (1+common_esm_PC|ourID), data = data1) #common
gd2<-lmer(ope_esm~1+ constrain_esm_PC+ (1+constrain_esm_PC|ourID), data = data1) #constrain
gd3<-lmer(ope_esm~1+pa_esm_PC+na_esm_PC+ common_esm_PC+ (1+pa_esm_PC+na_esm_PC+common_esm_PC|ourID), data = data1) 
gd4<-lmer(ope_esm~1+pa_esm_PC+na_esm_PC+ constrain_esm_PC+ (1+pa_esm_PC+na_esm_PC+constrain_esm_PC|ourID), data = data1) 
#Table 6, getting Rm and Rc
library(MuMIn)
r.squaredGLMM(gd1)
r.squaredGLMM(gd2)
r.squaredGLMM(gd3)
r.squaredGLMM(gd4)
#END Table 6

################### STUDY 2 - PAIRS ##################################
rm(list=ls(all=TRUE))
library(lme4)
data1 <- read.csv("~/Desktop/PAIRS esm.w1  3.17.16 .csv", sep=',', header = TRUE)
attach(data1)

# Let's person center (also called group-mean center) and grand mean center our variables
##PA esm.NQ11.w1
myMeans <- with(data1, tapply(esm.NQ11.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.NQ11.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.NQ11.w1_PC <- data1$esm.NQ11.w1-data1$esm.NQ11.w1_i ##person centered
data1$esm.NQ11.w1_GMC <- data1$esm.NQ11.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##NA esm.NQ12.w1
myMeans <- with(data1, tapply(esm.NQ12.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.NQ12.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.NQ12.w1_PC <- data1$esm.NQ12.w1-data1$esm.NQ12.w1_i ##person centered
data1$esm.NQ12.w1_GMC <- data1$esm.NQ12.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##EXTpos esm.BFI36.w1
myMeans <- with(data1, tapply(esm.BFI36.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI36.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI36.w1_PC <- data1$esm.BFI36.w1-data1$esm.BFI36.w1_i ##person centered
data1$esm.BFI36.w1_GMC <- data1$esm.BFI36.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##EXTneg esm.BFI21.w1
data1$esm.BFI21r.w1 <-(6-data1$esm.BFI21.w1)
myMeans <- with(data1, tapply(esm.BFI21r.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI21r.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI21r.w1_PC <- data1$esm.BFI21r.w1-data1$esm.BFI21r.w1_i ##person centered
data1$esm.BFI21r.w1_GMC <- data1$esm.BFI21r.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##AGRpos esm.BFI32.w1
myMeans <- with(data1, tapply(esm.BFI32.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI32.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI32.w1_PC <- data1$esm.BFI32.w1-data1$esm.BFI32.w1_i ##person centered
data1$esm.BFI32.w1_GMC <- data1$esm.BFI32.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##AGRneg esm.BFI37.w1
data1$esm.BFI37r.w1 <-(6-data1$esm.BFI37.w1)
myMeans <- with(data1, tapply(esm.BFI37r.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI37r.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI37r.w1_PC <- data1$esm.BFI37r.w1-data1$esm.BFI37r.w1_i ##person centered
data1$esm.BFI37r.w1_GMC <- data1$esm.BFI37r.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##CONpos esm.BFI13.w1
myMeans <- with(data1, tapply(esm.BFI13.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI13.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI13.w1_PC <- data1$esm.BFI13.w1-data1$esm.BFI13.w1_i ##person centered
data1$esm.BFI13.w1_GMC <- data1$esm.BFI13.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##CONneg esm.BFI23.w1
data1$esm.BFI23r.w1 <-(6-data1$esm.BFI23.w1)
myMeans <- with(data1, tapply(esm.BFI23r.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI23r.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI23r.w1_PC <- data1$esm.BFI23r.w1-data1$esm.BFI23r.w1_i ##person centered
data1$esm.BFI23r.w1_GMC <- data1$esm.BFI23r.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#CON item level, reliable, esm.BFI13.w1
myMeans <- with(data1, tapply(esm.BFI13.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI13.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI13.w1_PC <- data1$esm.BFI13.w1-data1$esm.BFI13.w1_i ##person centered
data1$esm.BFI13.w1_GMC <- data1$esm.BFI13.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#CON item level, lazy, esm.BFI23.w1
myMeans <- with(data1, tapply(esm.BFI23.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI23.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI23.w1_PC <- data1$esm.BFI23.w1-data1$esm.BFI23.w1_i ##person centered
data1$esm.BFI23.w1_GMC <- data1$esm.BFI23.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##NEUpos esm.BFI19.w1
myMeans <- with(data1, tapply(esm.BFI19.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI19.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI19.w1_PC <- data1$esm.BFI19.w1-data1$esm.BFI19.w1_i ##person centered
data1$esm.BFI19.w1_GMC <- data1$esm.BFI19.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
##NEUneg esm.BFI09.w1
data1$esm.BFI09r.w1 <-(6-data1$esm.BFI09.w1)
myMeans <- with(data1, tapply(esm.BFI09r.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BFI09r.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BFI09r.w1_PC <- data1$esm.BFI09r.w1-data1$esm.BFI09r.w1_i ##person centered
data1$esm.BFI09r.w1_GMC <- data1$esm.BFI09r.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
myMeans <- with(data1, tapply(esm.BFI23.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
#worked/studied esm.BH02.w1
myMeans <- with(data1, tapply(esm.BH02.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.BH02.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.BH02.w1_PC <- data1$esm.BH02.w1-data1$esm.BH02.w1_i ##person centered
data1$esm.BH02.w1_GMC <- data1$esm.BH02.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered
#liking esm.ST17.w1
data1$esm.ST17.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.ST17.w1_PC <- data1$esm.ST17.w1-data1$esm.ST17.w1_i ##person centered
data1$esm.ST17.w1_GMC <- data1$esm.ST17.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#want esm.ST02.w1
myMeans <- with(data1, tapply(esm.ST02.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.ST02.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.ST02.w1_PC <- data1$esm.ST02.w1-data1$esm.ST02.w1_i ##person centered
data1$esm.ST02.w1_GMC <- data1$esm.ST02.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#status esm.ST18.w1
myMeans <- with(data1, tapply(esm.ST18.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.ST18.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.ST18.w1_PC <- data1$esm.ST18.w1-data1$esm.ST18.w1_i ##person centered
data1$esm.ST18.w1_GMC <- data1$esm.ST18.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#impression esm.ST19.w1
myMeans <- with(data1, tapply(esm.ST19.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.ST19.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.ST19.w1_PC <- data1$esm.ST19.w1-data1$esm.ST19.w1_i ##person centered
data1$esm.ST19.w1_GMC <- data1$esm.ST19.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#common esm.ST01.w1
myMeans <- with(data1, tapply(esm.ST01.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.ST01.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.ST01.w1_PC <- data1$esm.ST01.w1-data1$esm.ST01.w1_i ##person centered
data1$esm.ST01.w1_GMC <- data1$esm.ST01.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 
#constrain/free esm.ST09.w1
data1$esm.ST09r.w1 <-(6-data1$esm.ST09.w1)
myMeans <- with(data1, tapply(esm.ST09r.w1, esm.IDnum.w1, mean, na.rm=TRUE)) ##getting each person's mean 
data1$esm.ST09r.w1_i <-myMeans[match(data1$esm.IDnum.w1, rownames(myMeans))] ##putting variable back into data
data1$esm.ST09r.w1_PC <- data1$esm.ST09r.w1-data1$esm.ST09r.w1_i ##person centered
data1$esm.ST09r.w1_GMC <- data1$esm.ST09r.w1_i-mean(myMeans, na.rm=TRUE) ##grand mean centered centered 



#Table 1
#item level correlations
#EXT and it's flip esm.BFI36.w1 & esm.BFI21r.w1
lmer(esm.BFI36.w1~1+esm.BFI21r.w1+ (1+esm.BFI21r.w1|esm.IDnum.w1), data = data1) # .67
#NEU and it's flip esm.BFI19.w1 & esm.BFI09r.w1
lmer(esm.BFI19.w1~1+esm.BFI09r.w1+ (1+esm.BFI09r.w1|esm.IDnum.w1), data = data1) # .51
#AGR and it's flip esm.BFI32.w1 & esm.BFI37r.w1
lmer(esm.BFI32.w1~1+esm.BFI37r.w1+ (1+esm.BFI37r.w1|esm.IDnum.w1), data = data1) # .20
#CON and it's flip esm.BFI13.w1 & esm.BFI23r.w1
lmer(esm.BFI13.w1~1+esm.BFI23r.w1+ (1+esm.BFI23r.w1|esm.IDnum.w1), data = data1) # .24
cor(esm.BFI13.w1,esm.BFI23.w1,use = "complete.obs")

#ICCs
lmer(esm.EXT.w1~ 1+ (1|esm.IDnum.w1), data = data1) 
0.3917/(1.1245+0.3917) =.26
lmer(esm.NEU.w1~ 1+ (1|esm.IDnum.w1), data = data1) 
0.4831/(0.8612+0.4831) = .36
lmer(esm.AGR.w1~ 1+ (1|esm.IDnum.w1), data = data1) 
0.3957/(0.5354+0.3957) = .42
lmer(esm.CON.w1~ 1+ (1|esm.IDnum.w1), data = data1) 
0.4607/(0.7960+0.4607) = .37
lmer(esm.NQ11.w1~ 1+ (1|esm.IDnum.w1), data = data1) 
0.4550/(0.8608+0.4550) = .35
lmer(esm.NQ12.w1~ 1+ (1|esm.IDnum.w1), data = data1) 
0.4707/(0.8940+0.4707) = .34
#crunch data from long to short so we can get means for Table 1
esm.EXT.w1_mean <-aggregate(esm.EXT.w1~esm.IDnum.w1, data=data1,FUN=function(x) c(mean=mean(x)))    
esm.NEU.w1_mean <-aggregate(esm.NEU.w1~esm.IDnum.w1, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(esm.EXT.w1_mean, esm.NEU.w1_mean, by.x = "esm.IDnum.w1", by.y = "esm.IDnum.w1", all=TRUE)
esm.AGR.w1_mean <-aggregate(esm.AGR.w1~esm.IDnum.w1, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, esm.AGR.w1_mean, by.x = "esm.IDnum.w1", by.y = "esm.IDnum.w1", all=TRUE)
esm.CON.w1_mean <-aggregate(esm.CON.w1~esm.IDnum.w1, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, esm.CON.w1_mean, by.x = "esm.IDnum.w1", by.y = "esm.IDnum.w1", all=TRUE)
esm.NQ11.w1_mean <-aggregate(esm.NQ11.w1~esm.IDnum.w1, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, esm.NQ11.w1_mean, by.x = "esm.IDnum.w1", by.y = "esm.IDnum.w1", all=TRUE)
esm.NQ12.w1_mean <-aggregate(esm.NQ12.w1~esm.IDnum.w1, data=data1,FUN=function(x) c(mean=mean(x)))    
small <- merge(small, esm.NQ12.w1_mean, by.x = "esm.IDnum.w1", by.y = "esm.IDnum.w1", all=TRUE)
small <- as.data.frame(small)
#getting means 
mean(small$esm.EXT.w1, na.rm=TRUE)
mean(small$esm.NEU.w1, na.rm=TRUE)
mean(small$esm.AGR.w1, na.rm=TRUE)
mean(small$esm.CON.w1, na.rm=TRUE)
mean(small$esm.NQ11.w1, na.rm=TRUE)
mean(small$esm.NQ12.w1, na.rm=TRUE)
## END of Table 1

#Table 2
#With PA
x1<-lmer(esm.EXT.w1~1+esm.NQ11.w1_PC+ (1+esm.NQ11.w1_PC|esm.IDnum.w1), data = data1)
x2<-lmer(esm.NEU.w1~1+esm.NQ11.w1_PC+ (1+esm.NQ11.w1_PC|esm.IDnum.w1), data = data1)
x3<-lmer(esm.AGR.w1~1+esm.NQ11.w1_PC+ (1+esm.NQ11.w1_PC|esm.IDnum.w1), data = data1)
x4<-lmer(esm.CON.w1~1+esm.NQ11.w1_PC+ (1+esm.NQ11.w1_PC|esm.IDnum.w1), data = data1)
##NA
x5<-lmer(esm.EXT.w1~1+esm.NQ12.w1_PC+ (1+esm.NQ12.w1_PC|esm.IDnum.w1), data = data1)
x6<-lmer(esm.NEU.w1~1+esm.NQ12.w1_PC+ (1+esm.NQ12.w1_PC|esm.IDnum.w1), data = data1)
x7<-lmer(esm.AGR.w1~1+esm.NQ12.w1_PC+ (1+esm.NQ12.w1_PC|esm.IDnum.w1), data = data1)
x8<-lmer(esm.CON.w1~1+esm.NQ12.w1_PC+ (1+esm.NQ12.w1_PC|esm.IDnum.w1), data = data1)
#PA&NA predicting Big5
x9<-lmer(esm.EXT.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC|esm.IDnum.w1), data = data1)
x10<-lmer(esm.NEU.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC|esm.IDnum.w1), data = data1)
x11<-lmer(esm.AGR.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC|esm.IDnum.w1), data = data1)
x12<-lmer(esm.CON.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC|esm.IDnum.w1), data = data1)
#Table 2, getting Rm and Rc
library(MuMIn)
r.squaredGLMM(x1)
r.squaredGLMM(x2)
r.squaredGLMM(x3)
r.squaredGLMM(x4)
r.squaredGLMM(x5)
r.squaredGLMM(x6)
r.squaredGLMM(x7)
r.squaredGLMM(x8)
r.squaredGLMM(x9)
r.squaredGLMM(x10)
r.squaredGLMM(x11)
r.squaredGLMM(x12)
#END Table 2

# Table 3
lmer(esm.NQ11.w1~1+esm.BH02.w1_PC+ (1+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) #worked predicting PA
lmer(esm.NQ12.w1~1+esm.BH02.w1_PC+ (1+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) #worked predicting NA
lmer(esm.NQ11.w1~1+esm.ST17.w1_PC+ (1+esm.ST17.w1_PC|esm.IDnum.w1), data = data1) #liking predicting PA
lmer(esm.NQ12.w1~1+esm.ST17.w1_PC+ (1+esm.ST17.w1_PC|esm.IDnum.w1), data = data1) #liking predicting NA
lmer(esm.NQ11.w1~1+esm.ST02.w1_PC+ (1+esm.ST02.w1_PC|esm.IDnum.w1), data = data1) #want predicting PA
lmer(esm.NQ12.w1~1+esm.ST02.w1_PC+ (1+esm.ST02.w1_PC|esm.IDnum.w1), data = data1) #want predicting NA
lmer(esm.NQ11.w1~1+esm.ST18.w1_PC+ (1+esm.ST18.w1_PC|esm.IDnum.w1), data = data1) #status predicting PA
lmer(esm.NQ12.w1~1+esm.ST18.w1_PC+ (1+esm.ST18.w1_PC|esm.IDnum.w1), data = data1) #status predicting NA
lmer(esm.NQ11.w1~1+esm.ST19.w1_PC+ (1+esm.ST19.w1_PC|esm.IDnum.w1), data = data1) #impression predicting PA
lmer(esm.NQ12.w1~1+esm.ST19.w1_PC+ (1+esm.ST19.w1_PC|esm.IDnum.w1), data = data1) #impression predicting NA
lmer(esm.NQ11.w1~1+esm.ST01.w1_PC+ (1+esm.ST01.w1_PC|esm.IDnum.w1), data = data1) #common predicting PA
lmer(esm.NQ12.w1~1+esm.ST01.w1_PC+ (1+esm.ST01.w1_PC|esm.IDnum.w1), data = data1) #common predicting NA
lmer(esm.NQ11.w1~1+esm.ST09r.w1_PC+ (1+esm.ST09r.w1_PC|esm.IDnum.w1), data = data1) #free predicting PA
lmer(esm.NQ12.w1~1+esm.ST09r.w1_PC+ (1+esm.ST09r.w1_PC|esm.IDnum.w1), data = data1) #free predicting NA
#END Table 3

#Table 4
lt1<-lmer(esm.EXT.w1~1+esm.BH02.w1_PC+ (1+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
lt2<-lmer(esm.NEU.w1~1+esm.BH02.w1_PC+ (1+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
lt3<-lmer(esm.AGR.w1~1+esm.BH02.w1_PC+ (1+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
lt4<-lmer(esm.CON.w1~1+esm.BH02.w1_PC+ (1+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
lt5<-lmer(esm.EXT.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ esm.BH02.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
lt6<-lmer(esm.NEU.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ esm.BH02.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
lt7<-lmer(esm.AGR.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ esm.BH02.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
lt8<-lmer(esm.CON.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ esm.BH02.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
#Table 4, getting Rm and Rc
library(MuMIn)
r.squaredGLMM(lt1)
r.squaredGLMM(lt2)
r.squaredGLMM(lt3)
r.squaredGLMM(lt4)
r.squaredGLMM(lt5)
r.squaredGLMM(lt6)
r.squaredGLMM(lt7)
r.squaredGLMM(lt8)
#END Table 4

#Table 5
yc1<-lmer(esm.AGR.w1~1+ esm.ST17.w1_PC+ (1+esm.ST17.w1_PC|esm.IDnum.w1), data = data1) #like
yc2<-lmer(esm.AGR.w1~1+ esm.ST02.w1_PC+ (1+esm.ST02.w1_PC|esm.IDnum.w1), data = data1) #want
yc3<-lmer(esm.AGR.w1~1+ esm.ST18.w1_PC+ (1+esm.ST18.w1_PC|esm.IDnum.w1), data = data1) #status
yc4<-lmer(esm.AGR.w1~1+ esm.ST19.w1_PC+ (1+esm.ST19.w1_PC|esm.IDnum.w1), data = data1) #impression
yc5<-lmer(esm.AGR.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ esm.ST17.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+esm.BH02.w1_PC|esm.IDnum.w1), data = data1) 
yc6<-lmer(esm.AGR.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ esm.ST02.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+esm.ST02.w1_PC|esm.IDnum.w1), data = data1) 
yc7<-lmer(esm.AGR.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ esm.ST18.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+esm.ST18.w1_PC|esm.IDnum.w1), data = data1) 
yc8<-lmer(esm.AGR.w1~1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+ esm.ST19.w1_PC+ (1+esm.NQ11.w1_PC+esm.NQ12.w1_PC+esm.ST19.w1_PC|esm.IDnum.w1), data = data1) 
#Table 5, getting Rm and Rc
library(MuMIn)
r.squaredGLMM(yc1)
r.squaredGLMM(yc2)
r.squaredGLMM(yc3)
r.squaredGLMM(yc4)
r.squaredGLMM(yc5)
r.squaredGLMM(yc6)
r.squaredGLMM(yc7)
r.squaredGLMM(yc8)
#END Table 5