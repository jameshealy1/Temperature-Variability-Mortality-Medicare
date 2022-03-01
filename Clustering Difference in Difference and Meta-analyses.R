
library(tidyverse)
library(sandwich)
library(gnm)
library(splines)
library(MASS)
library(broom)
library(tidyr)


counts<-readRDS("/nfs/home/J/jah6587/shared_space/ci3_jah6587/Medicare/FinalCounts.rds")

#Split Data by Climte Zone

ColdCounts <- counts[counts$BA_Climate=='Cold',]
HotDryCounts1 <- counts[counts$BA_Climate=='Hot-Dry',]
HotHumidCounts <- counts[counts$BA_Climate=='Hot-Humid',]
MarineCounts <- counts[counts$BA_Climate=='Marine',]
MixedHumidCounts <- counts[counts$BA_Climate=='Mixed-Humid',]
MixedDryCounts <- counts[counts$BA_Climate=='Mixed-Dry',]
VeryColdCounts <- counts[counts$BA_Climate== 'VeryCold',] 

AllColdCounts <- rbind(ColdCounts, VeryColdCounts)
HotDryCounts <- rbind(HotDryCounts1, MixedDryCounts)

#All Cold Zone

AllColdCovar=AllColdCounts[,c(3:7,9,12:16,17,18,19,20,21:28,36,41:44)]

AllColdCovar$other=ifelse(AllColdCovar$race=="Other",1,0)
AllColdCovar$black=ifelse(AllColdCovar$race=="Black",1,0)
AllColdCovar=dplyr::select(AllColdCovar,-c("race"))

AllColdCovar$male=as.numeric(AllColdCovar$male)
AllColdCovar$dual=as.numeric(AllColdCovar$dual)
AllColdCovar$old=as.numeric(AllColdCovar$old)

#####

#Take Random Sample
AllColdSample <- sample_n(AllColdCovar, 50000)

#NbClust Code
nc <- NbClust(AllColdSample, min.nc = 3, max.nc = 7, method = "kmeans",
              index = "gap")



AllColdCovar=ungroup(AllColdCovar)
ResultsAllColdZoneWarm<-matrix(NA,nrow=3,ncol=4)
ResultsAllColdZoneCold<-matrix(NA,nrow=3,ncol=4)
ResultsAllColdZoneWarmSD<-matrix(NA,nrow=3,ncol=4)
ResultsAllColdZoneColdSD<-matrix(NA,nrow=3,ncol=4)


#Cluster
tmp=filter(AllColdCovar)
tmp <- na.omit(tmp)
gc()
tmp=dplyr::select(tmp, -c(count, ColdAverage, WarmAverage, WarmSD, ColdSD))
tmp=tmp %>% mutate_all(scale)
dAllColdZone <- kmeans(tmp,3)
dAllColdZone <- na.omit(dAllColdZone)

#Merge d cluster into original dataset as specified by Joel to use regression below
AllColdCountsNoNA <- na.omit(AllColdCounts)
AllColdCountsNoNA <- cbind(AllColdCountsNoNA, clusterNum = dAllColdZone$cluster)
#saveRDS(AllColdCountsNoNA, "AllColdCountsMTempsSD3Clust.rds")
AllColdCountsNoNA <- readRDS("AllColdCountsMTempsSD3Clust.rds")

## Regression

for(j in 1) {
  tmp=AllColdCountsNoNA[AllColdCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsAllColdZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsAllColdZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsAllColdZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsAllColdZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]
   }

summary(mod)

for(j in 2) {
  tmp=AllColdCountsNoNA[AllColdCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsAllColdZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsAllColdZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsAllColdZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsAllColdZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]
  
  }

for(j in 3) {
  tmp=AllColdCountsNoNA[AllColdCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsAllColdZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsAllColdZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsAllColdZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsAllColdZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]
         }

##HotDry

HotDryCovar=HotDryCounts[,c(3:7,9,12:16,17,18,19,20,21:28,36,41:44)]

HotDryCovar$other=ifelse(HotDryCovar$race=="Other",1,0)
HotDryCovar$black=ifelse(HotDryCovar$race=="Black",1,0)
HotDryCovar=dplyr::select(HotDryCovar,-c("race"))

HotDryCovar$male=as.numeric(HotDryCovar$male)
HotDryCovar$dual=as.numeric(HotDryCovar$dual)
HotDryCovar$old=as.numeric(HotDryCovar$old)

######
HotDrySample <- sample_n(HotDryCovar, 75000)

#NbClust Code
nc <- NbClust(HotDrySample, min.nc = 3, max.nc = 7, method = "kmeans",
              index = "gap")
nc

#Climate Zone to Numeric
#Not in code: ColdCovar$BA_Climate=as.numeric(ColdCovar$BA_Climate)

HotDryCovar=ungroup(HotDryCovar)
ResultsHotDryZoneWarm<-matrix(NA,nrow=3,ncol=4)
ResultsHotDryZoneCold<-matrix(NA,nrow=3,ncol=4)
ResultsHotDryZoneWarmSD<-matrix(NA,nrow=3,ncol=4)
ResultsHotDryZoneColdSD<-matrix(NA,nrow=3,ncol=4)


#Cluster
tmp=filter(HotDryCovar)
tmp <- na.omit(tmp)
gc()
tmp=dplyr::select(tmp, -c(count, WarmAverage, ColdAverage, WarmSD, ColdSD))
tmp=tmp %>% mutate_all(scale)
dHotDryZone <- kmeans(tmp,3)
dHotDryZone <- na.omit(dHotDryZone)

#Merge d cluster into original dataset as specified by Joel to use regression below
HotDryCountsNoNA <- na.omit(HotDryCounts)
#HotDryCovarNoNA <- na.omit(HotDryCovar)
HotDryCountsNoNA <- cbind(HotDryCovarNoNA, clusterNum = dHotDryZone$cluster)
###HotDryCountsNoNA <- left_join(HotDryCountsNoNA,dHotDryZone, by=c(zip,year))
#saveRDS(HotDryCountsNoNA, "HotDryCountsMTempSD3Clust.rds")
HotDryCountsNoNA <- readRDS("HotDryCountsMTempSD3Clust.rds")


for(j in 1) {
  tmp=HotDryCountsNoNA[HotDryCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsHotDryZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsHotDryZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsHotDryZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsHotDryZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]}


for(j in 2) {
  tmp=HotDryCountsNoNA[HotDryCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsHotDryZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsHotDryZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsHotDryZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsHotDryZoneColdSD[j,]=summary(mod)$coefficients[28,1:4] }

for(j in 3) {
  tmp=HotDryCountsNoNA[HotDryCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsHotDryZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsHotDryZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsHotDryZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsHotDryZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]  }



##HotHumid

HotHumidCovar=HotHumidCounts[,c(3:7,9,12:16,17,18,19,20,21:28,36,41:44)]

HotHumidCovar$other=ifelse(HotHumidCovar$race=="Other",1,0)
HotHumidCovar$black=ifelse(HotHumidCovar$race=="Black",1,0)
HotHumidCovar=dplyr::select(HotHumidCovar,-c("race"))

HotHumidCovar$male=as.numeric(HotHumidCovar$male)
HotHumidCovar$dual=as.numeric(HotHumidCovar$dual)
HotHumidCovar$old=as.numeric(HotHumidCovar$old)

######
#HotHumidSample <- sample_n(HotHumidCovar, 75000)

#NbClust Code
#NbClust(HotHumidSample, min.nc = 3, max.nc = 7, method = "kmeans", index = "gap")



#Climate Zone to Numeric

HotHumidCovar=ungroup(HotHumidCovar)
ResultsHotHumidZoneWarm<-matrix(NA,nrow=3,ncol=4)
ResultsHotHumidZoneCold<-matrix(NA,nrow=3,ncol=4)
ResultsHotHumidZoneWarmSD<-matrix(NA,nrow=3,ncol=4)
ResultsHotHumidZoneColdSD<-matrix(NA,nrow=3,ncol=4)


#Cluster
tmp=filter(HotHumidCovar)
tmp <- na.omit(tmp)
gc()
tmp=dplyr::select(tmp, -c(count, WarmAverage, ColdAverage, WarmSD, ColdSD))
tmp=tmp %>% mutate_all(scale)
dHotHumidZone <- kmeans(tmp,3)
dHotHumidZone <- na.omit(dHotHumidZone)

#Merge d cluster into original dataset as specified by Joel to use regression below
HotHumidCountsNoNA <- na.omit(HotHumidCounts)
HotHumidCountsNoNA <- cbind(HotHumidCountsNoNA, clusterNum = dHotHumidZone$cluster)
#saveRDS(HotHumidCountsNoNA, "HotHumidCountsMTempSD3Clust.rds")
HotHumidCountsNoNA <- readRDS("HotHumidCountsMTempSD3Clust.rds")

for(j in 1) {
  tmp=HotHumidCountsNoNA[HotHumidCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsHotHumidZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsHotHumidZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsHotHumidZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsHotHumidZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]}



for(j in 2) {
  tmp=HotHumidCountsNoNA[HotHumidCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsHotHumidZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsHotHumidZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsHotHumidZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsHotHumidZoneColdSD[j,]=summary(mod)$coefficients[28,1:4] }

for(j in 3) {
  tmp=HotHumidCountsNoNA[HotHumidCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsHotHumidZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsHotHumidZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsHotHumidZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsHotHumidZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]}


MarineCovar=MarineCounts[,c(3:7,9,12:16,17,18,19,20,21:28,36,41:44)]

MarineCovar$other=ifelse(MarineCovar$race=="Other",1,0)
MarineCovar$black=ifelse(MarineCovar$race=="Black",1,0)
MarineCovar=dplyr::select(MarineCovar,-c("race"))

MarineCovar$male=as.numeric(MarineCovar$male)
MarineCovar$dual=as.numeric(MarineCovar$dual)
MarineCovar$old=as.numeric(MarineCovar$old)

######
#MarineSample <- sample_n(MarineCovar, 75000)

#NbClust Code
#nc <- NbClust(MarineSample, min.nc = 3, max.nc = 7, method = "kmeans",   index = "gap")


#Climate Zone to Numeric
#Not in code: ColdCovar$BA_Climate=as.numeric(ColdCovar$BA_Climate)

MarineCovar=ungroup(MarineCovar)
ResultsMarineZoneWarm <-matrix(NA,nrow=3,ncol=4)
ResultsMarineZoneCold <-matrix(NA,nrow=3,ncol=4)
ResultsMarineZoneWarmSD <-matrix(NA,nrow=3,ncol=4)
ResultsMarineZoneColdSD <-matrix(NA,nrow=3,ncol=4)


#Cluster

tmp=filter(MarineCovar)
tmp <- na.omit(tmp)
gc()
tmp=dplyr::select(tmp, -c(count, WarmAverage, ColdAverage, WarmSD, ColdSD))
tmp=tmp %>% mutate_all(scale)
dMarineZone <- kmeans(tmp,3)
dMarineZone <- na.omit(dMarineZone)

#Merge d cluster into original dataset as specified by Joel to use regression below
MarineCountsNoNA <- na.omit(MarineCounts)
MarineCountsNoNA <- cbind(MarineCountsNoNA, clusterNum = dMarineZone$cluster)
#saveRDS(MarineCountsNoNA, "MarineCountsMTempSD3Clust.rds")
MarineCountsNoNA <- readRDS("MarineCountsMTempSD3Clust.rds")

for(j in 1) {
  tmp=MarineCountsNoNA[MarineCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsMarineZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsMarineZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsMarineZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsMarineZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]}

for(j in 2) {
  tmp=MarineCountsNoNA[MarineCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsMarineZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsMarineZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsMarineZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsMarineZoneColdSD[j,]=summary(mod)$coefficients[28,1:4] }

for(j in 3) {
  tmp=MarineCountsNoNA[MarineCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+PctEye+as.factor(race)+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsMarineZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsMarineZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsMarineZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsMarineZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]}

#####

MixedHumidCovar=MixedHumidCounts[,c(3:7,9,12:16,17,18,19,20,21:28,36,41:44)]

MixedHumidCovar$other=ifelse(MixedHumidCovar$race=="Other",1,0)
MixedHumidCovar$black=ifelse(MixedHumidCovar$race=="Black",1,0)
MixedHumidCovar=dplyr::select(MixedHumidCovar,-c("race"))

MixedHumidCovar$male=as.numeric(MixedHumidCovar$male)
MixedHumidCovar$dual=as.numeric(MixedHumidCovar$dual)
MixedHumidCovar$old=as.numeric(MixedHumidCovar$old)

######
#MixedHumidSample <- sample_n(MixedHumidCovar, 75000)

#NbClust Code
#nc <- NbClust(MixedHumidSample, min.nc = 3, max.nc = 7, method = "kmeans", index = "gap")

MixedHumidCovar=ungroup(MixedHumidCovar)
ResultsMixedHumidZoneWarm<-matrix(NA,nrow=3,ncol=4)
ResultsMixedHumidZoneCold<-matrix(NA,nrow=3,ncol=4)
ResultsMixedHumidZoneWarmSD<-matrix(NA,nrow=3,ncol=4)
ResultsMixedHumidZoneColdSD<-matrix(NA,nrow=3,ncol=4)

#Cluster
tmp=filter(MixedHumidCovar)
tmp <- na.omit(tmp)
gc()
tmp=dplyr::select(tmp, -c(count, WarmAverage, ColdAverage, WarmSD, ColdSD))
tmp=tmp %>% mutate_all(scale)
dMixedHumidZone <- kmeans(tmp,3)
dMixedHumidZone <- na.omit(dMixedHumidZone)

#Merge d cluster into original dataset as specified by Joel to use regression below
MixedHumidCountsNoNA <- na.omit(MixedHumidCounts)
MixedHumidCountsNoNA <- cbind(MixedHumidCountsNoNA, clusterNum = dMixedHumidZone$cluster)
#saveRDS(MixedHumidCountsNoNA, "MixedHumidCountsMTemp3Clust.rds")
MixedHumidCountsNoNA <- readRDS("MixedHumidCountsMTemp3Clust.rds")

for(j in 1) {
  tmp=MixedHumidCountsNoNA[MixedHumidCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsMixedHumidZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsMixedHumidZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsMixedHumidZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsMixedHumidZoneColdSD[j,]=summary(mod)$coefficients[28,1:4] }

summary(mod)

for(j in 2) {
  tmp=MixedHumidCountsNoNA[MixedHumidCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsMixedHumidZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsMixedHumidZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsMixedHumidZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsMixedHumidZoneColdSD[j,]=summary(mod)$coefficients[28,1:4] }

for(j in 3) {
  tmp=MixedHumidCountsNoNA[MixedHumidCountsNoNA$cluster==j,]
  
  mod=gnm(I(count/ptime)~ns(year,3)+old+pm25+poverty+dual+pct_blk+
            mean_bmi+popdensity+PctLDL +medianhousevalue+male+
            education+hispanic+smoke_rate+amb_visit_pct+a1c_exm_pct+
            pct_owner_occ+as.factor(race)+PctEye+Pctmam+medhouseholdincome+WarmAverage+ColdAverage+WarmSD+ColdSD,
          eliminate=as.factor(zip), data=tmp,family=gaussian)
  
  ResultsMixedHumidZoneWarm[j,]=summary(mod)$coefficients[25,1:4]
  ResultsMixedHumidZoneCold[j,]=summary(mod)$coefficients[26,1:4]
  ResultsMixedHumidZoneWarmSD[j,]=summary(mod)$coefficients[27,1:4]
  ResultsMixedHumidZoneColdSD[j,]=summary(mod)$coefficients[28,1:4]}

###Save Files for Meta-Analyses

write.csv(ResultsHotDryZoneCold, file = "ResultsHotDryZoneColdMT3C.csv", row.names = FALSE)
write.csv(ResultsHotDryZoneWarm, file = "ResultsHotDryZoneWarmMT3C.csv", row.names = FALSE)
write.csv(ResultsHotDryZoneColdSD, file = "ResultsHotDryZoneColdMTSD3C.csv", row.names = FALSE)
write.csv(ResultsHotDryZoneWarmSD, file = "ResultsHotDryZoneWarmMTSD3C.csv", row.names = FALSE)
write.csv(ResultsHotHumidZoneCold, file = "ResultsHotHumidZoneColdMT3C.csv", row.names = FALSE)
write.csv(ResultsHotHumidZoneWarm, file = "ResultsHotHumidZoneWarmMT3C.csv", row.names = FALSE)
write.csv(ResultsHotHumidZoneColdSD, file = "ResultsHotHumidZoneColdMTSD3C.csv", row.names = FALSE)
write.csv(ResultsHotHumidZoneWarmSD, file = "ResultsHotHumidZoneWarmMTSD3C.csv", row.names = FALSE)
write.csv(ResultsMarineZoneWarm, file = "ResultsMarineZoneWarmMT3C.csv", row.names = FALSE)
write.csv(ResultsMarineZoneCold, file = "ResultsMarineZoneColdMT3C.csv", row.names = FALSE)
write.csv(ResultsMarineZoneWarmSD, file = "ResultsMarineZoneWarmMTSD3C.csv", row.names = FALSE)
write.csv(ResultsMarineZoneColdSD, file = "ResultsMarineZoneColdMTSD3C.csv", row.names = FALSE)
write.csv(ResultsMixedHumidZoneCold, file = "ResultsMixedHumidZoneColdMT3C.csv", row.names = FALSE)
write.csv(ResultsMixedHumidZoneWarm, file = "ResultsMixedHumidZoneWarmMT3C.csv", row.names = FALSE)
write.csv(ResultsMixedHumidZoneColdSD, file = "ResultsMixedHumidZoneColdMTSD3C.csv", row.names = FALSE)
write.csv(ResultsMixedHumidZoneWarmSD, file = "ResultsMixedHumidZoneWarmMTSD3C.csv", row.names = FALSE)
write.csv(ResultsAllColdZoneCold, file = "ResultsAllColdZoneColdMT3C.csv", row.names = FALSE)
write.csv(ResultsAllColdZoneWarm, file = "ResultsAllColdZoneWarmMT3C.csv", row.names = FALSE)
write.csv(ResultsAllColdZoneColdSD, file = "ResultsAllColdZoneColdMTSD3C.csv", row.names = FALSE)
write.csv(ResultsAllColdZoneWarmSD, file = "ResultsAllColdZoneWarmMTSD3C.csv", row.names = FALSE)



### Meta-Analysis

###Meta-Analysis for Cold Temperatures in Climate Zone: Hot Dry## 

MetaResultsHotDryZoneCold = rma.uni(yi=ResultsHotDryZoneCold$V1, sei=ResultsHotDryZoneCold$V2,digits=7)
MetaResultsHotDryZoneCold

###Meta-Analysis for Cold Temperatures Standard Deviations in Climate Zone: Hot Dry## 

MetaResultsHotDryZoneColdSD = rma.uni(yi=ResultsHotDryZoneColdSD$V1, sei=ResultsHotDryZoneColdSD$V2, digits=7)
MetaResultsHotDryZoneColdSD

###Meta-Analysis for Warm Temperatures in Climate Zone: Hot Dry## 
MetaResultsHotDryZoneWarm = rma.uni(yi=ResultsHotDryZoneWarm$V1, sei=ResultsHotDryZoneWarm$V2, digits=7)
MetaResultsHotDryZoneWarm

###Meta-Analysis for Warm Temperatures Standard Deviations in Climate Zone: Hot Dry## 

MetaResultsHotDryZoneWarmSD = rma.uni(yi=ResultsHotDryZoneWarmSD$V1, sei=ResultsHotDryZoneWarmSD$V2, digits=7)
MetaResultsHotDryZoneWarmSD

###Meta-Analysis for Cold Temperatures in Climate Zone: Hot Humid## 

MetaResultsHotHumidZoneCold = rma.uni(yi=ResultsHotHumidZoneCold$V1, sei=ResultsHotHumidZoneCold$V2, digits=7)
MetaResultsHotHumidZoneCold

###Meta-Analysis for Cold Temperatures Standard Deviations in Climate Zone: Hot Humid## 

MetaResultsHotHumidZoneColdSD = rma.uni(yi=ResultsHotHumidZoneColdSD$V1, sei=ResultsHotHumidZoneColdSD$V2, digits=7)
MetaResultsHotHumidZoneColdSD

###Meta-Analysis for Warm Temperatures in Climate Zone: Hot Humid## 

MetaResultsHotHumidZoneWarm = rma.uni(yi=ResultsHotHumidZoneWarm$V1, sei=ResultsHotHumidZoneWarm$V2, digits=7)
MetaResultsHotHumidZoneWarm

###Meta-Analysis for Warm Temperatures Standard Deviations in Climate Zone: Hot Humid## 

MetaResultsHotHumidZoneWarmSD = rma.uni(yi=ResultsHotHumidZoneWarmSD$V1, sei=ResultsHotHumidZoneWarmSD$V2, digits=7)
MetaResultsHotHumidZoneWarmSD

###Meta-Analysis for Cold Temperatures in Climate Zone: Mixed Humid## 

MetaResultsMixedHumidZoneCold = rma.uni(yi=ResultsMixedHumidZoneCold$V1, sei=ResultsMixedHumidZoneCold$V2, digits=7)
MetaResultsMixedHumidZoneCold

###Meta-Analysis for Cold Temperatures Standard Deviations in Climate Zone: Mixed Humid## 

MetaResultsMixedHumidZoneColdSD = rma.uni(yi=ResultsMixedHumidZoneColdSD$V1, sei=ResultsMixedHumidZoneColdSD$V2, digits=7)
MetaResultsMixedHumidZoneColdSD

###Meta-Analysis for Warm Temperatures in Climate Zone: Mixed Humid## 

MetaResultsMixedHumidZoneWarm = rma.uni(yi=ResultsMixedHumidZoneWarm$V1, sei=ResultsMixedHumidZoneWarm$V2, digits=7)
MetaResultsMixedHumidZoneWarm

###Meta-Analysis for Warm Temperatures Standard Deviations in Climate Zone: Mixed Humid## 

MetaResultsMixedHumidZoneWarmSD = rma.uni(yi=ResultsMixedHumidZoneWarmSD$V1, sei=ResultsMixedHumidZoneWarmSD$V2, digits=7)
MetaResultsMixedHumidZoneWarmSD

###Meta-Analysis for Cold Temperatures in Climate Zone: Marine## 

MetaResultsMarineZoneCold = rma.uni(yi=ResultsMarineZoneCold$V1, sei=ResultsMarineZoneCold$V2, digits=7)
MetaResultsMarineZoneCold

###Meta-Analysis for Cold Temperatures Standard Deviations in Climate Zone: Marine## 

MetaResultsMarineZoneColdSD = rma.uni(yi=ResultsMarineZoneColdSD$V1, sei=ResultsMarineZoneColdSD$V2, digits=7)
MetaResultsMarineZoneColdSD

###Meta-Analysis for Warm Temperatures in Climate Zone: Marine## 

MetaResultsMarineZoneWarm = rma.uni(yi=ResultsMarineZoneWarm$V1, sei=ResultsMarineZoneWarm$V2, digits=7)
MetaResultsMarineZoneWarm

###Meta-Analysis for Warm Temperatures Standard Deviations in Climate Zone: Marine ## 

MetaResultsMarineZoneWarmSD = rma.uni(yi=ResultsMarineZoneWarmSD$V1, sei=ResultsMarineZoneWarmSD$V2, digits=7)
MetaResultsMarineZoneWarmSD

###Meta-Analysis for Cold Temperatures in Climate Zone: All Cold Zones## 

MetaResultsAllColdZoneCold = rma.uni(yi=ResultsAllColdZoneCold$V1, sei=ResultsAllColdZoneCold$V2, digits=7)
MetaResultsAllColdZoneCold

###Meta-Analysis for Cold Temperatures Standard Deviations in Climate Zone: All Cold Zones ## 

MetaResultsAllColdZoneColdSD = rma.uni(yi=ResultsAllColdZoneColdSD$V1, sei=ResultsAllColdZoneColdSD$V2, digits=7)
MetaResultsAllColdZoneColdSD

###Meta-Analysis for Warm Temperatures in Climate Zone: All Cold Zones## 

MetaResultsAllColdZoneWarm = rma.uni(yi=ResultsAllColdZoneWarm$V1, sei=ResultsAllColdZoneWarm$V2, digits=7)
MetaResultsAllColdZoneWarm

###Meta-Analysis for Warm Temperature Standard Deviations in Climate Zone: All Cold Zones ## 

MetaResultsAllColdZoneWarmSD = rma.uni(yi=ResultsAllColdZoneWarmSD$V1, sei=ResultsAllColdZoneWarmSD$V2, digits=7)
MetaResultsAllColdZoneWarmSD
