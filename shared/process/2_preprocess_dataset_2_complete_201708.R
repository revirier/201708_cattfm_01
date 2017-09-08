---
# preprocess dataframe before analytics
# not normalize, to normalize with the h2o model
# with STD vars
  
# 20170807
# load from dataset 2
  
# completa dataset
# ds_icu_pm_2 20445 obs 148 vars
---
  
#libraries
  

require("caret")
require("dplyr")



load(file="~/shared/datasets/ds_icu_raw_2")


#first we converted the logical values to 1=TRUE / 0=FALSE

ds_icu_pm_2[,sapply(ds_icu_pm_2,is.logical)]<-ds_icu_pm_2[,sapply(ds_icu_pm_2,is.logical)]+0

#PAT_GENDER to 0,1 FEMALE->1, MALE->0

ds_icu_pm_2$pat_gender=ds_icu_pm_2$pat_gender=="F"
ds_icu_pm_2$pat_gender=ds_icu_pm_2$pat_gender+0

#delete column
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("pat_expire_hosp"))]

#delete icu_los and adt_hstays
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("icu_los","adt_hstays"))]

# asign to decades
ds_icu_pm_2$adt_hos_age<-cut(ds_icu_pm_2$adt_hos_age,breaks=seq(0,100,10),labels=seq(0,9))

#factorize
ds_icu_pm_2$adt_hos_age<-as.factor(ds_icu_pm_2$adt_hos_age)

#ICU_DBSOURCE
ds_icu_pm_2$icu_dbsource<-as.factor(ds_icu_pm_2$icu_dbsource)

#ICU_TYPE_FSERVICE
ds_icu_pm_2$icu_type_fservice<-as.factor(ds_icu_pm_2$icu_type_fservice)

#IMPUTATION + REFACTOR
ds_icu_pm_2$icu_type_fservice[which(ds_icu_pm_2$icu_type_fservice=="-")]<-"MEDICAL"
ds_icu_pm_2$icu_type_fservice<-droplevels(ds_icu_pm_2$icu_type_fservice)


#ICD9P_M
#FACTOR
ds_icu_pm_2$icd9p_m<-as.factor(as.character(ds_icu_pm_2$icd9p_m))
#summary


#ICD9D_M
#FACTOR
ds_icu_pm_2$icd9d_m<-as.factor(as.character(ds_icu_pm_2$icd9d_m))

#DIAG_M_HAS_DIABET / DIAG_S_HAS_DIABET
#combine columns OR
ds_icu_pm_2$diag_has_diabet<-(ds_icu_pm_2$diag_m_has_diabet | ds_icu_pm_2$diag_s_has_diabet) 
ds_icu_pm_2$diag_has_diabet<-ds_icu_pm_2$diag_has_diabet+0

#delete column
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("diag_s_has_diabet","diag_m_has_diabet"))]


#SCORES

ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("score_apsiii"))]
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("score_lods"))]
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("score_mlods"))]
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("score_oasis"))]
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("score_qsofa"))]
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("score_saps"))]
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("score_sirs"))]


#OUTLIERS WHITEBLOOD
ds_icu_pm_2$whiteblood_avg_24h[ds_icu_pm_2$whiteblood_avg_24h>200]<-NA
ds_icu_pm_2$whiteblood_avg_48h[ds_icu_pm_2$whiteblood_avg_48h>200]<-NA
ds_icu_pm_2$whiteblood_avg_48h[ds_icu_pm_2$whiteblood_avg_48h>200]<-NA


#GLUCOSE_VARIABILITY_ICU
ds_icu_pm_2$glucose_variability_icu[ds_icu_pm_2$glucose_variability_icu>500]<-NA
ds_icu_pm_2$glucose_variability_icu[ds_icu_pm_2$glucose_variability_24h>300]<-NA
ds_icu_pm_2$glucose_variability_icu[ds_icu_pm_2$glucose_variability_48h>300]<-NA
ds_icu_pm_2$glucose_variability_icu[ds_icu_pm_2$glucose_variability_72h>300]<-NA


#NUT_TPN
tpnl<-names(ds_icu_pm_2[grep("nut_tpn",names(ds_icu_pm_2))])
ds_icu_pm_2[tpnl][is.na(ds_icu_pm_2[tpnl])]<-0


#NUT_TFE
tfel<-names(ds_icu_pm_2[grep("nut_tfe",names(ds_icu_pm_2))])

#IMPUTATION
ds_icu_pm_2[tfel][is.na(ds_icu_pm_2[tfel])]<-0

#remove outliers
ds_icu_pm_2$hyper_gluce_percent_72h[ds_icu_pm_2$hyper_gluce_percent_72h<0]<-NA
ds_icu_pm_2$hyper_gluce_percent_72h[ds_icu_pm_2$hyper_gluce_percent_72h>100]<-NA

#remove outliers
ds_icu_pm_2$in_range_clinic_percent_72h[ds_icu_pm_2$in_range_clinic_percent_72h<0]<-NA
ds_icu_pm_2$in_range_clinic_percent_72h[ds_icu_pm_2$in_range_clinic_percent_72h>100]<-NA
ds_icu_pm_2$in_range_protocol_percent_72h[ds_icu_pm_2$in_range_protocol_percent_72h<0]<-NA
ds_icu_pm_2$in_range_protocol_percent_72h[ds_icu_pm_2$in_range_protocol_percent_72h>100]<-NA

#FINAL FORMAT DATASET
#only variables obtained for <72H
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% names(ds_icu_pm_2[grep("_icu",names(ds_icu_pm_2))]))]

#delete db_sources
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("icu_dbsource"))]

#problems with factor
#ds_icu_pm_2$adt_hos_age<-as.numeric(ds_icu_pm_2$adt_hos_age)

#delete only score_sapii data
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("score_sapsii"))]

#auxiliar variable
ds_icu_pm_2$score_sofa_more4<-as.numeric(ds_icu_pm_2$score_sofa>=4)

#factorize
ds_icu_pm_2$score_sofa<-as.factor(ds_icu_pm_2$score_sofa)

#group icd9_m_chapter
dicd9<-(ds_icu_pm_2 %>% group_by(icd9d_m_chapter) %>% summarise(n=n()) %>% arrange(n))

#refactoring as others
dicd_others<-filter(dicd9,n<1000)
ds_icu_pm_2$icd9d_m_chapter<-as.character(ds_icu_pm_2$icd9d_m_chapter)
ds_icu_pm_2<-ds_icu_pm_2 %>% mutate(icd9d_m_chapter=ifelse(icd9d_m_chapter %in% dicd_others$icd9d_m_chapter,"00-Others",icd9d_m_chapter))
ds_icu_pm_2$icd9d_m_chapter<-as.factor(ds_icu_pm_2$icd9d_m_chapter)
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("icd9d_m"))]

#delete column
ds_icu_pm_2<-ds_icu_pm_2[,-which(names(ds_icu_pm_2) %in% c("icd9p_m"))]

# 20170807
#imputation only with caret
preproc_ds_range<-preProcess(ds_icu_pm_2,method = c("medianImpute"))
ds_icu_pm_2<-predict(preproc_ds_range,ds_icu_pm_2)

#factorize outcome
ds_icu_pm_2$icu_dead_before_28<-as.factor(ds_icu_pm_2$icu_dead_before_28)
levels(ds_icu_pm_2$icu_dead_before_28)<-make.names(levels(ds_icu_pm_2$icu_dead_before_28))

# 20170807
#factorize all binary variables
bincols<-colMeans((ds_icu_pm_2==1| ds_icu_pm_2==0),na.rm=T)==1
for (i in 1:length(bincols)) {
  #turn the binary column as a factor
  if (bincols[i]){
    ds_icu_pm_2[[i]]<-as.factor(ds_icu_pm_2[[i]])
  }
}

#save(file="~/shared/datasets/ds_icu_pm_complete_2",ds_icu_pm_2)


