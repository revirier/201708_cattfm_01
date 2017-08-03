---
title: "Load-Format dataset 1"

---
  
#libraries
  
require("RPostgreSQL")
require("caret")
require("dplyr")
require("pROC")


#connect database

drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "mimicsel",
                 host = "192.168.1.106", port = 5432,
                 user = "mimicsel", password = "mimicsel")


# check for the cartable
dbExistsTable(con, "ds_icu_stays_1")
# TRUE


#dataframe from table


ds_icu_pm_1<-dbGetQuery(con,"select 
                        PAT_GENDER,PAT_EXPIRE_HOSP,ADT_HOS_AGE,ADT_HSTAYS,ADT_READMIT,ICU_DBSOURCE,ICU_LOS,ICU_TYPE_FSERVICE,ICU_DEAD_BEFORE_28,ICU_HAS_PRESC_INSULIN,
                        ICU_HAS_ADMIV_INSULIN,ICU_HAS_ADMIT_INSULIN,ICU_STAY_MORE72,ICD9P_M,ICD9D_M,ICD9D_M_CHAPTER,DIAG_M_HAS_DIABET,DIAG_S_HAS_DIABET,COM_CONGEST_HEA_FAILURE ,COM_CARDIAC_ARRHYTHMIAS ,
                        COM_VALVULA_DISEASE ,COM_PULMONA_CIR_DISORDERS ,COM_PERIPHE_VAS_DISORDERS ,COM_HYPERTENSION ,COM_PARALYSIS ,COM_OTHER_NEUR_DISORDERS ,COM_CHRONIC_PUL_DISEASE ,
                        COM_DIABETES_UNCOMPLICATED ,COM_DIABETES_COMPLICATED ,COM_HYPOTHYROIDISM ,COM_RENAL_FAILURE ,COM_LIVER_DISEASE ,COM_PEPTIC_ULC_DISEASE ,COM_AIDS ,
                        COM_LYMPHOMA ,COM_METASTATIC_CANCER ,COM_SOLID_TUM_WO_METASTASIS ,COM_RHEUMATOID_ART_DISEASES ,COM_COAGULOPATHY ,COM_OBESITY ,COM_WEIGHT_LOSS ,COM_FLUID_ELEC_DISORDERS ,
                        COM_BLOOD_LOS_ANEMIA ,COM_DEFICIENCY_ANEMIAS ,COM_ALCOHOL_ABUSE ,COM_DRUG_ABUSE ,COM_PSYCHOSES ,COM_DEPRESSION ,SCORE_APSIII,SCORE_LODS,
                        SCORE_MLODS,SCORE_OASIS,SCORE_QSOFA,SCORE_SAPS,SCORE_SAPSII,SCORE_SOFA,SCORE_SIRS,MED_HAS_ALLERGIE_INSULIN,MED_HAS_INFECTION,MED_HAS_EXPLICIT_SEPSIS,
                        MED_HAS_ORGAN_DYSFUNCTION,MED_HAS_SEPSIS,MED_HAS_VASO,MED_HAS_24H_VASO,MED_HAS_DIALYSIS,MED_HAS_24H_DIALYSIS,MED_HAS_MECHVENT,MED_HAS_24H_MECHVENT,
                        MED_HAS_ESTEROID_PRESC,PH_AVG_24H,PH_STD_24H,PO2_AVG_24H,PO2_STD_24H,WHITEBLOOD_AVG_24H,WHITEBLOOD_STD_24H,LACTATE_AVG_24H,LACTATE_STD_24H,
                        HEMOGLOBIN_AVG_24H,HEMOGLOBIN_STD_24H,PCO2_AVG_24H,PCO2_STD_24H,BICARBONATE_AVG_24H,BICARBONATE_STD_24H,CREATININE_AVG_24H,CREATININE_STD_24H,PH_AVG_48H,
                        PH_STD_48H,PO2_AVG_48H,PO2_STD_48H,WHITEBLOOD_AVG_48H,WHITEBLOOD_STD_48H,LACTATE_AVG_48H,LACTATE_STD_48H,HEMOGLOBIN_AVG_48H,HEMOGLOBIN_STD_48H,PCO2_AVG_48H,
                        PCO2_STD_48H,BICARBONATE_AVG_48H,BICARBONATE_STD_48H,CREATININE_AVG_48H,CREATININE_STD_48H,PH_AVG_72H,PH_STD_72H,PO2_AVG_72H,PO2_STD_72H,WHITEBLOOD_AVG_72H,
                        WHITEBLOOD_STD_72H,LACTATE_AVG_72H,LACTATE_STD_72H,HEMOGLOBIN_AVG_72H,HEMOGLOBIN_STD_72H,PCO2_AVG_72H,PCO2_STD_72H,BICARBONATE_AVG_72H,BICARBONATE_STD_72H,
                        CREATININE_STD_72H,CREATININE_AVG_72H,GLUCOSE_VARIABILITY_ICU,TIME_HIPO_CRITICAL_SUM_24H,TIME_HIPO_MODERATE_SUM_24H,TIME_HYPER_GLUCE_SUM_24H,TIME_RANGE_PROTOCOL_SUM_24H,
                        TIME_RANGE_CLINIC_SUM_24H,GLUCOSE_VARIABILITY_24H,TIME_HIPO_CRITICAL_SUM_48H,TIME_HIPO_MODERATE_SUM_48H,TIME_HYPER_GLUCE_SUM_48H,TIME_RANGE_PROTOCOL_SUM_48H,
                        TIME_RANGE_CLINIC_SUM_48H,GLUCOSE_VARIABILITY_48H,TIME_HIPO_CRITICAL_SUM_72H,TIME_HIPO_MODERATE_SUM_72H,TIME_HYPER_GLUCE_SUM_72H,TIME_RANGE_PROTOCOL_SUM_72H,
                        TIME_RANGE_CLINIC_SUM_72H,
                        HIPO_CRITICAL_PERCENT_ICU,HIPO_MODERATE_PERCENT_ICU,HYPER_GLUCE_PERCENT_ICU,IN_RANGE_CLINIC_PERCENT_ICU,IN_RANGE_PROTOCOL_PERCENT_ICU,
                        HIPO_CRITICAL_PERCENT_24H,HIPO_MODERATE_PERCENT_24H,HYPER_GLUCE_PERCENT_24H,IN_RANGE_CLINIC_PERCENT_24H,IN_RANGE_PROTOCOL_PERCENT_24H,
                        HIPO_CRITICAL_PERCENT_48H,HIPO_MODERATE_PERCENT_48H,HYPER_GLUCE_PERCENT_48H,IN_RANGE_CLINIC_PERCENT_48H,IN_RANGE_PROTOCOL_PERCENT_48H,
                        HIPO_CRITICAL_PERCENT_72H,HIPO_MODERATE_PERCENT_72H,HYPER_GLUCE_PERCENT_72H,IN_RANGE_CLINIC_PERCENT_72H,IN_RANGE_PROTOCOL_PERCENT_72H,
                        GLUCOSE_VARIABILITY_72H,NUT_TPN_IN_24H,NUT_NPO_IN_24H,NUT_PPN_IN_24H,
                        NUT_TFE_IN_24H,NUT_TPN_IN_48H,NUT_NPO_IN_48H,NUT_PPN_IN_48H,NUT_TFE_IN_48H,NUT_TPN_IN_72H,NUT_NPO_IN_72H,NUT_PPN_IN_72H,NUT_TFE_IN_72H
                        from 
                        ds_icu_stays_1")



# 20445 of 168 variables

#first we converted the logical values to 1=TRUE / 0=FALSE

ds_icu_pm_1[,sapply(ds_icu_pm_1,is.logical)]<-ds_icu_pm_1[,sapply(ds_icu_pm_1,is.logical)]+0



#PAT_GENDER to 0,1 FEMALE->1, MALE->0

ds_icu_pm_1$pat_gender=ds_icu_pm_1$pat_gender=="F"
ds_icu_pm_1$pat_gender=ds_icu_pm_1$pat_gender+0


#delete column
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("pat_expire_hosp"))]


# asign to decades
ds_icu_pm_1$adt_hos_age<-cut(ds_icu_pm_1$adt_hos_age,breaks=seq(0,100,10),labels=seq(0,9))

#delete column
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("adt_readmit"))]

#ICU_DBSOURCE
ds_icu_pm_1$icu_dbsource<-as.factor(ds_icu_pm_1$icu_dbsource)

#ICU_TYPE_FSERVICE
ds_icu_pm_1$icu_type_fservice<-as.factor(ds_icu_pm_1$icu_type_fservice)

#IMPUTATION + REFACTOR
ds_icu_pm_1$icu_type_fservice[which(ds_icu_pm_1$icu_type_fservice=="-")]<-"MEDICAL"
ds_icu_pm_1$icu_type_fservice<-droplevels(ds_icu_pm_1$icu_type_fservice)

#delete column
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("med_has_allergie_insulin"))]

#ICD9P_M
#FACTOR
ds_icu_pm_1$icd9p_m<-as.factor(as.character(ds_icu_pm_1$icd9p_m))
#summary

#delete column
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("icd9p_m"))]

#ICD9D_M
#FACTOR
ds_icu_pm_1$icd9d_m<-as.factor(as.character(ds_icu_pm_1$icd9d_m))

#DIAG_M_HAS_DIABET / DIAG_S_HAS_DIABET
#combine columns OR
ds_icu_pm_1$diag_has_diabet<-(ds_icu_pm_1$diag_m_has_diabet | ds_icu_pm_1$diag_s_has_diabet) 
ds_icu_pm_1$diag_has_diabet<-ds_icu_pm_1$diag_has_diabet+0


#delete column
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("diag_s_has_diabet","diag_m_has_diabet"))]

#COM_
#nulos
df_com<-ds_icu_pm_1 %>% dplyr::select(starts_with("COM_"))

#near zero var
nzv<-nearZeroVar(df_com)
(czv<-names(df_com[nzv]))
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% czv)]
rm(df_com)

#SCORES

ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("score_apsiii"))]
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("score_lods"))]
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("score_mlods"))]
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("score_oasis"))]
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("score_qsofa"))]
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("score_saps"))]
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("score_sirs"))]
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("ph_avg_48h","po2_avg_48h","lactate_avg_48h","pco2_avg_48h","ph_avg_72h",
                                                           "po2_avg_72h","lactate_avg_72h","pco2_avg_72h","ph_std_48h","po2_std_48h","lactate_std_48h","pco2_std_48h","ph_std_72h",
                                                           "po2_std_72h","lactate_std_72h","pco2_std_72h"))]


#OUTLIERS WHITEBLOOD
ds_icu_pm_1$whiteblood_avg_24h[ds_icu_pm_1$whiteblood_avg_24h>200]<-NA
ds_icu_pm_1$whiteblood_avg_48h[ds_icu_pm_1$whiteblood_avg_48h>200]<-NA
ds_icu_pm_1$whiteblood_avg_48h[ds_icu_pm_1$whiteblood_avg_48h>200]<-NA


#STD
stdl<-names(ds_icu_pm_1[grep("_std_",names(ds_icu_pm_1))])

#DELETE STD
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% stdl)]

#GLUCOSE_VARIABILITY_ICU
ds_icu_pm_1$glucose_variability_icu[ds_icu_pm_1$glucose_variability_icu>500]
ds_icu_pm_1$glucose_variability_icu[ds_icu_pm_1$glucose_variability_24h>300]
ds_icu_pm_1$glucose_variability_icu[ds_icu_pm_1$glucose_variability_48h>300]
ds_icu_pm_1$glucose_variability_icu[ds_icu_pm_1$glucose_variability_72h>300]

#DELETE TIME_
#finally didnt include any TIME_
timel<-names(ds_icu_pm_1[grep("time_",names(ds_icu_pm_1))])
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% timel)]


#NUT_NPO
#DELETE
npol<-names(ds_icu_pm_1[grep("nut_npo",names(ds_icu_pm_1))])
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% npol)]

#NUT_TPN
tpnl<-names(ds_icu_pm_1[grep("nut_tpn",names(ds_icu_pm_1))])
ds_icu_pm_1[tpnl][is.na(ds_icu_pm_1[tpnl])]<-0
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% tpnl)]

#NUT_PPN
ppnl<-names(ds_icu_pm_1[grep("nut_ppn",names(ds_icu_pm_1))])
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% ppnl)]

#NUT_TFE
tfel<-names(ds_icu_pm_1[grep("nut_tfe",names(ds_icu_pm_1))])

#IMPUTATION
ds_icu_pm_1[tfel][is.na(ds_icu_pm_1[tfel])]<-0

#remove outliers
ds_icu_pm_1$hyper_gluce_percent_72h[ds_icu_pm_1$hyper_gluce_percent_72h<0]<-NA
ds_icu_pm_1$hyper_gluce_percent_72h[ds_icu_pm_1$hyper_gluce_percent_72h>100]<-NA

#remove outliers
ds_icu_pm_1$in_range_clinic_percent_72h[ds_icu_pm_1$in_range_clinic_percent_72h<0]<-NA
ds_icu_pm_1$in_range_clinic_percent_72h[ds_icu_pm_1$in_range_clinic_percent_72h>100]<-NA
ds_icu_pm_1$in_range_protocol_percent_72h[ds_icu_pm_1$in_range_protocol_percent_72h<0]<-NA
ds_icu_pm_1$in_range_protocol_percent_72h[ds_icu_pm_1$in_range_protocol_percent_72h>100]<-NA


#FINAL FORMAT DATASET
#only variables obtained for <72H
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% names(ds_icu_pm_1[grep("_icu",names(ds_icu_pm_1))]))]

#delete icu_los and adt_hstays
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("icu_los","adt_hstays"))]

#db_sources
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("icu_dbsource"))]

#problems with factor
ds_icu_pm_1$adt_hos_age<-as.numeric(ds_icu_pm_1$adt_hos_age)

#delete only score_sapii data
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("score_sapsii"))]

#auxiliar variable
ds_icu_pm_1$score_sofa_more4<-as.numeric(ds_icu_pm_1$score_sofa>=4)

#group icd9_m_chapter
dicd9<-(ds_icu_pm_1 %>% group_by(icd9d_m_chapter) %>% summarise(n=n()) %>% arrange(n))

#refactoring as others
dicd_others<-filter(dicd9,n<1000)
ds_icu_pm_1$icd9d_m_chapter<-as.character(ds_icu_pm_1$icd9d_m_chapter)
ds_icu_pm_1<-ds_icu_pm_1 %>% mutate(icd9d_m_chapter=ifelse(icd9d_m_chapter %in% dicd_others$icd9d_m_chapter,"00-Others",icd9d_m_chapter))
ds_icu_pm_1$icd9d_m_chapter<-as.factor(ds_icu_pm_1$icd9d_m_chapter)
ds_icu_pm_1<-ds_icu_pm_1[,-which(names(ds_icu_pm_1) %in% c("icd9d_m"))]


#imputation and normalize with caret
preproc_ds_range<-preProcess(ds_icu_pm_1,method = c("medianImpute","range"))
ds_icu_pm_1<-predict(preproc_ds_range,ds_icu_pm_1)


#factorize outcome
ds_icu_pm_1$icu_dead_before_28<-as.factor(ds_icu_pm_1$icu_dead_before_28)
levels(ds_icu_pm_1$icu_dead_before_28)<-make.names(levels(ds_icu_pm_1$icu_dead_before_28))


save(file="~/shared/ds_icu_pm_1",ds_icu_pm_1)




