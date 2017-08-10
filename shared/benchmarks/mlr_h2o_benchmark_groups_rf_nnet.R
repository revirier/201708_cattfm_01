require(dplyr)
require(mlr)
require(h2o)

#random seed

set.seed(1000)



#start h2o

h2o.init(nthreads=-1, max_mem_size = "14G")
#h2o.removeAll()
#h2o.shutdown(prompt = FALSE)


# LOAD DATASET

load(file = "~/shared/datasets/ds_icu_pm_2")


#variables

y<-"icu_dead_before_28"


#datasets


#---------------------------------------------------------------------
#more/less than 3 icu stays
ds_icu_stay_more72<-filter(ds_icu_pm_2,icu_stay_more72==1)
ds_icu_stay_less72<-filter(ds_icu_pm_2,icu_stay_more72==0)


#more/less than 4 sofa score
ds_icu_sofa_score_more4<-filter(ds_icu_pm_2,score_sofa_more4==1)
ds_icu_sofa_score_less4<-filter(ds_icu_pm_2,score_sofa_more4==0)


#yes/no diabetics
ds_icu_diabetes_yes<-filter(ds_icu_pm_2,diag_has_diabet==1)
ds_icu_diabetes_no<-filter(ds_icu_pm_2,diag_has_diabet==0)


#yes/no insuline IV
ds_icu_insulineiv_yes<-filter(ds_icu_pm_2,icu_has_admiv_insulin==1)
ds_icu_insulineiv_no<-filter(ds_icu_pm_2,icu_has_admiv_insulin==0)


#yes/no sepsis in
ds_icu_sepsis_yes<-filter(ds_icu_pm_2,med_has_sepsis==1)
ds_icu_sepsis_no<-filter(ds_icu_pm_2,med_has_sepsis==0)



#tasks

cltask1 = makeClassifTask(id = "icu_stay_more72", data = ds_icu_stay_more72, target = "icu_dead_before_28",positive="X1")
cltask2 = makeClassifTask(id = "icu_stay_less72", data = ds_icu_stay_less72, target = "icu_dead_before_28",positive="X1")
cltask3 = makeClassifTask(id = "icu_sofa_score_more4", data = ds_icu_sofa_score_more4, target = "icu_dead_before_28",positive="X1")
cltask4 = makeClassifTask(id = "icu_sofa_score_less4", data = ds_icu_sofa_score_less4, target = "icu_dead_before_28",positive="X1")
cltask5 = makeClassifTask(id = "icu_diabetes_yes", data = ds_icu_diabetes_yes, target = "icu_dead_before_28",positive="X1")
cltask6 = makeClassifTask(id = "icu_diabetes_no", data = ds_icu_diabetes_no, target = "icu_dead_before_28",positive="X1")
cltask7 = makeClassifTask(id = "icu_insulineiv_yes", data = ds_icu_insulineiv_yes, target = "icu_dead_before_28",positive="X1")
cltask8 = makeClassifTask(id = "icu_insulineiv_no", data = ds_icu_insulineiv_no, target = "icu_dead_before_28",positive="X1")
cltask9 = makeClassifTask(id = "icu_sepsis_yes", data = ds_icu_sepsis_yes, target = "icu_dead_before_28",positive="X1")
cltask10 = makeClassifTask(id = "icu_sepsis_no", data = ds_icu_sepsis_no, target = "icu_dead_before_28",positive="X1")
cltask11 = makeClassifTask(id = "total", data = ds_icu_pm_2, target = "icu_dead_before_28",positive="X1")

ltasks=list(cltask1,cltask2,cltask3,cltask4,cltask5,cltask6,cltask7,cltask8,cltask9,cltask10,cltask11)


#parameters

#dl
par_dl<-list()
par_dl$hidden = c(1000,1000)
par_dl$epochs =72
par_dl$rho =0.9615025
par_dl$epsilon =1.883177e-07
par_dl$l1=1e-6
par_dl$input_dropout_ratio = 0.1
par_dl$balance_classes = TRUE
par_dl$variable_importances = TRUE
par_dl$shuffle_training_data=TRUE
par_dl$fast_mode=TRUE

#rf
par_rf<-list()
par_rf$ntrees = 1763
par_rf$mtries = 14
par_rf$max_depth = 5
par_rf$balance_classes = TRUE

lrns<-list(
  makeLearner("classif.h2o.randomForest",predict.type = "prob",par.vals=par_rf),
  makeLearner("classif.h2o.deeplearning",predict.type = "prob",par.vals=par_dl)
)


rdesc <- makeResampleDesc("CV", iters = 10L)
meas = list(auc,mmce,timetrain,f1)


bmr = benchmark(lrns, ltasks , rdesc, meas, show.info = TRUE)



save(file="~/shared/results/bmr_rf_nnet",bmr)

