require(h2o)
require(caret)
require(pROC)
require(mlr)

require(doMC)

#Parallel

registerDoMC(cores=8)


set.seed(1000)

#start cluster H2O

#h2o.init(nthreads=-1, max_mem_size = "7G")


# LOAD DATASET

load(file = "~/shared/ds_icu_pm_2")

#standarize
preproc_ds_range<-preProcess(ds_icu_pm_2,method = c("range"))
ds_icu_pm_2<-predict(preproc_ds_range,ds_icu_pm_2)


#variables
# rename icu_dead to y

y<-"y"

ds_icu_pm_2$y<-ds_icu_pm_2$icu_dead_before_28
ds_icu_pm_2$icu_dead_before_28 <- NULL


#small dataset to begin tests

small_sample<-createDataPartition(y=ds_icu_pm_2$y,p=.20,list = FALSE)
ds_small_sample<-ds_icu_pm_2[small_sample,]

#training and test datasets

small_train<-createDataPartition(y=ds_small_sample$y,p=.60,list = FALSE)
ds_small_train<-ds_small_sample[small_train,]
ds_small_test<-ds_small_sample[-small_train,]


full_train<-createDataPartition(y=ds_icu_pm_2$y,p=.60,list = FALSE)
ds_train<-ds_icu_pm_2[full_train,]
ds_test<-ds_icu_pm_2[-full_train,]



#tasks

#small dataset
cltask_1 = makeClassifTask(id = "c1", data = ds_small_sample, target="y",positive="X1")

#full dataset
cltask_2 = makeClassifTask(id = "c2", data = ds_icu_pm_2, target = "y",positive="X1")



#feature selection GA
# masure auc
# ctrl

ctrl = makeFeatSelControlGA(same.resampling.instance = TRUE, impute.val = NULL,
                            maxit = 500, max.features = NA_integer_, comma = FALSE,
                            mu = 50L, lambda=5, crossover.rate = 0.5, mutation.rate = 0.05,
                            tune.threshold = TRUE,tune.threshold.args = list(measure=list(auc)))



#glm learner
#default values

lrn<-makeLearner("classif.glm",predict.type = "prob")


# 10 cv resampling
rdesc = makeResampleDesc("CV", iters = 5)


#feature selection
sfeats_ga = selectFeatures(learner = lrn, task = cltask_1, resampling = rdesc, control = ctrl,
                        show.info = TRUE)


save(sfeats_ga,file = "~/shared/feats_ga")
