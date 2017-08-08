require(h2o)
require(caret)
require(pROC)
require(mlr)

set.seed(1000)

#start cluster H2O

h2o.init(nthreads=-1, max_mem_size = "7G")


# LOAD DATASET

load(file = "~/shared/ds_icu_pm_2")


#small dataset to begin tests

small_sample<-createDataPartition(y=ds_icu_pm_2$icu_dead_before_28,p=.20,list = FALSE)
ds_small_sample<-ds_icu_pm_2[small_sample,]

#training and test datasets

small_train<-createDataPartition(y=ds_small_sample$icu_dead_before_28,p=.60,list = FALSE)
ds_small_train<-ds_small_sample[small_train,]
ds_small_test<-ds_small_sample[-small_train,]


full_train<-createDataPartition(y=ds_icu_pm_2$icu_dead_before_28,p=.60,list = FALSE)
ds_train<-ds_icu_pm_2[full_train,]
ds_test<-ds_icu_pm_2[-full_train,]

#variables

y<-"icu_dead_before_28"


#tasks

#small dataset
cltask_1 = makeClassifTask(id = "c1", data = ds_small_sample, target="icu_dead_before_28",positive="X1")

#full dataset
cltask_2 = makeClassifTask(id = "c2", data = ds_icu_pm_2, target = "icu_dead_before_28",positive="X1")



#feature selection
# masure auc
# ctrl

ctrl = makeFeatSelControlSequential(same.resampling.instance = TRUE,
                                    impute.val = NULL, method='sfbs', maxit = 10, max.features = NA_integer_
                                    , alpha = 0.001, beta=-0.001,tune.threshold = TRUE,tune.threshold.args = list(measure=list(auc)))



#glm learner
#default values

lrn<-makeLearner("classif.h2o.glm",predict.type = "prob")


# 10 cv resampling
rdesc = makeResampleDesc("CV", iters = 5)


#feature selection
sfeats = selectFeatures(learner = lrn, task = cltask_1, resampling = rdesc, control = ctrl,
                        show.info = TRUE)


save(sfeats,file = "~/shared/feats")
