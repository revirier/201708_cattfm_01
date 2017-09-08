require(h2o)
require(caret)
require(mlr)

set.seed(1000)

#start cluster H2O

h2o.init(nthreads=-1, max_mem_size = "7G")


# LOAD DATASET

load(file = "~/shared/datasets/ds_icu_pm_complete__no_fact_2")


full_train<-createDataPartition(y=ds_icu_pm_2$icu_dead_before_28,p=.60,list = FALSE)
ds_train<-ds_icu_pm_2[full_train,]
ds_test<-ds_icu_pm_2[-full_train,]

#variables

y<-"icu_dead_before_28"

#tasks

#full dataset
cltask_1 = makeClassifTask(id = "c2", data = ds_icu_pm_2, target = "icu_dead_before_28",positive="X1")



#feature selection
# masure auc
# ctrl

ctrl = makeFeatSelControlSequential(same.resampling.instance = TRUE,
                                    impute.val = NULL, 
                                    method='sfbs', 
                                    maxit = 500, 
                                    max.features = NA_integer_,
                                    alpha = 0.001, 
                                    beta=-0.0005)

#glm learner
#default values

lrn<-makeLearner("classif.h2o.glm",predict.type = "prob")


# 3 cv resampling
rdesc = makeResampleDesc("CV", iters = 3)


#feature selection
sfeats = selectFeatures(learner = lrn, task = cltask_1, resampling = rdesc, control = ctrl,
                        show.info = TRUE)


save(sfeats,file = "~/shared/results/feat_selection_glm_6")
