#require(h2o)
require(caret)
require(pROC)
require(mlr)
require(doMC)

#Parallel

registerDoMC(cores=8)

set.seed(1000)

#start cluster H2O

#h2o.init(nthreads=-1, max_mem_size = "16G")


# LOAD DATASET

load(file = "~/shared/datasets/ds_icu_pm_complete__no_fact_2")

#problem with sofa score 22, there are only one value
ds_icu_pm_2$score_sofa[ds_icu_pm_2$score_sofa==22]<-21

#variables

y<-"icu_dead_before_28"


#tasks


#full dataset
cltask_2 = makeClassifTask(id = "c2", data = ds_icu_pm_2, target = "icu_dead_before_28",positive="X1")


#feature selection
# masure auc
# ctrl

ctrl = makeFeatSelControlSequential(same.resampling.instance = TRUE,
                                    impute.val = NULL, method='sfbs', 
                                    maxit = 100, max.features = NA_integer_,
                                    alpha = 0.01, 
                                    beta=-0.001
                                    #,
                                    #tune.threshold = TRUE,
                                    #tune.threshold.args = list(measure=list(auc))
                                    )


#glm learner
#default values

lrn<-makeLearner("classif.binomial",predict.type = "prob")


# cv resampling
rdesc = makeResampleDesc("Holdout",stratify = TRUE)


#feature selection
sfeats = selectFeatures(learner = lrn, task = cltask_2, resampling = rdesc, control = ctrl,
                        show.info = TRUE)


save(sfeats,file = "~/results/feat_selection_glm_2")
