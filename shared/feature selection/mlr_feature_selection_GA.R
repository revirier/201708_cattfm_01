#require(h2o)
require(caret)
require(mlr)
require(doMC)

set.seed(1000)

#start cluster H2O

#h2o.init(nthreads=-1, max_mem_size = "7G")

#parallel
registerDoMC(cores=6)


# LOAD DATASET

load(file = "~/shared/datasets/ds_icu_pm_complete_no_fact_3")


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

ctrl = makeFeatSelControlGA(same.resampling.instance = TRUE,
                                    impute.val = NULL, 
                                    maxit = 300, 
                                    max.features = NA_integer_,
				    comma=FALSE,
				    mu=5L,
				    lambda=20L,
				    crossover.rate=0.5,
				    mutation.rate=0.05,
				    tune.threshold=TRUE,
			      tune.threshold.args = list(measure=list(auc)))
				    
#glm learner
lrn<-makeLearner("classif.binomial",predict.type = "prob")


# 3 cv resampling
rdesc = makeResampleDesc("CV", iters = 3)


#feature selection
sfeats = selectFeatures(learner = lrn, task = cltask_1, resampling = rdesc, control = ctrl,
                        show.info = TRUE)


save(sfeats,file = "~/shared/results/feat_selection_glm_GA_3_1")
