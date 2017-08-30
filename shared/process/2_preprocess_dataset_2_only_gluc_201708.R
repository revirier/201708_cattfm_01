---
# preprocess dataframe before analytics
# not normalize, to normalize with the h2o model
  
# 20170807
# load from dataset 2
  
# only vars related to glucose dataset

---
  
#libraries
  
  
require("caret")
require("dplyr")


load(file="~/shared/datasets/ds_icu_raw_2")

ds_icu_pm_2<-select (ds_icu_pm_2,matches("^glucose|^in_range|^time|^hipo|^hyper|^icu_dead"))


# 20445 of 40 variables

#first we converted the logical values to 1=TRUE / 0=FALSE

ds_icu_pm_2[,sapply(ds_icu_pm_2,is.logical)]<-ds_icu_pm_2[,sapply(ds_icu_pm_2,is.logical)]+0


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

save(file="~/shared/datasets/ds_icu_pm_only_glucose_2",ds_icu_pm_2)


