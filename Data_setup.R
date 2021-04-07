#### Step1 : Data screening, preprocessing


## Read data. 


#Read data.
#source("d:/r_program/function.r")

source("https://raw.githubusercontent.com/ahns0070/RR/edit.functionR/function.R")

library(mi)

#setwd("e:/stat/prediction_yang")
setwd("/Users/siha/Documents/ML")
data0 <- read.csv("newdata.csv", na = c("", NA, '.'))
dim(data0)

data0$delirium2 = factor(ifelse(data0$delirium == 1, "yes", "no"))

with(data0, plot(wbc_max, wbc_min))
with(data0, cor.test(wbc_max, wbc_min))
with(data0, boxplot(wbc_max~ delirium ))
with(data0, boxplot(wbc_min~ delirium ))


#Variables name list up

dim(data0)

contiVars1 <- c(
  "age","bmi","sofa_total","apache2","GCS", #Demographics
  "systolic_blood_pressure","diastolic_blood_pressure","map","heart_rate", #At Admision
  "respiratory_rate","body_temperature","saturation",
  "wbc_max","wbc_min","hemoglobin","hematocrit","platelet","total_bilirubin","ast",
  "alt","albumin","bun","creatinine","sodium","potassium","chloride",
  "CPK","LDH",
  "c_reactive_protein",
  "procalcitonin",  "glu_max","glu_min",
  "inr","troponin_i","nt_probnp",
  "hba1c","ph",
  "paco2","pao2","hco3",
  "sao2","Lactic",
  "icu_length_of_stay" #Outcomes
)

conti.cov <- c(
  "age","bmi",
  # "sofa_total","apache2", 
  #"GCS", #Demographics
  "systolic_blood_pressure","diastolic_blood_pressure","map","heart_rate", #At Admision
  "respiratory_rate","body_temperature","saturation",
  "wbc_max","wbc_min","hemoglobin","hematocrit","platelet","total_bilirubin","ast",
  "alt","albumin","bun","creatinine","sodium","potassium","chloride",
  #"CPK","LDH",
  "c_reactive_protein",
  #"procalcitonin",  "glu_max","glu_min",
  "inr","troponin_i"
  # "nt_probnp",   "hba1c","ph",   "paco2","pao2","hco3",   "sao2","Lactic",
  # "icu_length_of_stay" #Outcomes
)


factorVars1 <- c(
  "sex",
  "location_before_icu",
  "urgent","icu_admission_status",#Demographics
  "cardiac_arrest_before_icu",
  "htn","dm",
  "smoking", #smoking
  "stroke", #Comorbidities
  "hemorrhage","dementia","psychotic",
  "tumor","CND","CNS_ds_y_n","chronic_lung_dz",
  "renal_impairment","liver_dz",
  "solid_cancer","hematologic_malig",
  "prev_coronary_dz","prev_pci",
  "prev_cabg","heart_failure","prev_H_transplantation",
  "diagnosis", #Reasons
  "rhythm","vasoactive_drug","IABP","ECMO","MCS", "SHOCK","oxygen_therapy_type", #At Admision
  "icu_survival","hosp_survival","icu_readmission","hosp_readmission","delirium", "delirium2"  #Outcomes
)


factor.cov <- c(
  "sex",
  #"location_before_icu",
  "urgent","icu_admission_status",#Demographics
  "cardiac_arrest_before_icu",
  "htn","dm",
  #"smoking","stroke", #Comorbidities
  "hemorrhage","dementia","psychotic",
  "tumor","CND","CNS_ds_y_n","chronic_lung_dz",
  "renal_impairment","liver_dz",
  "solid_cancer",
  #"hematologic_malig",
  "prev_coronary_dz","prev_pci",
  "prev_cabg","heart_failure","prev_H_transplantation",
  "diagnosis", #Reasons
  "rhythm","vasoactive_drug","IABP","ECMO","MCS", "SHOCK","oxygen_therapy_type" #At Admision
)



data1 <- data0[!is.na(data0$rhythm), c(contiVars1,factorVars1)]
str(data1)



#Creating dummy variables 

#find multi-category variable.
multi.vars <- factorVars1[sapply(data1[factorVars1], function(x) length(levels(factor(x)))>2)]



#multi.vars <- c("icu_admission_status", "diagnosis", "rhythm", "oxygen_therapy_type", "renal_impairment")
data1[multi.vars]  = lapply(data1[multi.vars], factor)
head(data1)

fff <- as.formula(paste("~",paste((paste(multi.vars, collapse = "+")) )))

dummies <- dummyVars(fff , data = data1)

data1$delirium2
data1.temp <- predict(dummies, newdata = data1)

head(data1.temp)
names(data1.temp)

dim(data1.temp)

library(gdata)
is.what(data1.temp)

names(as.data.frame(data1.temp))

data1 <- data.frame(data1, data.frame(data1.temp))
names(data1) <- sub("\\.", replacement ="",  names(data1)) #delete "." in variable name.
names(data1)
names(data1)

multi.vars
#renewing categorical variable list up.
factor.cov <- c(
  "sex",
  #"location_before_icu",
  "urgent","icu_admission_status2","icu_admission_status3",#Demographics
  "cardiac_arrest_before_icu",
  "htn","dm",
  #"smoking","stroke", #Comorbidities
  "hemorrhage","dementia","psychotic",
  "tumor","CND","CNS_ds_y_n","chronic_lung_dz",
  "renal_impairment1","renal_impairment2","liver_dz",
  "solid_cancer",
  #"hematologic_malig",
  "prev_coronary_dz","prev_pci",
  "prev_cabg","heart_failure","prev_H_transplantation",
  "diagnosis2","diagnosis3", #Reasons
  "vasoactive_drug","IABP","ECMO","MCS", "SHOCK",
  "oxygen_therapy_type1",  "oxygen_therapy_type2", "rhythm" #At Admision
)

data2 <- data1[, c(conti.cov, factor.cov, "delirium", "delirium2")]
head(data2)
sum(is.na(data0$rhythm))

dim(data0)
dim(data1)
dim(data2)



##################################################################################
#### step2 : Split data to training and test set. 

#data splitting
#partitioning dataset to training and Test dataset. 

set.seed(3456)
index = createDataPartition(data1$delirium, p = .7, list = F, times = 30)







#delirium #single split data
training <-  data.frame(data2[index[,1],] )
test <-  data.frame(data2[-index[,1],])


names(training)

delinum = grep("delirium", names(training))
delinum 


set.seed(99)
preProcValues <- preProcess(training[,-delinum], method = c("center", "scale", "YeoJohnson", "knnImpute"))
preProcValues
summary(preProcValues)

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)





# categorical variables is factor status 

training2 = training
training2[factor.cov] = lapply(training[factor.cov], factor)


set.seed(99)
preProcValues2 <- preProcess(training2[-delinum], method = c("center", "scale", "YeoJohnson"))
preProcValues2
summary(preProcValues2)

basic(preProcValues$yj)

trainTransformed2 <- predict(preProcValues2, training)
testTransformed2 <- predict(preProcValues2, test)
