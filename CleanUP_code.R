
#### Step1 : Data screening, preprocessing

## Read data. 


#Read data.
#source("d:/r_program/function.r")

source("https://raw.githubusercontent.com/ahns0070/RR/main/function.R")




library(mi)

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
  # "sofa_total","apache2", #Ç¥???? ?Ö½? ???? ?? sofa_total, apache2??????Ú¾? ???? ???Ú¾î°¡ ???î°¡?? ?Ç¾? ??Á¦ ?????? ?? ?????? ?? ???? Á¦????À¸?? ?Ï°?,
  #"GCS", #Demographics ????,?GCS?? ??Á¦ ?Ó»ó¿¡¼? Á¡???? ?????Ï´? ???? ?î¶°?? ?é¿¡???? ?Ö°????? ?Îº??? ?Ö¾? ???????? Á¦???Ï¿?À¸?? ?Õ´Ï´?.
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
  "smoking", #smoking?? P value ?? ???? ?Ê°? ?Ó»??? ?Ù°Å°? ???? Á¦????À¸?? ?Ï°?,
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


#missing value?? ?????? covariate?? Á¦???? ?????Í¼irium2")]
head(data2)
sum(is.na(data0$rhythm))

dim(data0)
dim(data1)
dim(data2)




#???????? ?? ?? ?Ù¸? ?Úµå¿¡ ?Ö´???À» ?Å°Ü¾? ??. 



## ???torThe descriptive statistics.data=data1, factorVars=factorVars1, includeNA = T)contiVars1, 
print(ct1, noSpac = s=F, , 
                      d=F, showAllLevels = T, quote = T, oF)

ll = T)
#innoSpaces=Fages('psych')
psych::describe(data1[contiVars1],quareateTableOne(vars=c(factor.cov), data=training, factorVars=factorVars1, includeNA = T)
print(ct1, noSpaces=F, smd=F, showAllLevels = T, quote = T, oveall = T)
#install.packages('psych')
psych::describe(training[conti.cov],quant=c(.25,.75) ) 


ct1 <- CreateTableOne(vars=c(factor.cov), data=trainTransformed, factorVars=factorVars1, includeNA = T)
print(ct1, noSpaces=F, smd=F, showAllLevels = T, quote = T, oveall = T)
#install.packages('psych')
getOption("max.print")
psych::describe(trainTransformed[c(conti.cov)],quant=c(.25,.75) ) 



ct1 <- CreateTableOne(vars=c(factorVars1), data=trainTransformed2, factorVars=factorVars1, includeNA = T)
print(ct1, noSpaces=F, smd=F, showAllLevels = T, quote = T, oveall = T)
#install.packages('psych')
psych::describe(trainTransformed2[conti.cov],
quant=c(.25,.75) ) 

getOption("max.print")



###simple logistic regression 

#continuous covariate
cov1 <- c(factor.cov, conti.cov)
trainTransformed$delirium <- trainTransformed2$delirium

trainTransformed$age

hist(trainTransformed$age)


list1 <- lapply(trainTransformed[conti.cov],  function(x) glm(delirium ~ x , data = trainTransformed, family = binomial) )
list2 <- lapply(list1, summary)

kk <- cbind(
  t(sapply(list2, function(x) x$coef[-1,])),
  (sapply(list2, function(x) exp(x$coef[-1,1]))),
  t(sapply(list1, function(x) exp(confint(x)[-1,]))))
colnames(kk ) <- c("Estimate", "Std.Error", "Z-value", "P-value", "OR", "lower.limit", "upper.limit")
kk

clear_clip()

#categorical covariate
#caution : multi-level categorical variable
list1 <- lapply(data1[factor.cov],  function(x) glm(delirium  ~ x , data = data1, family = binomial) )
list2 <- lapply(list1, summary)

jj <- cbind(
  oo <- do.call('rbind', lapply(list2, function(x) x$coef[-1,])),
  exp(oo[,1]),
  oo2 <- do.call('rbind', lapply(list1, function(x) exp(confint(x)[-1,])))
)
rep(factorVars1, unlist(lapply(data1[factorVars1], nlevels))-1)
rownames(jj) <- paste(rep(factorVars1, unlist(lapply(data1[factorVars1], nlevels))-1) , unlist(lapply(data1[factorVars1], function(x) levels(x)[-1] )), sep = "")
colnames(jj) <- c("Estimate", "Std.Error", "Z-value", "P-value", "OR", "lower.limit", "upper.limit")
jj



##################################################################################
#### step2 : Split data to training and test set. 

#data splitting
#partitioning dataset to training and Test dataset. 

set.seed(3456)
index = createDataPartition(data1$delirium, p = .7, list = F, times = 30)


#delirium ???? ?ß°?. #single split data
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

training2$delirium

trainTransformed$delirium

############################################################################
####step3 : Feature selection using machine learning method on training set
#see caret2.r

####step4 : Fit the logistic model on training set (repeated K fold CV method for testing model on hold-out sample)


#orange model. 





###########################################################################
## comparison AUC between models. 


formula0 <- formula(delirium2 ~ albumin + inr + hemoglobin + c_reactive_protein + bun
+ log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 + heart_rate + creatinine + platelet + ast
+ SHOCK + age + sex + vasoactive_drug + dementia + cardiac_arrest_before_icu + CNS_ds_y_n)

training$delirium2




#Addtional Feature selection using RF- ok

rf.ctrl <- trainControl ( method = "cv"
                          , number = 10
                          , savePredictions = T
                          , classProbs = T
                          , summaryFunction = twoClassSummary
                          )

#Random Forest

training$log_wbc_max <- log(training$wbc_max)

set.seed(1231)
rf.select.model <- train(form = formula0, data =r
interact0.f <- as.formula(delirium2 ~SHOCK*(albumin + inr + hemoglobin 
                                      + c_reactive_protein + 
  bun + log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 + 
  heart_rate + creatinine + platelet + ast + age + 
    sex + vasoactive_drug + dementia + cardiac_arrest_before_icu + 
  CNS_ds_y_n))

glm(interact0.f, data = training, family = binomial)
  


getModelInfo(model="rf")            , metric = "ROCset.seed(1231)
rf.select.model <- train(form = interact0.f, data = training,
                         meth         ,    , trControl = rf.ctrl
              ,enter", "scale")
  , 
                         .time <- proc.time()
end.time - s
                         #,tuneGrid = list(mtry = seq(2,10,2))
                         serf.select.model
summary(rf.select.model)
rff <- rf.select.model 


lect.model))


## preproend.time - start.timeo
el.nonscaled <in(form = formula0, data = trai;ning
                                   , metric = "ROC"
                                   , trControl = rf.ctrl
)
rf.select.model.nonscaled
getTrainPerf(rf.select.model.nonscaled)
varImp(rf.select.model.nonscaled)
plot(varImp(rf.select.model.nonscaled))


#linear discriminant analysis - ok
set.seed(1231)
lda.select.model <- train(form = formula0, data = training, method = "lda"
                         , metric = "ROC"
                         , trControl = rf.ctrl
                         , preProcess = c("center", "scale")
)


lda.select.model
getTrainPerf(lda.select.model)
varImp(lda.select.model)
plot(varImp(lda.select.model))

defaultSummary(lda.select.model)





#?Ì°Åº??? ?Ã°??? ?À·? ?É¸???. ok

set.seed(1231)
adaboost.select.modeln#It takes quite long time.
st <- proc.time()
set.seed(1231)
adaboost.select.model <- train(form = formula0, data = training, method = "adaboost"
                         , metric = "ROC"
                         , trControl = rf.ctrl
                         , preProcess = c("center", "scale")
)
endt <- proc.time()
endt - st


lect.model)
plot(varImp(adaboost.select.model))


#?Ì°? ?Ð¼? ????

gamboost.select.model <- train(form = formula0running faileding, method = "gamboost"
                               , metric = "ROC"
                               , trControl = rf.ctrl
                               , preProcess = c("center", "scale")
)


#?Ì°? ?Ã°??? ?î¸¶?Ï°? ?É¸??Âµ? 
deepboost.ctrl <- trainControl ( method = "
vfailed.number = 10
                          , savePredictions = T
                          #, classProbs = T
                          #, summaryFunction = twoClassSummary
                          )

set.seed(1231)
deepboost.select.model <- train(form = formula0, data = training, method = "deepboost"
                               #, metric = "ROC"
                               , trControl = deepboost.ctrl
                               , preProcess = c("center", "scale")
                               )
#Error in evalSummaryFunction(y, wts = weights, ctrl = trControl, lev = classLevels,  : 
#train()'s use of ROC codes requires class probabilities. See the classProbs option of trainControl()
#?ß°?Á¤??: ?????Þ½???(??): 
#In train.default(x, y, weights = w, ...) :
#  Class probabilities were requested for a model that does not implement them
  



glmStepAIC

#stepwise selection
set.seed(1231)
glmStepAIC.select.model <- train(form = formula0, data = training, method = "glmStepAIC"
                                , metric = "ROC"
                                , trControl = rf.ctrl
                                , preProcess = c("center", "scale")
)
glmStepAIC.select.model
getTrainPerf(glmStepAIC.select.model)
varImp(glmStepAIC.select.model)
plot(varImp(glmStepAIC.select.model))




#?Æ·????? ?????? ?Èµ?
set.seed(1231)
glmnet_h20.select.model <- train(form = formula0, data = training, method = "glmnet_h20"
                                 , metric = "ROC"
                                 , trControl = rf.ctrl
                                 , preProcess = c("center", "scale")
)
glmnet_h20.select.model
getTrainPerf(glmnet_h20.select.model)
varImp(glmnet_h20.select.model)
plot(varImp(glmnet_h20.select.model))



#?Ã°??? Á¶?? ?É¸?, ok ?×·??? ???Ãµ? ?????????? ???Ý±????Ï°??? ???? ???Ì³?. 
set.seed(1231)
C5.0.select.model <- train(form = formula0, data = training, method = "C5.0"
                                 , metric = "ROC"
                                 , trControl = rf.ctrl
                                 , preProcess = c("center", "scale")
)
C5.0.select.model
getTrainPerf(C5.0.select.model)
varImp(C5.0.select.model)
plot(varImp(C5.0.select.model))


#install.packages('adabag') - success
#AdaBag - success
set.seed(1231)
AdaBag.select.model <- train(form = formula0, data = training, method = "AdaBag"
                           , metric = "ROC"
                           , trControl = rf.ctrl
                           , preProcess = c("center", "scale")
)
AdaBag.select.model
getTrainPerf(AdaBag.select.model)
varImp(AdaBag.select.model)
plot(varImp(AdaBag.select.model))


##bagging - success
set.seed(1231)
bag.select.model <- train(form = formula0, data = training, method = "treebag"
                          ,trControl = rf.ctrl
                          , preProcess = c("center", "scale")
                          , metric = "ROC")

warnings()
bag.select.model
getTrainPerf(bag.select.model)
varImp(bag.select.model)
plot(varImp(bag.select.model))








## stepwise selection 






formula1 <- formula(delirium2 ~ albumin + bun + oxygen_therapy_type1 + oxygen_therapy_type2 + 
  heart_rate + platelet + ast + wbc_min + age + sex + dementia + 
  CNS_ds_y_n+ SHOCK)

formula2 <- formula(delirium2 ~ oxygen_therapy_type1 + oxygen_therapy_type2 + age + 
                      CNS_ds_y_n + albumin)

formula3 <- formula(delirium2 ~ oxygen_therapy_type1 + oxygen_therapy_type2 + inr + 
                      albumin + age)

formula4 <- formula(delirium ~ albumin + oxygen_therapy_type1 + oxygen_therapy_type2 + 
                      c_reactive_protein + log_wbc_max + platelet + heart_rate + age + 
                       + ast + CNS_ds_y_n + SHOCK)

##
#albumin	100
#log_wbc_max	91.54
#inr	90.74
#c_reactive_protein	83.5
#bun	81.88
#oxygen_therapy_type1	79.51
#platelet	79.31
#hematocrit	77.3
#creatinine	74.7
#hemoglobin	73.4
#heart_rate	72.22
#age	70.66
#ast	66.54
#CNS_ds_y_n	16.74
#SHOCK	10.93
#oxygen_therapy_type2	8.76
#cardiac_arrest_before_icu	7.06
#sex	6.78
#vasoactive_drug	4.99
#dementia	0
#
  
formula0 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                       +oxygen_therapy_type1 +heart_rate +creatinine +platelet +ast
                       +SHOCK +age +vasoactive_drug +CNS_ds_y_n +cardiac_arrest_before_icu +oxygen_therapy_type2 +sex +dementia)


formula5 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                       +oxygen_therapy_type1 +heart_rate +creatinine +platelet +ast
                       +SHOCK +age +vasoactive_drug +CNS_ds_y_n +cardiac_arrest_before_icu +oxygen_therapy_type2)

formula6 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                       +oxygen_therapy_type1 +heart_rate +creatinine +platelet +ast
                       +SHOCK +age +vasoactive_drug +CNS_ds_y_n  +oxygen_therapy_type2)

formula7 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                       +oxygen_therapy_type1 +heart_rate +creatinine +platelet +ast
                       +SHOCK +age +vasoactive_drug +oxygen_therapy_type2 )

formula8 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                       +oxygen_therapy_type1 +heart_rate +creatinine +platelet +ast
                       +SHOCK +age +oxygen_therapy_type2)

formula9 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                       +oxygen_therapy_type1 +heart_rate +creatinine +platelet +ast
                       +SHOCK +oxygen_therapy_type2)


library(rms)
lrm(formula0, data = training)
lrm(formula1, data = training)
lrm(formula2, data = training)
lrm(formula5, data = training)
lrm(formula6, data = training)
lrm(formula7, data = training)
lrm(formula8, data = training)
lrm(formula9, data = training)


formula10 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                        +oxygen_therapy_type1 +heart_rate +creatinine +platelet +ast
                        +SHOCK +oxygen_therapy_type2 )

test$log_wbc_max <- log(test$wbc_max)
lrm(formula10, data = test)




library(pROC)
cv.f <- function(formula1, number = 10, data = data1, seed1 = 1, add=FALSE){
  
  dd = data;
  set.seed(seed1);
  cv10 <- createFolds(y = dd$delirium2, k = number , returnTrain = T)
  str(cv10)
  
  list1 <- vector('list', number)
  for ( i in 1:number) list1[[i]] <- dd[cv10[[i]],]
  
  test.list <- vector('list', number)
  for ( i in 1:number) test.list[[i]] <- dd[-cv10[[i]],]
  
  glm.test.list <- lapply(test.list, function(x) glm(formula1, data = x, family = binomial))
  cv.prob.list <- mapply(function(x,y) predict(x, newdata = y, type = "response"), glm.test.list, test.list, SIMPLIFY =F )
  
  cv.id <- names(unlist(cv.prob.list))
  testset.cv <- dd[cv.id,]
  testset.cv$cv_prob <- unlist(cv.prob.list)
  
  #test discriminant power -  10-fold CV aggregation. 
  testset.cv$delirium2
  roc1 <- with(testset.cv, roc(response = delirium2, predictor =  testset.cv$cv_prob
                               , ci = T, plot = T))
  
  roc1 <- roc(delirium2~ testset.cv$cv_prob
              , data = testset.cv
              , ci = T, plot = T)
  Agg.roc1 <- roc1
  
  #test discriminant power -  in each fold as 10-fold CV . 
  roc1 <- roc(delirium2~ testset.cv$cv_prob
              , data = testset.cv
              , ci = T, plot = T)
  auc.list <- mapply(function(x,y) roc(response = y$delirium, predictor = x, ci=T)  , cv.prob.list, test.list, SIMPLIFY = F)
  auc.mat <- t(sapply(auc.list, function(x) x$ci[c(2,1,3)]))
  rownames(auc.mat) <- paste("Fold", 1:number, sep ="")
  colnames(auc.mat) <- c("Auc", "Lower.limit", "Upper.limit")
  
  #test calibration
  calCurve <- calibration(delirium2~ testset.cv$cv_prob
                          , data = testset.cv, class = "yes")
  cal.plot <- xyplot(calCurve, auto.key = list(columns = 2), rwd = 2, add=add)
  
  structure(list(training.list = list1, testset.list = test.list, Fooled.testset = testset.cv, AUC = Agg.roc1, 
                 eachAUC = auc.mat, calCurve = calCurve , calibration.plot = cal.plot))
}
cv.f

k  = 100
cv.perf.logistic0.list<- vector('list', k)
cv.perf.logistic1.list<- vector('list', k)
cv.perf.logistic2.list<- vector('list', k)
cv.perf.logistic3.list<- vector('list', k)
cv.perf.logistic4.list<- vector('list', k)
cv.perf.logistic5.list<- vector('list', k)
cv.perf.logistic6.list<- vector('list', k)
cv.perf.logistic7.list<- vector('list', k)
cv.perf.logistic8.list<- vector('list', k)
cv.perf.logistic9.list<- vector('list', k)
cv.perf.logistic10.list<- vector('list', k)

set.seed(1222310)
seed.list <- as.integer(runif(k)*1000)
training$log_wbc_max <- log(training$wbc_max)

for(i in 1:k) cv.perf.logistic0.list[[i]] <- cv.f(formula0 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic1.list[[i]] <- cv.f(formula1 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic2.list[[i]] <- cv.f(formula2 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic3.list[[i]] <- cv.f(formula3 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic4.list[[i]] <- cv.f(formula4 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic5.list[[i]] <- cv.f(formula5 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic6.list[[i]] <- cv.f(formula6 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic7.list[[i]] <- cv.f(formula7 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic8.list[[i]] <- cv.f(formula8 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic9.list[[i]] <- cv.f(formula9 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic10.list[[i]] <- cv.f(formula10 , seed = seed.list[i], number = 10, data = training, add=T)



auc.result.f <- function(xx, numberK){
  auc.results <- t(sapply(xx, function(x) x$AUC$ci[c(2,1,3)]))
  colnames(auc.results) = c("AUC", "Lower.limit", "Upper.limit")
  rownames(auc.results) = paste("Repeat", 1:numberK, sep = "")
  ave.auc = apply(auc.results, 2, mean)
  sd.auc = apply(auc.results, 2, sd)
  structure(list(each.fold.auc = auc.results, ave.auc = ave.auc, sd.auc = sd.auc))
}

str(cv.perf.logistic1.list)

auc.result0 <- auc.result.f(cv.perf.logistic0.list, numberK = k)
auc.result1 <- auc.result.f(cv.perf.logistic1.list, numberK = k)
auc.result2 <- auc.result.f(cv.perf.logistic2.list, numberK = k)
auc.result3 <- auc.result.f(cv.perf.logistic3.list, numberK = k)
auc.result4 <- auc.result.f(cv.perf.logistic4.list, numberK = k)
auc.result5 <- auc.result.f(cv.perf.logistic5.list, numberK = k)
auc.result6 <- auc.result.f(cv.perf.logistic6.list, numberK = k)
auc.result7 <- auc.result.f(cv.perf.logistic7.list, numberK = k)
auc.result8 <- auc.result.f(cv.perf.logistic8.list, numberK = k)
auc.result9 <- auc.result.f(cv.perf.logistic9.list, numberK = k)
auc.result10 <- auc.result.f(cv.perf.logistic10.list, numberK = k)


auc.result0
auc.result1 
auc.result2
auc.result3 
auc.result4 

auc.result5 
auc.result6 
auc.result7 
auc.result8 
auc.result9 
auc.result10

formula0
formula6
formula8
formula10

plot(cv.perf.logistic1.list[[1]]$AUC)
plot(cv.perf.logistic1.list[[1]]$AUC)
plot(cv.perf.logistic2.list[[1]]$AUC, add = T, col = "lightgrey")
plot(cv.perf.logistic3.list[[1]]$AUC, add = T, col = "lightblue")
plot(cv.perf.logistic4.list[[1]]$AUC, add = T, col = "red")
plot(cv.perf.logistic5.list[[1]]$AUC, add = T, col = "orange")

par(mfrow = c(1,1))
plot(cv.perf.logistic0.list[[1]]$AUC, col = "red")
plot(cv.perf.logistic6.list[[1]]$AUC, add = T, col = "blue")
plot(cv.perf.logistic8.list[[1]]$AUC, add = T, col = "darkgrey")
plot(cv.perf.logistic9.list[[1]]$AUC, add = T, col = "orange")




formula5
formula6
formula7
formula8
formula9

repeat.auc.f <- function(xx) {
  kk.auc2 = lapply(xx, function(x) x$eachAUC)
  do.call('rbind', kk.auc2)
  names(kk.auc2) = paste("Repeat", 1:numberK)
  kk.auc2
}

numberK = 100
repeat.auc.f(cv.perf.logistic0.list)
repeat.auc.f(cv.perf.logistic6.list)
repeat.auc.f(cv.perf.logistic8.list)
repeat.auc.f(cv.perf.logistic9.list)



#repeated k fold CV AUC plot
library(lava)
plot(cv.perf.logistic0.list[[1]]$AUC, col = "red")
for(i in 2:numberK) plot(cv.perf.logistic0.list[[i]]$AUC, add=T, col = Col("red", 0.1))
for(i in 1:numberK) plot(cv.perf.logistic6.list[[i]]$AUC, add=T, col = Col("blue", 0.2))
for(i in 1:numberK) plot(cv.perf.logistic8.list[[i]]$AUC, add=T, col = Col("grey", 0.2))
for(i in 1:numberK) plot(cv.perf.logistic9.list[[i]]$AUC, add=T, col = Col("orange", 0.2))

formula0
formula6
formula8
formula9


####step5:  Scoring model construction on entire training set

#spline curve 


#Albumin

library(lava)
hist(training$albumin)
rcspline.plot(x = training$albumin, y = training$delirium, model = "logistic", statloc="none")
par(new=T)
with(training, hist(training$albumin, xlim = c(1.5,5), ylim = c(0, 1000), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))


#INR
basic(training$inr)
hist(log(training$inr))
par(mfrow = c(1,1))
rcspline.plot(x = log(training$inr), y = training$delirium, model = "logistic", statloc="none", xlab = "log(inr)")
abline(v=0.5, lty = 2, col = "lightgrey")
abline(v=0.2, lty = 2, col = "lightgrey")


with(training, boxplot(log(inr) ~ delirium))

hist(training$inr)
hist(log(log(training$inr)+1))
basic(log(log(training$inr)+1))



par(mfrow = c(1,2))
with(training, boxplot(log(log(training$inr)+1) ~ delirium)) #log?? ?Î¹? ??È­?Ï³? ?Ñ¹? ??È­?Ï³? ????Àº ?????Øº???.
with(training, boxplot(log(training$inr) ~ delirium))


abline(h = 0.2, col = 2)
abline(h = 0.5, col = 2)


basic(log(training$inr[training$delirium ==0]))
training$inr1g <- ifelse(training$inr>1.2, 1, 0)

log(1.22)



inr.f <- function(logvalue){
  training$inr_g2 <- ifelse(training$inr > exp(logvalue), 1, 0)
  with(training, confusionMatrix(data=factor(inr_g2), reference = factor(delirium) ))
  
}

inr.f(0.1)
inr.f(0.2)
inr.f(0.3)
inr.f(0.4)
inr.f(0.5)

#inr ?? ?Ò¶? ???? kappa?? ????. exp(0.2)?? Â¥??
training$inr_g2 <- ifelse(training$inr > exp(0.2), 1, 0)
with(training, confusionMatrix(factor(inr_g2), reference = factor(delirium) ))



#hemoglobin
library(lava)
win.graph()
hist(training$hemoglobin)
par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = training$hemoglobin, y = training$delirium, model = "logistic", statloc="none")
plot(rc1)
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$hemoglobin))

as.integer(range(rc1$x))

par(new=T)
with(training, hist(training$hemoglobin, xlim = c(4,18), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))



#c_reactive_protein
library(lava)
hist(training$c_reactive_protein)
psych::describe(training$c_reactive_protein)
hist(log(training$c_reactive_protein))

par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = log(training$c_reactive_protein), y = training$delirium, model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$c_reactive_protein))

as.integer(range(rc1$x))

par(new=T)
with(training, hist(log(training$c_reactive_protein), xlim = c(-4,4), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

with(training, boxplot(log(training$c_reactive_protein)~delirium))




#bun
library(lava)
hist(training$bun)
psych::describe(training$bun)
hist(log(training$bun))

par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = log(training$bun), y = training$delirium, model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$bun))

as.integer(range(rc1$x))

par(new=T)
with(training, hist(log(training$bun), xlim = c(1.5,5), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

with(training, boxplot(log(training$bun)~delirium))


?rcspline.plot
#log_wbc_max
library(lava)
hist(training$log_wbc_max)
psych::describe(training$log_wbc_max)
hist(log(training$log_wbc_max))

par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = training$log_wbc_max, y = training$delirium
                    , model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$log_wbc_max))

as.integer(range(rc1$x))

par(new=T)
with(training, hist(training$log_wbc_max, xlim = c(1,4), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

with(training, boxplot(log(training$log_wbc_max)~delirium))



#heart_rate
library(lava)
hist(training$heart_rate)
psych::describe(training$heart_rate)
hist(log(training$heart_rate))

par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = (training$heart_rate), y = training$delirium, model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$heart_rate))

as.integer(range(rc1$x))


par(new=T)
with(training, hist((training$heart_rate), xlim = c(20, 180), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

with(training, boxplot(log(training$heart_rate)~delirium))


#Heart rate ?? ???? 60~70 ???Ì¸? À¯???? ???? risk ?? ???? ??À½. 

training$hr_g <- ifelse(training$heart_rate>60 & training$heart_rate<70, 0, 1)
with(training, table(hr_g, delirium))
confusionMatrix(data = factor(training$hr_g), reference = factor(training$delirium))

hr.f <- function(v1, v2){
  training$hr_g <- ifelse(training$heart_rate>v1 & training$heart_rate<v2, 0, 1)
  with(training, table(hr_g, delirium))
  confusionMatrix(data = factor(training$hr_g), reference = factor(training$delirium))
}

hr.f(60, 90)
hr.f(40, 100)
hr.f(40, 90)
hr.f(40, 80)
hr.f(30, 100)
hr.f(0, 100)
hr.f(0, 90)
hr.f(0, 90)
hr.f(0, 90)

hr.f(40, 90)
hr.f(30, 100)

hr.f(30, 90) #?Ì°? Á¦?? ??À½. 

abline(v = 30, col = 2, lty = 2)
abline(v = 90, col = 2, lty = 2)




#creatinine
library(lava)
hist(training$creatinine)
psych::describe(training$creatinine, quant = c(0.25, 0.5, 0.75))
hist(log(training$creatinine))

par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = log(training$creatinine), y = training$delirium, model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$creatinine))

as.integer(range(rc1$x))

par(new=T)
with(training, hist(log(training$creatinine), xlim = c(-1, 2.5), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

with(training, boxplot(log(training$creatinine)~delirium))


cr.f <- function(v1, v2){
  training$cr_g <- ifelse(log(training$creatinine)>v1 & log(training$creatinine)<v2, 0, 1)
    print(confusionMatrix(data = factor(training$cr_g), reference = factor(training$delirium)))
  return(with(training, table(cr_g, delirium)))
}

cr.f(-0.5, 0.2)
cr.f(-0.2, 0.2)
cr.f(-0.5, 0.2)
cr.f(-2, 0.4)
cr.f(-2, 0.3)
cr.f(-0.7, 0.3)
cr.f(-0.6, 0.3)

range(log(training$creatinine))

prop.table(cr.f(-0.1, 0.3),2)
barplot(prop.table(cr.f(-0.7, 0.4),2)[2,])
barplot(prop.table(cr.f(-0.8, 0.4),2)[2,])
barplot(prop.table(cr.f(-0.9, 0.4),2)[2,])
barplot(prop.table(cr.f(-1, 0.4),2)[2,])

barplot(prop.table(cr.f(-1.0, 0.4),2)[2,])
barplot(prop.table(cr.f(-1.0, 0.6),2)[2,])


barplot(prop.table(cr.f(-2, 0.4),2)[2,])
barplot(prop.table(cr.f(-1, 0.4),2)[2,])
barplot(prop.table(cr.f(-2, 0.4),2)[2,])
barplot(prop.table(cr.f(-0.9, 0.4),2)[2,])

exp(-0.9)
exp(0.4)
describe(training$creatinine, quant = c(0.25, .5, .75))
with(training, table(cut(training$creatinine, c(0, 0.7, 0.8, 0.9, 1.0, 1.1, 1.4, 1.7, 2.5, 5, 20))))
t1 <- with(training, table(delirium, cut(training$creatinine, c(0, 0.7, 0.8, 0.9, 1.0, 1.1,1.4, 1.7, 2.5, 5, 20))))
barplot(prop.table(t1, 2)[2,])



#platelet
library(lava)
hist(training$platelet)
psych::describe(training$platelet)
hist(log(training$platelet))

par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = (training$platelet), y = training$delirium, model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$platelet))

as.integer(range(rc1$x))

par(new=T)
with(training, hist((training$platelet), xlim = c(0,500), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

with(training, boxplot(log(training$platelet)~delirium))



pl.f <- function(v1, v2, data1 = training){
  data1$pl_g <- ifelse((data1$platelet)>v1 & (data1$platelet)<v2, 0, 1)
  print(confusionMatrix(data = factor(data1$pl_g), reference = factor(data1$delirium)))
  return(with(data1, table(pl_g, delirium)))
}

pl.f(150, 300)
pl.f(160, 300)
pl.f(170, 300)
pl.f(140, 300)
pl.f(130, 300)
pl.f(120, 300)

pl.f(110, 300)

pl.f(120, 290)
pl.f(120, 300)
pl.f(120, 310)
pl.f(120, 320)
pl.f(120, 330)

pl.f(120, 340)
pl.f(120, 600)

pl.f(120, 350)
pl.f(120, 360)
pl.f(120, 400)
pl.f(120, 500)
pl.f(120, 600)
pl.f(110, 330)


abline(v = c(120, 340), col = 2, lty = 2)



pl.f(120, 340, data1 = test)
pl.f(120, 600, data1 = test)

# variable coding correction 




#ast
library(lava)
hist(training$ast)
psych::describe(training$ast, quant = c(0.25, 0.5, 0.75))
describe(training$ast, quant = c(0.25, 0.5, 0.75))

hist(log(log(training$ast)))




par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = log(log(training$ast)), y = training$delirium, model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$ast))

as.integer(range(rc1$x))

par(new=T)
with(training, hist(log(log(training$ast)), xlim = c(.8,2.2), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))



par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = (log(training$ast)), y = training$delirium, model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$ast))

as.integer(range(rc1$x))

par(new=T)
with(training, hist((log(training$ast)), xlim = c(2,9), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

abline(v = c(log(40), log(424)), col = 2, lty = 2)

with(training, boxplot(log(training$ast)~delirium))


prop.table(with(training, table(delirium, cut(log(log(ast)), c(0,1.2,1.4,1.6,1.8,2,5)))), 2)
prop.table(with(test, table(delirium, cut(log(log(ast)), c(0,1.2,1.4,1.6,1.8,2,5)))), 2)

prop.table(with(training, table(delirium, cut(log(log(ast)), c(0,1.2,1.4,1.6,1.8,5)))), 2)
prop.table(with(test, table(delirium, cut(log(log(ast)), c(0,1.2,1.6,1.4,1.8,5)))), 2)


prop.table(with(training, table(delirium, cut(log(log(ast)), c(0,1.3,1.8,5)))), 2)
prop.table(with(test, table(delirium, cut(log(log(ast)), c(0,1.3,1.8,5)))), 2)

exp(exp(1.2))
exp(exp(1.6))
exp(exp(1.8))
exp(exp(1.3))

training$ast_g <- as.integer(cut(training$ast, c(0, 40, 424, 200000)))


table(with(training,(cut(ast, c(0, 40, 424, 200000)))), 
      training$age_g <- with(training,as.integer(cut(ast, c(0, 40, 424, 200000)))))


#age
library(lava)
hist(training$age)
psych::describe(training$age, quant = c(0.25, 0.5, 0.75))
hist(log(training$age))

par(mfrow =c(1,1))
rc1 <- rcspline.plot(x = (training$age), y = training$delirium, model = "logistic", statloc="none")
str(rc1)
rc1$knots
range(rc1$x)

as.integer(range(training$age))

as.integer(range(rc1$x))

par(new=T)
with(training, hist((training$age), xlim = c(20,100), ylim = c(0, 500), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

with(training, boxplot(log(training$age)~delirium))

prop.table(with(training, table(delirium, cut(age, c(0,40,45,50,55, 60, 65, 70, 75, 80, 85, 90, 120)))), 2)
prop.table(with(test, table(delirium, cut(age, c(0,40,45,50,55, 60, 65, 70, 75, 80, 85, 90, 120)))), 2)

prop.table(with(training, table(delirium, cut(age, c(0,40,50,60, 70, 80, 90, 120)))), 2)
prop.table(with(test, table(delirium, cut(age, c(0,40,50,60, 70, 80, 90, 120)))), 2)

prop.table(with(training, table(delirium, cut(age, c(0,60, 70, 80, 90, 120)))), 2)
prop.table(with(test, table(delirium, cut(age, c(0,60, 70, 80, 90, 120)))), 2)


training$age_g <- with(training,as.integer(cut(age, c(0,60, 70, 80, 90, 120))))

with(training,table(as.integer(cut(age, c(0,60, 70, 80, 90, 120))), (cut(age, c(0,60, 70, 80, 90, 120)))))
with(test,table(as.integer(cut(age, c(0,60, 70, 80, 90, 120))), (cut(age, c(0,60, 70, 80, 90, 120)))))




# estimate scoring model 



training$log_albumin <- log(training$albumin)
training$inr_g2 <- ifelse(training$inr > exp(0.2), 1, 0)
training$log_hb <- log(training$hemoglobin)
training$log_crp <- log(training$c_reactive_protein)
training$log_bun <- log(training$bun)
training$logwbcmax_g <- cut(training$log_wbc_max, c(0, 2, 2.2, 2.4, 2.6, 2.8, 3.0, 5))
training$hr_g <- ifelse(training$heart_rate>60 & training$heart_rate<70, 0, 1)
training$cr_g <- ifelse(log(training$creatinine)>-0.9 & log(training$creatinine)<0.4, 0, 1)
training$cr_g <- ifelse(training$creatinine<1.4, 0, 1)
training$plt_g <- ifelse((training$platelet)>120 & (training$platelet)<340, 0, 1)
training$ast_g <- as.integer(cut(training$ast, c(0, 40, 424, 200000)))
training$age_g <- with(training,as.integer(cut(age, c(0,60, 70, 80, 90, 120))))

test$log_albumin <- log(test$albumin)
test$inr_g2 <- ifelse(test$inr > exp(0.2), 1, 0)
test$log_hb <- log(test$hemoglobin)
test$log_crp <- log(test$c_reactive_protein)
test$log_bun <- log(test$bun)
test$logwbcmax_g <- cut(test$log_wbc_max, c(0, 2, 2.2, 2.4, 2.6, 2.8, 3.0, 5))
test$hr_g <- ifelse(test$heart_rate>60 & test$heart_rate<70, 0, 1)
test$cr_g <- ifelse(log(test$creatinine)>-0.9 & log(test$creatinine)<0.4, 0, 1)
test$cr_g <- ifelse(test$creatinine<1.4, 0, 1)
test$plt_g <- ifelse((test$platelet)>120 & (test$platelet)<340, 0, 1)
test$ast_g <- as.integer(cut(test$ast, c(0, 40, 424, 200000)))
test$age_g <- with(test,as.integer(cut(age, c(0,60, 70, 80, 90, 120))))


dd = datadist(training)
options(datadist = "dd")



detach(training)
attach(training)

options(max.print = 200000000)
(rownames(training))

training$oxygen_therapy_type <-  data1$oxygen_therapy_type[index[,1]]
test$oxygen_therapy_type <-  data1$oxygen_therapy_type[-index[,1]]



table(data2$age[index[,1]] == training$age)
table(training$oxygen_therapy_type, training$oxygen_therapy_type2)
table(training$oxygen_therapy_type, training$oxygen_therapy_type1)

table(data2$age[-index[,1]] == test$age)
table(test$oxygen_therapy_type, test$oxygen_therapy_type2)
table(test$oxygen_therapy_type, test$oxygen_therapy_type1)

 

training.score <- training 
tr.s <- training.score

score.cont <- c("log_albumin", "log_hb", "log_crp", "log_bun", "log_wbc_max")
score.fact <- c("inr_g2", "oxygen_therapy_type", "hr_g", "cr_g", "plt_g", "ast_g", "SHOCK", "age_g")



training[score.fact] = lapply(training[score.fact], factor)
test[score.fact] = lapply(test[score.fact], factor)


lrm1 <- lrm(delirium ~ log_albumin + inr_g2 + log_hb + log_crp + log_bun + 
  log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 
  + hr_g + cr_g + plt_g + ast_g + SHOCK + age_g , data = training)

glm1 <- glm(delirium ~ log_albumin + inr_g2 + log_hb + log_crp + log_bun + 
      log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 
    + hr_g + cr_g + plt_g + ast_g + SHOCK + age_g , data = training, family = binomial())


#cr?? bun ?? ?????? ???? ?Ï³??? ????. bunÀ» ????. 
lrm1 <- lrm(delirium ~ log_albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 
            + hr_g + cr_g + plt_g + ast_g + SHOCK + age_g , data = training)

glm1 <- glm(delirium ~ log_albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 
            + hr_g + cr_g + plt_g + ast_g + SHOCK + age_g , data = training, family = binomial())

#cr?? bun ?? ?????? ???? ?Ï³??? ????. bunÀ» ????. 
lrm2 <- lrm(delirium ~ log_albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 
            + hr_g + cr_g + plt_g + ast_g + age_g , data = training)

glm2 <- glm(delirium ~ log_albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 
            + hr_g + cr_g + plt_g + ast_g + age_g , data = training, family = binomial())



vif(glm1)

n1 <- nomogram(lrm1)
plot(n1)
n2 <- nomogram(lrm2)
plot(n2)


training$delirium
training$log_albumin
training$inr_g2
training$log_hb
training$log_crp
training$log_bun
training$log_wbc_max
training$oxygen_therapy_type1
training$oxygen_therapy_type2
training$hr_g
training$cr_g
training$plt_g
training$ast_g
training$SHOCK
training$age_g


roc(response = test$delirium, predictor = predict(glm1, newdata = test, type = "response"), ci = T)

vif(glm1)
plot(training$log_bun, log(training$creatinine))
cor.test(training$log_bun, log(training$creatinine), method = "spearman")



####step6: Evaluate performance of scoring model on validation set. 

formula9





ptm <- proc.time()
for (i in 1:50) mad(stats::runif(5000))
proc.time() - ptm





















