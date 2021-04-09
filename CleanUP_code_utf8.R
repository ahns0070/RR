
source("https://raw.githubusercontent.com/ahns0070/RR/prediction_code/Data_setup.R")



library(tableone)

ct1 <- CreateTableOne(vars=c(factorVars1), data=data1, factorVars=factorVars1, includeNA = T)
print(ct1, noSpaces=F, smd=F, showAllLevels = T, quote = T, oveall = T)
#install.packages('psych')
psych::describe(data1[contiVars1],quant=c(.25,.75) ) 


training2$delirium

trainTransformed$delirium

ct1 <- CreateTableOne(vars=c(factor.cov), data=training, factorVars=factorVars1, includeNA = T)
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
psych::describe(trainTransformed2[conti.cov],quant=c(.25,.75) ) 

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

library(clipr)
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


############################################################################
####step3 : Feature selection using machine learning method on training set
#see caret2.r

#20210404 Interpretable ML 



#install.packages('iml')
library(iml)
library(pre)

#pre - interaction effect evaluation
#install.packages('pre')
library(pre)

set.seed(1322)
start.time <- proc.time()
pre1 <- pre(formula0, data = training, famliy = binomial())
end.time <- proc.time()
end.time - start.time

pre1

with(training, table(age <= 75 & platelet > 126 & oxygen_therapy_type1 == 0, delirium))

summary(pre1)

par(mfrow =c(5,5))
plot(pre1)

#using randomForest, initial model construction.
library(randomForest)
set.seed(344); rf <- randomForest(formula0, data = training, ntree = 50)
#either rf or rf.select.model
rf.select.model


#X <- training[c(conti.cov[-c(11,13)], factor.cov, "log_wbc_max")] #delete wbc_min & hematocrit.



head(tr4iml)

#loss The loss function. Either the name of a loss (e.g. "ce" for classification or "mse") 
#or a function. See Details for allowed losses.

formula0 <- formula(delirium2 ~ albumin + inr + hemoglobin + c_reactive_protein + bun
                    + wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 + heart_rate 
                    + creatinine + platelet + ast
                    + SHOCK + age + sex + vasoactive_drug + dementia + cardiac_arrest_before_icu 
                    + CNS_ds_y_n)


#Addtional Feature selection using RF- ok
library(caret)
rf.ctrl <- trainControl ( method = "cv"
                          , number = 10
                          , savePredictions = T
                          , classProbs = T
                          , summaryFunction = twoClassSummary
)



#Random Forest

training$log_wbc_max <- log(training$wbc_max)

start.t <- proc.time()
set.seed(1231)
rf.select.model <- train(form = formula0, data = training, method = "rf"
                         , metric = "ROC"
                         , trControl = rf.ctrl
                         , preProcess = c("center", "scale")
)
end.t <- proc.time()
end.t - start.t

rf.select.model
getTrainPerf(rf.select.model)
varImp(rf.select.model)
plot(varImp(rf.select.model))



#install.packages('dplyr')
library(dplyr)
tr4iml <- trainTransformed2
dim(tr4iml)

library(iml)
predictor <- Predictor$new(rf.select.model, data = tr4iml, y = "delirium", type="prob")
start.time1 <- proc.time()
imp <- FeatureImp$new(predictor, loss = "ce", n.repetitions = 100, compare = "difference")
end.time1 <- proc.time()
end.time1 - start.time1

library("ggplot2")
plot(imp)

#Interaction effect.
start.time2 <- proc.time()
inter1 <- Interaction$new(predictor, grid.size = 2)
end.time2 <- proc.time()
end.time2 - start.time2
inter1
plot(inter1)
inter1$results
inter1$plot()

start.time3 <- proc.time()
ale <- FeatureEffect$new(predictor, feature = "SHOCK")
par(mfrow=c(3,3))
ale$plot()
end.time3 <- proc.time()
end.time3 - start.time3
ale$set.feature("SHOCK")
ale$plot()



start.time4 <- proc.time()
interact4 <- Interaction$new(predictor, feature = "oxygen_therapy_type1")
plot(interact4)
end.time4 <- proc.time()
end.time4 - start.time4


start.time5 <- proc.time()
interact5 <- Interaction$new(predictor, feature = "oxygen_therapy_type2")
plot(interact5)
end.time5 <- proc.time()
end.time5 - start.time5


start.time6 <- proc.time()
interact6 <- Interaction$new(predictor, feature = "SHOCK")
plot(interact6)
end.time6 <- proc.time()
end.time6 - start.time6



start.time7 <- proc.time()
interact7 <- Interaction$new(predictor, feature = "inr")
plot(interact7)
end.time7 <- proc.time()
end.time7 - start.time7



start.time8 <- proc.time()
interact8 <- Interaction$new(predictor, feature = "CNS_ds_y_n")
plot(interact8)
end.time8 <- proc.time()
end.time8 - start.time8



start.time9 <- proc.time()
interact9 <- Interaction$new(predictor, feature = "creatinine")
plot(interact9)
end.time9 <- proc.time()
end.time9 - start.time9
effs <- FeatureEffects$new(predictor)
plot(effs)

tree <- TreeSurrogate$new(predictor, maxdepth = 2)
plot(tree)

head(tree$predict(training))


####step4 : Fit the logistic model on training set (repeated K fold CV method for testing model on hold-out sample)


#orange model. 




###########################################################################
## comparison AUC between models. 


formula0 <- formula(delirium2 ~ albumin + inr + hemoglobin + c_reactive_protein + bun
                    + log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 + heart_rate + creatinine + platelet + ast
                    + SHOCK + age + sex + vasoactive_drug + dementia + cardiac_arrest_before_icu + CNS_ds_y_n)

training$delirium2




interact.f0 <- formula(delirium2 ~ SHOCK*(albumin + inr + hemoglobin + c_reactive_protein + bun
                    + log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 + heart_rate + creatinine + platelet + ast
                    + age + sex + vasoactive_drug + dementia + cardiac_arrest_before_icu + CNS_ds_y_n))

training$delirium2


interact.f1 <- formula(delirium2 ~ SHOCK*(albumin + bun+inr + heart_rate+ age+ hemoglobin + c_reactive_protein
                                          + log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 ) 
                       + creatinine + platelet + ast
                       + sex + vasoactive_drug + dementia + cardiac_arrest_before_icu 
                       + CNS_ds_y_n)




library(rms)
lrm(delirium2~SHOCK*bun, data = training)
lrm(delirium2~SHOCK*heart_rate, data = training)
lrm(delirium2~SHOCK*age, data = training)
lrm(delirium2~SHOCK*c_reactive_protein, data = training)
lrm(delirium2~SHOCK*log_wbc_max, data = training)
lrm(delirium2~SHOCK*oxygen_therapy_type1, data = training)
lrm(delirium2~SHOCK*oxygen_therapy_type1+SHOCK*oxygen_therapy_type2, data = training)

training$inter_shock_bun <- with(training, SHOCK*bun)
training$inter_shock_hr <- with(training, SHOCK*heart_rate)
training$inter_shock_age <- with(training, SHOCK*age)
training$inter_shock_crp <- with(training, SHOCK*c_reactive_protein)
training$inter_shock_wbc <- with(training, SHOCK*log_wbc_max)
training$inter_shock_ott1 <- with(training, SHOCK*oxygen_therapy_type1)
training$inter_shock_ott2 <- with(training, SHOCK*oxygen_therapy_type2)

test$inter_shock_bun <- with(test, SHOCK*bun)
test$inter_shock_hr <- with(test, SHOCK*heart_rate)
test$inter_shock_age <- with(test, SHOCK*age)
test$inter_shock_crp <- with(test, SHOCK*c_reactive_protein)
test$inter_shock_wbc <- with(test, SHOCK*log_wbc_max)
test$inter_shock_ott1 <- with(test, SHOCK*oxygen_therapy_type1)
test$inter_shock_ott2 <- with(test, SHOCK*oxygen_therapy_type2)


interact.f2 <- formula(delirium2 ~ SHOCK+bun+inr + heart_rate+ age+ hemoglobin 
                                           + c_reactive_protein
                                           + log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2
                       + creatinine + platelet + ast
                       + sex + vasoactive_drug + dementia + cardiac_arrest_before_icu 
                       + CNS_ds_y_n+albumin+inter_shock_age+inter_shock_bun+inter_shock_crp
                        + inter_shock_hr+ inter_shock_ott1++ inter_shock_ott1
                      + inter_shock_wbc)




#test
lrm(delirium2~SHOCK*bun, data = training)
lrm(delirium2 ~ bun+SHOCK+inter_shock_bun, data = training)



data1$log_wbc_max <- log(data1$wbc_max)
training$log_wbc_max <- log(training$wbc_max)
test$log_wbc_max <- log(test$wbc_max)

lrm(delirium2~SHOCK*bun, data = test)
lrm(delirium2~SHOCK*heart_rate, data = test)
lrm(delirium2~SHOCK*age, data = test)
lrm(delirium2~SHOCK*c_reactive_protein, data = test)
lrm(delirium2~SHOCK*log_wbc_max, data = test)
lrm(delirium2~SHOCK*oxygen_therapy_type1, data = test)
lrm(delirium2~SHOCK*oxygen_therapy_type1+SHOCK*oxygen_therapy_type2, data = test)





#Addtional Feature selection using RF- ok

rf.ctrl <- trainControl ( method = "cv"
                          , number = 10
                          , savePredictions = T
                          , classProbs = T
                          , summaryFunction = twoClassSummary
)



#Random Forest

training$log_wbc_max <- log(training$wbc_max)
start.t <- proc.time()
set.seed(1231)
rf.select.model <- train(form = formula0, data = training, method = "rf"
                         , metric = "ROC"
                         , trControl = rf.ctrl
                         , preProcess = c("center", "scale")
)
end.t <- proc.time()
end.t - start.t

rf.select.model
getTrainPerf(rf.select.model)
varImp(rf.select.model)
plot(varImp(rf.select.model))


formula15 <- formula(
delirium2 ~ albumin + SHOCK * inr + hemoglobin + SHOCK * c_reactive_protein + 
  bun + SHOCK * log_wbc_max + SHOCK * oxygen_therapy_type1 + 
  SHOCK * heart_rate + creatinine + platelet + ast + SHOCK * 
  oxygen_therapy_type2)

formula16 <- formula(
  delirium2 ~ albumin + SHOCK * inr + hemoglobin + SHOCK * c_reactive_protein + 
    bun + SHOCK * log_wbc_max + SHOCK * oxygen_therapy_type1 + 
    SHOCK * heart_rate + creatinine + platelet + ast + SHOCK * 
    oxygen_therapy_type2)

formula17 <- formula(
  delirium2 ~ albumin + SHOCK * inr + hemoglobin + SHOCK * c_reactive_protein + 
    bun + SHOCK * log_wbc_max + SHOCK * oxygen_therapy_type1 + 
    SHOCK * heart_rate + creatinine + platelet + ast + SHOCK * 
    oxygen_therapy_type2)

lrm(formula15, data = test)


#Consider interaction effect


library(vip)

rf.select.model
library(gdata)
is.what(rf.select.model)

vi(rf.select.model)

#using 'pre' package, estimate H statistic for interaction effect. 





#end.


training$log_wbc_max <- log(training$wbc_max)
start.t <- proc.time()
set.seed(1231)
rf.select.model <- train(form = formula15, data = training, method = "rf"
                         , metric = "ROC"
                         , trControl = rf.ctrl
                         , preProcess = c("center", "scale")
)
end.t <- proc.time()
end.t - start.t

rf.select.model
getTrainPerf(rf.select.model)
varImp(rf.select.model)
plot(varImp(rf.select.model))



#Consider intereaction effect


interact.f1 <- formula(delirium2 ~ SHOCK*(albumin + bun+inr + heart_rate+ age+ hemoglobin + c_reactive_protein
                                          + log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 ) 
                                          + creatinine + platelet + ast
                                           + sex + vasoactive_drug + dementia + cardiac_arrest_before_icu 
                                          + CNS_ds_y_n)

training$log_wbc_max <- log(training$wbc_max)
start.t <- proc.time()
set.seed(1231)
rf.inter.model <- train(form = interact.f1, data = training, method = "rf"
                         , metric = "ROC"
                         , trControl = rf.ctrl
                         , preProcess = c("center", "scale")
)
end.t <- proc.time()
end.t - start.t

rf.inter.model
getTrainPerf(rf.inter.model)
varImp(rf.inter.model)
plot(varImp(rf.inter.model))

str(rf.inter.model)


## preprocess randomforest- ok
set.seed(1231)
rf.select.model.nonscaled <- train(form = formula0, data = training
                                   , metric = "ROC"
                                   , trControl = rf.ctrl
)
rf.select.model.nonscaled
getTrainPerf(rf.select.model.nonscaled)
varImp(rf.select.model.nonscaled)
plot(varImp(rf.select.model.nonscaled))


#linear discriminant analysis - ok
set.seed(1231)
start.t <- proc.time()
lda.select.model <- train(form = interact.f1, data = training, method = "lda"
                          , metric = "ROC"
                          , trControl = rf.ctrl
                          , preProcess = c("center", "scale")
)
end.time <- proc.time()
end.time - start.t

lda.select.model
getTrainPerf(lda.select.model)
varImp(lda.select.model)
plot(varImp(lda.select.model))

defaultSummary(lda.select.model)





set.seed(1231)
start.t <- proc.time()
adaboost.select.model <- train(form = interact.f1, data = training, method = "adaboost"
                               , metric = "ROC"
                               , trControl = rf.ctrl
                               , preProcess = c("center", "scale")
)
end.time <- proc.time()
end.time - start.t
(end.time - start.t)/60


adaboost.select.model
getTrainPerf(adaboost.select.model)
varImp(adaboost.select.model)
plot(varImp(adaboost.select.model))


set.seed(1231)
gamboost.select.model <- train(form = formula0, data = training, method = "gamboost"
                               , metric = "ROC"
                               , trControl = rf.ctrl
                               , preProcess = c("center", "scale")
)

deepboost.ctrl <- trainControl ( method = "cv"
                                 , number = 10
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
#In train.default(x, y, weights = w, ...) :
#  Class probabilities were requested for a model that does not implement them




glmStepAIC

#stepwise selection
set.seed(1231)
glmStepAIC.select.model <- train(form = interact.f1, data = training, method = "glmStepAIC"
                                 , metric = "ROC"
                                 , trControl = rf.ctrl
                                 , preProcess = c("center", "scale")
)
glmStepAIC.select.model
getTrainPerf(glmStepAIC.select.model)
varImp(glmStepAIC.select.model)
plot(varImp(glmStepAIC.select.model))


#cannot run.
set.seed(1231)
glmnet_h20.model <- train(form = interact.f1, data = training, method = "glmnet_h20"
                                 , metric = "ROC"
                                 , trControl = rf.ctrl
                                 , preProcess = c("center", "scale")
)
install.packages('h20')
packageVersion('caret')


glmnet_h20.select.model
getTrainPerf(glmnet_h20.select.model)
varImp(glmnet_h20.select.model)
plot(varImp(glmnet_h20.select.model))


set.seed(1231)
start.time <- proc.time()
C5.0.select.model <- train(form = interact.f1, data = training, method = "C5.0"
                           , metric = "ROC"
                           , trControl = rf.ctrl
                           , preProcess = c("center", "scale")
)
end.time <- proc.time()
end.time - start.time

C5.0.select.model
getTrainPerf(C5.0.select.model)
varImp(C5.0.select.model)
plot(varImp(C5.0.select.model))


#install.packages('adabag') - success
#AdaBag - success
set.seed(1231)
start.time <- proc.time()
AdaBag.select.model <- train(form = formula0, data = training, method = "AdaBag"
                             , metric = "ROC"
                             , trControl = rf.ctrl
                             , preProcess = c("center", "scale")
)
end.time <- proc.time()
end.time - start.time


AdaBag.select.model
getTrainPerf(AdaBag.select.model)
varImp(AdaBag.select.model)
plot(varImp(AdaBag.select.model))


##bagging - success
set.seed(1231)
start.time <- proc.time()
bag.select.model <- train(form = formula0, data = training, method = "treebag"
                          ,trControl = rf.ctrl
                          , preProcess = c("center", "scale")
                          , metric = "ROC")

end.time <- proc.time()
end.time - start.time



warnings()
bag.select.model
getTrainPerf(bag.select.model)
varImp(bag.select.model)
plot(varImp(bag.select.model))


#The weighted subspace random forest
start.time <- proc.time()
set.seed(1231)
wsrf.select.model <- train(form = interact.f0, data = training, method = "wsrf"
                           ,trControl = rf.ctrl
                           , preProcess = c("center", "scale")
                           , metric = "ROC")

end.time <- proc.time()
end.time - start.time

wsrf.select.model
getTrainPerf(wsrf.select.model)
varImp(wsrf.select.model)
plot(varImp(wsrf.select.model))

summary(wsrf.select.model)
wsrf.select.model$tuneValue



#The weighted subspace random forest
start.time <- proc.time()
set.seed(1231)
wsrf.select.model <- train(form = interact.f1, data = training, method = "wsrf"
                                  ,trControl = rf.ctrl
                                  , preProcess = c("center", "scale")
                                  , metric = "ROC")

end.time <- proc.time()
end.time - start.time

wsrf.select.model
getTrainPerf(wsrf.select.model)
varImp(wsrf.select.model)
plot(varImp(wsrf.select.model))

test$log_wbc_max <- log(test$wbc_max)
test$wsrf.pred <- predict(wsrf.select.model, newdata= test, type = "prob")[,2]

library(pROC)
roc(predictor=test$wsrf.pred, response = test$delirium, ci=T, data = test)




#The nodeHarvest
start.time <- proc.time()
set.seed(1231)
nodeHarvest.select.model <- train(form = interact.f1, data = training, method = "nodeHarvest"
                                  ,trControl = rf.ctrl
                                  , preProcess = c("center", "scale")
                                  , metric = "ROC")

end.time <- proc.time()
end.time - start.time


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


data1$log_wbc_max
training$log_wbc_max

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



formula11 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                        +SHOCK*oxygen_therapy_type1 +heart_rate +creatinine +platelet +ast
                        +SHOCK*oxygen_therapy_type2)

formula12 <- as.formula(delirium ~ albumin+inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                        +SHOCK*oxygen_therapy_type1 +SHOCK*heart_rate +creatinine +platelet +ast
                        +SHOCK*oxygen_therapy_type2)

formula13 <- as.formula(delirium ~ albumin+SHOCK*inr +hemoglobin +c_reactive_protein +bun +log_wbc_max
                        +SHOCK*oxygen_therapy_type1 +SHOCK*heart_rate +creatinine +platelet +ast
                        +SHOCK*oxygen_therapy_type2)

formula14 <- as.formula(delirium ~ albumin+SHOCK*inr +hemoglobin +c_reactive_protein +bun +SHOCK*log_wbc_max
                        +SHOCK*oxygen_therapy_type1 +SHOCK*heart_rate +creatinine +platelet +ast
                        +SHOCK*oxygen_therapy_type2)


formula15 <- as.formula(delirium ~ albumin+SHOCK*inr +hemoglobin +SHOCK*c_reactive_protein +bun +SHOCK*log_wbc_max
                        +SHOCK*oxygen_therapy_type1 +SHOCK*heart_rate +creatinine +platelet +ast
                        +SHOCK*oxygen_therapy_type2)


formula15 <- as.formula(delirium ~ albumin+SHOCK*inr +hemoglobin +SHOCK*c_reactive_protein +bun +SHOCK*log_wbc_max
                        +SHOCK*oxygen_therapy_type1 +SHOCK*heart_rate +creatinine +platelet +ast
                        +SHOCK*oxygen_therapy_type2)

library(vip)
install.packages('vip')



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
cv.perf.logistic11.list<- vector('list', k)
cv.perf.logistic12.list<- vector('list', k)
cv.perf.logistic13.list<- vector('list', k)
cv.perf.logistic14.list<- vector('list', k)
cv.perf.logistic15.list<- vector('list', k)


start.time <- proc.time()
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
for(i in 1:k) cv.perf.logistic11.list[[i]] <- cv.f(formula11 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic12.list[[i]] <- cv.f(formula12 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic13.list[[i]] <- cv.f(formula13 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic14.list[[i]] <- cv.f(formula14 , seed = seed.list[i], number = 10, data = training, add=T)
for(i in 1:k) cv.perf.logistic15.list[[i]] <- cv.f(formula15 , seed = seed.list[i], number = 10, data = training, add=T)
end.time <- proc.time()
end.time - start.time


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
par(mfrow = c(1,1))
hist(training$albumin)
rcspline.plot(x = training$albumin, y = training$delirium, model = "logistic", statloc="none")
par(new=T)
with(training, hist(training$albumin, xlim = c(1.5,5), ylim = c(0, 1000), main = ""
                    , col = Col("lightgrey", 0.3),yaxt="n", ylab="", nclass = 30), axis(side =4))

log
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
with(training, boxplot(log(log(training$inr)+1) ~ delirium)) 
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

hr.f(30, 90)

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



training$albumin <- training$albumin
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

test$albumin <- (test$albumin)
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

score.cont <- c("albumin", "log_hb", "log_crp", "log_bun", "log_wbc_max")
score.fact <- c("inr_g2", "oxygen_therapy_type", "hr_g", "cr_g", "plt_g", "ast_g", "SHOCK", "age_g")



training[score.fact] = lapply(training[score.fact], factor)
test[score.fact] = lapply(test[score.fact], factor)


lrm1 <- lrm(delirium ~ albumin + inr_g2 + log_hb + log_crp + log_bun + 
              log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 
            + hr_g + cr_g + plt_g + ast_g + SHOCK + age_g , data = training)

glm1 <- glm(delirium ~ albumin + inr_g2 + log_hb + log_crp + log_bun + 
              log_wbc_max + oxygen_therapy_type1 + oxygen_therapy_type2 
            + hr_g + cr_g + plt_g + ast_g + SHOCK + age_g , data = training, family = binomial())

#remove cr bun.
lrm1 <- lrm(delirium ~ albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type
            + hr_g + cr_g + plt_g + ast_g + SHOCK + age_g , data = training)

glm1 <- glm(delirium ~ albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type 
            + hr_g + cr_g + plt_g + ast_g + SHOCK + age_g , data = training, family = binomial())

# delete bun, shock 
lrm2 <- lrm(delirium ~ albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type
            + hr_g + cr_g + plt_g + ast_g + age_g , data = training)

glm2 <- glm(delirium ~ albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type
            + hr_g + cr_g + plt_g + ast_g + age_g , data = training, family = binomial())





ct1 <- CreateTableOne(vars  = c(score.cont, score.fact), strata = "SHOCK"
                      , data = training[training$delirium==1,]
                      , factorVars = score.fact)
print(ct1)
print(ct1, nonnormal = conti.cov, standardize = T)

lrm(delirium ~ oxygen_therapy_type, data = training)
lrm(delirium ~ SHOCK, data = training)

ct2 <- CreateTableOne(vars  = c(score.cont, score.fact), strata = "SHOCK"
                      , data = training[training$delirium==0,]
                      , factorVars = score.fact)

print(ct2, nonnormal = conti.cov, standardize = T)

par(mfrow = c(2,2))
boxplot(albumin ~ SHOCK)
boxplot(inr ~ SHOCK)



vif(glm1)

n1 <- nomogram(lrm1)
plot(n1)
n2 <- nomogram(lrm2)
plot(n2)


# delete oxygen therapy type , albumin 
lrm2 <- lrm(delirium ~  inr_g2 + log_hb + log_crp  + 
              log_wbc_max + SHOCK
            + hr_g + cr_g + plt_g + ast_g + age_g , data = training)
lrm2
glm2 <- glm(delirium ~ albumin + inr_g2 + log_hb + log_crp  + 
              log_wbc_max + oxygen_therapy_type
            + hr_g + cr_g + plt_g + ast_g + age_g , data = training, family = binomial())




training$delirium
training$albumin
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



scoring.tr <- training[c("delirium", score.cont, score.fact)]
names(scoring.tr)



describe(albumin)

lrm1 <- lrm(delirium~., data = scoring.tr)
n1 <- nomogram(lrm1, albumin= seq(0, 6, 1))
plot(n1)

win.graph()
par(mfrow = c(1,2))
plot(n1, col.conf=c(1,.5,.2), naxes=11, fun=function(x)1/(1+exp(-x)),  # or fun=plogis
     fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
     funlabel="Risk of Delirium")

par(mfrow = c(1,2))
boxplot(albumin ~ SHOCK)
boxplot(albumin ~ oxygen_therapy_type)



lrm1 <- lrm(delirium~ SHOCK/(oxygen_therapy_type + log_bun + log_crp + age_g + albumin + inr_g2+ast_g+plt_g
                             +hr_g+log_wbc_max) , data = training)
lrm1
n1 <- nomogram(lrm1)
plot(n1)



glm1 <- glm(delirium~ SHOCK/(oxygen_therapy_type + log_bun + log_crp + age_g + albumin + inr_g2+plt_g
                             +hr_g+log_wbc_max) , data = training, family = binomial)
summary(glm1)

test$interact.p <- predict(glm1, newdata = test, type = "response")

library(pROC)
with(test, roc(predictor = interact.p, response = delirium, ci =T))


glm1 <- glm(delirium~ SHOCK/(oxygen_therapy_type+inr_g2 + log_wbc_max+log_crp + age_g) , data = training, family = binomial)
summary(glm1)
car::Anova(glm1, 3)


glm2 <- glm(delirium~ SHOCK*(oxygen_therapy_type+log_wbc_max ) , data = training, family = binomial)
summary(glm2)




table(SHOCK, delirium, oxygen_therapy_type)



####step6: Evaluate performance of scoring model on validation set. 

formula9





ptm <- proc.time()
for (i in 1:50) mad(stats::runif(5000))
proc.time() - ptm




















