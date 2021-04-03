
#upload this file to git.

mirror.name <- getCRANmirrors()
chooseCRANmirror(ind = grep("Korea", mirror.name$Name )[1])

options("scipen"=100, "digits"=4)



librarylist = c("mi", "FSelector", "MatchIt", "survival", "psych", "PSAgraphics", "plotrix", "lava", "rms", "tableone", "geepack", "ggplot2",
"gganimate", "gifski", "zoo", "googleVis", "plotly", "Hmisc", "foreign", "Matching", "pROC", "ROCR", "gdata", "irr", "epiR", 
"caret", "quantreg", "coin", "nlme", "arm", "nonrandom", "boot", "mlogit", "crrSC", "cmprsk", "crrstep", "descr", "coxphf",
"pairwiseCI", "pec", "xlsx", "cqAUC", "rmeta", "prodlim", "timereg", "rpart", "partykit", "PredictABEL", "tiff", "gee", "glmnet",
"muhas", "survminer", "clipr", "cvTools", "controlTest", "moonBook", "brglm2", "chron", "vcd", "reshape", "reshape2", "gbm", "ipw", "ISLR", "gapminder"
, "transformr", "kader", "maxstat", "caret", "DescTools")

#lapply(as.list(librarylist), install.packages)
library(DescTools)
library(caret)
library(mi)
library(FSelector)
library(MatchIt)
library(survival)
library(psych)
library(PSAgraphics)
library(plotrix)
library(lava)
#library(rms)
library(tableone)

library(googleVis)
library(gganimate)
library(scales)
library(zoo)
library(geepack)
library(gapminder)
library(gifski)
library(plotly)
library(psych)
library(Hmisc)
library(transformr)
library(entropy)



if (!("devtools" %in% rownames(installed.packages()))) { 
  warning("installing devtools from https://cran.rstudio.com")
  install.packages("devtools", repo = "https://cran.rstudio.com")
}
library(devtools)

#devtools::install_github("dewittpe/pstools", build_vignettes = TRUE)





if (!requireNamespace("BiocManager", quietly = TRUE))
   install.packages("BiocManager")

#BiocManager::install("survcomp")

#BiocManager::install("nonrandom")




basic = function (var1)
{
if(length(var1) == 0){
base = c(0, rep(NA, 9))
return(base)
}
else if(identical(as.numeric(var1), rep(mean(var1), length(var1)) )){
normal=NA
base=round(c(
sum(!is.na(var1)),
mean( var1, na.rm = TRUE),
sd( var1, na.rm = TRUE),
quantile( var1, na.rm = TRUE),
diff(range(var1, na.rm = TRUE)),normal
), 3)
 return(base)
}
else 
{
if (sum(!is.na(var1))<2000 && sum(!is.na(var1))>3 ){
normal=shapiro.test( var1)$p.value
}
else {
normal=ks.test(var1, rnorm(length(var1)))$p.value
}
#
#tem = seq(min(var1),max(var1), by=0.0001)
#dn=dnorm(tem, mean=mean(var1), sd = sd(var1))
#hist(var1, nclass=as.integer(length(var1)/30), xlim=c(min(var1),max(var1)), ylim=NULL)
#par(new=T)
#plot(tem,dn, type="l", xlim=c(min(var1),max(var1)), ylim=c(0,0.5), ylab="", xlab="")
#
base=round(c(
sum(!is.na(var1)),
mean( var1, na.rm = TRUE),
sd( var1, na.rm = TRUE),
quantile( var1, na.rm = TRUE),
diff(range(var1, na.rm = TRUE)),normal
), 3)


names(base)=c("n", "mean",  "std", "min", "q_1", "median", "q_3", "max", "range", "normality")
return(base)
}
}





mcnemar.f <- function(var1, var2) {
#vv <- rle(sort(var2))$value
vv <- levels((var1))
keke <- vector("list", length(vv))

for(i in 1: length(keke))
keke[[i]] = table( var1==vv[i],var2==vv[i])

mat1 <- matrix(NA, ncol = 7, nrow= length(keke))

for(i in 1:length(keke)){
mat1[i,1] = margin.table(keke[[i]],1)[2]
mat1[i,2] = margin.table(keke[[i]],2)[2]
mat1[i,3] = margin.table(keke[[i]],1)[2]/margin.table(keke[[1]])
mat1[i,4] = margin.table(keke[[i]],2)[2]/margin.table(keke[[1]])
mat1[i,5] = tryCatch(mcnemar.test(keke[[i]])$p.value, error=function(e) {print(NA)}, finally="")
}
mat1[,6] = p.adjust(mat1[,4], method = "bonferroni")
mat1[,7] = p.adjust(mat1[,4], method = "fdr")
colnames(mat1) = c("freq.wielt", "freq.sielt", "m.prop.wielt", "m.prop.sielt", "mcnemar.test", "bon", "fdr")
rownames(mat1) = levels(var1)
l1 <- list(keke, mat1)
names(l1) = c("table", "raw.p")
structure(l1)
}



basicn = function (var1)
{

#tem = seq(min(var1),max(var1), by=0.0001)
#dn=dnorm(tem, mean=mean(var1), sd = sd(var1))
#hist(var1, nclass=as.integer(length(var1)/30), xlim=c(min(var1),max(var1)), ylim=NULL)
#par(new=T)
#plot(tem,dn, type="l", xlim=c(min(var1),max(var1)), ylim=c(0,0.5), ylab="", xlab="")
#
base=round(c(
sum(!is.na(var1)),
mean( var1, na.rm = TRUE),
sd( var1, na.rm = TRUE),
quantile( var1, na.rm = TRUE),
diff(range(var1, na.rm = TRUE))
), 3)


names(base)=c("n", "mean",  "std", "min", "q_2", "median", "q_3", "max", "range")
return(base)
}


basic2<-function (var1)
{
library(psych)
if (sum(!is.na(var1))<2000 && sum(!is.na(var1))>3 ){
normal=shapiro.test( var1)$p.value
}
else {
normal=ks.test(var1, rnorm(length(var1)))$p.value
}
#
#tem = seq(min(var1),max(var1), by=0.0001)
#dn=dnorm(tem, mean=mean(var1), sd = sd(var1))
#hist(var1, nclass=as.integer(length(var1)/30), xlim=c(min(var1),max(var1)), ylim=NULL)
#par(new=T)
#plot(tem,dn, type="l", xlim=c(min(var1),max(var1)), ylim=c(0,0.5), ylab="", xlab="")
#
base=round(c(
sum(!is.na(var1)),
mean( var1, na.rm = TRUE),
sd( var1, na.rm = TRUE),
quantile( var1, na.rm = TRUE),
diff(range(var1, na.rm = TRUE)),normal, skew(var1), kurtosi(var1) 
), 3)


names(base)=c("n", "mean",  "std", "min", "q_1", "median", "q_3", "max", "range", "normality", "skewness", "kurtosis")
return(base)
}




compcorr <- function(n1, r1, n2, r2){
 # compare two correlation coefficients
 # return difference and p-value as list(diff, pval)

 #	Fisher Z-transform
 	zf1 <- 0.5*log((1 + r1)/(1 - r1))
 	zf2 <- 0.5*log((1 + r2)/(1 - r2))

 #	difference
 	dz <- (zf1 - zf2)/sqrt(1/(n1 - 3) + (1/(n2 - 3)))

 #	p-value
 	pv <- 2*(1 - pnorm(abs(dz)))

 	return(list(diff=dz, pval=pv))
 }


compcorr_dep <- function(n, r1, r2, r3){
hatr = (1-r1^2-r2^2-r3^2)+(2*r1*r2*r3)
tstat = (r1-r2)*sqrt(((n-1)*(1+r3))/(2*((n-1)/(n-3))*(hatr)+((r1+r2)^2/4)*(1-r3)^3))
p.value=2*(1-pt(abs(tstat), n-3))
k = c(tstat, p.value)
names(k) = c("statistic", "p.value")
return(k)
print("method: William's (1959b) method for comparing two overlapping dependent correlations")

}





##Comparison of ppv, npv

bennett = function (n11, n12, n13, n14, n01, n02, n03, n04) {
a1 = n11+n12 ; b1 = n01 + n02 ; a2 = n11+n13 ; b2 = n01 + n03 ; 
chisq1 = (a1*b2 - a2*b1)^2/(a1*b2^2 - 2*n11*b1*b2 + a2*b1^2 + b1*a2^2 - 2*n01*a1*a2 + b2*a1^2)

a1 = n14+n12 ; b1 = n04 + n02 ; a2 = n14+n13 ; b2 = n04 + n03 ; 
chisq2 = (a1*b2 - a2*b1)^2/(a1*b2^2 - 2*n14*b1*b2 + a2*b1^2 + b1*a2^2 - 2*n04*a1*a2 + b2*a1^2)
p.value1 = 1-pchisq(chisq1,1)
p.value2 = 1-pchisq(chisq2,1)

sol = c(chisq1, p.value1)
sol2 = c(chisq2, p.value2)
names(sol) = c("Statistic_for_ppv", "p-value")
names(sol2) = c("Statistic_for_npv", "p-value")
print(c(sol, sol2))
}


##?ڷᱸ��?? ?˻?1, ?˻?2, gold standard ?? binomial variable?? ?־??? ?? (0 or 1) 
#0�� ��??�� 1�� ?缺�� ?ǹ???. 
#??, gold standard?? 1?? ?? ?˻?1?? 1?̸? ?˻?1?? sensitivity???? ??��??. 

#x1 = sanu$KI.67
#x2 = dynamin
#y = patho1

bennett = function (x1, x2, y, pos =1) {

##?ڷᱸ��?? ?˻?1, ?˻?2, gold standard ?? bivariate variable?? ?־??? ?? (0 or 1, 1 ?Ǵ? 2 ??) 
#pos?? "?缺"�� ??Ÿ???? ???? ?Ǵ? ???ڸ? ?????ϸ? ??. default?? 1?? ?Ǿ? ??��. 
#
#x1 = ?˻?1, x2=?˻?2, y=gold standard
#


t1 = as.numeric(factor(x1))-1
t2 = as.numeric(factor(x2))-1
yy = as.numeric(factor(y))-1

id = 1:length(x1)
count = rep(1, length(id))

n11 = sum(count[t1==1 & t2==1 & yy==1])
n12 = sum(count[t1==1 & t2==0 & yy==1])
n13 = sum(count[t1==0 & t2==1 & yy==1])
n14 = sum(count[t1==0 & t2==0 & yy==1])

n01 = sum(count[t1==1 & t2==1 & yy==0])
n02 = sum(count[t1==1 & t2==0 & yy==0])
n03 = sum(count[t1==0 & t2==1 & yy==0])
n04 = sum(count[t1==0 & t2==0 & yy==0])

a1 = n11+n12 ; b1 = n01 + n02 ; a2 = n11+n13 ; b2 = n01 + n03 ; 
chisq1 = (a1*b2 - a2*b1)^2/(a1*b2^2 - 2*n11*b1*b2 + a2*b1^2 + b1*a2^2 - 2*n01*a1*a2 + b2*a1^2)

a1 = n14+n12 ; b1 = n04 + n02 ; a2 = n14+n13 ; b2 = n04 + n03 ; 
chisq2 = (a1*b2 - a2*b1)^2/(a1*b2^2 - 2*n14*b1*b2 + a2*b1^2 + b1*a2^2 - 2*n04*a1*a2 + b2*a1^2)
p.value1 = 1-pchisq(chisq1,1)
p.value2 = 1-pchisq(chisq2,1)

sol = c(chisq1, p.value1)
sol2 = c(chisq2, p.value2)
names(sol) = c("Statistic_for_ppv", "p-value")
names(sol2) = c("Statistic_for_npv", "p-value")
statp = c(sol, sol2)
k=matrix(c(n11, n12, n13, n14, n01, n02, n03, n04), ncol = 2)
colnames(k) = c("Diseased", "Normal")
rownames(k) = c("+ +","+ -","- +","- -" )


structure(list(k, statp))
}

#bennett(sanu$KI.67, dynamin, patho1)
#bennett(sanu2$KI.67, dynamin, patho1)



#x1=sanu2$KI.67
#x2=dynamin
#y=patho1


#x1 = FNA ; x2 = FNA.BRAF ;  y = final
#pos = 3 ; pos1 = 3; pos2= 3



dxcomp <- function (x1, x2, y, pos =1, pos1=1, pos2=1) {

#
t1 = rep(0, length(x1))
t2 = rep(0, length(x2))
yy = rep(0, length(y))

if(length(x1) != length(x2) |  length(x1) != length(y) | length(y) != length(x2)){
print("error: the size of variables must be same!!")
}
else {

t1[x1==pos1]=1 ; 
t2[x2==pos2]=1 ;
yy[y==pos]=1 ;

k1 = factor(t1)
k2 = factor(t2)

library(ROCR)
dx.x1 = rep(NA, length=4)
dx.x2 = rep(NA, length=4)

pred1 <- prediction( t1, yy)
pred2 <- prediction( t2, yy)

measures = c('sens', 'spec', 'ppv', 'npv', 'acc')
i = 1
for (measure in measures) {
     dx.x1[i] <- performance(pred1, measure)@y.values[[1]][2]
     dx.x2[i] <- performance(pred2, measure)@y.values[[1]][2]
i = i+1
}

dxsR = rbind(dx.x1, dx.x2)
colnames(dxsR) = c("Sensitivity", "Specificy", "PPV", "NPV", "Accuracy")

sent = table(k1[yy==1], k2[yy==1])
spet = table(k1[yy==0], k2[yy==0])
sen1 = mcnemar.test(sent, correct=F)
spe1 = mcnemar.test(spet, correct=F)

acc1 = rep(0, length(x1))
acc2 = rep(0, length(x2))
acc1[t1==1 & yy==1] = 1 ; acc1[t1==0 & yy==0] = 1
acc2[t2==1 & yy==1] = 1 ; acc2[t2==0 & yy==0] = 1
acc = table(acc1, acc2)
acc1 = mcnemar.test(acc, correct=F)

id = 1:length(x1)
count = rep(1, length(id))

n11 = sum(count[t1==1 & t2==1 & yy==1])
n12 = sum(count[t1==1 & t2==0 & yy==1])
n13 = sum(count[t1==0 & t2==1 & yy==1])
n14 = sum(count[t1==0 & t2==0 & yy==1])

n01 = sum(count[t1==1 & t2==1 & yy==0])
n02 = sum(count[t1==1 & t2==0 & yy==0])
n03 = sum(count[t1==0 & t2==1 & yy==0])
n04 = sum(count[t1==0 & t2==0 & yy==0])



a1 = n11+n12 ; b1 = n01 + n02 ; a2 = n11+n13 ; b2 = n01 + n03 ; 
chisq1 = (a1*b2 - a2*b1)^2/(a1*b2^2 - 2*n11*b1*b2 + a2*b1^2 + b1*a2^2 - 2*n01*a1*a2 + b2*a1^2)

a1 = n14+n12 ; b1 = n04 + n02 ; a2 = n14+n13 ; b2 = n04 + n03 ; 
chisq2 = (a1*b2 - a2*b1)^2/(a1*b2^2 - 2*n14*b1*b2 + a2*b1^2 + b1*a2^2 - 2*n04*a1*a2 + b2*a1^2)
p.value1 = 1-pchisq(chisq1,1)
p.value2 = 1-pchisq(chisq2,1)

sol = c(chisq1, p.value1)
sol2 = c(chisq2, p.value2)
names(sol) = c("Statistic_for_ppv", "p-value")
names(sol2) = c("Statistic_for_npv", "p-value")
statp = c(sol, sol2)
k=matrix(c(n11, n12, n13, n14, n01, n02, n03, n04), ncol = 2)
colnames(k) = c("Diseased", "Normal")
rownames(k) = c("+ +","+ -","- +","- -" )


ref1=print("Reference: On Comparisons of Sensitivity, Specificity and Predictive Value of a Number of Diagnostic Procedures")
ref2 = "Author(s): B. M. Bennett"
ref3 = "Source: Biometrics, Vol. 28, No. 3, (Sep., 1972), pp. 793-800"

ref = rbind(ref1, ref2, ref3)

cif = function(p1, alpha=0.05, n){
q1 = 1-p1 
uci = (p1+ (1/(2*n))*qnorm(1-alpha/2)^2 - qnorm(1-alpha/2)*sqrt((p1*q1/n)+(qnorm(1-alpha/2)^2/(4*n^2)))) / (1+qnorm(1-alpha/2)^2/n)
lci = (p1+ (1/(2*n))*qnorm(1-alpha/2)^2 + qnorm(1-alpha/2)*sqrt((p1*q1/n)+(qnorm(1-alpha/2)^2/(4*n^2)))) / (1+qnorm(1-alpha/2)^2/n)
return(c(uci, lci))
}


Diagnostic.statistic = rbind(dx.x1, dx.x2)
colnames(Diagnostic.statistic) = c("Sensitivity","Specificity", "PPV", "NPV", "Accuracy")

ci1 = cif(dxsR[1,1], 0.05, n11+n12+n13+n14) ; ci2 = cif(dxsR[2,1], 0.05, n11+n12+n13+n14)
ci3 = cif(dxsR[1,2], 0.05, n01+n02+n03+n04) ; ci4 = cif(dxsR[2,2], 0.05, n01+n02+n03+n04)

ci5 = cif(dxsR[1,3], 0.05, n11+n12+n01+n02) ; ci6 = cif(dxsR[2,3], 0.05, n11+n13+n01+n03)
ci7 = cif(dxsR[1,4], 0.05, n13+n14+n03+n04) ; ci8 = cif(dxsR[2,4], 0.05, n12+n14+n02+n04)

ci9 = cif(dxsR[1,5], 0.05, n11+n12+n13+n14+n01+n02+n03+n04) ; ci10 = cif(dxsR[2,5], 0.05, n11+n12+n13+n14+n01+n02+n03+n04)

Confidence.Interval = t(matrix(c(ci1, ci2, ci3, ci4, ci5, ci6, ci7, ci8, ci9, ci10), nrow =4 ))
colnames(Confidence.Interval) = c("x1.lower.limit", "x1.upper.limit", "x2.lower.limit", "x2.upper.limit")
rownames(Confidence.Interval) = c("sen", "spe", "PPV", "NPV", "accuracy")

structure(list(Diagnostic.statistic, Confidence.Interval, sent, sen1, spet, spe1, ref, k, statp, acc, acc1))
}
}




#text("Reference: On Comparisons of Sensitivity, Specificity and Predictive Value of a Number of Diagnostic Procedures")

clog <- function(x) log(x+0.5)

cfac <- function(x, breaks = NULL) {
 if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
 x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
 levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
 c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
 sep = "")
 return(x)
 }



AUC.estimate <- function(rdata, ind){

	### rdata <- c(y, kk$prob)
	### AUC estimate and standard error using boot(rdata, AUC.estimate, 10000)
	### library(ROCR); library(boot); library(vcd) 

	names(rdata) <- c("group", "predict.y")

	#rdata0 <- rdata[rdata$group==0,]
	#rdata1 <- rdata[rdata$group==1,]

	#rdata0.p <- rdata0[ind,]
	#rdata1.p <- rdata1[ind,]

	#combined <- rbind(rdata0.p, rdata1.p)

	combined <- rdata[ind,]

	predic <- prediction(combined$predict.y, combined$group)
	return(performance(predic, "auc")@y.values[[1]])
}

#library(ROCR); library(boot); library(vcd) 


#b1=  boot(rdata, AUC.estimate, 1000)
#boot.ci(b1, type=c("norm", "basic", "perc"))

#predv=CA19.9
#dx=type
#title1="CA19-9"




rocjh <- function(predv, dx, title1, maxval=0.8, maxppv=0.8, maxnpv=0.8, cutvalue=NULL) {
library(ROCR)


pred <- prediction( predv, dx)

perf1 <- performance(pred, "sens")
perf2 <- performance(pred, "spec")
perf3 <- performance(pred, "ppv")
perf4 <- performance(pred, "npv")
perf5 <- performance(pred, "acc")

perf6 <- performance(pred, "fpr")
perf7 <- performance(pred, "fnr")
perf8 <- performance(pred, "odds")

exp(-6.037) / (1+exp(-6.037))


dxvalue = cbind(perf1@x.values[[1]], 
perf1@y.values[[1]],perf2@y.values[[1]],perf3@y.values[[1]],perf4@y.values[[1]],perf5@y.values[[1]]
,perf6@y.values[[1]]
,perf7@y.values[[1]]
,perf8@y.values[[1]]
,perf1@y.values[[1]] / (1-perf2@y.values[[1]])
,(1-perf1@y.values[[1]]) / (perf2@y.values[[1]])
)
colnames(dxvalue) = c("cutpoint", "sen", "spe", "ppv", "npv", "acc", "fpr", "fnr", "odds", "LR+", "LR-")




measures = c('sens', 'spec', 'ppv', 'npv', 'acc')
win.graph(300,200)
par(mfrow=c(2,3))


cutoffset=rep(NA, 5)

for (measure in measures) {
     perf <- performance(pred, measure)

temp2=max(perf@y.values[[1]][perf@y.values[[1]]!="NaN"])
temp1=maxval
temp3 = maxppv
temp4 = maxnpv


kk=perf@x.values[[1]][perf@y.values[[1]]>=temp1]
maxkk2 = kk[kk!=Inf]
maxkk = max(maxkk2[is.na(maxkk2)==FALSE])
minkk2 = kk[kk!=Inf]
minkk = min(minkk2[is.na(minkk2)==FALSE])



npvtemp=perf@x.values[[1]][perf@y.values[[1]]>temp4]
npvtemp2 = npvtemp[npvtemp!=Inf]
npvtemp3 = max(npvtemp2[is.na(npvtemp2)==FALSE])

ppvtemp=perf@x.values[[1]][perf@y.values[[1]]>temp3]
ppvtemp2 = ppvtemp[ppvtemp!=Inf]
ppvtemp3 = min(ppvtemp2[is.na(ppvtemp2)==FALSE])

if(measure == 'sens'){
plot(perf,avg="vertical",spread.estimate="boxplot", main = title1 , 
xlab=paste("when ", title1, "= ", round(maxkk,3) , "\n", perf@y.name[[1]], ">= ", maxval))
#print(paste(perf@y.name[[1]], "=", max((temp1))
cutoffset[1]=maxkk
}
else if(measure == 'spec') {
plot(perf,avg="vertical",spread.estimate="boxplot", main = title1 , 
xlab=paste("when ", title1, "= ", round(minkk,3) , "\n", perf@y.name[[1]], ">= ", maxval))
cutoffset[2]=minkk
}
else if(measure == 'ppv'){
plot(perf,avg="vertical",spread.estimate="boxplot", main = title1 , 
xlab=paste("when ", title1, "= ", round(ppvtemp3,3) , "\n", perf@y.name[[1]], ">= ", temp3))
cutoffset[3]=ppvtemp3
}
else if(measure == 'npv'){
plot(perf,avg="vertical",spread.estimate="boxplot", main = title1 , 
xlab=paste("when ", title1, "= ", round(npvtemp3,3) , "\n", perf@y.name[[1]], ">= ", temp4))
cutoffset[4]=npvtemp3
}

else if(measure == 'acc'){

maxacc=max(perf@y.values[[1]])
optimalacc=min(perf@x.values[[1]][perf@y.values[[1]]==maxacc])
cutoffset[5]=optimalacc
plot(perf,avg="vertical",spread.estimate="boxplot", main = title1 , 
xlab=paste("when ", title1, "= ", round(optimalacc,3) , "\n", perf@y.name[[1]], "'s ", round(maxacc,3)))

}
}


perf <- performance(pred,"tpr","fpr")
k=performance(pred, "auc")

ss= 1-perf@x.values[[1]] + perf@y.values[[1]]
max(ss)
print(max(ss))

if(is.null(cutvalue)){
cut=perf@alpha.values[[1]][ss==max(ss)]
}
else
{
cut = as.numeric(cutvalue)
}

plot(perf, main=paste("ROC curve for",title1))
abline(0,1)
legend(0.45, 0.2, paste("C-statistic =", round(k@y.values[[1]],3), "\n", "sen+spe cut-off = ", round(cut,3) ))



measures = c('sens', 'spec', 'ppv', 'npv', 'acc')
for (measure in measures) {
     perf <- performance(pred, measure)
print(c("cut point= ", cut, paste( measure,"= " , round(perf@y.values[[1]][perf@x.values[[1]]==cut],3))))

}
measures = c('sens', 'spec', 'ppv', 'npv', 'acc')
measuress = cbind(measures, 1:5)
for (measure in measures) {
     perf <- performance(pred, measure)
print(c("cut point= ", optimalacc, paste( measure,"= " , round(perf@y.values[[1]][perf@x.values[[1]]==optimalacc],3))))

}
structure(list(dxvalue,cutoffset))
}

#?Լ? ??






friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
	# formu is a formula of the shape: 	Y ~ X | block
	# data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
 
	# Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
 
 
	# Loading needed packages
	if(!require(coin))
	{
		print("You are missing the package 'coin', we will now try to install it...")
		install.packages("coin")		
		library(coin)
	}
 
	if(!require(multcomp))
	{
		print("You are missing the package 'multcomp', we will now try to install it...")
		install.packages("multcomp")
		library(multcomp)
	}
 
	if(!require(colorspace))
	{
		print("You are missing the package 'colorspace', we will now try to install it...")
		install.packages("colorspace")
		library(colorspace)
	}
 
 
	# get the names out of the formula
	formu.names <- all.vars(formu)
	Y.name <- formu.names[1]
	X.name <- formu.names[2]
	block.name <- formu.names[3]
 
	if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	# In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
 
	# Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
 
	# stopping in case there is NA in the Y vector
	if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
 
	# make sure that the number of factors goes with the actual values present in the data:
	data[,X.name ] <- factor(data[,X.name ])
	data[,block.name ] <- factor(data[,block.name ])
	number.of.X.levels <- length(levels(data[,X.name ]))
	if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
 
	# making the object that will hold the friedman test and the other.
	the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons	
						   teststat = "max",
						   xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
						   ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
						)
	# if(to.print.friedman) { print(the.sym.test) }
 
 
	if(to.post.hoc.if.signif)
		{
			if(pvalue(the.sym.test) < signif.P)
			{
				# the post hoc test
				The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
 
 
				# plotting
				if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
 
				if(to.plot.parallel)
				{
					X.names <- levels(data[, X.name])
					X.for.plot <- seq_along(X.names)
					plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
 
					if(color.blocks.in.cor.plot) 
					{
						blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
					} else {
						blocks.col <- 1 # black
					}					
 
					data2 <- data
					if(jitter.Y.in.cor.plot) {
						data2[,Y.name] <- jitter(data2[,Y.name])
						par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"				
					} else {
						par.cor.plot.text <- "Parallel coordinates plot"
					}				
 
					# adding a Parallel coordinates plot
					matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
									 direction="wide")[,-1])  , 
							type = "l",  lty = 1, axes = FALSE, ylab = Y.name, 
							xlim = plot.xlim,
							col = blocks.col,
							main = par.cor.plot.text)
					axis(1, at = X.for.plot , labels = X.names) # plot X axis
					axis(2) # plot Y axis
					points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
				}
 
				if(to.plot.boxplot)
				{
					# first we create a function to create a new Y, by substracting different combinations of X levels from each other.
					subtract.a.from.b <- function(a.b , the.data)
					{
						the.data[,a.b[2]] - the.data[,a.b[1]]
					}
 
					temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
									 direction="wide") 	#[,-1]
					wide.data <- as.matrix(t(temp.wide[,-1]))
					colnames(wide.data) <- temp.wide[,1]
 
					Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
					names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
 
					the.ylim <- range(Y.b.minus.a.combos)
					the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
					is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
 
					boxplot(Y.b.minus.a.combos,
						names = names.b.minus.a.combos ,
						col = is.signif.color,
						main = "Boxplots (of the differences)",
						ylim = the.ylim
						)
					legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
					abline(h = 0, col = "blue")
 
				}
 
				list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
				if(to.print.friedman) {print(list.to.return)}				
				return(list.to.return)
 
			}	else {
					print("The results where not significant, There is no need for a post hoc test")
					return(the.sym.test)
				}					
	}
 
# Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
# http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
} 


 

clusteredROC <- function (data1){

##Hmisc library ?? ??ġ?ؾ? ?մϴ?. 
##Hmisc library ?? survival, splines library?? ?ʿ??? ?մϴ?. 

##data?? ?Էº????? ?ݵ??? (??��????, ????????, ???ܺ???)?? ?־??? ?մϴ?. 
##???ܺ????? subject id?? ???? ?Ǵµ?, ??��?? 1???? ???????? ?־????մϴ?. 
##???? 4???? 5???? ?ڷḦ ?????? ?ִٸ?, id = c(1,1,2,3,4) ó???Ǿ? ?մϴ?. 
##c(1,4,5,7,7) ?̷??? ?߰??? 2?? ???????? ?ȵǿ?. ?Ф?

dd = na.omit(data.frame(data1))
disease=dd[,1]
x=dd[,2]
cid=dd[,3]

###if(max(data1[,3])>length(data1[,3]))
###{
###cid=rank(dd[,3], ties.method="min")
###}
###else
###cid=dd[,3]
###}
###dd1 = data.frame(cbind(disease, x, cid))

fu1 = function(X, Y){
if (X<Y) return(1)
if (X==Y) return(0.5)
if (X>Y) return(0)
}

xij = na.omit(dd[disease==1,-1])
yij = na.omit(dd[disease==0,-1])
si = as.vector(table(cid))
I = length(table(cid))
M = sum(table(xij[,2]))
N = sum(table(yij[,2]))
I10 = length(table(xij[,2]))  
I01 = length(table(yij[,2]))

iauc_pop0= matrix(NA, ncol=M, nrow=N)
iauc_pop1= t(matrix(xij[,1], ncol=length(yij[,1]), nrow = length(xij[,1])))
iauc_pop2= (matrix(yij[,1], ncol=length(xij[,1]), nrow = length(yij[,1])))

dim(iauc_pop0)
dim(iauc_pop2)

iauc_pop0[iauc_pop1<iauc_pop2] = 0
iauc_pop0[iauc_pop1==iauc_pop2] = 0.5
iauc_pop0[iauc_pop1>iauc_pop2] = 1

theta=sum(iauc_pop0, na.rm=TRUE)/(sum(!is.na(iauc_pop0)))
iauc2 = wilcox.test(xij[,1], yij[,1])$statistic / (sum(!is.na(iauc_pop0)))
wilcoxtest1 <- wilcox.test(xij[,1], yij[,1])$statistic
wilcoxtest2 <- wilcox.test(xij[,1], yij[,1])$p.value 


V10 = (margin.table(iauc_pop0, 2))/N
V01 = (margin.table(iauc_pop0, 1))/M

V10.temp = cbind(V10, xij[,2])
V01.temp = cbind(V01, yij[,2])

library(Hmisc)

wtb1 = wtd.table(V10.temp[,2], weights=V10)
wtb2 = wtd.table(V01.temp[,2], weights=V01)
kk1 = wtb1$sum.of.weights-table(V10.temp[,2])*theta
kk2 = wtb2$sum.of.weights-table(V01.temp[,2])*theta

s10 = (I10/((I10-1)*M)) * sum(as.vector(kk1^2))
s01 = (I01/((I01-1)*N)) * sum(as.vector(kk2^2))

names(kk1)

s11.temp = matrix(0, ncol = 2, nrow = I)
s11.temp[as.numeric(names(kk1)),1] = kk1
s11.temp[as.numeric(names(kk2)),2] = kk2
#s11.temp[,1] * s11.temp[,2]

s11 = I/(I-1) * sum(s11.temp[,1] * s11.temp[,2])

vartheta = s10/M+s01/N+s11*2/(M*N)
setheta = sqrt(vartheta)

#all.names=c("AUC", "I","I10","I01","M","N","s10","s01","s11","se_AUC")

#c(theta, I, I10, I01, M, N, s10, s01, s11, setheta)

e1 <- new.env()
e1$auc <- theta
e1$I <- I
e1$I10 <- I10
e1$I01 <- I01
e1$M <- M
e1$N <- N
e1$s10 <- s10
e1$s01 <- s01
e1$s11 <- s11
e1$se <- setheta
e1$var <- vartheta
e1$kk1 <- kk1
e1$kk2 <- kk2
e1$wilcox.statistic <- wilcoxtest1
e1$wilcox.pval <- wilcoxtest2
structure(as.list(e1))
}

compareCAUC = function(c1, c2) {
#c1, c2 ?? object?? ?ݵ??? clusteredAUC ?Լ??? ???? ?????? ??ü?̾??? ?մϴ?. 
cc1 = c1
cc2 = c2
if(cc1$I01 != cc2$I01 |cc1$I10 != cc2$I10 | cc1$I != cc2$I | cc1$M != cc2$M | cc1$N != cc2$N){
	print("These objects is not constructed from the correlated sample")
}
else
{

I01 = cc1$I01
I10 = cc1$I10
I = cc1$I
M = cc1$M
N = cc1$N

	s10_12 = (I10/((I10-1)*M)) * sum(c1$kk1*c2$kk1)
	s01_12 = (I10/((I10-1)*N)) * sum(c1$kk2*c2$kk2)

	c1.temp = matrix(0, ncol = 2, nrow = I)
	c1.temp[as.numeric(names(c1$kk1)),1] = c1$kk1
	c1.temp[as.numeric(names(c1$kk2)),2] = c1$kk2

	c2.temp = matrix(0, ncol = 2, nrow = I)
	c2.temp[as.numeric(names(c2$kk1)),1] = c2$kk1
	c2.temp[as.numeric(names(c2$kk2)),2] = c2$kk2

	s11_12 = (I/((I-1))) * sum(c1.temp[,1]*c2.temp[,2])
	s11_21 = (I/((I-1))) * sum(c1.temp[,2]*c2.temp[,1])

cov_12 = s10_12/M + s01_12/N + s11_12/(M*N) + s11_21/(M*N)
var_12 = c1$var + c2$var -2*cov_12
se_12 = sqrt(var_12)
diff = (c1$auc - c2$auc)
zvalue = (c1$auc - c2$auc)/se_12
onesidepval = 1-pnorm(abs(zvalue))
twosidepval = 2*onesidepval

e1 <- new.env()
e1$cov <- cov_12
e1$var <- var_12
e1$se <- se_12
e1$statistic <- zvalue
e1$p.value <- twosidepval
e1$difference <- diff
structure(as.list(e1))
}
}


sauc.ci<-function(rdata, ind, time1, tmax){
### rdata <- c("survival.status","survival.time", "ER", "Ki670", "Ki67q")
### AUC estimate and standard error using boot(rdata, AUC.estimate, 10000)
### library(ROCR); library(boot); library(vcd) 
library(ROCR); library(boot); library(vcd) 
names(rdata) <- c("survival.status","survival.time", "ER", "Ki670", "Ki67q")
combined <- rdata[ind,]

survival.time<-combined$survival.time
survival.status <- combined$survival.status
survival.time[survival.time<1]=1

Srv = Surv(survival.time, survival.status)

fit00 <- coxph(Srv ~ combined$ER+factor(combined$Ki67q) , method="breslow")
eta<- fit00$linear.predictor

ROC.CC=risksetROC(Stime=survival.time, status=survival.status, marker=eta, predict.time=time1, method="Cox", plot=FALSE)
#AUC.CC30=risksetAUC(Stime=survival.time, status=survival.status, marker=eta, method="Cox",main="AUC", tmax=tmax, plot=FALSE)
return(ROC.CC$AUC)
}


scindex.ci<-function(rdata, ind, time1=NULL, tmax=NULL){
### rdata <- c(y, kk$prob)
### AUC estimate and standard error using boot(rdata, AUC.estimate, 10000)
### library(ROCR); library(boot); library(vcd) 
library(ROCR); library(boot); library(vcd) 
names(rdata) <- c("survival.status","survival.time", "ER", "Ki670", "Ki67q")
combined <- rdata[ind,]

survival.time<-combined$survival.time
survival.status <- combined$survival.status
survival.time[survival.time<1]=1

Srv = Surv(survival.time, survival.status)

fit00 <- coxph(Srv ~ combined$ER+factor(combined$Ki67q) , method="breslow")
eta<- fit00$linear.predictor

#ROC.CC=risksetROC(Stime=survival.time, status=survival.status, marker=eta, predict.time=time1, method="Cox", plot=FALSE)
AUC.CC30=risksetAUC(Stime=survival.time, status=survival.status, marker=eta, method="Cox",main="AUC", tmax=tmax, plot=FALSE)
return(AUC.CC30$Cindex)
}

##
## usage:
##





matchdata = function(raw, matchobj){
caseid = as.numeric(names(table(matchobj$index.treated)))
case2 = raw[caseid,]
control2 = raw[matchobj$index.control,]

npair=1/matchobj$weights[1]
mdata = cbind(rbind(case2, control2), c(1:length(caseid),sort(rep(1:length(caseid),npair))))
names(mdata) = c(names(raw), "matchto")
mdata = mdata[order(mdata$matchto),]
return(mdata)
}

freq.table<-function(t1){
#t2 = matrix(t1, nrow = dim(t1)[1], ncol = dim(t1)[2])
temp1 <- as.matrix(t1)
odertemp = rep(1:dim(temp1)[1], 4)
#colnames(temp1) = 1:length(colnames(temp1))
p1 = prop.table(temp1)
p2 = prop.table(temp1, 1)
p3 = prop.table(temp1, 2)

rr1 = rbind(temp1, p1, p2, p3)
rr1 = rr1[order(odertemp),]

name1 = matrix(NA, dim(temp1)[1], ncol = 4)
for(i  in 1:dim(temp1)[1]){
name1[i,] = c(rownames(t1)[i], "percent", "row.percent", "col.percent")
}
rr1 <- as.matrix(rr1)
#colnames(rr1)=colnames(t1)
rownames(rr1)=c(t(name1))
rr1<-as.array(rr1)
row.sum = margin.table(rr1,1)
sum2 = margin.table(temp1,2)
col.sum = c(sum2, sum(sum2))
col.percent = c(prop.table(sum2), sum(prop.table(sum2)))
rr2 = cbind(rr1, row.sum)
rr3 = rbind(rr2, col.sum, col.percent)
rr4 = round(rr3, 4)

rr4[rownames(rr3)=="col.percent", colnames(rr3)=="row.sum"]=NA
return(rr4)
}





'Bland.Altman' <- function(x,y,alpha=.05,rep.meas=FALSE,subject,...){
#**********************************************************************
#* Construct a Bland Altman Plot
#* 1. Set a few constants
#* 2. Calculate mean difference
#* 3. Calculate difference standard deviation
#* 4. Calculate upper and lower confidence limits
#* 5. Make Plot
#**********************************************************************

#*** 1. Set a few constants
  z <- qnorm(1-alpha/2)  ## value of z corresponding to alpha
  d <- x-y               ## pair-wise differences
  m <- (x+y)/2           ## pair-wise means

#*** 2. Calculate mean difference
  d.mn <- mean(d,na.rm=TRUE)

#*** 3. Calculate difference standard deviation
  if(rep.meas==FALSE){ d.sd=sqrt(var(d,na.rm=TRUE)) }
  else{

    #*** 3a. Ensure subject is a factor variable
    if(!is.factor(subject)) subject <- as.factor(subject)

    #*** 3b. Extract model information
    n <- length(levels(subject))      # Number of subjects
    model <- aov(d~subject)           # One way analysis of variance
    MSB <- anova(model)[[3]][1]       # Degrees of Freedom
    MSW <- anova(model)[[3]][2]       # Sums of Squares

    #*** 3c. Calculate number of complete pairs for each subject
    pairs <- NULL
    for(i in 1:length(levels(as.factor(saubject)))){
      pairs[i] <- sum(is.na(d[subject==levels(subject)[i]])==FALSE)
    }
    Sig.dl <- (MSB-MSW)/((sum(pairs)^2-sum(pairs^2))/((n-1)*sum(pairs)))
    d.sd <- sqrt(Sig.dl+MSW)
  }

#*** 4. Calculate lower and upper confidence limits
  ucl <- d.mn+z*d.sd
  lcl <- d.mn-z*d.sd

#*** 5. Make Plot
  plot(m, d,abline(h=c(d.mn,ucl,lcl)),  ...)
  values <- round(cbind(lcl,d.mn,ucl),4)
  colnames(values) <- c("LCL","Mean","UCL")
  if(rep.meas==FALSE) Output <- list(limits=values,Var=d.sd^2)
    else Output <- list(limits=values,Var=Sig.dl)
  return(Output)
}


 hosmerlem <-
 function (y, yhat, g = 10) 
 {
     cutyhat <- cut(yhat, breaks = unique(quantile(yhat, probs = seq(0, 
         1, 1/g))), include.lowest = T)
     obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
     expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
     chisq <- sum((obs - expect)^2/expect)
     P <- 1 - pchisq(chisq, g - 2)
     c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
 }
#??????
#hosmerlem(blocker, glmfit1$fitted, g=10)




bfunc	 <- function(mb1){
matpop1<-matrix(NA, ncol = 12, nrow = length(mb1$BeforeMatching))
for(i in 1:length(mb1$BeforeMatching)){
matpop1[i,1] <- (mb1$BeforeMatching)[[i]]$mean.Tr
matpop1[i,2] <- (mb1$BeforeMatching)[[i]]$mean.Co
matpop1[i,3] <- (mb1$AfterMatching)[[i]]$mean.Tr
matpop1[i,4] <- (mb1$AfterMatching)[[i]]$mean.Co
matpop1[i,5] <- (mb1$BeforeMatching)[[i]]$sdiff
matpop1[i,6] <- (mb1$AfterMatching)[[i]]$sdiff
matpop1[i,7] <- (mb1$BeforeMatching)[[i]]$var.Tr
matpop1[i,8] <- (mb1$BeforeMatching)[[i]]$var.Co
matpop1[i,9] <- (mb1$AfterMatching)[[i]]$var.Tr
matpop1[i,10] <- (mb1$AfterMatching)[[i]]$var.Co
matpop1[i,11] <- (mb1$BeforeMatching)[[i]]$var.ratio
matpop1[i,12] <- (mb1$AfterMatching)[[i]]$var.ratio
}
colnames(matpop1) = c("BM.mean.Tr", "BM.mean.Co", "AM.mean.Tr", "AM.mean.Co", 
"BM.mean.sdiff", "AM.mean.sdiff", "BM.var.Tr","BM.var.Co", "AM.var.Tr", "AM.var.Co", "BM.var.ratio", "AM.var.ratio")
return(matpop1)
}
#rownames(matpop1) = c("age", levels(T_RE)[-1], levels(N_RE)[-1], levels(M_RE)[-1], levels(year)[-1])

bfunc.original = function(mb1){
matpop1<-matrix(NA, ncol = 6, nrow = length(mb1$BeforeMatching))
for(i in 1:length(mb1$BeforeMatching)){
matpop1[i,1] <- (mb1$BeforeMatching)[[i]]$mean.Tr
matpop1[i,2] <- (mb1$BeforeMatching)[[i]]$mean.Co
matpop1[i,3] <- (mb1$BeforeMatching)[[i]]$sdiff
matpop1[i,4] <- (mb1$BeforeMatching)[[i]]$var.Tr
matpop1[i,5] <- (mb1$BeforeMatching)[[i]]$var.Co
matpop1[i,6] <- (mb1$BeforeMatching)[[i]]$var.ratio
}
colnames(matpop1) = c("mean.Tr", "mean.Co", "mean.sdiff", "var.Tr","var.Co", "var.ratio")
return(matpop1)
}






#
#var2 ?? ???????ָ? ???? ??, var2???ָ? ?Ѿ? ?????? pairwise comaprison ?ϴ? ?Լ???.

mf <- function(var1, var2) {
vv <- rle(sort(var2))$value
#vv <- unique(var2)
pg <- combn(vv, 2)
keke <- vector("list", dim(pg)[2])

for(i in 1: length(keke))
keke[[i]] = table( var1[var2 %in% pg[,i]],var2[var2 %in% pg[,i]])
mat1 <- matrix(NA, ncol = 7, nrow= length(keke))

for(i in 1:length(keke)){
mat1[i,1:2] = pg[,i]
mat1[i,3] = tryCatch(chisq.test(keke[[i]])$p.value, error=function(e) {print(NA)}, finally="")
mat1[i,4] = tryCatch(fisher.test(keke[[i]])$p.value, error=function(e) {print(NA)}, finally="")
mat1[i,5] <- sum(chisq.test(table(keke[[i]]))$expected<5)>0
}
mat1[,6] = p.adjust(mat1[,4], method = "bonferroni")
mat1[,7] = p.adjust(mat1[,4], method = "fdr")
colnames(mat1) = c("group1", "group2", "chisq.p", "fisher.p", "expected", "bon", "fdr")
l1 <- list(keke, mat1)
names(l1) = c("table", "raw.p")
structure(l1)
}



#????????, ICC ?????? ??.
fi.test = function(rho1, rho2, k1, k2, n1, n2, rho12=0){

z1 = 0.5*log((1+(k1-1)*rho1)/(1-rho1))
z2 = 0.5*log((1+(k2-1)*rho2)/(1-rho2))

theta = 0.5*log((1+(k1-1)*rho1)/(1+(k2-1)*rho2))

var1 <-ifelse(k1==2, 1/(n1-3/2), k1/(2*(k1-1)*(n1-2)))
var2 <-ifelse(k2==2, 1/(n2-3/2), k2/(2*(k2-1)*(n2-2)))

cov1 <- (k1*k2*rho12^2) /(2*n1 * (1+(k1-1)*rho1) *(1+(k2-1)*rho2))

V <- var1+var2 -2*cov1

Tz = ((z1 - z2) - theta) / sqrt(V)
p.value = 2*(1-pnorm(abs(Tz)))
k = list(c(Tz, p.value))
return(k)
}



hl.ext2<-function(p,y,g=10, df=g-2)
{
matres	<-matrix(NA,nrow=g,ncol=5)
sor	<-order(p)
p	<-p[sor]
y	<-y[sor]
groep	<-cut2(p,g=g)									#g more or less equal sized groups

len		<-tapply(y,groep,length)					#n per group
sump	<-tapply(p,groep,sum)						#expected per group
sumy	<-tapply(y,groep,sum)						#observed per group
meanp	<-tapply(p,groep,mean)						#mean probability per group
meany	<-tapply(y,groep,mean)						#mean observed per group
matres	<-cbind(len,meanp,meany, sump, sumy)
contr<-((sumy-sump)^2)/(len*meanp*(1-meanp))		#contribution per group to chi square
chisqr<-sum(contr)									#chi square total
pval<-1-pchisq(chisqr,df)							#p-value corresponding to chi square with df degrees of freedom 
cat("\nChi-square",chisqr," p-value", pval,"\n")
dimnames(matres)	<-list(c(1:g),Cs(n,avg(p),avg(y), Nexp, Nobs))
result  <- list(table(groep), matres,chisqr,pval) 
}



uni1 <- function(raw, catnum=NULL, connum=NULL, exceptnum=NULL, groupn) {

#exceptnum �� raw ???? ?????ͼ¿??? ?м??????? ?ƴ? ???? ?ѹ????͸? ???մϴ?. 
#groupn �� raw ???? ?????ͼ¿??? ????????�� ǥ???? group?? ��ġ(integer) ?? ??Ÿ?��ϴ?.

if(is.null(exceptnum)) {
anal2 = raw
}
else {
anal2 = raw[,-exceptnum]
}

dim(anal2)
dim(covariate)

covariate <- anal[,-c(exceptnum,groupn)]
group <- anal[,groupn]

dim1 = dim(anal2)
dataname <- names(anal2)


cat.or.con = rep(NA, length=dim(covariate)[2])
for (i in 1:dim(covariate)[2]){
cat.or.con[i] = is.factor(covariate[,i])
}

catnum <-sum(c(cat.or.con))
connum <- length(cat.or.con) - catnum

cat1 = vector("list", length = catnum)
con1  = vector("list", length = connum)

names(cat1) = names(covariate)[cat.or.con]
names(con1) = names(covariate)[(1-cat.or.con)==1]

for (i in 1:length(cat1)) cat1[[i]] = covariate[,cat.or.con][,i]
for (i in 1:length(con1)) con1[[i]] = covariate[,(1-cat.or.con)==1][,i]

g.level = sort(unique(group))
lug1 <- length(unique(group))

conmat1 <- matrix(NA, ncol = 11, nrow = length(con1)*lug1)
fupop <- vector("list", length=lug1)
for(i in 1:lug1) fupop[[i]] <- seq(i,length(con1)*lug1, by = lug1)
mgroup = group

for(j in 1: lug1) {
for (i in 1:length(fupop[[j]])){
conmat1[fupop[[j]][i],2:11] <- basic(con1[[i]][mgroup==g.level[j]])
}
}

conmat1[,1] = rep(g.level, 3)
colnames(conmat1) = c("class",names(basic(con1[[j]])))
rownames(conmat1) = rep(names(con1), each=lug1)


mat1 <- matrix(NA, ncol=2, nrow=length(con1))
if(lug1<2){
print("group variable have only 1 class")
}
else if(lug1==2){
for(i in 1:length(con1)){
mat1[i,1] <- t.test(con1[[i]]~mgroup, correct=F)$p.value
mat1[i,2] <- wilcox.test(con1[[i]]~mgroup, correct=F)$p.value
}
}
else{
for(i in 1:length(con1)){
mat1[i,1] <- anova(aov((con1[[i]]~mgroup)))[5][[1]][1]
mat1[i,2] <- kruskal.test(con1[[i]]~mgroup)$p.value
}
}
rownames(mat1) = names(con1)
colnames(mat1) = c("parametric", "non-parametric")
mat1

freqlist <- vector("list", length=length(cat1))
for (i in 1:length(cat1)){
freqlist[[i]] <- (freq.table(table(cat1[[i]], mgroup)))
}
names(freqlist) = names(cat1)

mat2 <- matrix(NA, ncol=3, nrow=length(cat1))
for(i in 1:length(cat1)){
mat2[i,3] <- sum(chisq.test(table(cat1[[i]], mgroup), correct=F)$expected<5)>0
mat2[i,1] <- chisq.test(table(cat1[[i]], mgroup), correct=F)$p.value
mat2[i,2] <- tryCatch(fisher.test(table(cat1[[i]], group), workspace=200000000)$p.value, error=function(e) { print(NA)}, finally = "")
}

rownames(mat2) = names(cat1)
colnames(mat2) = c("Chi-test", "exact-test", "expected")
answer <- list(conmat1, mat1, freqlist, mat2)
names(answer) = c("descriptive_statistic", "test_continuous", "frequency_table", "test_categorical")
structure(answer) 
}



jhforestplot <- function(or)  {
x = rownames(or)
y = or[,1]
ylo = or[,2]
yhi = or[,3]
orr <- data.frame(x,y,ylo,yhi)
p <- ggplot(orr, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
geom_pointrange(shape=1, lty=.1, size=.5)+coord_flip()+geom_hline(yintercept=1, aes(x=0, shape=1), lty=2)+xlab('variable')+ylab('Odds ratio')+
geom_errorbar(size=0.2, linetype=1, width=0.3, color=1) +
theme_bw()
colnames(orr) <- c("variablename","or", "lower", "upper")
structure(list(orr,p))
}

matchit.data <- function(matchobj, rawdata.orig){

orig.id <- 1:dim(rawdata.orig)[1]
matchto <- matchobj$match.matrix[!is.na(matchobj$match.matrix[,1]),]
ps <- matchobj$distance
psdata <- data.frame(orig.id,rawdata.orig, ps)

if( !is.null(rawdata.orig$id) & all(orig.id==rawdata.orig$id)){

#1:1?? ????
	if(is.vector(matchto)){
		matchpop1 <- match.data(matchobj)
		case1 <- psdata [names(matchto),]
		cont1 <- psdata [matchto,]
		matchid <- rep(1:dim(case1)[1], 2)
		list1 <- list(case1, cont1)
		matchpop2.temp <- rbind(cont1, case1)
		matchpop2 <- data.frame(matchpop2.temp, matchid)
		last.match <- matchpop2[order(matchpop2$matchid),]
		print(paste(c("subject N=","#variable="),dim(last.match)))
		print("matching data variable names are ")
            print(names(last.match))
	}
	else {
#1:n?? ????
		matchid <- 1:dim(matchto)[1]
		matchpop1 <-match.data(matchobj)
		names(matchpop1)

		contlist <- vector("list", length= dim(matchobj$match.matrix)[2])
		casedata1 <- data.frame(psdata[rownames(matchto),], matchid)
	        casedata1 <- casedata1[!is.na(casedata1$ps),]

##
i=1

			for(i in 1:length(contlist )){
				oodata <- data.frame(psdata[matchto[,i],], matchid)
				contlist[[i]] <- oodata[!is.na(oodata$orig.id),]
			}

		names(contlist) <- paste("control.data", 1:length(contlist))
		df <- do.call("rbind", contlist)
		matchpop2 <- rbind(casedata1, df)
#dim(matchpop1)
#dim(matchpop2)
		matchpop1$id - matchpop2$orig.id
		matchpop1.temp <- matchpop1[order(matchpop1$id),]
		matchpop2.temp <- matchpop2[order(matchpop2$orig.id),]
		#names(matchpop1.temp)
		#names(matchpop2.temp)
		mweight <- matchpop1.temp$weights
		matchpop3 <- data.frame(matchpop2.temp, mweight)
		last.match <- (matchpop3[order(matchpop3$matchid),])
		print(paste(c("subject N=","#variable="),dim(last.match)))
		print("matching data variable names are ")
            print(names(last.match))
	}
return(last.match)
}
else print("Check Identification number called 'id' in original data set, arrange 'id' to subjects 1:N")
}






spss.f <- function(today, orig.data, current.time="15:00:00", time1="00:00:01", time2= "23:59:59") {
library(chron)

event.date <- chron(dates=as.character(orig.data$date), times=as.character(orig.data$time), format=c('y-m-d','h:m:s'), origin = c(month=7, day=1, year=2014))
ref.time<- chron(dates=today, times= current.time, format=c('y-m-d','h:m:s'), origin = c(month=7, day=1, year=2014))
start1 <- chron(dates=today, times= time1, format=c('y-m-d','h:m:s'), origin = c(month=7, day=1, year=2014))
end1 <- chron(dates=today, times= time2, format=c('y-m-d','h:m:s'), origin = c(month=7, day=1, year=2014))

dd <- orig.data[(start1<event.date & event.date < end1),]
tt1 <- table(dd$Host, dd$Trans)
today.event.date <- chron(dates=as.character(dd$date), times=as.character(dd$time), format=c('y-m-d','h:m:s'), origin = c(month=7, day=1, year=2014))
today.people.n <- length(table(factor(dd$Host)))

plot(dd$NumKey~today.event.date, type="o", ylim=c(0,15));grid(32)

#mean(dd$NumKey~today.event.date)
copy.over15.time <- today.event.date[dd$NumKey==15]

#plot(dd$NumKey~dd$time)

#boxplot(dd$NumKey~cut(today.event.date , breaks=24*6))
#names(dd)

banned.per.time <- tryCatch(table(dd$Host[dd$Trans==1], cut(today.event.date[dd$Trans==1] , breaks=24*6)), error=function(e) print("Everyone can use SPSS"), finally="")
banned.per.time.pool <- tryCatch(apply(banned.per.time, 2, sum), error=function(e) print(""), finally="")
 
if (all(dd$Trans != 1)) { today.banned.user=0 } else{ today.banned.user=as.character(rownames(table(dd$Host, dd$Trans))[table(dd$Host, dd$Trans)[,2]>0]) }

max.banned <- tryCatch(max(banned.per.time.pool), error=function(e) print(NA), finally="")
mean.banned <- tryCatch(mean(banned.per.time.pool), error=function(e) print(NA), finally="")

tt <- table(orig.data$Host, orig.data$Trans)
current.user <- rownames(tt)[table(orig.data$Host, orig.data$Trans)[,1]>table(orig.data$Host, orig.data$Trans)[,3]]
bb <- orig.data[orig.data$Host %in% current.user,]
cc <- bb[order(bb$Host),]
cc$Host = factor(cc$Host)

ta <- table(cc$Host)
ta1 <- vector('list', length(ta))
names(ta1) <- names(ta)
for(i in 1:length(ta)) ta1[[i]] <- cc[cc$Host==names(ta1)[i] & cc$Trans==0,]
for(i in 1:length(ta)) ta1[[i]] <- ta1[[i]][order(ta1[[i]]$date, ta1[[i]]$time),]
#ta1[[i]]
#ta1[[i]][dim(ta1[[i]])[1],]
ta2 <- vector('list', length(ta))
for(i in 1:length(ta)) ta2[[i]] <- ta1[[i]][dim(ta1[[i]])[1],]
ta3 <- do.call('rbind', ta2)

today.banned.user.n.temp = ifelse(today.banned.user==0, 0, length(today.banned.user))
today.banned.user.n <- mean(today.banned.user.n.temp)

kk <- list(today.people.n ,today.banned.user.n ,today.banned.user, ta3, banned.per.time.pool,max.banned, mean.banned, copy.over15.time)
names(kk) = c("Today people N for SPSS","Today banned user #n","Today banned user", "current.user",  "banned pool", "max.banned", "average.banned", "over 15 copy time")
structure(kk)
}



whattimef <- function(date.from.spssf) paste(as.integer(I(24*date.from.spssf )), '??', 60*(24*date.from.spssf-as.integer(I(24*date.from.spssf ) ) ) , '??' )


#
#spss.f("2014-07-01", aa1)
#spss.f("2014-07-02", aa2)
#spss.f("2014-07-03", aa3)
#spss.f("2014-07-04", aa4)
#
# SPSS FUNCTION ??????. 
# ???? ?α??ε? ??????�� ???��?, '?��ó?¥', '?��? ??????'
#
# ??ǥ ???? ??????Ȳ�� ��???Ϸ��?, '??ǥ??¥', '??ǥ??¥ ?״?��?????? ?????? ??????'
#
# whattimef ?Լ??? ????, banned pool ?? ?ð?�� ?? ?? ?ִ?. banned pool ?? ?????? ��???κ?�� 7?? x ??�� ??Ÿ????.
#





choci.f.logistic <- function(logis1, x.title="predictor", y.title="Primary endpoint", main.t = ""){
library(plotrix)
conf.int1 <- cbind(exp(summary(logis1)$coef[,1]) 
, exp(summary(logis1)$coef[,1]-1.96*summary(logis1)$coef[,2])
, exp(summary(logis1)$coef[,1]+1.96*summary(logis1)$coef[,2])  )
oos = conf.int1
oos[1,] = 1
print(oos)
oos2 <- ifelse(oos==Inf, oos[oos[,3]==Inf,1], oos)
zz <- rownames(summary(logis1)$coef)
plot(1:(length(zz)), oos2[,1], xlim=c(0, length(zz)+2), ylim=c(0, max(oos2)+1), type="l", 
xlab=paste("cut(",x.title, ", quantile(",x.title,"), include.lowest=T)"), ylab="Hazard ratio & CI", main=main.t, xaxt='n')
axis(1, c("Reference level", zz), at=1:(length(zz)+1))
plotCI(oos2[,1], ui=oos2[,3], li=oos2[,2], , xlim=c(0, length(zz)+1), ylim=c(0, max(oos2)+1), add=T)
library(car)
legend("topright", paste("p-value =", round(Anova(logis1, 3)[1,3],4) ) )
}







choci.f <- function(cox1, x.title="predictor", y.title="Primary endpoint", main.t = ""){
library(plotrix)
oos <- rbind(c(1,1,1,1),summary(cox1)$conf.int)
zz <- rownames(summary(cox1)$coef)
plot(1:(length(zz)+1), oos[,1], xlim=c(0, length(zz)+2), ylim=c(0, max(oos)+1), type="l", 
xlab=paste("cut(",x.title, ", quantile(",x.title,"), include.lowest=T)"), ylab="Hazard ratio & CI", main=main.t, xaxt='n')
axis(1, c("Reference level", zz), at=1:(length(zz)+1))
plotCI(oos[,1], ui=oos[,4], li=oos[,3], , xlim=c(0, length(zz)+1), ylim=c(0, max(oos)+1), add=T)
}



choci.f <- function(cox1, x.title="predictor", y.title="Primary endpoint", main.t = ""){
library(plotrix)
oos <- rbind(c(1,1,1,1),summary(cox1)$conf.int)
oos2 <- ifelse(oos==Inf, oos[oos[,4]==Inf,1], oos)
oos2[,2]=0
zz <- rownames(summary(cox1)$coef)
plot(1:(length(zz)+1), oos2[,1], xlim=c(0, length(zz)+2), ylim=c(0, max(oos2)+1), type="l", 
xlab=paste("cut(",x.title, ", quantile(",x.title,"), include.lowest=T)"), ylab="Hazard ratio & CI", main=main.t, xaxt='n')
axis(1, c("Reference level", zz), at=1:(length(zz)+1))
plotCI(oos2[,1], ui=oos2[,4], li=oos2[,3], , xlim=c(0, length(zz)+1), ylim=c(0, max(oos2)+1), add=T)
}




# hosmerlem <-
#  function (y, yhat, g = 10) 
#  {
#      cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
#          1, 1/g)), include.lowest = T)
#      obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
#      expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
#      chisq <- sum((obs - expect)^2/expect)
#      P <- 1 - pchisq(chisq, g - 2)
#      c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
#  }
#??????
#hosmerlem(blocker, glmfit1$fitted, g=10)


hosmerlem <- function (y, yhat, g = 10) 
 {
     cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
         1, 1/g)), include.lowest = T)
     obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
     expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
     chisq <- sum((obs - expect)^2/expect)
     P <- 1 - pchisq(chisq, g - 2)
     test.output <- c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)

     structure(list(obs, expect, test.output))
 }

#time ROC?? ?????? object?? print ?ϴ? ?Լ? (?Ϲ????? survival)

print.timeROC <- function(timeroc.object){
oo <- cbind(
timeroc.object$times,
timeroc.object$Stats,
timeroc.object$AUC,
timeroc.object$inference$vect_sd_1,
confint(timeroc.object)$CI_AUC/100
)
colnames(oo) <- c("time", "Cases", "Survivors", "Censored", "AUC", "SE", "lower.limit", "upper.limit")
return(oo)
}


#time ROC ?? ?????? object ?? printing ?ϴ? ?Լ?. (competing risk ????)
print.timeROC.c <- function(timeroc.object){
oo <- cbind(
timeroc.object$times,
timeroc.object$Stats,
timeroc.object$AUC_1,
timeroc.object$inference$vect_sd_1,
confint(timeroc.object)$CI_AUC_1/100,
timeroc.object$AUC_2,
timeroc.object$inference$vect_sd_2,
confint(timeroc.object)$CI_AUC_2/100
)
#colnames(oo) <- c("time", "Cases", "Survivors", "Censored", "AUC", "SE", "lower.limit", "upper.limit")
return(oo)
}


#barchart ��?? error bar?? ?׸??? ?Լ?.

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){


if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))


stop("vectors must be same length")


arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)


} 






#?? Create a Kaplan-Meier plot using ggplot2

#??

#?? @param sfit a \code{\link[survival]{survfit}} object

#?? @param returns logical: if \code{TRUE}, return an ggplot object

#?? @param xlabs x-axis label

#?? @param ylabs y-axis label

#?? @param ystratalabs The strata labels. \code{Default = levels(summary(sfit)$strata)}

#?? @param ystrataname The legend name. Default = ??Strata??

#?? @param timeby numeric: control the granularity along the time-axis

#?? @param main plot title

#?? @param pval logical: add the pvalue to the plot?

#?? @return a ggplot is made. if returns=TRUE, then an ggplot object

#?? is returned

#?? @author Abhijit Dasgupta with contributions by Gil Tomas

#?? \url{http://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/}

#?? @export

#?? @examples

#?? \dontrun{

#?? data(colon)

#?? fit <- survfit(Surv(time,status)~rx, data=colon)

#' ggkm(fit, timeby=500)

#' }

ggkm <- function(sfit, returns = FALSE,

xlabs = "Time", ylabs = "survival probability",

ystratalabs = NULL, ystrataname = NULL,

timeby = 100, main = "Kaplan-Meier Plot",

pval = TRUE, ...) {

require(plyr)

require(ggplot2)

require(survival)

require(gridExtra)

if(is.null(ystratalabs)) {

   ystratalabs <- as.character(levels(summary(sfit)$strata))

}

m <- max(nchar(ystratalabs))

if(is.null(ystrataname)) ystrataname <- "Strata"

times <- seq(0, max(sfit$time), by = timeby)

.df <- data.frame(time = sfit$time, n.risk = sfit$n.risk,

    n.event = sfit$n.event, surv = sfit$surv, strata = summary(sfit, censored = T)$strata,

    upper = sfit$upper, lower = sfit$lower)

levels(.df$strata) <- ystratalabs

zeros <- data.frame(time = 0, surv = 1, strata = factor(ystratalabs, levels=levels(.df$strata)),

    upper = 1, lower = 1)

.df <- rbind.fill(zeros, .df)

d <- length(levels(.df$strata))

p <- ggplot(.df, aes(time, surv, group = strata)) +

    geom_step(aes(linetype = strata), size = 0.7) +

    theme_bw() +

    theme(axis.title.x = element_text(vjust = 0.5)) +

    scale_x_continuous(xlabs, breaks = times, limits = c(0, max(sfit$time))) +

    scale_y_continuous(ylabs, limits = c(0, 1)) +

    theme(panel.grid.minor = element_blank()) +

    theme(legend.position = c(ifelse(m < 10, .28, .35), ifelse(d < 4, .25, .35))) +

    theme(legend.key = element_rect(colour = NA)) +

    labs(linetype = ystrataname) +

    theme(plot.margin = unit(c(0, 1, .5, ifelse(m < 10, 1.5, 2.5)), "lines")) +

    ggtitle(main)

 

if(pval) {

    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))

    pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail = FALSE)

    pvaltxt <- ifelse(pval < 0.0001, "p < 0.0001", paste("p =", signif(pval, 3)))

    p <- p + annotate("text", x = 0.6 * max(sfit$time), y = 0.1, label = pvaltxt)

}

## Plotting the graphs

    print(p)

    if(returns) return(p)

   

}

# ggkmTable.R

#?? Create a Kaplan-Meier plot using ggplot2

#??

#?? @param sfit a \code{\link[survival]{survfit}} object

#?? @param table logical: Create a table graphic below the K-M plot, indicating at-risk numbers?

#?? @param returns logical: if \code{TRUE}, return an arrangeGrob object

#?? @param xlabs x-axis label

#?? @param ylabs y-axis label

#?? @param ystratalabs The strata labels. \code{Default = levels(summary(sfit)$strata)}

#?? @param ystrataname The legend name. Default = ??Strata??

#?? @param timeby numeric: control the granularity along the time-axis

#?? @param main plot title

#?? @param pval logical: add the pvalue to the plot?

#?? @return a ggplot is made. if return=TRUE, then an arrangeGlob object

#?? is returned

#?? @author Abhijit Dasgupta with contributions by Gil Tomas

#?? \url{http://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/}

#?? @export

#?? @examples

#?? \dontrun{

#?? data(colon)

#?? fit <- survfit(Surv(time,status)~rx, data=colon)

#' ggkm(fit, timeby=500)

#' }

ggkmTable <- function(sfit, table=TRUE,returns = FALSE,

xlabs = "Time", ylabs = "survival probability",

ystratalabs = NULL, ystrataname = NULL,

timeby = 100, main = "Kaplan-Meier Plot",

pval = TRUE, ...) {

require(plyr)

require(ggplot2)

require(survival)

require(gridExtra)

if(is.null(ystratalabs)) {

   ystratalabs <- as.character(levels(summary(sfit)$strata))

}

m <- max(nchar(ystratalabs))

if(is.null(ystrataname)) ystrataname <- "Strata"

times <- seq(0, max(sfit$time), by = timeby)

.df <- data.frame(time = sfit$time, n.risk = sfit$n.risk,

    n.event = sfit$n.event, surv = sfit$surv, strata = summary(sfit, censored = T)$strata,

    upper = sfit$upper, lower = sfit$lower)

levels(.df$strata) <- ystratalabs

zeros <- data.frame(time = 0, surv = 1, strata = factor(ystratalabs, levels=levels(.df$strata)),

    upper = 1, lower = 1)

.df <- rbind.fill(zeros, .df)

d <- length(levels(.df$strata))

p <- ggplot(.df, aes(time, surv, group = strata)) +

    geom_step(aes(linetype = strata), size = 0.7) +

    theme_bw() +

    theme(axis.title.x = element_text(vjust = 0.5)) +

    scale_x_continuous(xlabs, breaks = times, limits = c(0, max(sfit$time))) +

    scale_y_continuous(ylabs, limits = c(0, 1)) +

    theme(panel.grid.minor = element_blank()) +

    theme(legend.position = c(ifelse(m < 10, .28, .35), ifelse(d < 4, .25, .35))) +

    theme(legend.key = element_rect(colour = NA)) +

    labs(linetype = ystrataname) +

    theme(plot.margin = unit(c(0, 1, .5, ifelse(m < 10, 1.5, 2.5)), "lines")) +

    ggtitle(main)

 

if(pval) {

    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))

    pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail = FALSE)

    pvaltxt <- ifelse(pval < 0.0001, "p < 0.0001", paste("p =", signif(pval, 3)))

    p <- p + annotate("text", x = 0.6 * max(sfit$time), y = 0.1, label = pvaltxt)

}


## Create a blank plot for place-holding

## .df <- data.frame()

blank.pic <- ggplot(.df, aes(time, surv)) +

    geom_blank() +

    theme_bw() +

    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),

        axis.title.x = element_blank(), axis.title.y = element_blank(),

        axis.ticks = element_blank(), panel.grid.major = element_blank(),

        panel.border = element_blank())

if(table) {

    ## Create table graphic to include at-risk numbers

    risk.data <- data.frame(strata = summary(sfit, times = times, extend = TRUE)$strata,

        time = summary(sfit, times = times, extend = TRUE)$time,

#        n.risk = summary(sfit, times = times, extend = TRUE)$n.risk)
        n.risk = round(summary(sfit, times = times, extend = TRUE)$n.risk), 0)


    data.table <- ggplot(risk.data, aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +

        #, color = strata)) +

        geom_text(size = 3.5) +

        theme_bw() +

        scale_y_discrete(breaks = as.character(levels(risk.data$strata)), labels = ystratalabs) +

        # scale_y_discrete(#format1ter = abbreviate,

        # breaks = 1:3,

        # labels = ystratalabs) +

        scale_x_continuous("Numbers at risk", limits = c(0, max(sfit$time))) +

        theme(axis.title.x = element_text(size = 10, vjust = 1), panel.grid.major = element_blank(),

        panel.grid.minor = element_blank(), panel.border = element_blank(),

        axis.text.x = element_blank(), axis.ticks = element_blank(),

        axis.text.y = element_text(face = "bold", hjust = 1))

    data.table <- data.table + theme(legend.position = "none") +

        xlab(NULL) + ylab(NULL)

    data.table <- data.table +

        theme(plot.margin = unit(c(-1.5, 1, 0.1, ifelse(m < 10, 2.5, 3.5)-0.28 * m), "lines"))

## Plotting the graphs

## p <- ggplotGrob(p)

## p <- addGrob(p, textGrob(x = unit(.8, "npc"), y = unit(.25, "npc"), label = pvaltxt,

    ## gp = gpar(fontsize = 12)))

    grid.arrange(p, blank.pic, data.table,

        clip = FALSE, nrow = 3, ncol = 1,

        heights = unit(c(2, .1, .25),c("null", "null", "null")))

    if(returns) {

        a <- arrangeGrob(p, blank.pic, data.table, clip = FALSE,

            nrow = 3, ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null")))

        return(a)

    }

}

else {

    ## p <- ggplotGrob(p)

    ## p <- addGrob(p, textGrob(x = unit(0.5, "npc"), y = unit(0.23, "npc"),

    ## label = pvaltxt, gp = gpar(fontsize = 12)))

    print(p)

    if(returns) return(p)
	
    }

}


print.timeROC <- function(timeroc.object){
oo <- cbind(
timeroc.object$times,
timeroc.object$Stats,
timeroc.object$AUC,
timeroc.object$inference$vect_sd_1,
confint(timeroc.object)$CI_AUC
)
colnames(oo) <- c("time", "Cases", "Survivors", "Censored", "AUC", "SE", "lower.limit", "upper.limit")
return(oo)
}


has.interaction <- function(x,terms){
    out <- sapply(terms,function(i){
        sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
    })
    return(sum(out)>0)
}



model.select <- function(model,keep,sig=0.05,verbose=F){
      counter=1
      # check input
      if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
      # calculate scope for drop1 function
      terms <- attr(model$terms,"term.labels")
      if(missing(keep)){ # set scopevars to all terms
          scopevars <- terms
      } else{            # select the scopevars if keep is used
          index <- match(keep,terms)
          # check if all is specified correctly
          if(sum(is.na(index))>0){
              novar <- keep[is.na(index)]
              warning(paste(
                  c(novar,"cannot be found in the model",
                  "\nThese terms are ignored in the model selection."),
                  collapse=" "))
              index <- as.vector(na.omit(index))
          }
          scopevars <- terms[-index]
      }

      # Backward model selection : 

      while(T){
          # extract the test statistics from drop.
          test <- drop1(model, scope=scopevars,test="F")

          if(verbose){
              cat("-------------STEP ",counter,"-------------\n",
              "The drop statistics : \n")
              print(test)
          }

          pval <- test[,dim(test)[2]]

          names(pval) <- rownames(test)
          pval <- sort(pval,decreasing=T)

          if(sum(is.na(pval))>0) stop(paste("Model",
              deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))

          # check if all significant
          if(pval[1]<sig) break # stops the loop if all remaining vars are sign.

          # select var to drop
          i=1
          while(T){
              dropvar <- names(pval)[i]
              check.terms <- terms[-match(dropvar,terms)]
              x <- has.interaction(dropvar,check.terms)
              if(x){i=i+1;next} else {break}              
          } # end while(T) drop var

          if(pval[i]<sig) break # stops the loop if var to remove is significant

          if(verbose){
             cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
          }

          #update terms, scopevars and model
          scopevars <- scopevars[-match(dropvar,scopevars)]
          terms <- terms[-match(dropvar,terms)]

          formul <- as.formula(paste(".~.-",dropvar))
          model <- update(model,formul)

          if(length(scopevars)==0) {
              warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
              return()
          }
          counter=counter+1
      } # end while(T) main loop
      return(model)
}

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}



##scatter plot, matrix using "pairs" function, 
#see R help 'pair'
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

#pairs( ~ Age+LVEF_Matching + eGFR+ SyntaxScore, diag.panel=panel.hist)  #?̷??? ?ϰų? ?ؿ?ó?? ?ϰų?.
#pairs( x=as.data.frame(aa[contiVars1]), diag.panel=panel.hist)

SRS.f <- function(NN, SDD, B=0.1){
DD = B^2/4
n= NN*SDD^2 / ((NN-1)*DD+SDD^2)
return(n)
}


wtd.basic <- function(var1, weight.v = siptw) {
kk <- c(sum((as.numeric(!is.na(var1)) * weight.v)), wtd.mean(var1, weight.v),
sqrt(wtd.var(var1, weights=weight.v )),
wtd.quantile(var1, weights=weight.v ))
names(kk) <- c("wtd.n", "wtd.mean", "wtd.sd", "min", "25%", "median", "75%", "max")
return(kk)
}


wtd.basic <- function(var1, mweight1 = mweight) {
kk <- c(sum((ifelse(is.na(var1), 0, 1) * mweight1)), wtd.mean(var1, mweight1),
sqrt(wtd.var(var1, weights=mweight1 )),
wtd.quantile(var1, weights=mweight1 ))
names(kk) <- c("wtd.n", "wtd.mean", "wtd.sd", "min", "25%", "median", "75%", "max")
return(kk)
}

HLTest = function(obj, g) {
 # first, check to see if we fed in the right kind of object
 stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
 y = obj$model[[1]]
 trials = rep(1, times = nrow(obj$model))
 if(any(colnames(obj$model) == "(weights)")) 
  trials <- obj$model[[ncol(obj$model)]]
 # the double bracket (above) gets the index of items within an object
 if (is.factor(y)) 
  y = as.numeric(y) == 2  # Converts 1-2 factor levels to logical 0/1 values
 yhat = obj$fitted.values 
 interval = cut(yhat, quantile(yhat, 0:g/g), include.lowest = TRUE)  # Creates factor with levels 1,2,...,g
 Y1 <- trials*y
 Y0 <- trials - Y1
 Y1hat <- trials*yhat
 Y0hat <- trials - Y1hat
 obs = xtabs(formula = cbind(Y0, Y1) ~ interval)
 expect = xtabs(formula = cbind(Y0hat, Y1hat) ~ interval)
 if (any(expect < 5))
  warning("Some expected counts are less than 5. Use smaller number of groups")
 pear <- (obs - expect)/sqrt(expect)
 chisq = sum(pear^2)
 P = 1 - pchisq(chisq, g - 2)
 # by returning an object of class "htest", the function will perform like the 
 # built-in hypothesis tests
 return(structure(list(
  method = c(paste("Hosmer and Lemeshow goodness-of-fit test with", g, "bins", sep = " ")),
  data.name = deparse(substitute(obj)),
  statistic = c(X2 = chisq),
  parameter = c(df = g-2),
  p.value = P,
  pear.resid = pear,
  expect = expect,
  observed = obs
 ), class = 'htest'))
}


o.r.test = function(obj) {
  # first, check to see if we fed in the right kind of object
  stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
  mf <- obj$model
  trials = rep(1, times = nrow(mf))
  if(any(colnames(mf) == "(weights)")) 
    trials <- mf[[ncol(mf)]]
  prop = mf[[1]]
  # the double bracket (above) gets the index of items within an object
  if (is.factor(prop)) 
    prop = as.numeric(prop) == 2  # Converts 1-2 factor levels to logical 0/1 values
  pi.hat = obj$fitted.values 
  y <- trials*prop
  yhat <- trials*pi.hat
  nu <- yhat*(1-pi.hat)
  pearson <- sum((y - yhat)^2/nu)
  c = (1 - 2*pi.hat)/nu
  exclude <- c(1,which(colnames(mf) == "(weights)"))
  vars <- data.frame(c,mf[,-exclude]) 
  wlr <- lm(formula = c ~ ., weights = nu, data = vars)
  rss <- sum(nu*residuals(wlr)^2 )
  J <- nrow(mf)
  A <- 2*(J - sum(1/trials))
  z <- (pearson - (J - ncol(vars) - 1))/sqrt(A + rss)
  p.value <- 2*(1 - pnorm(abs(z)))
  cat("z = ", z, "with p-value = ", p.value, "\n")
}


stukel.test = function(obj) {
  # first, check to see if we fed in the right kind of object
  stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
  high.prob <- (obj$fitted.values >= 0.5) 
  logit2 <- obj$linear.predictors^2
  z1 = 0.5*logit2*high.prob
  z2 = 0.5*logit2*(1-high.prob)
  mf <- obj$model
  trials = rep(1, times = nrow(mf))
  if(any(colnames(mf) == "(weights)")) 
    trials <- mf[[ncol(mf)]]
  prop = mf[[1]]
  # the double bracket (above) gets the index of items within an object
  if (is.factor(prop)) 
    prop = (as.numeric(prop) == 2)  # Converts 1-2 factor levels to logical 0/1 values
  pi.hat = obj$fitted.values 
  y <- trials*prop
  exclude <- which(colnames(mf) == "(weights)")
  vars <- data.frame(z1, z2, y, mf[,-c(1,exclude)])
  full <- glm(formula = y/trials ~ ., family = binomial(link = logit), weights = trials, data = vars)
  null <- glm(formula = y/trials ~ ., family = binomial(link = logit), weights = trials, data = vars[,-c(1,2)])
  LRT <- anova(null,full)
  p.value <- 1 - pchisq(LRT$Deviance[[2]], LRT$Df[[2]])
  cat("Stukel Test Stat = ", LRT$Deviance[[2]], "with p-value = ", p.value, "\n")
}






simple.cox <- function(var1) { 
if(is.factor(var1)) {
nlevel = nlevels(var1)
} else { 
nlevel = 2
}

list.coef <- lapply(surv.list, function(x) summary(coxph(x~ var1))$coef)
list.or <- lapply(surv.list, function(x) summary(coxph(x~ var1))$conf.int)
names(list.coef) <- names(surv.list)
names(list.or) <- names(surv.list)

kk1 <- do.call('rbind', list.coef)
kk2 <- do.call('rbind', list.or)
kk <- cbind(kk1,kk2)
rownames(kk) <- rep(names(surv.list), each= nlevel-1)
return(kk)
}


rms.coef.f <- function(x){
cbind(x$coefficients,
sqrt(diag(x$var)),
x$coefficients /sqrt(diag(x$var)),
2*(1-pnorm(abs(x$coefficients /sqrt(diag(x$var)) ) ))
)
}






clx <- function(fm, dfcw, cluster)
{	
					
#Clustered on state, replicating Stock and Watson						
#> clx(fmlm, dfcw, Fatality$state)						
							
 	library(sandwich)						
 	library(lmtest)						
 	M <- length(unique(cluster))						
 	N <- length(cluster)						
 	dfc <- (M/(M-1))*((N-1)/(N-fm$rank))						
 	u <- apply(estfun(fm),2,						
 	function(x) tapply(x, cluster, sum))						
 	vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw						
 	coeftest(fm, vcovCL) 
}						





#####################################################################
# NAME: Tom Loughin                                                 #
# DATE: 1-10-13                                                     #
# PURPOSE: Functions to compute Hosmer-Lemeshow, Osius-Rojek, and   #
#     Stukel goodness-of-fit tests                                  #
#                                                                   #
# NOTES:                                                            #
#####################################################################
# Single R file that contains all three goodness-of fit tests


# Adapted from program published by Ken Kleinman as Exmaple 8.8 on the SAS and R blog, sas-and-r.blogspot.ca 
#  Assumes data are aggregated into Explanatory Variable Pattern form.

HLTest = function(obj, g) {
 # first, check to see if we fed in the right kind of object
 stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
 y = obj$model[[1]]
 trials = rep(1, times = nrow(obj$model))
 if(any(colnames(obj$model) == "(weights)")) 
  trials <- obj$model[[ncol(obj$model)]]
 # the double bracket (above) gets the index of items within an object
 if (is.factor(y)) 
  y = as.numeric(y) == 2  # Converts 1-2 factor levels to logical 0/1 values
 yhat = obj$fitted.values 
 interval = cut(yhat, quantile(yhat, 0:g/g), include.lowest = TRUE)  # Creates factor with levels 1,2,...,g
 Y1 <- trials*y
 Y0 <- trials - Y1
 Y1hat <- trials*yhat
 Y0hat <- trials - Y1hat
 obs = xtabs(formula = cbind(Y0, Y1) ~ interval)
 expect = xtabs(formula = cbind(Y0hat, Y1hat) ~ interval)
 if (any(expect < 5))
  warning("Some expected counts are less than 5. Use smaller number of groups")
 pear <- (obs - expect)/sqrt(expect)
 chisq = sum(pear^2)
 P = 1 - pchisq(chisq, g - 2)
 # by returning an object of class "htest", the function will perform like the 
 # built-in hypothesis tests
 return(structure(list(
  method = c(paste("Hosmer and Lemeshow goodness-of-fit test with", g, "bins", sep = " ")),
  data.name = deparse(substitute(obj)),
  statistic = c(X2 = chisq),
  parameter = c(df = g-2),
  p.value = P,
  pear.resid = pear,
  expect = expect,
  observed = obs
 ), class = 'htest'))
}

# Osius-Rojek test
# Based on description in Hosmer and Lemeshow (2000) p. 153.
# Assumes data are aggregated into Explanatory Variable Pattern form.

o.r.test = function(obj) {
 # first, check to see if we fed in the right kind of object
 stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
 mf <- obj$model
 trials = rep(1, times = nrow(mf))
 if(any(colnames(mf) == "(weights)")) 
  trials <- mf[[ncol(mf)]]
 prop = mf[[1]]
 # the double bracket (above) gets the index of items within an object
 if (is.factor(prop)) 
  prop = as.numeric(prop) == 2  # Converts 1-2 factor levels to logical 0/1 values
 pi.hat = obj$fitted.values 
 y <- trials*prop
 yhat <- trials*pi.hat
 nu <- yhat*(1-pi.hat)
 pearson <- sum((y - yhat)^2/nu)
 c = (1 - 2*pi.hat)/nu
 exclude <- c(1,which(colnames(mf) == "(weights)"))
 vars <- data.frame(c,mf[,-exclude]) 
 wlr <- lm(formula = c ~ ., weights = nu, data = vars)
 rss <- sum(nu*residuals(wlr)^2 )
 J <- nrow(mf)
 A <- 2*(J - sum(1/trials))
 z <- (pearson - (J - ncol(vars) - 1))/sqrt(A + rss)
 p.value <- 2*(1 - pnorm(abs(z)))
 cat("z = ", z, "with p-value = ", p.value, "\n")
}

# Stukel Test
# Based on description in Hosmer and Lemeshow (2000) p. 155.
# Assumes data are aggregated into Explanatory Variable Pattern form.

stukel.test = function(obj) {
 # first, check to see if we fed in the right kind of object
 stopifnot(family(obj)$family == "binomial" && family(obj)$link == "logit")
 high.prob <- (obj$fitted.values >= 0.5) 
 logit2 <- obj$linear.predictors^2
 z1 = 0.5*logit2*high.prob
 z2 = 0.5*logit2*(1-high.prob)
 mf <- obj$model
 trials = rep(1, times = nrow(mf))
 if(any(colnames(mf) == "(weights)")) 
  trials <- mf[[ncol(mf)]]
 prop = mf[[1]]
 # the double bracket (above) gets the index of items within an object
 if (is.factor(prop)) 
  prop = (as.numeric(prop) == 2)  # Converts 1-2 factor levels to logical 0/1 values
 pi.hat = obj$fitted.values 
 y <- trials*prop
 exclude <- which(colnames(mf) == "(weights)")
 vars <- data.frame(z1, z2, y, mf[,-c(1,exclude)])
 full <- glm(formula = y/trials ~ ., family = binomial(link = logit), weights = trials, data = vars)
 null <- glm(formula = y/trials ~ ., family = binomial(link = logit), weights = trials, data = vars[,-c(1,2)])
 LRT <- anova(null,full)
 p.value <- 1 - pchisq(LRT$Deviance[[2]], LRT$Df[[2]])
 cat("Stukel Test Stat = ", LRT$Deviance[[2]], "with p-value = ", p.value, "\n")
}




survfit.f <- function(surv.fit, colname = c("group", "n.risk", "n.event", "survival_rate", "std.err", "lower.limit", "upper.limit"))
{

oo <- cbind(
as.integer(surv.fit$ strata)-1,
surv.fit$ n.risk,
surv.fit$ n.event,
surv.fit$ surv,
surv.fit$ std.err,
surv.fit$ lower,
surv.fit$ upper)

rownames(oo) <- surv.fit$time
colnames(oo) <- colname
return(oo)
}

survfit.f2 <- function(surv.fit, colname = c("n.risk", "n.event", "survival_rate", "std.err", "lower.limit", "upper.limit"))
{

oo <- cbind(
surv.fit$ n.risk,
surv.fit$ n.event,
surv.fit$ surv,
surv.fit$ std.err,
surv.fit$ lower,
surv.fit$ upper)

rownames(oo) <- surv.fit$time
colnames(oo) <- colname
return(oo)
}


##survival function ?? ?ŷڱ??? ???ϱ?. 


survival.se.f <- function(x,y) {
ff = log(x)-log(1-x)
sef <- y/(x * (1-x))
lf = ff-1.96*sef
uf = ff+1.96*sef
lower.limit = exp(lf)/(1+exp(lf))
upper.limit = exp(uf)/(1+exp(uf))
return(cbind(lower.limit, upper.limit))
}





or.test <- function(object)
{
### ancillary function
## ags adapted from agg.sum provided by Bill Dunlap	
  ags <- function(x, by){
    by <- data.frame(by)
    ord <- do.call("order", unname(by))
    x <- x[ord]
    by <- by[ord,  ]
    logical.diff <- function(group) group[-1] != group[ - length(group)]
    change <- logical.diff(by[[1]])
    for(i in seq(along = by)[-1])
      change <- change | logical.diff(by[[i]])
    by <- by[c(T, change),  , drop = F]
    by$x <- diff(c(0, cumsum(x)[c(change, T)]))
    by
    }
###
### computations
###
  mf <- model.frame(object)
## collapse the original data by covariate pattern
  xx <- ags(rep(1, nrow(mf)), mf[-1])
## observed number of cases by covariate pattern
  yy <- unname(unlist(ags(mf[ , 1], mf[-1])[ncol(xx)]))
## fitted proba
  pp <- predict(object, newdata = xx, type = "response")
## number of rows with the same covariate pattern
  mm <- unname(unlist(xx[ncol(xx)]))
## new model frame
  xx <- xx[ , - ncol(xx)]
## weights
  nu <- mm * pp * (1 - pp)
## new response
  cc <- (1 - 2 * pp) / nu
### Pearson's X2
  X2 <- sum( (yy - mm * pp)^2 / nu)
### weighted regression
  mod <- lm(cc ~ . , weights = nu, data = xx)
  rss <- sum( nu * resid(mod)^2 )
### compute the stat.
  J <- nrow(xx)
  A <- 2 * (J - sum( 1 / mm))
  z <- abs( (X2 - (J - length( coef(object) ) ) ) / sqrt(A + rss) )
### report results
  print(object$call)
  cat("Osius & Rojek's goodness-of-fit test for logistic models.\n")
  cat("Null hypothesis: model fits the data well.\n")
  cat("z =", round(z, 3), "; P =", round(2 * (1 - pnorm(z)), 3), "\n")
  }


####################################################################################

cv.f <- function(formula1, data1) {

list1 <- as.list(1:nrow(data1))

list2 <- lapply(list1, function(x) data1[-x,])
list2.1 <- lapply(list1, function(x) data1[x,])

list3 <- lapply(list2, function(x) glm(formula1, data=x, family = binomial) )

list4 <- vector('list', nrow(data1))
for(i in 1:nrow(data1)) list4[[i]] <- predict(list3[[i]], newdata=list2.1[[i]], type="response")
predicted <- factor(sapply(list4, function(x) ifelse(x<0.5, 0, 1)), levels=c(0,1))
tt <- table(Decan, predicted)
print(freq.table(tt))
#tr?Լ??? psych ?? ?Լ??? ???? psych::tr
error.rate <- (sum(tt) - psych::tr(as.matrix(tt, ncol=2, nrow=2)) ) / sum(tt)
pred <- as.numeric(list4)

kk <- (list(pred , error.rate))
names(kk) <- c("pred", "error.rate")
return( structure(kk))
}




####################################################################################
smd.var.f <- function(x, group, weight = rep(1, length(x)), round1 = 3) {

if( length(x) != length(group) ) {
print("length of variable is wrong")
}

if( is.numeric(x)) {
group1 = factor(group)
x1 <- x[group1 == levels(group1)[1]]
x2 <- x[group1 == levels(group1)[2]]

wtd1 <- weight [group1 == levels(group1)[1]]
wtd2 <- weight [group1 == levels(group1)[2]]


count1 <- length(x1)
count2 <- length(x2)
wtd.n1 <- sum(wtd1)
wtd.n2 <- sum(wtd2)

wtd.mean1 <- wtd.mean(x1, weights = wtd1)
wtd.mean2 <- wtd.mean(x2, weights = wtd2)
wtd.var1 <- wtd.var(x1, weights = wtd1)
wtd.var2 <- wtd.var(x2, weights = wtd2)
smd.v <- abs(wtd.mean1 - wtd.mean2) / sqrt((wtd.var1 + wtd.var2)/2)
VR <- wtd.var1/wtd.var2

kk <- round( c(count1, wtd.n1, wtd.mean1, sqrt(wtd.var1), count2, wtd.n2, wtd.mean2, sqrt(wtd.var2), smd.v, VR), round1)
names(kk) <- c("count1", "wtd.n1", "wtd.mean1", "wtd.sd1", "count2", "wtd.n2", "wtd.mean2", "wtd.sd2", "SMD", "Variance.ratio")

kk2 <- round( c(wtd.n1, wtd.mean1, sqrt(wtd.var1), wtd.n2, wtd.mean2, sqrt(wtd.var2), smd.v, VR), round1)
names(kk2) <- c("wtd.n1", "wtd.mean1", "wtd.sd1", "wtd.n2", "wtd.mean2", "wtd.sd2", "SMD", "Variance.ratio")
}

else 
{
group1 = factor(group)

#sex
#x = sex
#group1 = factor(maze )
#weight = rep(1, nrow(aa))


x1 <- x[group1 == levels(group1)[1]]
x2 <- x[group1 == levels(group1)[2]]

wtd1 <- weight [group1 == levels(group1)[1]]
wtd2 <- weight [group1 == levels(group1)[2]]


count1 <- length(x1)
count2 <- length(x2)
wtd.n1 <- wtd.table(x1, wtd1)$sum.of.weights[2]
wtd.n2 <- wtd.table(x2, wtd2, na.rm=F)$sum.of.weights[2]
wtd.mean1 <- prop.table(wtd.table(x1, wtd1)$sum.of.weights)[2]
wtd.mean2 <- prop.table(wtd.table(x2, wtd2)$sum.of.weights)[2]

wtd.var1 <- prop.table(wtd.table(x1, wtd1)$sum.of.weights)[1] * prop.table(wtd.table(x1, wtd1)$sum.of.weights)[2]
wtd.var2 <- prop.table(wtd.table(x2, wtd2)$sum.of.weights)[1] * prop.table(wtd.table(x2, wtd2)$sum.of.weights)[2]

wtd.mean1 <- ifelse(is.na(wtd.mean1), 0, wtd.mean1)
wtd.mean2 <- ifelse(is.na(wtd.mean2 ), 0, wtd.mean2)
wtd.var1 <- ifelse(is.na(wtd.var1 ), 0, wtd.mean1)
wtd.var2 <- ifelse(is.na(wtd.var2 ), 0, wtd.mean2)

smd.v <- abs(wtd.mean1 - wtd.mean2) / sqrt(sum(wtd.var1 , wtd.var2, na.rm=T)/2)
VR <- wtd.var1/wtd.var2
smd.v <- ifelse(is.nan (smd.v), 0, smd.v)
kk <- round( c(count1, wtd.n1, wtd.mean1, sqrt(wtd.var1), count2, wtd.n2, wtd.mean2, sqrt(wtd.var2), smd.v, VR), round1)
names(kk) <- c("count1", "wtd.n1", "wtd.mean1", "wtd.sd1", "count2", "wtd.n2", "wtd.mean2", "wtd.sd2", "SMD", "Variance.ratio")

kk2 <- round( c(wtd.n1, wtd.mean1, sqrt(wtd.var1), wtd.n2, wtd.mean2, sqrt(wtd.var2), smd.v, VR), round1)
names(kk2) <- c("wtd.n1", "wtd.mean1", "wtd.sd1", "wtd.n2", "wtd.mean2", "wtd.sd2", "SMD", "Variance.ratio")

}
return(kk)
}



### usage

#oo1 <- data.frame(t(sapply(aa2[contiVars1], function(x) smd.var.f(x, group = aa$Group )) ) )
#oo2 <- data.frame(t(sapply(aa2[factorVars3], function(x) smd.var.f(x, group = aa$Group)) ) )

#oo3 <- data.frame(t(sapply(mdata2[contiVars1], function(x) smd.var.f(x, group = mdata2$Group, weight = mdata2$mweight)) ))
#oo4 <- data.frame(t(sapply(mdata2[factorVars3], function(x) smd.var.f(x, group = mdata2$Group, weight = mdata2$mweight)) ))

#rbind(oo1, oo2)
#rbind(oo3, oo4)





####################################################################################



#irr ??Ű?????? kappa.fleiss ?Լ??? ci?? ǥ?????? ?ʾƼ?, ?̸? ǥ???ǰ? ?ڵ? ??��??. 


kappam.fleiss.ci <- function (ratings, exact = FALSE, detail = FALSE) 
{
    ratings <- as.matrix(na.omit(ratings))
    ns <- nrow(ratings)
    nr <- ncol(ratings)
    lev <- levels(as.factor(ratings))
    for (i in 1:ns) {
        frow <- factor(ratings[i, ], levels = lev)
        if (i == 1) 
            ttab <- as.numeric(table(frow))
        else ttab <- rbind(ttab, as.numeric(table(frow)))
    }
    ttab <- matrix(ttab, nrow = ns)
    agreeP <- sum((apply(ttab^2, 1, sum) - nr)/(nr * (nr - 1))/ns)
    if (!exact) {
        method <- "Fleiss' Kappa for m Raters"
        chanceP <- sum(apply(ttab, 2, sum)^2)/(ns * nr)^2
    }
    else {
        method <- "Fleiss' Kappa for m Raters (exact value)"
        for (i in 1:nr) {
            rcol <- factor(ratings[, i], levels = lev)
            if (i == 1) 
                rtab <- as.numeric(table(rcol))
            else rtab <- rbind(rtab, as.numeric(table(rcol)))
        }
        rtab <- rtab/ns
        chanceP <- sum(apply(ttab, 2, sum)^2)/(ns * nr)^2 - sum(apply(rtab, 
            2, var) * (nr - 1)/nr)/(nr - 1)
    }
    value <- (agreeP - chanceP)/(1 - chanceP)

    if (!exact) {
        pj <- apply(ttab, 2, sum)/(ns * nr)
        qj <- 1 - pj
        varkappa <- (2/(sum(pj * qj)^2 * (ns * nr * (nr - 1)))) * 
            (sum(pj * qj)^2 - sum(pj * qj * (qj - pj)))
        SEkappa <- sqrt(varkappa)
        u <- value/SEkappa
        p.value <- 2 * (1 - pnorm(abs(u)))

##Additional code for confidence interval. 

lower.CI1 <- value - 1.96 * SEkappa 
upper.CI1 <- value + 1.96 * SEkappa 


#log-log approach
l1 = log(value) - log(1-value) - qnorm(0.975)* (SEkappa/(value*(1-value)))
u1 = log(value) - log(1-value) + qnorm(0.975)* (SEkappa/(value*(1-value)))
lower.CI2 = exp(l1)/(1+exp(l1))
upper.CI2 = exp(u1)/(1+exp(u1))


#Fisher's z-transformation

z1 = 0.5 * log((1+value)/(1-value) )
l2 <- z1 - qnorm(0.975) * ( SEkappa)
u2 <- z1 + qnorm(0.975) * ( SEkappa)

l4 <- z1 - qnorm(0.975) * 1/sqrt( ns-3)
u4 <- z1 + qnorm(0.975) * 1/sqrt( ns-3)

lower.CI3 = (exp(2*l2)-1) / (exp(2*l2)+1)
upper.CI3 = (exp(2*u2)-1) / (exp(2*u2)+1)

lower.CI4 = (exp(2*l4)-1) / (exp(2*l4)+1)
upper.CI4 = (exp(2*u4)-1) / (exp(2*u4)+1)

##End. Additional code for confidence interval. 


        if (detail) {
            pj <- apply(ttab, 2, sum)/(ns * nr)
            pjk <- (apply(ttab^2, 2, sum) - ns * nr * pj)/(ns * 
                nr * (nr - 1) * pj)
            kappaK <- (pjk - pj)/(1 - pj)
            varkappaK <- 2/(ns * nr * (nr - 1))
            SEkappaK <- sqrt(varkappaK)

		##Additional code for confidence interval of detail table. 

		lower.ci <- kappaK - 1.96 * SEkappaK
		upper.ci <- kappaK + 1.96 * SEkappaK


		st = kappaK
		sest = SEkappaK
		kl = log(st) - log(1-st) - qnorm(0.975)* (sest/(st*(1-st)))
		ku = log(st) - log(1-st) + qnorm(0.975)* (sest/(st*(1-st)))
		lower.ci2 = exp(kl)/(1+exp(kl))
		upper.ci2 = exp(ku)/(1+exp(ku))

##End. Additional code for confidence interval of detail table. 


            uK <- kappaK/SEkappaK
            p.valueK <- 2 * (1 - pnorm(abs(uK)))
            tableK <- as.table(round(cbind(kappaK, SEkappaK ,uK, p.valueK, lower.ci, upper.ci, lower.ci2, upper.ci2), 
                digits = 3))
            rownames(tableK) <- lev
            colnames(tableK) <- c("Kappa", "SE", "z", "p.value", "lower.ci1", "upper.ci1", "lower.ci2", "upper.ci2")
        }
    }
    if (!exact) {
        if (!detail) {
            rval <- list(method = method, subjects = ns, raters = nr, 
                irr.name = "Kappa", value = value, SE =SEkappa , lowerCI = lower.CI1, upperCI = upper.CI1, lowerCI2 = lower.CI2, upperCI2 = upper.CI2, lowerCI4 = lower.CI4, upperCI4 = upper.CI4)
        }
        else {
            rval <- list(method = method, subjects = ns, raters = nr, 
                irr.name = "Kappa", value = value, detail = tableK)
        }
        rval <- c(rval, stat.name = "z", statistic = u, p.value = p.value, SE =SEkappa , lowerCI = lower.CI1, upperCI = upper.CI1 , lowerCI2 = lower.CI2, upperCI2 = upper.CI2, lowerCI4 = lower.CI4, upperCI4 = upper.CI4)
    }
    else {
        rval <- list(method = method, subjects = ns, raters = nr, 
            irr.name = "Kappa", value = value, SE = SEkappa , lowerCI = lower.CI1, upperCI = upper.CI1, lowerCI2 = lower.CI2, upperCI2 = upper.CI2, lowerCI4 = lower.CI4, upperCI4 = upper.CI4 )
    }
    class(rval) <- "irrlist"
    return(rval)
}









proportion.ci <- function(pp, se) {
		st = pp
		sest = se
		kl = log(st) - log(1-st) - qnorm(0.975)* (sest/(st*(1-st)))
		ku = log(st) - log(1-st) + qnorm(0.975)* (sest/(st*(1-st)))
		lower.ci2 = exp(kl)/(1+exp(kl))
		upper.ci2 = exp(ku)/(1+exp(ku))
}



##irr ??Ű?????? ci?? ?ȳ??ͼ? ??��?? ?????? 

print.jh <- function (x, ...) 
{
    cat(" ", x$method, "\n\n", sep = "")
    cat(paste(" Subjects =", x$subjects, "\n"))
    cat(paste("   Raters =", x$raters, "\n"))
    results <- paste(formatC(x$irr.name, width = 9, flag = "+"), 
        "=", format(x$value, digits = 3), "\n")
    cat(results)
    if (!is.null(x$statistic)) {
        statistic <- paste(formatC(x$stat.name, width = 9, flag = "+"), 
            "=", format(x$statistic, digits = 3), "\n")
        cat("\n", statistic, sep = "")
        cat(paste("  p-value =", format(x$p.value, digits = 3), 
            "\n"))
        cat(paste("  SE =", format(x$SE, digits = 3), 
            "\n"))
        cat(paste("  lower.limit =", format(x$lowerCI, digits = 3), 
            "\n"))
        cat(paste("  upper.limit =", format(x$upperCI, digits = 3), 
            "\n"))
        cat(paste("  lower.limit(lla) =", format(x$lowerCI2, digits = 3), 
            "\n"))
        cat(paste("  upper.limit(lla) =", format(x$upperCI2, digits = 3), 
            "\n"))
        cat(paste("  lower.limit(FZT) =", format(x$lowerCI4, digits = 3), 
            "\n"))
        cat(paste("  upper.limit(FZT) =", format(x$upperCI4, digits = 3), 
            "\n"))
    }
    if (!is.null(x$detail)) {
        cat("\n")
        if (is.table(x$detail)) 
            print(x$detail)
        else cat(x$detail, "\n")
    }
    if (!is.null(x$error)) 
        cat("\n ", x$error, "\n", sep = "")
}





spline.f <- function(x,y, data1=aa, kk = "left", quant1=4, xlab="") {
quant = seq(0, 1, 1/quant1)[-c(1, length(seq(0, 1, 1/quant1)))]
knots1= quantile(x, quant)

library(splines)
glm1 = glm(y ~ bs(x), data = data1, family = binomial)
summary(glm1)
plot(x, y, xlim = range(x), pch =3, col = "darkgrey", xlab=xlab)
lines(sort(x), glm1$fitted[order(x)], xlim = range(x), ylim = c(0,1), type = "l", lwd =2)
glm2 = glm(y ~ bs(x, knots = knots1), data = data1, family = binomial)
summary(glm2)
lines(x=sort(x), y=glm2$fitted[order(x)], col = 2, type = "l", lwd=2)
glm3 = glm(y ~ rcs(x, knots = knots1), data = data1, family = binomial)
summary(glm3)
lines(x=sort(x), y=glm3$fitted[order(x)], col = 4, type = "l", lwd=2)

glm4 = glm(y ~ ns(x), data = data1, family = binomial)
summary(glm4)
lines(x=sort(x), y=glm4$fitted[order(x)], col = 7, type = "l", lwd=2)
glm5 = glm(y ~ ns(x, knots = knots1), data = data1, family = binomial)
summary(glm5)
lines(x=sort(x), y=glm5$fitted[order(x)], col = 8, type = "l", lwd=2)


abline(v = knots1, col = 2, lty = 2)

legend(kk, c("bs without knot", "bs with knots", "rcs with knots", "ns without knots", "ns with knot"),
col = c(1,2,4,7,8), lwd = 2, box.col = "white", bg = "white")

structure(list(bs=glm1, bs1=glm2, rcs1=glm3, ns=glm4, ns1=glm5, knots=knots1))

}



