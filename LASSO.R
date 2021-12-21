
#Required Packages

library(tidyverse)
library(aVirtualTwins)
library(party, verbose = F)
library(caret, verbose = F)
library(randomForest, verbose = F)
library("e1071")
library(knitr)
library(rpart.plot)
library("ggplot2")
library("ranger")
library("ggfortify")


set.seed(123)

#Reading Data

th_data=read.csv("D:/Thesis_BSU/data_thesis.csv")
head(th_data)

## LASSO

sum(is.na(dat))
dat=na.omit(dat)
names(data)
b=sum(is.na(dat))

Ldat=select(dat,-c(follow_up_yrs))
dim(Ldat)
y=Ldat$mortality_status

mmatrix <- model.matrix(mortality_status~(age+bmi+weight+glucose+total_cholesterol+med_beta+med_hypertensives+med_lipid+
                                            med_diabetes+fitness_rank
                                          +sex_code+inactivity+smoker+obesity+dyslipidemia+
                                            hypertension+diabetes)^2,dat)

library(glmnet)
cvfit <- cv.glmnet(x=mmatrix,y=dat$mortality_status, type.measure="auc",
                   family='binomial',
                   alpha=1
)
coef(cvfit, s = "lambda.min")
plot(cvfit)

cvfit$lambda.min

cvfit$lambda.lse

lassofit=glmnet(x=mmatrix,y=dat$mortality_status,
                family='binomial',
                alpha=1
)

par(mar=c(4.5,4.5,1,5))
plot(lassofit)

vn=colnames(mmatrix)
vnat=coef(lassofit)

vnat=vnat[-1,ncol(vnat)]
axis(4,at=vnat,line=-.5,label=vn,las=1,tick=FALSE,cex.axis=.8)



