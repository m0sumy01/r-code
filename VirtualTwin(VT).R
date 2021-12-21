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
  


#Selecting Important Variables
attach(th_data)
dim(th_data)
dat=select(th_data,-c(ID,sex, test_number,record_date,ethnicity,hdl,ldl,cancer_comment, cancer_yn, heart_comments, 
                        lung_comments,artery_comments,other_disease_comments,height,
                        peak_rpe,peak_dbp,peak_sbp,death_cause_generic,death_date,death_year, 
                        mortality_grouping, protocol, percent_body_fat, waist,VO2_rel, VO2_abs,getchell_PA,getchell_smoking,test_mode,
                        smoker_binary,hip_waist_ratio,height_SI,weight_SI,record_year,trig,resting_sbp,resting_dbp,
                        resting_hr,max_rer,max_hr))
 
#Exploratory Data Analysis 
summary(dat)
head(dat)
dim(dat)
  

## Data Arrangement

##Two Categories of dependent variable
  
dat_th=mutate(dat,CRF=ifelse(fitness_rank<=.33, "0", "1")) ## categories CRF
dat_th=select(dat_th,-c(fitness_rank))
names(dat_th)
  

##Checking Missing values

sum(is.na(dat_th))
data=na.omit(dat_th)
names(data)
b=sum(is.na(data))
  
  

  
###Virtual Twin
set.seed(12)
vt.0 <- vt.data(data, "mortality_status", "CRF", TRUE)

##Single Random Forest
  
  
model.rf <- randomForest(x = vt.0$getX(interactions = T),
                           y = vt.0$getY(), ntree=200)
vt.f.rf <- vt.forest("one", vt.data = vt.0, model = model.rf, interactions = T)
  
summary(vt.f.rf$difft)
  
  
  
  
# Classification tree
# initialize classification tree
  
tr.class <- vt.tree(tree.type="class",vt.f.rf,threshold=.002,control=rpart.control(minbucket=500))
  

# tr.class is a list if threshold is a vector
  
  class(tr.class)
  class(tr.class$tree)
  vt.sbgrps <- vt.subgroups(tr.class)
  vt.sbgrps
  
  
# Virtual Twin Classification tree
par(mfrow=c(1,1))
rpart.plot(tr.class$tree,main="Classification tree",extra=102,roundint=FALSE)
  
  
  node1=subset(data,bmi<33.15 & age<56.5 & weight>=198.3)
  dim(node1)
  node2=subset(data,bmi< 33.15 & age>=56.5)
  dim(node2)
  
  node3=subset(data,bmi>=33.15)
  dim(node3)
  
  





