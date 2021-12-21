## GUIDE_Regression and Classification

#Helpful GUIDE manual: guideman.pdf

#Used GUIDE.exe (GUIDE ver. 38.0) for GUIDE Interaction and GUIDE sum tree


## Proportional Hazard Model (Guide_Interaction)

par(mfrow=c(2,2))
library(survival)
leg.txt=c("CRF = 1","CRF = 0")
leg.col=c("red","blue")
leg.lty=1:2
xr=range(data_guide$follow_up_yrs)
zg=read.csv("G:/gcl.csv")
head(zg)
nodes <- zg$node
uniq.gp <- unique(sort(nodes))

plotted <- FALSE

for(g in uniq.gp){
  gp <- nodes == g
  y <- data_guide$follow_up_yrs[gp]
  stat <- data_guide$mortality_status[gp]
  treat <- data_guide$CRF[gp]
  fit <- survfit(Surv(y,stat) ~ treat, conf.type="none")
  
  if(plotted){
    plot(fit,xlim=xr,mark.time=FALSE,xlab="",ylab="",col=c("red","blue"),lwd=2)
  } else {
    plot(fit,xlim=xr,mark.time=FALSE,xlab="",ylab="Survival probability",
         col=c("red","blue"),lwd=2)
    plotted <- TRUE
  }
  
  title(paste("Node",g))
  legend("bottomleft",legend=leg.txt,lty=1,col=leg.col,lwd=2)
}


## Proportional Hazard model (Guide_sum)

par(mfrow=c(2,2))
library(survival)
leg.txt=c("CRF = no","CRF = yes")
leg.col=c("blue","red")
leg.lty=1:2
xr=range(data_guide$follow_up_yrs)
zg=read.csv("G:/G.gs.csv")
zg

nodes <- zg$node
nodes
uniq.gp <- unique(sort(nodes))
plotted <- FALSE
for(g in uniq.gp){
  gp <- nodes == g
  y <- data_guide$follow_up_yrs[gp]
  stat <- data_guide$mortality_status[gp]
  treat <- data_guide$CRF[gp]
  fit <- survfit(Surv(y,stat) ~ treat, conf.type="none")
  if(plotted){
    plot(fit,xlim=xr,mark.time=TRUE,xlab="time",ylab="",col=c("blue","red"),lwd=2)
  } else {
    plot(fit,xlim=xr,mark.time=TRUE,xlab="time",ylab="Survival probability",
         col=c("blue","red"),lwd=2)
    plotted <- TRUE
  }
  title(paste("Node",g))
  legend("bottomleft",legend=leg.txt,lty=1,col=leg.col,lwd=2)
}


