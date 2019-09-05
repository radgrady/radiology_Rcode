# Calibration plot
library(rms)
library(Hmisc)
library(survival)
library(survminer)
library(survcomp)
dd=datadist(mydata)
options(datadist="dd")
head(mydata)
f <- psm(Surv(time,status,type = "right")~ , data=mydata, x=TRUE, y=TRUE, dist = "lognormal")
cal1 <- calibrate(f, cmethod = "KM", method="boot",u=24,m=70,B=1000)
cal2 <- calibrate(f, cmethod = "KM", method="boot",u=60,m=70,B=1000)
par(mai=c(1,1,1,1))
plot(cal1, xaxt="n",yaxt="n",errbar.col = "DodgerBlue",lwd = 3,lty=1, cex.axis =1.2,
     cex.lab = 1.2,xlab="Model predicted survival probability", ylab="Observed survival (probability)",
     xlim = c(0,1),ylim = c(0,1))
lines(cal1[,c("mean.predicted","KM")],type = "b",lwd = 3,col = "DodgerBlue",pch = 16)
par(new=TRUE)
plot(cal1,xaxt="n",yaxt="n", errbar.col = "HotPink",lwd = 3,lty=1, cex.axis =1.2,cex.lab = 1.2,
     xlab="Model predicted survival probability", ylab="Observed survival (probability)",xlim = c(0,1),
     ylim = c(0,1))
lines(cal1[,c("mean.predicted","KM")],type = "b",lwd = 3,col = "HotPink",pch = 16)

# Discrimination C-index
library(survcomp)
f1 <- coxph(Surv(time,status,type = "right")~ , data=mydata)
f2 <- coxph(Surv(time,status,type = "right")~ , data=mydata)
cindex1 <- concordance.index(predict(f1),surv.time = mydata$time,surv.event = mydata$status,method="noether")
cindex2 <- concordance.index(predict(f2),surv.time = mydata$time,surv.event = mydata$status,method="noether")
cindex1$c.index; cindex1$lower; cindex1$upper
cindex2$c.index; cindex2$lower; cindex2$upper
cindex.comp(cindex1,cindex2)

# Time-dependent ROC analysis
library(survival)
library(timeROC)
surroc <-  timeROC(T = mydata$time,delta = mydata$status,marker = mydata$marker,
                   cause=1,times=c(12,24,36,48,60),iid = TRUE)
surroc$AUC

# Integrated Brier score and prediction error curve
library(pec)
library(riskRegression)
library(rms)
library(Hmisc)
library(survival)
library(survminer)
library(survcomp)
dd=datadist(mydata)
options(datadist="dd")
Models <- list("Radiomics model-pre"= coxph(Surv(time,status)~ , data=mydata,x=TRUE,y=TRUE),
               "Radiomics model-post"= coxph(Surv(time,status)~ , data=mydata,x=TRUE,y=TRUE))
p <- pec(object = Models,formula=Surv(time,status)~1, data=mydata, splitMethod="Boot632plus",
         B=1000,reference = FALSE)
print(p)
par(mai=c(1,1,1,1))
plot(p,type="l",smooth=TRUE,legend = FALSE,xlim=c(0,60),axis1.at=seq(0,60,12),
     xlab="Time after hepatectomy (months)", ylab="Prediction error",col = c("MediumPurple", "OrangeRed"),
     lwd = c(3,3),lty = c(1,1))

# Decision curve analysis
setwd("C:\\Decision Curve Analysis")
source("stdca.R")
library(survival)
f <- coxph(Surv(time,status,type = "right")~  , data=mydata)
train$2year = c(1- (summary(survfit(f, newdata=mydata), times=24)$surv))
train$5year = c(1- (summary(survfit(f, newdata=mydata), times=60)$surv))
2year = stdca(data=mydata, outcome="status", ttoutcome="time", predictors= c("2year"),timepoint=24,smooth=TRUE)
5year = stdca(data=mydata, outcome="status", ttoutcome="time", predictors= c("5year"),timepoint=60,smooth=TRUE)
par(mai=c(1,1,1,1))
plot(2year$net.benefit.threshold,2year$net.benefit.none, type = "l", lwd=3, xlim=c(0,1.0), ylim=c(-0.05,0.35),
     cex.axis =1.2,cex.lab = 1.2,xlab = "Threshold Probability", ylab = "Net Benefit")
lines(2year$net.benefit$threshold, 2year$net.benefit$none, type="l", col="Black", lwd=3, lty=1)
lines(2year$net.benefit$threshold, 2year$net.benefit$all, type="l", col="DarkGrey", lwd=3,lty=1)
plot(5year$net.benefit.threshold, 5year$net.benefit.none, type = "l", lwd=3, xlim=c(0,1.0), ylim=c(-0.05,0.55), 
     cex.axis =1.2,cex.lab = 1.2,xlab = "Threshold Probability", ylab = "Net Benefit")
lines(5year$net.benefit$threshold, 5year$net.benefit$none, type="l", col="Black", lwd=3, lty=1)
lines(5year$net.benefit$threshold, 5year$net.benefit$all, type="l", col="DarkGrey", lwd=3,lty=1)