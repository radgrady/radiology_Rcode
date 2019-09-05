# Nomogram
library(rms)
library(survival)
dd=datadist(mydata)
options(datadist="dd")
f <- cph(Surv(time,status,type = "right")~ ,data=mydata, x=TRUE, y=TRUE, surv = TRUE)
survival <- Survival(f)
survival1 <- function(x)survival(24,x)
survival2 <- function(x)survival(60,x)
nom <- nomogram(f, fun=list(survival1, survival2),lp=F,fun.at = c (0.1, seq(0.1,0.9,by=0.1),0.9), funlabel= c("Probability of 2-year RFS", "Probability of 5-year RFS"))
plot(nom,xfrac=0.32,cex.axis=0.75,cex.var=1, lmgp=.4)

# Online calculator
library(DynNom)
library(rsconnect)
library(PKI)
f <-coxph(Surv(time,status)~ ,data=mydata)
DNbuilder.coxph(f,mydata)
rsconnect::deployApp("C:/Users/user/Documents/DynNomapp")