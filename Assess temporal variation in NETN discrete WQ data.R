#### this code imports aNETNs discrete water quality data
## Code then evaluates variability of core parms across netowrk and per site
## Version 1 Aaron Weed 11/4/2015
#
############################LOAD IN PACKAGES############################
library(plyr)
library(lattice)
library(latticeExtra)
library(xts)
require(dygraphs)
library(reshape)
library(zoo)
library(ggplot2)
library(nlme)

###################### LOAD IN FUNCTIONS ###############################
source("Theme.r")
source("lm_eqn.r")

#######################LOAD IN and RESHAPE NETN WQ DATA################################
discrete <- read.csv("discreteNETN.csv")
names(discrete)

#### CREATE MOLTEN DATAFRAME
discrete.melt<-melt(discrete, id.vars=c("Import_SiteGroup",  "NETNCode" ,"LocationType","Year","Month"), measure.vars=c("Discharge","DO_mgL","pH","SpCond" ,"Temp_C" ,"Turbidity"))
str(discrete.melt)

#### RESHAPE DF AND SETUP TIME
TS<-cast(discrete.melt,NETNCode+ LocationType + Month +Year ~ variable,fun=mean,add.missing=T , fill=NA  )# using mean function because > one value per cell
TS$date<-paste(TS$Month,1,TS$Year, sep="-") ## add in the first day of each  month
TS$date<-as.POSIXct(TS$date, format = "%m-%d-%Y") 
TS<-arrange(TS,NETNCode,LocationType,Year,Month)
TS$log_Temp<-log(TS$Temp_C)

## CREATE ANNUAL MIN AND MAX TS
TS.min<-cast(discrete.melt,Import_SiteGroup + NETNCode+ LocationType + Year ~ variable, fun=min)# min recorded annual

TS.max<-cast(discrete.melt,Import_SiteGroup + NETNCode+ LocationType + Year~ variable, fun=max)# max recorded annual

#### SAMPLING HISTORY
samps<-cast(discrete.melt,NETNCode+ Month +Year ~ variable,fun=length)

#################### VARIATION IN DATA ACROSS NETN#################----
###### CALC SITE-LEVEL SUMM STATS

n_samps<-cast(discrete.melt,Import_SiteGroup + NETNCode+ LocationType ~ variable)# no. samples per site
TS.mean<-cast(discrete.melt,Import_SiteGroup + NETNCode+ LocationType ~ variable, fun=mean)# Mean value per site
TS.sd<-cast(discrete.melt,Import_SiteGroup + NETNCode+ LocationType ~ variable, fun=sd)# SD value per site

min.melt<-melt(TS.min, id.vars=c("Import_SiteGroup",  "NETNCode" ,"LocationType","Year","Month"), measure.vars=c("Discharge","DO_mgL","pH","SpCond" ,"Temp_C" ,"Turbidity"))
Tmin.sd<-cast(min.melt,Import_SiteGroup + NETNCode+ LocationType ~ variable, fun=sd)# SD value per site
max.melt<-melt(TS.max, id.vars=c("Import_SiteGroup",  "NETNCode" ,"LocationType","Year","Month"), measure.vars=c("Discharge","DO_mgL","pH","SpCond" ,"Temp_C" ,"Turbidity"))
Tmax.sd<-cast(max.melt,Import_SiteGroup + NETNCode+ LocationType ~ variable, fun=sd)# SD value per site

#HISTOGRAMS showing site-level temporal variation across NETN
par(mfrow =c(3,6))
str(TS.sd)
hist(freq=T,TS.sd$Discharge, col="gray", xlab="Standard Deviation Discharge (cb f/s)", main="")
hist(freq=T,TS.sd$DO_mgL, col="gray", xlab="Standard Deviation DO (mg/L)", main="")
hist(freq=T,TS.sd$pH, col="gray", xlab="Standard Deviationin pH", main="")
hist(freq=T,TS.sd$SpCond, col="gray", xlab="Standard Deviation SpC (µS/cm)", main="")
hist(freq=T,TS.sd$Temp_C, col="gray", xlab="Standard Deviation Temperature (C)", main="")
hist(freq=T,TS.sd$Turbidity, col="gray", xlab="Standard Deviation Turbidity", main="")

#annual mins
str(Tmin.sd)
hist(freq=T,Tmin.sd$Discharge, col="gray", xlab="Standard Deviation Discharge (cb f/s)", main="")
hist(freq=T,Tmin.sd$DO_mgL, col="gray", xlab="Standard Deviation DO (mg/L)", main="")
hist(freq=T,Tmin.sd$pH, col="gray", xlab="Standard Deviationin pH", main="")
hist(freq=T,Tmin.sd$SpCond, col="gray", xlab="Standard Deviation SpC (µS/cm)", main="")
hist(freq=T,Tmin.sd$Temp_C, col="gray", xlab="Standard Deviation Temperature (C)", main="")
hist(freq=T,Tmin.sd$Turbidity, col="gray", xlab="Standard Deviation Turbidity", main="")

#annual maxs
str(Tmax.sd)
hist(freq=T,Tmax.sd$Discharge, col="gray", xlab="Standard Deviation Discharge (cb f/s)", main="")
hist(freq=T,Tmax.sd$DO_mgL, col="gray", xlab="Standard Deviation DO (mg/L)", main="")
hist(freq=T,Tmax.sd$pH, col="gray", xlab="Standard Deviationin pH", main="")
hist(freq=T,Tmax.sd$SpCond, col="gray", xlab="Standard Deviation SpC (µS/cm)", main="")
hist(freq=T,Tmax.sd$Temp_C, col="gray", xlab="Standard Deviation Temperature (C)", main="")
hist(freq=T,Tmax.sd$Turbidity, col="gray", xlab="Standard Deviation Turbidity", main="")
####################CORRLEATION AMONG VARS ACROSS NETN #####################################
source("panel_cor.R")
names(TS)
pairs(na.omit(TS[,c("Discharge","DO_mgL","pH" ,"SpCond","Temp_C","Turbidity" )]), upper.panel = panel.cor)

############################### EVALUATE DATA BY SITE ##############################----
levels(discrete$NETNCode)
"ACABIN" "ACBRKB" "ACBRWN" "ACBUBO" "ACCADS" "ACDKLI" "ACDUCK" "ACEGLO" "ACHADB" "ACHNTR" "ACHTHB" "ACJRDO"
"ACKEBO" "ACLKWO" "ACLSIE" "ACLVYB" "ACMOWB" "ACMRSL" "ACOTRC" "ACSGTB" "ACSTNL" "MABISA" "MIMASA" "MIMASB"
"MIMASC" "MORRSA" "MORRSB" "MORRSC" "MORRSD" "MORRSE" "ROVASA" "ROVASB" "ROVASC" "ROVASD" "ROVASE" "ROVASF"
"SAGASA" "SAGASB" "SAIRSA" "SAIRSB" "SARASA" "SARASB" "SARASC" "SARASD"

site<-"SARASA"
# "Discharge","DO_mgL","pH","SpCond" ,"Temp_C" ,"Turbidity"

df<-TS[TS$NETNCode %in% site,]
#  TS, TS.min, TS.max

############## Modeling temporal variation in WQ parms
############# Select value(s) you want to work with ##################
## CREATE XTS OBJECT
names(df)
df<-df[,c("date","Discharge","DO_mgL","pH","SpCond" ,"Temp_C" ,"Turbidity")]
series<-xts(df, order.by= df$date)# convert to xts for use in dygraphs
names(series)<-c("date","Discharge","DO_mgL","pH","SpCond" ,"Temp_C" ,"Turbidity")
storage.mode(series) <- "numeric" ## FORCE SERIES AS NUMERIC

## Graph data
# stacked Time series
xyplot(series[,2:7], main=site,cex.lab=1.3, cex.axis= 1.2, type="o", pch=16)
# CORR plot of site
pairs(na.omit(df[,c("Discharge","DO_mgL","pH" ,"SpCond","Temp_C","Turbidity" )]), upper.panel = panel.cor)

par(mfrow=c(2,3))
hist(df$Temp_C, main= "Temp"); sd(na.omit(site$temp))
hist(log(df$DO_mgL), main="DO_mgL")
hist(df$SpCond, main="SpCond")
hist(df$pH, main="pH")
hist(df$Discharge, main="Discharge")
hist(df$Turbidity, main="Turbidity")

### Autocorrelation analysis
par(mfrow=c(6,2), mar=c(2.25,4,2.8,2))
plot(series$Temp_C, main="Temperature", ylab="Celsius", type=c("o"))
acf(series$Temp_C, main="", na.action=na.pass)
plot(series$DO_mgL, main="DO", ylab= "mg/L", type=c("o"))
acf(log(series$DO_mgL), main="", na.action=na.pass)
plot(series$pH, main="pH", ylab= "pH units", type=c("o"))
acf(log(series$pH), main="", na.action=na.pass)
plot(series$SpCond, main="SpC", ylab= "µS/cm", type=c("o"))
acf(series$SpCond, main="", na.action=na.pass)
plot(series$Discharge, main="Discharge", ylab="Discharge (cfs)", type=c("o"))
acf(series$Discharge, main="", na.action=na.pass)
plot(series$Turbidity, main="Turbidity", ylab="Turbidity (NTU)", type=c("o"))
acf(series$Turbidity, main="", na.action=na.pass)

par(mfrow=c(1,1))
dev.off()
##### What is the best model to Estimate standard deviation of slope estimate?----
##PIck the var(s)
CharVal<-"SpCond"
CharVal2<-"Discharge"

hist(series[,CharVal], main= CharVal, xlab= CharVal);shapiro.test(series[,CharVal])
plot(series[,CharVal], ylab=CharVal, main= CharVal, type="o", pch=16)

### MODEL 1: SIMPLE OLS REGRESSION var vs time
New.time <- time(series[,CharVal])
mod1<-lm(series[,CharVal] ~ New.time, na.action=na.omit) ## OLS
summary(mod1)
sd_mod1<-summary(mod1)$tTable[2,2]*sqrt(length(mod1$fitted))
plot(ACF(mod1, maxLag=10))## clearly significant acf
coef(mod1)
hist(mod1$resid);shapiro.test(mod1$resid)

### MODEL 2: ADDITIVE REGRESSION vs time WITH AR PROCESS
New.time <- time(series[,CharVal])
mod2<-gls(series[,CharVal] ~ New.time,na.action=na.omit, correlation=corAR1(form=~New.time)) ## 
summary(mod2)
plot(ACF(mod2, maxLag=10))
sd_mod2<-summary(mod2)$tTable[2,2]*sqrt(length(mod2$fitted))
hist(mod2$resid);shapiro.test(mod2$resid)

Sea<-cycle(series[,CharVal])
New.time<-time(series[,CharVal])
mod3<-gls(series[,CharVal] ~ New.time+factor(Sea), na.action=na.omit)#, correlation=corAR1(form=~New.time)
summary(mod3)
plot(ACF(mod3, maxLag=10))
sd_mod3<-summary(mod3)$tTable[2,2]*sqrt(length(mod3$fitted))
hist(mod3$resid);shapiro.test(mod3$resid)

## What is the best model and SD estimate
sapply(list(mod1,mod2,mod3), AIC)
c(sd_mod1,sd_mod2,sd_mod3)

newtemp<-as.ts(predict(mod3, New.time))
plot(series[,1], ylab="Temp (c)", main=a)
par(new = T)
plot(newtemp, col ="red", axes=F, ylab="", xlab="")
legend('bottomleft',c('Raw', "Predicted"),col = c("black","red"), lwd=c("1","1"),bty="n", cex= 0.8)

## Check prediction
fit<-as.data.frame(mod3$fitted); fit$row<-row.names(as.data.frame(fit)); colnames(fit)<-c("fitted","rows")
rows<-row.names(as.data.frame(series[,1]))
raw<-as.data.frame(series[,1]); 
raw<-cbind(rows,raw)
diag<-join(raw,fit, by="rows")
plot(diag$x,diag$fitted); abline(lm(diag$fitted~diag$x))






