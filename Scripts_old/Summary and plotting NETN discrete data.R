#### this code imports and plots NETNs discrete water quality data
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

###################### LOAD IN FUNCTIONS ###############################
source("Theme.R")
source("lm_eqn.R")

#######################LOAD IN NETN WQ DATA################################
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
samps<-cast(discrete.melt,NETNCode+ Month +Year ~ variable,fun=sum)

###########################PLOTTING #########################################
#######NETWORK LEVEL #############---

### Plotting meant to be semi dynamic:
# Pick data set you want to plot from
df<-TS
#  Options : TS, TS.min, TS.max

############# Select value(s) you want to plot##################
#### Univariate
CharVal<-"pH"
# "Discharge","DO_mgL","pH","SpCond" ,"Temp_C" ,"Turbidity"
#### Bivariate
CharVal2<-"DO_mgL"

### PRIOR TO PLOTTING RUN THE FOLLOWING CODE TO SETUYP AXIS LABELS
########## DEFINE PLOT LABELS BY CHAR SLECTION----
if(CharVal == "Temp_C"){
  units<- "(C)"
  lab="Water Temperature"}
if(CharVal == "pH"){
  units<- ""
  lab="pH"}
if(CharVal == "DO_mgL (DO)"){
  units<- "(mg/l)"
  lab="Dissolved Oxygen"}
if(CharVal == "SpCond"){
  units<- "(µS/cm)"
  lab="Specific Conductance"}
if(CharVal == "Turbidity"){
  units<- "(NTU)"
  lab="Turbidity"}
if(CharVal == "Discharge"){
  units<- "(cfs)"
  lab="Discharge"}

if(CharVal2 == "Water Temperature"){
  units2<- "(C)"
  lab2="Water Temperature"}
if(CharVal2 == "pH"){
  units2<- ""
  lab2="pH"}
if(CharVal2 == "Dissolved Oxygen (DO)"){
  units2<- "(mg/l)"
  lab2="Dissolved Oxygen"}
if(CharVal2 == "Specific Conductance"){
  units2<- "(µS/cm)"
  lab2="Specific Conductance"}
if(CharVal2 == "Turbidity"){
  units2<- "(NTU)"
  lab2="Turbidity"}
if(CharVal2 == "Discharge"){
  units2<- "(cfs)"
  lab2="Discharge"}
#####----
#################################################################

##### ####Histogram #####
qplot(df[,CharVal],data=df, facets=~NETNCode, geom="histogram", xlab=paste(lab, units, sep=" "), ylab="Count") 

########Time Series PLOT with linear trend########
p<-ggplot(df, aes(date, df[,CharVal]))+ labs(x = "Date", y = paste(lab, units, sep=" "))+
  geom_point(size=2)+geom_smooth(method="lm",se=TRUE)
p+ facet_wrap(~NETNCode)

#### ADD REGRESSION STATS (IN PROGESS)
head(df)
dm<-df[,c("NETNCode","date",CharVal)]
colnames(dm)<-c("NETNCode","x","y")
eq <- ddply(dm,.(NETNCode),lm_eqn)
p <- ggplot(dm, aes(x, y))+ labs(x = "Date", y = paste(lab, units, sep=" "))+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()
 p + geom_text(data=eq,aes(x = 2006, y = 30,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_wrap(~NETNCode)

qplot(date, df[,CharVal],data=df, facets=~NETNCode, geom=c("smooth","point"), method="lm", ylab=paste(lab, units, sep=" "), xlab="Date")## time series with add regression line
## TS OF EXTREME DATA

lab<-"Tmin"
lab<-"Tmax"
qplot(Year, df[,CharVal],data=df, facets=~NETNCode, geom=c("smooth","point"), method="lm", ylab=paste(lab, units, sep=" "), xlab="Year")## time series with add regression line

################### PLOTS WITH PREVIOUS YEAR'S DATA (BY MONTH) ################
ggplot(df, aes(Month, df[,CharVal]))+labs(title= site,x = "Date", y = paste(lab, units, sep=" "))+
  geom_point(aes(colour=Year),size=2)+
  facet_wrap(~NETNCode)

##################### BIVARIATE PLOTS BY SITE FOR ENTIRE NETWORK#############
CharVal2<-"DO_mgL"

p<-ggplot(df, aes(df[,CharVal2], df[,CharVal]))+ labs(x =paste(lab2, units2, sep=" "), y = paste(lab, units, sep=" "))+geom_point(size=2, aes(colour=Year))+geom_smooth(method="lm",se=TRUE)
p+ facet_wrap(~NETNCode)

############################### PLOT DATA BY SITE ##############################----
levels(discrete$NETNCode)
"ACABIN" "ACBRKB" "ACBRWN" "ACBUBO" "ACCADS" "ACDKLI" "ACDUCK" "ACEGLO" "ACHADB" "ACHNTR" "ACHTHB" "ACJRDO"
"ACKEBO" "ACLKWO" "ACLSIE" "ACLVYB" "ACMOWB" "ACMRSL" "ACOTRC" "ACSGTB" "ACSTNL" "MABISA" "MIMASA" "MIMASB"
"MIMASC" "MORRSA" "MORRSB" "MORRSC" "MORRSD" "MORRSE" "ROVASA" "ROVASB" "ROVASC" "ROVASD" "ROVASE" "ROVASF"
"SAGASA" "SAGASB" "SAIRSA" "SAIRSB" "SARASA" "SARASB" "SARASC" "SARASD"

site<-"ROVASA"
# "Discharge","DO_mgL","pH","SpCond" ,"Temp_C" ,"Turbidity"

df<-TS[TS$NETNCode %in% site,]
#  TS, TS.min, TS.max

## TS PLOT with linear trend
ggplot(df, aes(date, df[,CharVal]))+ labs(title= site,x = "Date", y = paste(lab, units, sep=" "))+geom_point(size=4)+geom_smooth(method="lm",se=TRUE)


#### ADD REGRESSION STATS (IN PROGESS)

## Connected line TS PLOT
ggplot(df, aes(date, df[,CharVal]))+ labs(title= site,x = "Date", y = paste(lab, units, sep=" "))+geom_point(size=4)+geom_line()

## Add in horizontal assessment line
ggplot(df, aes(date, df[,CharVal]))+ labs(title= site,x = "Date", y = paste(lab, units, sep=" "))+
geom_point(size=4)+geom_line()+geom_hline(aes(yintercept =28, lwd=1), colour="red")

## Overlay by Month
ggplot(df, aes(Month, df[,CharVal]))+labs(title= site,x = "Date", y = paste(lab, units, sep=" "))+
  geom_point(aes(colour=Year),size=4)

########################### BIVARIATE PLOTS #############################
CharVal2<-"Discharge"

ggplot(df, aes(df[,CharVal2], df[,CharVal]))+ labs(title= site,x = paste(lab2, units2, sep=" "), y = paste(lab, units, sep=" "))+
  geom_point(size=4, aes(colour=factor(Year)))+geom_smooth(method="lm",se=TRUE)




####----
ggplot(as.data.frame(TS), aes(date, Temp_C)) + 
  geom_bar(stat = "identity")+ facet_wrap(~NETNCode, scales="free")

xyplot(Temp_C ~  date |NETNCode, data=TS, type="p")
 
par.strip.text=list(cex=1),
scales=list(alternating=1, x=list(cex=1, rot=90),y=list(cex=1.2)), 
par.settings=list((axis.text=list(cex=2))),
#auto.key = list(title = "Sampler",lines=TRUE)
, ylab="ln(sum animals) per m2",xlab="Year",panel=function(x,y,...){
  panel.xyplot(x,y, pch=16, type="l")
  panel.lmline(x,y,col.line="dark gray", lwd=1, lty=2)
})+layer(panel.ablineq(lm(y ~ x), r.sq = TRUE,x=1985,y=6, adj=0:1, col.text=1, fontfamily="arial", cex.text=.6))
