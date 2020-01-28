# WQ power analysis Spring 2016 Aaron Weed
### This code imports discrete (monthly) data from the MIDN WQ protocol
### Code then manipulated data and conducts power analysis on simulated mean increases in each parmeter for 4, 8, and 12 years
## power analysis for Appendix B in the protocol

######Import and setup data----
library(plyr)
library(lattice)
library(latticeExtra)
library(boot)
library(pwr)
library(Kendall)
library(nlme)
library(TTR)
require(dygraphs)
library(xts)
library(reshape)
library(rtf)
library(ggplot2)

source("power_functions.R")# load in powReg functions

## Import discrete in situ data, complied by Nate Jan 8 2015
# data collection started May 2010 and ran monthly until Dec 2014
discrete <- read.csv("NETN_Water_Data_RViz.csv")
str(discrete)

discrete$Value<-as.numeric(as.character(discrete$Value))

discrete$Visit.Start.Date<-as.Date(discrete$Visit.Start.Date, format= "%Y-%m-%d") #convert to StartDate
discrete$Year<-as.factor(format(discrete$Visit.Start.Date,"%Y")) #extract Year
discrete$month<-as.factor(format(discrete$Visit.Start.Date,"%m")) #extract month
#
head(discrete)

##################### POWER TO DETECT CHANGES IN MONTHLY VALUES OF SELECTED PARMS ###############################


################### Calc monthly avg's and SEs for each core parameter ####################
summ<-function (x) c(mean =mean(x,na.rm = TRUE),sd= sd(x,na.rm = TRUE), N= length(x))
names(discrete)
### This is calcing the mean over all depths in lakes and ponds. Check with Bill to see if that is OK.

avg.mon<-cast(discrete, ParkCode+ LocationType+ StationID  + month + Local.Characteristic.Name ~ ., summ, value ="Value")
head(avg.mon)
avg.mon$SE<-avg.mon$sd/sqrt(avg.mon$N)

names(avg.mon)

###### Clean up data
#Remove vars with 5 or more N 
final<-avg.mon[avg.mon$N >4,]
#Remove vars with NAs in SE
final<-final[!is.na(final$SE),]
#Remove vars with NAs in mean
final<-final[!is.na(final$mean),]
# sort
final<-final[order(final$StationID,final$variable,final$month),]

### Power analysis
############################# etimate power in ability to detect different mean changes over time ##########################
## use pwrReg function

## Rename df for conven
b<-final

#Set some parms
alpha= 0.05
n.yr.period<-10

#### INCREASE IN MEAN: Calculate power to detect 10, 20 and 50% increases in metric for 4, 8 and 12 years

b$inc.10.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=1.1)
b$inc.20.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=1.2)
b$inc.50.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=1.5)

b$inc.10.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=1.1)
b$inc.20.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=1.2)
b$inc.50.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=1.5)

b$inc.10.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=1.1)
b$inc.20.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=1.2)
b$inc.50.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=1.5)

final.mean.inc<-b

write.table(final.mean.inc,"power_WQ_inc.csv", sep=",", row.names=FALSE)


##### DECREASE IN MEAN: Calculate power to detect 10, 20 and 50% increases in metric for 4, 8 and 12 years

b$inc.10.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=0.9)
b$inc.20.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=0.8)
b$inc.50.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=0.5)

b$inc.10.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=0.9)
b$inc.20.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=0.8)
b$inc.50.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=0.5)

b$inc.10.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=0.9)
b$inc.20.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=0.8)
b$inc.50.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=0.5)


final.mean.dec<-b

write.table(final.mean.dec,"power_WQ_DEC.csv", sep=",", row.names=FALSE)

#################  Power analysis of annual extremes, all sites ###################################
min2<-function (x) {## function to grab max and min remving NAs first
  x<-x[!is.na(x)]
  c(min(x), length(x))
}

max2<-function (x) {## function to grab max and min remving NAs first
  x<-x[!is.na(x)]
  c(max(x), length(x))
}

--------### Estimate Annual MIN, all parm------
ext.min<-cast(discrete, ParkCode+ LocationType+ StationID  + Year + Local.Characteristic.Name ~ ., min2, value ="Value")
colnames(ext.min)<-c("ParkCode", "LocationType" , "StationID",  "Year","variable","min", "N")
head(ext.min)

# Est summary stats of annual min among years
ext.avg.min<-cast(ext.min, ParkCode+ LocationType+ StationID  + variable ~ ., summ, value ="min")
colnames(ext.avg.min)<-c("ParkCode", "LocationType" , "StationID",  "variable","mean","sd","N")
head(ext.avg.min)
ext.avg.min$SE<-ext.avg.min$sd/sqrt(ext.avg.min$N)

###### Clean up data
#Remove vars with 5 or more N 
final.min<-ext.avg.min[ext.avg.min$N >4,]

#Remove vars with NAs in mean
final.min<-final.min[final.min$mean !="Inf",]
# sort
final.min<-final.min[order(final.min$StationID,final.min$variable),]
final.min$parm<-"min"


----### Estimate Annual  MAX all parms----
ext.max<-cast(discrete, ParkCode+ LocationType+ StationID  + Year + Local.Characteristic.Name ~ ., max2, value ="Value")
colnames(ext.max)<-c("ParkCode", "LocationType" , "StationID",  "Year","variable","max", "N")
head(ext.max)

# Est summary stats of annual min among years
ext.avg.max<-cast(ext.max, ParkCode+ LocationType+ StationID  + variable ~ ., summ, value ="max")
colnames(ext.avg.max)<-c("ParkCode", "LocationType" , "StationID",  "variable","mean","sd","N")
head(ext.avg.max)
ext.avg.max$SE<-ext.avg.max$sd/sqrt(ext.avg.max$N)

###### Clean up data
#Remove vars with 5 or more N 
final.max<-ext.avg.max[ext.avg.max$N >4,]

#Remove vars with NAs in mean
final.max<-final.max[final.max$mean !="-Inf",]
# sort
final.max<-final.max[order(final.max$StationID,final.max$variable),]
final.max$parm<-"max"

### Power analysis
## use pwrReg function

## Pick a variable
b<-final.min
b<-final.max

#Set some parms
alpha= 0.05
n.yr.period<-10

######## Calculate power to detect 10, 20 and 50% increases in metric for 4, 8 and 12 years

b$inc.10.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=1.1)
b$inc.20.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=1.2)
b$inc.50.4yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=4,rchange=1.5)

b$inc.10.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=1.1)
b$inc.20.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=1.2)
b$inc.50.8yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=8,rchange=1.5)

b$inc.10.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=1.1)
b$inc.20.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=1.2)
b$inc.50.12yr<-regPwr(mn=b$mean,se=b$SE,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=12,rchange=1.5)


## save to final
final.min<-b
final.max<-b

## append for later plotting
final.min$type<-"min"
final.max$type<-"max"

### write tables
write.table(final.min,"power_WQ_max.csv", sep=",", row.names=FALSE)
write.table(final.max,"power_WQ_min.csv", sep=",", row.names=FALSE)

########################################################################################################

########################-------------PLOT /SUMMARIZE RESULTS-------------------------###################

########################################################################################################
# ASSIGN dfs
df<-final.mean.inc
df<-final.mean.dec

names(df)

######################    MONLTHY DATA     ######################
### setup for summary and plotting
# subset
df<-df[,c("ParkCode","StationID","LocationType", "month", "variable","inc.10.4yr" , "inc.50.4yr" , "inc.10.8yr","inc.50.8yr",  "inc.10.12yr" ,"inc.50.12yr")]

df$LocationType<-mapvalues(df$LocationType, from=c("Pond"), to = "Lake")

colnames(df)<-c("ParkCode", "StationID","LocationType", "month", "parm","10% 4yr","50% 4yr","10% 8yr","50% 8yr","10% 12yr","50% 12yr")
head(df)

dfmelt<-melt.data.frame(df, id.vars=c("ParkCode", "StationID","LocationType", "month", "parm"), measure.vars = c("10% 4yr","50% 4yr","10% 8yr","50% 8yr","10% 12yr","50% 12yr"))
head(dfmelt)

##### Summarize by location type, month, and parameter
# for table export
df.wide<-cast(dfmelt, ParkCode + LocationType+ parm + month ~variable , value = "value", fun = summ, subset= LocationType =="Stream")
head(df.wide)
colnames(df.wide)<-c("ParkCode", "LocationType" ,"parm","month","10% 4yrmean","10% 4yr_se"  , "10% 4yr_N","50% 4yr_mean","50% 4yr_se","50% 4yr_N","10% 8yr_mean",
         "10% 8yr_se","10% 8yr_N","50% 8yr_mean","50% 8yr_se","50% 8yr_N","10% 12yr_mean","10% 12yr_se","10% 12yr_N","50% 12yr_mean","50% 12yr_se" ,"50% 12yr_N" )

write.table(df.wide,"clipboard", sep=",", row.names=FALSE)

######  boxplots to show variation in power by argument
levels(dfmelt$ParkCode) #vels(dfmelt$ParkCode)
[1] "ACAD" "MABI" "MIMA" "MORR" "ROVA" "SAGA" "SAIR" "SARA" "WEFA"
levels(dfmelt$parm)

phys<-c("Water Temperature" , "pH", "Specific conductance","DO_mg.L" )
nuts<-c( "NH3_mg/L"  ,"NH4_mg/L" ,"TN_mg/L"  ,"TP_ug/L" ,  "T_Dis_N_mgL","CHLA_ugL" )
acid<-c("PO4_ug/L", "SO4_ueqL", "NO2_mg/L" , "ANC", "NO2+NO3_mg/L"   )
salts<-c("Ca_ueqL", "Na_ueqL" , "Mg_ueqL" ,"Cl_ueqL", "Al_ugL" , "K_ueqL"   )
#trophic <-c("TP_ug/L" , "CHLA_ugL")

# or slect all parks (
df.plot<-dfmelt
# subset by park and parm set
df.plot<-subset(dfmelt, ParkCode == "WEFA")

df.plot<-df.plot[df.plot$parm %in% nuts,]
df.plot<-droplevels(df.plot)

y2<-ggplot(df.plot, aes(x=variable, y= value)) + labs(y = "Power", x= "") +
  geom_boxplot(aes(colour= LocationType))
 
y2
summary(y2)
y2<-(y2+facet_grid(month ~ parm) + 
       theme(legend.position = "top") +
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12 * 0.8,face="bold"))+
       theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 * 0.8, face="bold")) +
       theme(strip.text.x= element_text(size=10, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90"))+
       theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
       theme(strip.background= element_rect(size=10, color="gray" )))
y2

#### Tabular summary

month.sum<-cast(dfmelt, LocationType+ parm + month ~variable , value = "value", fun = summ, margins = TRUE)



############################# ANNUAL EXTREME DATA #############################
#combine max and min data 
df<-rbind(final.min,final.max)
#$type vector diff max and min data

# subset
df<-df[,c("ParkCode","StationID","LocationType", "variable","type","inc.10.4yr" , "inc.50.4yr" , "inc.10.8yr","inc.50.8yr",  "inc.10.12yr" ,"inc.50.12yr")]

df$LocationType<-mapvalues(df$LocationType, from=c("Pond"), to = "Lake")

colnames(df)<-c("ParkCode", "StationID","LocationType", "parm","type", "10% 4yr","50% 4yr","10% 8yr","50% 8yr","10% 12yr","50% 12yr")
head(df)

dfmelt<-melt.data.frame(df, id.vars=c("ParkCode", "StationID","LocationType", "type", "parm"), measure.vars = c("10% 4yr","50% 4yr","10% 8yr","50% 8yr","10% 12yr","50% 12yr"))
head(dfmelt)

# subset by park and parm set
df.plot<-subset(dfmelt, ParkCode == "WEFA")
# or slect all parks (
df.plot<-dfmelt
# subset by parm set
df.plot<-df.plot[df.plot$parm %in% nuts,]
df.plot<-droplevels(df.plot)


##################### BOXPLOTS ################
y2<-ggplot(df.plot, aes(x=variable, y= value)) + labs(y = "Power", x= "") +
  geom_boxplot(aes(colour= LocationType))

summary(y2)
y2<-(y2+facet_grid(type~parm) + 
       theme(legend.position = "top") +
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12 * 0.8,face="bold"))+
       theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 * 0.8, face="bold")) +
       theme(strip.text.x= element_text(size=10, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90"))+
       theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
       theme(strip.background= element_rect(size=10, color="gray" )))
#+scale_x_discrete(limits=rev(data$newvars),)

y2

#### Tabular summary

month.sum<-cast(dfmelt, LocationType+ parm + month ~variable , value = "value", fun = summ, margins = TRUE)


########################################################################################
#############                 CORRELATION AMONG PARMS                ################
########################################################################################

source("panel_cor.R")
## make data into wide format

df.wide<- cast(discrete, ParkCode+ LocationType+ StationID  + month ~ Local.Characteristic.Name , fun= function(x)mean(x, rm.na= TRUE), value ="Value")
names(df.wide)

phys<-c("Water Temperature" , "pH", "Specific conductance","DO_mg.L", "Discharge" )
nuts<-c( "NH3_mg/L"  ,"TN_mg/L"  ,"TP_ug/L" ,  "T_Dis_N_mgL","CHLA_ugL"  )
acid<-c("PO4_ug/L", "SO4_ueqL", "NO2_mg/L" , "ANC", "NO2+NO3_mg/L" ,"NO3_ueq/L" , "Al_ugL"   )
salts<-c("Ca_ueqL", "Na_ueqL" , "Mg_ueqL" ,"Cl_ueqL", "Al_ugL" , "K_ueqL"   )

nut_phys<-c(phys ,"TP_ug/L")
ac_phys<-c(phys,acid)
sal_phys<-c("Water Temperature" , "pH", "Specific conductance","DO_mg.L", salts  )

### create subset dfs for comparisons
z<-na.omit(df.wide[, nuts])
head(z)

z<-z[z$'TP_ug/L' < 500,] ## outlier at MORRSC Aug 2011

pairs(z, upper.panel = panel.cor, cex.labels =1.5)



##### Plot var in analytes per site/park
# subset

z<-df.wide[c("ParkCode", "LocationType",  "StationID", "month", nuts)]

head(z[,1:8])

dfmelt<-melt.data.frame(z, id.vars=c("ParkCode", "StationID","LocationType", "month"), measure.vars = c(nuts))
head(dfmelt)


y2<-ggplot(dfmelt, aes(x=variable, y= value)) + labs(y = "", x= "") +
  geom_boxplot(aes(colour= month))

y2<-(y2+facet_wrap(~ParkCode, ncol= 3, scales = "free_y") + 
       theme(legend.position = "top") +
       theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 12 * 0.8,face="bold"))+
       theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 14 * 0.8, face="bold")) +
       theme(strip.text.x= element_text(size=10, face=c("bold.italic"))) +
       theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
       theme(panel.background =  element_rect(fill="white", colour="black")) +
       theme(panel.grid.major = element_line(colour = "grey90"))+
       theme(plot.title=element_text(size=15, vjust=2, face= "bold")) +
       theme(strip.background= element_rect(size=10, color="gray" )))
#+scale_x_discrete(limits=rev(data$newvars),)

y2




