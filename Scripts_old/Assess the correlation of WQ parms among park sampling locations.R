
## Assess the correlation of WQ parms among sampling locations taken in the same month at the park scale
## this code also calcs the spatio-temporal correlation in ACAD parms below because of the large number fo sites

##load packages

library(reshape2)
library(readxl)
library(readr)

## import th df:
## data exported from NETN Water DB in Dec 2016 from Bill Gawley


streams <- read.csv("AllData_Streams10022017.csv")

lakes <- read.csv("AllData_Lakes_Surface10022017.csv")

###### DATA MANIPULATION ###########
####################################

#### Streams data manipulation

str(streams)

## subset df to include only reps "" and "1"

streams.sub<-subset(streams, is.na(streams$Rep) | streams$Rep %in% "1")

## create molten data frame (all values are represented for each site*time combination)
# needed to force as data frame for some reason for this to work.
names(streams.sub)
stream.melt<-melt(streams.sub, id.vars=c( "Park" ,"NETNCode", "Date", "WQ.Depth..m."), measure.vars=c("Temp..C.","DO..mg.L.","DO..Sat",     "pH_Lab" ,                                                                                            "Sonde.pH","SpCond..uS.cm.","Sonde.Turbidity..FNU.", "Turbidity_NTU","Discharge_cfs" ,
                                                                                                       "ANC_ueqL","AppColor_PCU" ,"DOC_mgL" ,"TP_ugL", "TotDissP_ugL", "PO4_ugL",
                                                                                                        "TN_mgL"  , "TotDissN_mgL", "NO2_mgL", "NO2.NO3_mgL"  , "NO3_ueqL", 
                                                                                                      "SO4_ueqL" ,"Cl_ueqL" ))

head(stream.melt)

# add GPS coords to df for later modeling
sites <- read.csv("~/R/NETN/NETN_Water/NETN_Water protocol projects/tblLocations.csv")
head(sites)
# grab the cols needed
coords<-sites[,c("NETNCode","StartLat", "StartLon")]
head(coords)
# bind to df
stream.melt<-join(coords,stream.melt, "NETNCode")
head(stream.melt)

## create month variable
stream.melt$Date<-as.Date(stream.melt$Date, "%m/%d/%Y")
stream.melt$month<-as.factor(format(stream.melt$Date,"%m"))
## create year variable
stream.melt$year<-as.factor(format(stream.melt$Date,"%Y"))
## create mon_year variable for spatial modeling (Sncf)
stream.melt$mon_year<-as.factor(format(stream.melt$Date,"%m%Y"))
# stream.melt$NETNCode<-as.factor(stream.melt$NETNCode)
# stream.melt$variable<-as.character(stream.melt$variable)

# create a list object corresponding to each of the parms per site for calcing corr matrix
# reshape to parms by date on long by NETNCode (wide)
## ACAD
library(reshape) # for some reason dcast in reshape2 doesn't like it when "variable" is a factor
# create a 
streams.ACAD<-cast(stream.melt,  year + month ~ NETNCode| variable, fun= function(x) mean(x, na.rm= T), subset = stream.melt$Park %in% "ACAD") 
head(streams.ACAD)

streams.LNETN<-cast(stream.melt,  year + month ~ NETNCode| variable, fun= function(x) mean(x, na.rm= T), subset = stream.melt$Park != "ACAD") 
head(streams.LNETN)



###############################
#### Lakes data manipulation####
##################################################

## create molten data frame (all values are represented for each site*time combination)
# needed to force as data frame for some reason for this to work.
names(lakes)
lake.melt<-melt(lakes, id.vars=c( "Park" ,"NETNCode", "Date","SDepth..m."), measure.vars=c("Avg.Surface.Temp..C.","Avg.Surface.DO..mg.L.","Avg.Surface.DO..Sat","Avg.Surface.pH","Avg.Surface.SpCond..uS.cm." ,
                                                                                           "Avg.Surface.Turbidity..FNU.", "ANC_ueqL","AppColor_PCU" ,"DOC_mgL" ,"ChlA_ugL" ,"TP_ugL", "TotDissP_ugL", "PO4_ugL",
                                                                                           "TN_mgL"  , "TotDissN_mgL", "NO2_mgL", "NO2.NO3_mgL"  , "NO3N_Calc_mgL" ,  "NO3_ueqL", "SO4_ueqL" ,"Cl_ueqL"  ))

str(lake.melt)

# add GPS coords to df for later modeling
sites <- read.csv("~/R/NETN/NETN_Water/NETN_Water protocol projects/tblLocations.csv")
head(sites)
# grab the cols needed
coords<-sites[,c("NETNCode","StartLat", "StartLon")]

# bind to df
lake.melt<-join(lake.melt,coords, "NETNCode")
head(lake.melt)

## create month variable
lake.melt$Date<-as.Date(lake.melt$Date,"%m/%d/%Y")
lake.melt$month<-as.factor(format(lake.melt$Date,"%m"))
## create year variable
lake.melt$year<-as.factor(format(lake.melt$Date,"%Y"))
## create mon_year variable for spatial modeling (Sncf)
lake.melt$mon_year<-as.factor(format(lake.melt$Date,"%m%Y"))


# create a list object corresponding to each of the parms per site for calcing corr matrix
# reshape to parms by date on long by NETNCode (wide)
# avging because of the presence of some repeats
lakes.ACAD<-cast(lake.melt,  year + month ~ NETNCode| variable, fun= function(x) mean(x, na.rm= T), subset = lake.melt$Park %in% "ACAD") 
head(lakes.ACAD)

# CHECK THE MATRIZ FOR ONE PARM
View(lakes.ACAD$Avg.Surface.Temp..C.)

#LNETN
lakes.LNETN<-cast(lake.melt,  year + month ~ NETNCode| variable, fun= function(x) mean(x, na.rm= T), subset = lake.melt$Park != "ACAD") 
str(lakes.LNETN)

View(lakes.LNETN$Avg.Surface.pH)


###### # calculate the correlation matrix among sites for each parm #####
####################################
library(plyr)
library(Hmisc)

 ### STREAMS
#test for one  measurement first
#pH
names(streams.ACAD)
names(streams.ACAD$Temp..C.)
cor.t<-round(cor(streams.ACAD$Sonde.pH, use = "pairwise.complete.obs" ),2)
View(cor.t)
View(streams.ACAD$Sonde.pH)


# calc corr matrices for all measurements
#ACAD
cor.out<-lapply(streams.ACAD, function (x) round(cor(x, use = "pairwise.complete.obs" ),2))
cor.dfs<-ldply(cor.out)# compile a table of the corrs

write.table(cor.dfs,"ACAD_stream_corr_matrices.csv", sep=",", row.names = F)

#LNETN

cor.out1<-lapply(streams.LNETN, function (x) round(cor(x, use = "pairwise.complete.obs" ),2))
cor.LNETN.dfs<-ldply(cor.out1)# compile a table of the corrs

write.table(cor.LNETN.dfs,"LNETN_stream_corr_matrices.csv", sep=",", row.names = F)

# contstruct correlogram
library(corrgram)
corrgram(streams.ACAD$pH,order=FALSE, lower.panel=panel.shade, upper.panel=panel.pie)

corrgram(streams.ACAD$pH,order=FALSE, panel= panel.ellipse)

####LAKES/PONDS
#ACAD
#test for one  measurement first
#pH
names(lakes.ACAD$Avg.Surface.Temp..C.)
cor.t<-round(cor(lakes.ACAD$Avg.Surface.Temp..C., use = "pairwise.complete.obs" ),2)
View(cor.t)

# calc corr matrices for all measurements
cor.out.lake<-lapply(lakes.ACAD, function (x) round(cor(x, use = "pairwise.complete.obs" ),2))
cor.lake.dfs<-ldply(cor.out.lake)# compile a table of the corrs

write.table(cor.lake.dfs,"ACAD_lakes_corr_matrices.csv", sep=",", row.names = F)

## LNETN
# calc corr matrices for all measurements
cor.out.lake2<-lapply(lakes.LNETN, function (x) round(cor(x, use = "pairwise.complete.obs" ),2))
cor.lake.NETN.dfs<-ldply(cor.out.lake2)# compile a table of the corrs

write.table(cor.lake.NETN.dfs,"LNETN_lakes_corr_matrices.csv", sep=",", row.names = F)


#### Calcuate the spatio-temporal correlation among measurements  using Sncf function #### 

## reshape for ncf functions
  # sites down by mon_yr acorss the top (wide)
#####
# STREAMS
###
streams.ACAD.ncf<-cast(stream.melt,  NETNCode+ StartLat+ StartLon~ mon_year| variable, fun= function(x) mean(x, na.rm= T), subset = stream.melt$Park %in% "ACAD") 
names(streams.ACAD.ncf)

View(streams.ACAD.ncf$Temp..C.)

#lakes/pond

## first change factor level names in lakes to streams only for the YSI measurements

names(lake.melt)

intersect(levels(stream.melt$variable), levels(lake.melt$variable))# what names match?

lake.melt$variable<-mapvalues(lake.melt$variable, from= c("Avg.Surface.Temp..C.","Avg.Surface.DO..mg.L.","Avg.Surface.DO..Sat","Avg.Surface.pH",             
                                            "Avg.Surface.SpCond..uS.cm." , "Avg.Surface.Turbidity..FNU."), 
                                            to= c("Temp..C.","DO..mg.L.","DO..Sat","Sonde.pH","SpCond..uS.cm.","Sonde.Turbidity..FNU."))

lakes.ACAD.ncf<-cast(lake.melt,  NETNCode+ StartLat+ StartLon~ mon_year| variable, fun= function(x) mean(x, na.rm= T), subset = lake.melt$Park %in% "ACAD") 
head(lakes.ACAD.ncf)
View(lakes.ACAD.ncf$Temp..C.)

#### calculat the spatial correlation

# setup up data frames
# select wheteher working with streams or lake data
df<-streams.ACAD.ncf
df<-lakes.ACAD.ncf

# grab longitude
x.ncf<-as.vector(df$Temp..C.[,3])
x.ncf<-as.numeric(x.ncf)

# grab latitude
y.ncf<-as.vector(df$Temp..C.[,2])
y.ncf<-as.numeric(y.ncf)

plot(x.ncf,y.ncf)


library(ncf)

#NCFs per one measurement

# choose var to model: Temp

z.ncf<-as.matrix(df$Temp..C.[,4:ncol(df$Temp..C.)])

fit.tempncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.tempncf)
plot.Sncf(fit.tempncf, xmax=10, text=T , main = "Temp (C)")


# choose var to model pH
z.ncf<-as.matrix(df$Sonde.pH[,4:ncol(df$Sonde.pH)])

fit.pH.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.pH.ncf)
plot.Sncf(fit.pH.ncf, xmax=10, text=T)


# choose var to model: SpC
z.ncf<-as.matrix(df$SpCond..uS.cm.[,4:ncol(df$SpCond..uS.cm.)])

fit.SpC.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.SpC.ncf)
plot.Sncf(fit.SpC.ncf, xmax=10, text=T)

# choose var to model: Discharge (only works with streams)
z.ncf<-as.matrix(df$Discharge_cfs[,4:ncol(df$Discharge_cfs)])

fit.Dis.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.Dis.ncf)
plot.Sncf(fit.Dis.ncf, xmax=10, text=T)

# choose var to model: ANC
z.ncf<-as.matrix(df$ANC_ueqL[,4:ncol(df$ANC_ueqL)])

fit.ANC.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.ANC.ncf)
plot.Sncf(fit.ANC.ncf, xmax=10, text=T)

# choose var to model: Turbidity
z.ncf<-as.matrix(df$Sonde.Turbidity..FNU.[,4:ncol(df$Sonde.Turbidity..FNU.)])

fit.Turbidity.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.Turbidity.ncf)
plot.Sncf(fit.Turbidity.ncf, xmax=10, text=T)

# choose var to model: DO
z.ncf<-as.matrix(df$DO..mg.L.[,4:ncol(df$DO..mg.L.)])

fit.DO.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.DO.ncf)
plot.Sncf(fit.DO.ncf, xmax=10, text=T)

# choose var to model: DOC
z.ncf<-as.matrix(df$DOC_mgL[,4:ncol(df$DOC_mgL)])

fit.DOC.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.DOC.ncf)
plot.Sncf(fit.DOC.ncf, xmax=10, text=T)

# choose var to model: TP
z.ncf<-as.matrix(df$TP_ugL[,4:ncol(df$TP_ugL)])

fit.TP.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.TP.ncf)
plot.Sncf(fit.TP.ncf, xmax=10, text=T)

# choose var to model: TN
z.ncf<-as.matrix(df$TN_mgL[,4:ncol(df$TN_mgL)])

fit.TN.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.TN.ncf)
plot.Sncf(fit.TN.ncf, xmax=10, text=T)

# choose var to model: App Color
z.ncf<-as.matrix(df$AppColor_PCU[,4:ncol(df$AppColor_PCU)])

fit.AC.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.AC.ncf)
plot.Sncf(fit.AC.ncf, xmax=10, text=T)

# choose var to model: Cl
z.ncf<-as.matrix(df$Cl_ueqL[,4:ncol(df$Cl_ueqL)])

fit.Cl.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.Cl.ncf)
plot.Sncf(fit.Cl.ncf, xmax=10, text=T)

# choose var to model: SO4_ueqL
z.ncf<-as.matrix(df$SO4_ueqL[,4:ncol(df$SO4_ueqL)])

fit.SO4.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.SO4.ncf)
plot.Sncf(fit.SO4.ncf, xmax=10, text=T)


# choose var to model: 
z.ncf<-as.matrix(df$NO2_mgL[,4:ncol(df$NO2_mgL)])

fit.NO3.ncf<-Sncf(x=x.ncf,y=y.ncf,z=z.ncf,resamp=1000, type= "boot", latlon= TRUE, na.rm = TRUE, quiet=TRUE)
summary(fit.NO3.ncf)
plot.Sncf(fit.NO3.ncf, xmax=10, text=T)
