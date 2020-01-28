#### This script creates a flat file to be used with JP's R package and visualizer
 # version 12/1/2016Aaron Weed


######Load packages----
library(plyr)
library(ggplot2)
require(dygraphs)
library(xts)
library(reshape)
  

### load in functions for data query and plotting ####
# data frames from BE
source("Chemistry_DataFunc.R")
source("DiscreteDataFunc.R")

### load in data and lookup tables (functions need to be uupdated to work with 2017 BE)
chem<-NETN_Nutdata(x)
discrete<-NETN_discretedata(x)

data<-


sites <- read.csv("tblLocations.csv") ### import site metadata
month_tlu<-read.csv("month.csv", colClasses = "character") 

######################## Create output file of data ################### 
# subset dfs prior to merging
head(discrete)
temp1<-discrete[,c( "ParkCode","NETNCode", "LocationType","StartDate", "Temp_C","SpCond" ,"DO_mg.L","Do_sat" , "pH", "BP_mmHg","Discharge","TotalArea","TotalVel")]

temp2<-chem[,c( "ParkCode","NETNCode","LocationType", "StartDate","DEPTH","TYPE", "FPH",
                 "eqPH","Color_Flag","ACOLOR","TCOLOR","TColor_Flag","COND",
                 "ANC_ueqL","Ca_ueqL","Mg_ueqL","K_ueqL","K_Flag","Na_ueqL","Cl_ueqL","Cl_Flag"  ,     
                 "SO4_ueqL","SO4_Flag","NO3_ueq/L","NO3_ueq/L_Flag", "NO2+NO3_mg/L","NO2+NO3_Flag", "NO2_mg/L","NO2_Flag",      
                 "T_Dis_N_mgL","T_Dis_N_Flag","NH4_mg/L","NH4_Flag","NH3_mg/L","NH3_Flag","TN_mg/L","TN_Flag",       
                 "TP_ug/L","TP_Flag","T_Dis_P_ugL","T_Dis_P_Flag","PO4_ug/L","PO4_Flag","CHLA_ugL","CHLA_Flag" ,    
                 "DOC_mgL","DOC_Flag","DIC_mgL","Si_mgL","Al_ugL","Al_Flag","FPHMETH","CONDMETH" ,     
                 "COLORMETH","ALKMETH")]

temp3<-join(temp1, temp2, by=c("NETNCode", "StartDate"), type= "full")

temp3$"NPSTORET Org ID/Code"<-"NETN"


names(temp3)

#write.table(temp3, "allWQparms.csv", sep=",", row.names= F)

#### melt df

df.long<-melt.data.frame(temp3, id.vars=c("NPSTORET Org ID/Code", "NETNCode" , "LocationType","StartDate", "DEPTH"), measure.vars=c("Temp_C","SpCond" ,"DO_mg.L","Do_sat" , "pH", "BP_mmHg","Discharge","TotalArea","TotalVel", "FPH","eqPH", "ACOLOR","TCOLOR",
                                                                                                           "COND","ANC_ueqL","Ca_ueqL","Mg_ueqL","K_ueqL","Na_ueqL","Cl_ueqL"  , "SO4_ueqL","NO3_ueq/L","NO2+NO3_mg/L","NO2_mg/L","T_Dis_N_mgL" ,
                                                                                                           "NH4_mg/L","NH3_mg/L","TN_mg/L", "TP_ug/L","T_Dis_P_ugL","T_Dis_P_Flag","PO4_ug/L","CHLA_ugL" ,"DOC_mgL","DIC_mgL","Si_mgL",
                                                                                                           "Al_ugL","FPHMETH" , "CONDMETH","COLORMETH","ALKMETH"))

# remove missing values from value

df.long<-df.long[!is.na(df.long$value),]

df.long<-join(df.long,sites, by="NETNCode")

df.long<-df.long[c("NPSTORET Org ID/Code" ,"LocationType", "NETNCode","StartDate","variable","DEPTH", "value","ParkCode","Description")]

## rename cols to match JPs function
colnames(df.long)<-c(	"NPSTORET Org ID/Code","LocationType", "StationID"	,"Visit Start Date",	"Local Characteristic Name", "DEPTH", 	"Value","ParkCode" , "Description")
head(df.long)

## rename some levels 

df.long$'Local Characteristic Name'<-mapvalues(df.long$'Local Characteristic Name', from=c("Temp_C" , "Do_sat", "SpCond", "ANC_ueqL"), 
                    to=c("Water Temperature", "DO (%)", "Specific conductance", "ANC"))

##rename Pond to Lake for indexing in Viz
df.long$'LocationType'<-mapvalues(df.long$'LocationType', from=c("Pond"), 
                                               to=c("Lake"))
levels(df.long$LocationType)


write.table(df.long, "NETN_Water_Data_RViz.csv", sep=",", row.names= F)
# write to R viz dATA directory
write.table(df.long, "C:/Users/aweed/Documents/R/NETN/NETN_Water/NETNWQ_Viz/Data/NETN_Water_Data_RViz.csv", sep=",", row.names= F)


#######  metadata following JP's format
# cols: Network	ParkCode	ShortName	LongName	SiteCode	SiteName	lat	long	Type	CharacteristicName	DisplayName	DataName	Units	LowerPoint	UpperPoint	DataType	LowerDescription	UpperDescription	AssessmentDetails






