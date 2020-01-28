#### This script combines NETN's profile and nutirent data into on flat file for plotting

# version 12/22/206 Aaron Weed

NETNWQdata<-function(x){
######Load packages----
library(plyr)


### load in functions for data query and plotting ####
# data frames from BE
source("Chemistry_DataFunc.R")
source("DiscreteDataFunc.R")

### load in data and lookup tables
chem<-NETN_Nutdata(x)
discrete<-NETN_discretedata(x)

sites <- read.csv("tblLocations.csv") ### import site metadata
month_tlu<-read.csv("month.csv", colClasses = "character") 

######################## Create output file of data ################### 
# subset dfs prior to merging

temp1<-discrete[,c( "ParkCode","NETNCode", "StartDate", "Temp_C","SpCond" ,"DO_mg.L","Do_sat" , "pH", "BP_mmHg","Discharge","TotalArea","TotalVel")]

temp2<-chem[,c( "ParkCode","NETNCode","StartDate","DEPTH","TYPE", "FPH",
                "eqPH","Color_Flag","ACOLOR","TCOLOR","TColor_Flag","COND",
                "ANC_ueqL","Ca_ueqL","Mg_ueqL","K_ueqL","K_Flag","Na_ueqL","Cl_ueqL","Cl_Flag"  ,     
                "SO4_ueqL","SO4_Flag","NO3_ueq/L","NO3_ueq/L_Flag", "NO2+NO3_mg/L","NO2+NO3_Flag", "NO2_mg/L","NO2_Flag",      
                "T_Dis_N_mgL","T_Dis_N_Flag","NH4_mg/L","NH4_Flag","NH3_mg/L","NH3_Flag","TN_mg/L","TN_Flag",       
                "TP_ug/L","TP_Flag","T_Dis_P_ugL","T_Dis_P_Flag","PO4_ug/L","PO4_Flag","CHLA_ugL","CHLA_Flag" ,    
                "DOC_mgL","DOC_Flag","DIC_mgL","Si_mgL","Al_ugL","Al_Flag","FPHMETH","CONDMETH" ,     
                "COLORMETH","ALKMETH")]

temp3<-join(temp1, temp2, by=c("NETNCode", "StartDate"), type= "full")

temp3$"NPSTORET Org ID/Code"<-"NETN"


temp4<-join(temp3,sites, by="NETNCode")

write.table(temp4, "allWQparms.csv", sep=",", row.names= F)

temp4
}
