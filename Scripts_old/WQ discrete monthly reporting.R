
#### This code imports discrete (monthly) data from flat file, and plots time series and boxplots of past data plus current year values,
######Import and setup data----
library(plyr)
library(ggplot2)
require(dygraphs)
library(xts)
library(reshape2)
library(reshape)   

### load in functions for data query and plotting ####
#Plotting
source("NETN_WQ plotting functions.R") # load in plotting functions
# data frames from BE
source("Chemistry_DataFunc.R")
source("DiscreteDataFunc.R")
source("NETNWQdataimport.R")
#### Data import #################
### load in data individually
chem<-NETN_Nutdata(x)
discrete<-NETN_discretedata(x)

# OR, as one file
WQdata<-NETNWQdata(x)

# lookup tables
units<-read.csv("tlu_Units.csv") ## table with untis for plotting
sites <- read.csv("tblLocations.csv") ### import site metadata
month_tlu<-read.csv("month.csv", colClasses = "character") 

#### Site selection arguments ##########################################
levels(discrete$NETNCode)
[1] "ACABIN" "ACANTB" "ACBOWL" "ACBRBK" "ACBRKB" "ACBRWN" "ACBUBL" "ACBUBO" "ACCADS" "ACDKLI" "ACDKPD" "ACDUCK" "ACEAGL" "ACECHO" "ACEGLO"
[16] "ACHADB" "ACHNTR" "ACHODG" "ACHTHB" "ACJORD" "ACJRDO" "ACKEBO" "ACLBRK" "ACLHAD" "ACLKWO" "ACLONG" "ACLPIH" "ACLSIE" "ACLVYB" "ACMOWB"
[31] "ACMRSL" "ACOTRC" "ACROUN" "ACSAMP" "ACSEAL" "ACSEAW" "ACSGTB" "ACSTNL" "ACTARN" "ACUBRK" "ACUHAD" "ACWHOL" "ACWOOD" "MABIPA" "MABISA"
[46] "MIMASA" "MIMASB" "MIMASC" "MORRSA" "MORRSB" "MORRSC" "MORRSD" "MORRSE" "ROVAPA" "ROVASA" "ROVASB" "ROVASC" "ROVASD" "ROVASE" "ROVASF"
[61] "SAGAPA" "SAGASA" "SAGASB" "SAIRSA" "SAIRSB" "SARASA" "SARASB" "SARASC" "SARASD" "WEFAPA"

#Parks
levels(discrete$ParkCode)
"ACAD" "MABI" "MIMA" "MORR" "ROVA" "SAGA" "SAIR" "SARA" "WEFA"

#type:
"stream" "lk_pnd"

## variables for plotting
# Discharge"    "DO_mg.L"      "pH"           "SpCond"       "Temp_C" , "Do_sat", "TotalArea", "TotalVel"

#Variable names for plotting nutrient data
"DEPTH"          "TYPE"           "FPH"            "eqPH"           "Color_Flag"     "ACOLOR"         "TCOLOR"         "TColor_Flag"   
 "COND"           "ANC_ueqL"       "Ca_ueqL"        "Mg_ueqL"        "K_ueqL"         "K_Flag"         "Na_ueqL"        "Cl_ueqL"       
"Cl_Flag"        "SO4_ueqL"       "SO4_Flag"       "NO3_ueq/L"      "NO3_ueq/L_Flag" "NO2+NO3_mg/L"   "NO2+NO3_Flag"   "NO2_mg/L"      
"NO2_Flag"       "T_Dis_N_mgL"    "T_Dis_N_Flag"   "NH4_mg/L"       "NH4_Flag"       "NH3_mg/L"       "NH3_Flag"       "TN_mg/L"       
 "TN_Flag"        "TP_ug/L"        "TP_Flag"        "T_Dis_P_ugL"    "T_Dis_P_Flag"   "PO4_ug/L"       "PO4_Flag"       "CHLA_ugL"      
"CHLA_Flag"      "DOC_mgL"        "DOC_Flag"       "DIC_mgL"        "Si_mgL"         "Al_ugL"         "Al_Flag"        "FPHMETH"       
"CONDMETH"       "COLORMETH"      "ALKMETH"

################################ Plot time series per site or park ##################################
###########################################################################################################
# interactive plot
SeriesBySiteInt(data= WQdata, site="ACJORD",parm= "pH")

# static plot and you can add linear trend; scale = "norm" or "log"
scattertimesite(data= WQdata, site="ACJORD",parm= "pH", trend ="Y", scale ="norm")

scattertimepark(data= WQdata,type = "",park="ROVA",parm= "TP_ug/L", trend ="N", scale ="norm", overlay ="N")
# option to add linear trend line
#type = "" for all locationtypes; "stream" for streams; "lk_pnd" for lakes and ponds

####### Plots two variables against each other at a site wiht options to color by year and add stats ######
###########################################################################################################
# select reg ="LM" if you want to show regression stats of linear model
# select reg ="M" if you want to show smoothed model with SE (no stats)
# select reg ="C" if you want to show correlation w/stats

BivarSite(data=  WQdata, site = "ACMRSL", x= "Discharge", y = "SpCond", reg = "C")

####### Plots two variables against each other at a site wiht options to color by year and add stats ######
# select reg ="LM" if you want to show regression stats of linear model
# select reg ="M" if you want to show smoothed model with SE (no stats)
# select reg ="C" if you want to show correlation w/stats

#### STILL IN PROGRESS, ALL OF THE STATS NEED TO BE UPDATED TO CALC PER SITE
BivarPark(data=  WQdata, type= "stream", park = "SAGA", x= "Discharge", y = "SpCond" , reg = "N")


############## Boxplot showing current year monthly value vs past year's variation AT SITE LEVEL
#curyr = current year
#parm = temp, SpC, pH, or DO 
levels(discrete$year)

#Check data for QC purposes (no y limts or thresholds) with this function
boxyrsiteQC(data= discrete, curyr= 2014,site="ROVASF",parm= "pH") ## autoscale to detect data erros

boxyrsite(data= discrete, curyr= 2014,site="ROVASF",parm= "pH")

############## Boxplot showing current year value vs past year's variation AT PARK LEVEL for all sites
boxyrpark(data= discrete, curyr= 2014,park="MORR",parm= "pH")

############## Scatterplot showing current year value vs past year's AT indiv site

scattermonthsite(data= discrete, curyr= 2014,site="ACABIN",parm= "SpCond")

############## Scatterplot showing current year value vs past year's AT PARK LEVEL for all sites

scattermonthpark(data= discrete, curyr= 2014,park="MABI",parm= "pH")

####### Plots annual datum for a specified  monthly over time per site ######

### not plotting the trend right now (maybe too few points)

TrendPerMonthsite(data= discrete, month = "08", site="ROVASA",parm= "Temp_C", trend = "Y")

####### Plots trend in monthly value per site at park level ##
### not plotting the trend right now 

TrendPerMonthPark(data= discrete, month = "", park="MABI",parm= "pH", trend = "N")

### Scatter plot of monthly values (colored by month) over time in each site at park level

ScatterPerMonthPark(data= discrete, month = "08", park="MORR",parm= "pH", trend = "N")




