#### This code imports discrete (monthly) data from flat file, and plots time series and boxplots of past data plus current year values,


######Import and setup data----
library(plyr)
library(ggplot2)
require(dygraphs)
library(xts)
library(reshape2)
library(reshape)

source("NETN_WQ plotting functions.R") # load in plotting functions
#sites <- read.csv("WQ_site_locations.csv")## import site metadata (you'll want to add the missing sites to this list)
month_tlu<-read.csv("month.csv", colClasses = "character") 

#### Data import #################
## Import discrete in situ data 
# data collection started May 2010 and ran monthly until Dec 2014
discrete <- read.csv("~/NETN detail Fall 2015/Data/NETN Water quality/discreteNETN.csv")

#### Site selection option ##########################################
levels(discrete$NETNCode)
[1] "ACABIN" "ACBRKB" "ACBRWN" "ACBUBO" "ACCADS" "ACDKLI" "ACDUCK" "ACEGLO" "ACHADB" "ACHNTR" "ACHTHB" "ACJRDO" "ACKEBO" "ACLKWO" "ACLSIE"
[16] "ACLVYB" "ACMOWB" "ACMRSL" "ACOTRC" "ACSGTB" "ACSTNL" "MABISA" "MIMASA" "MIMASB" "MIMASC" "MORRSA" "MORRSB" "MORRSC" "MORRSD" "MORRSE"
[31] "ROVASA" "ROVASB" "ROVASC" "ROVASD" "ROVASE" "ROVASF" "SAGASA" "SAGASB" "SAIRSA" "SAIRSB" "SARASA" "SARASB" "SARASC" "SARASD"

"Discharge"    "DO_mg.L"      "pH"           "SpCond"       "Temp_C" 
############## Plot time series per site ###################
# interactive plot
SeriesBySiteInt(data= discrete, site="ACABIN",parm= "Temp_C")

# static plot and you can add linear trend
scattertimesite(data= discrete,site="ACABIN",parm= "Temp_C", trend ="N")

############## Plot time series per site in each park #####
# option to add linear trend line
scattertimepark(data= discrete,park="",parm= "pH", trend ="N")

############## Boxplot showing current year monthly value vs past year's variation AT SITE LEVEL
#
#curyr = current year
#parm = temp, SpC, pH, or DO 
levels(discrete$year)

#Check data for QC purposes (no y limts or thresholds) with this function
boxyrsiteQC(data= discrete, curyr= 2014,site="ACABIN",parm= "pH") ## autoscale to detect data erros

boxyrsite(data= discrete, curyr= 2014,site="ACABIN",parm= "pH")

############## Boxplot showing current year value vs past year's variation AT PARK LEVEL for all sites
boxyrpark(data= discrete, curyr= 2014,park="",parm= "pH")

############## Scatterplot showing current year value vs past year's AT PARK LEVEL for all sites

scattermonthsite(data= discrete, curyr= 2014,park="ACABIN",parm= "Temp_C")

####### Plots trend in monthly value per site ######
### not plotting the trend right now (maybe too few points)
TrendPerMonthsite(data= discrete, month = "12", site="ACABIN",parm= "Temp_C", trend = "Y")

####### Plots trend in monthly value per site at park level ######
### not plotting the trend right now (maybe too few points)

TrendPerMonthPark(data= discrete, month = "11", park="ACABIN",parm= "Temp_C", trend = "N")

####### Plots maximum value per site ######





