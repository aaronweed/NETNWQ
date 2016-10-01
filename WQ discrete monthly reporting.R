#### This code imports discrete (monthly) data from flat file, and plots time series and boxplots of past data plus current year values,


######Import and setup data----
library(plyr)
library(ggplot2)
require(dygraphs)
library(xts)
library(reshape2)
library(reshape)

source("NETN_WQ plotting functions.R") # load in plotting functions
sites <- read.csv("tblLocations.txt") ### import site metadata


month_tlu<-read.csv("month.csv", colClasses = "character") 

#### Data import #################
## Import discrete in situ data 
# data collection started May 2010 and ran monthly until Dec 2014
discrete <- read.csv("discreteNETN.csv")

#### Site selection option ##########################################
levels(discrete$NETNCode)
[1] "ACABIN" "ACBRKB" "ACBRWN" "ACBUBO" "ACCADS" "ACDKLI" "ACDUCK" "ACEGLO" "ACHADB" "ACHNTR" "ACHTHB" "ACJRDO" "ACKEBO" "ACLKWO" "ACLSIE"
[16] "ACLVYB" "ACMOWB" "ACMRSL" "ACOTRC" "ACSGTB" "ACSTNL" "MABISA" "MIMASA" "MIMASB" "MIMASC" "MORRSA" "MORRSB" "MORRSC" "MORRSD" "MORRSE"
[31] "ROVASA" "ROVASB" "ROVASC" "ROVASD" "ROVASE" "ROVASF" "SAGASA" "SAGASB" "SAIRSA" "SAIRSB" "SARASA" "SARASB" "SARASC" "SARASD"

"Discharge"    "DO_mg.L"      "pH"           "SpCond"       "Temp_C" 

############## Plot time series per site ###################
# interactive plot
SeriesBySiteInt(data= discrete, site="MORRSC",parm= "pH")

# static plot and you can add linear trend
scattertimesite(data= discrete,site="MORRSC",parm= "pH", trend ="Y")

############## Plot time series per site in each park #####
# option to add linear trend line
scattertimepark(data= discrete,park="SAGA",parm= "pH", trend ="N")

############## Boxplot showing current year monthly value vs past year's variation AT SITE LEVEL
#curyr = current year
#parm = temp, SpC, pH, or DO 
levels(discrete$year)

#Check data for QC purposes (no y limts or thresholds) with this function
boxyrsiteQC(data= discrete, curyr= 2014,site="ROVASF",parm= "pH") ## autoscale to detect data erros

boxyrsite(data= discrete, curyr= 2014,site="ROVASF",parm= "pH")

############## Boxplot showing current year value vs past year's variation AT PARK LEVEL for all sites
boxyrpark(data= discrete, curyr= 2014,park="MORR",parm= "SpCond")

############## Scatterplot showing current year value vs past year's AT indiv site


scattermonthsite(data= discrete, curyr= 2014,site="ACABIN",parm= "SpCond")

############## Scatterplot showing current year value vs past year's AT PARK LEVEL for all sites

scattermonthpark(data= discrete, curyr= 2014,park="ROVA",parm= "SpCond")

####### Plots trend in monthly value per site ######

### not plotting the trend right now (maybe too few points)

TrendPerMonthsite(data= discrete, month = "08", site="ACABIN",parm= "Temp_C", trend = "N")


####### Plots trend in monthly value per site at park level ######
### not plotting the trend right now (maybe too few points)

TrendPerMonthPark(data= discrete, month = "08", park="MORR",parm= "pH", trend = "Y")

### Scatter plot of monthly values (colored by month) over time in each site at park level

ScatterPerMonthPark(data= discrete, month = "", park="MORR",parm= "Temp_C", trend = "N")

####### Plots two variables against each other at a site wiht options to color by year and add stats ######
# select reg ="LM" if you want to show regression stats of linear model
# select reg ="M" if you want to show smoothed model with SE (no stats)
# select reg ="C" if you want to show correlation w/stats

BivarSite(data=  discrete, site = "ACABIN", x= "Temp_C", y = "DO_mg.L", reg = "C")

####### Plots two variables against each other at a site wiht options to color by year and add stats ######
# select reg ="LM" if you want to show regression stats of linear model
# select reg ="M" if you want to show smoothed model with SE (no stats)
# select reg ="C" if you want to show correlation w/stats

BivarPark(data=  discrete, park = "MORR", x= "Discharge", y = "SpCond", reg = "LM")


