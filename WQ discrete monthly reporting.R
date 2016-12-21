
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

### load in data and lookup tables
chem<-NETN_Nutdata(x)
discrete<-NETN_discretedata(x)

sites <- read.csv("tblLocations.txt") ### import site metadata
month_tlu<-read.csv("month.csv", colClasses = "character") 

#### Data import #################
## Import discrete in situ data (streams)
# data collection started May 2010 and ran monthly until Dec 2014
discrete <- read.csv("discreteNETN.csv")

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

# discrete: Discharge"    "DO_mg.L"      "pH"           "SpCond"       "Temp_C" 

# chem:
############## Plot time series per site ###################
# interactive plot
SeriesBySiteInt(data= discrete, site="ACSTNL",parm= "pH")

# static plot and you can add linear trend
scattertimesite(data= discrete, site="ACSTNL",parm= "pH", trend ="Y")

############## Plot time series per site in each park #####
# option to add linear trend line
#type = "" for all locationtypes; "stream" for streams; "lk_pnd" for lakes and ponds
-
scattertimepark(data= discrete,type = "stream",park="ACAD",parm= "SpCond", trend ="N")

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

scattermonthpark(data= discrete, curyr= 2014,park="ROVA",parm= "pH")

####### Plots trend in monthly value per site ######

### not plotting the trend right now (maybe too few points)

TrendPerMonthsite(data= discrete, month = "10", site="ACABIN",parm= "Temp_C", trend = "N")


####### Plots trend in monthly value per site at park level ######
### not plotting the trend right now 

TrendPerMonthPark(data= discrete, month = "", park="MORR",parm= "pH", trend = "N")

### Scatter plot of monthly values (colored by month) over time in each site at park level

ScatterPerMonthPark(data= discrete, month = "08", park="MORR",parm= "pH", trend = "N")

####### Plots two variables against each other at a site wiht options to color by year and add stats ######
# select reg ="LM" if you want to show regression stats of linear model
# select reg ="M" if you want to show smoothed model with SE (no stats)
# select reg ="C" if you want to show correlation w/stats

BivarSite(data=  discrete, site = "ACMRSL", x= "SpCond", y = "Temp_C", reg = "C")

####### Plots two variables against each other at a site wiht options to color by year and add stats ######
# select reg ="LM" if you want to show regression stats of linear model
# select reg ="M" if you want to show smoothed model with SE (no stats)
# select reg ="C" if you want to show correlation w/stats

#### STILL IN PROGRESS, ALL OF THE STATS NEED TO BE UPDATED TO CALC PER SITE
BivarPark(data=  discrete, type= "stream", park = "ACAD", x= "SpCond", y = "Temp_C" , reg = "N ")




=======
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

### load in data and lookup tables
chem<-NETN_Nutdata(x)
discrete<-NETN_discretedata(x)

sites <- read.csv("tblLocations.txt") ### import site metadata
month_tlu<-read.csv("month.csv", colClasses = "character") 

#### Data import #################
## Import discrete in situ data (streams)
# data collection started May 2010 and ran monthly until Dec 2014
discrete <- read.csv("discreteNETN.csv")

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

# discrete: Discharge"    "DO_mg.L"      "pH"           "SpCond"       "Temp_C" 

# chem:
############## Plot time series per site ###################
# interactive plot
SeriesBySiteInt(data= discrete, site="ACSTNL",parm= "pH")

# static plot and you can add linear trend
scattertimesite(data= discrete, site="ACSEAW",parm= "SpCond", trend ="N")

############## Plot time series per site in each park #####
# option to add linear trend line
#type = "" for all locationtypes; "stream" for streams; "lk_pnd" for lakes and ponds
-
scattertimepark(data= discrete,type = "lk_pnd",park="ACAD",parm= "SpCond", trend ="N")

############## Boxplot showing current year monthly value vs past year's variation AT SITE LEVEL
#curyr = current year
#parm = temp, SpC, pH, or DO 
levels(discrete$year)

#Check data for QC purposes (no y limts or thresholds) with this function
boxyrsiteQC(data= discrete, curyr= 2014,site="ROVASF",parm= "pH") ## autoscale to detect data erros

boxyrsite(data= discrete, curyr= 2014,site="ROVASF",parm= "pH")

############## Boxplot showing current year value vs past year's variation AT PARK LEVEL for all sites
boxyrpark(data= discrete, curyr= 2014,park="ACAD",parm= "Temp_C")

############## Scatterplot showing current year value vs past year's AT indiv site

scattermonthsite(data= discrete, curyr= 2014,site="ACABIN",parm= "SpCond")

############## Scatterplot showing current year value vs past year's AT PARK LEVEL for all sites

scattermonthpark(data= discrete, curyr= 2014,park="ROVA",parm= "pH")

####### Plots trend in monthly value per site ######

### not plotting the trend right now (maybe too few points)

TrendPerMonthsite(data= discrete, month = "05", site="ACABIN",parm= "Temp_C", trend = "N")


####### Plots trend in monthly value per site at park level ######
### not plotting the trend right now 

TrendPerMonthPark(data= discrete, month = "", park="MORR",parm= "pH", trend = "N")

#### Scatter plot of monthly values (colored by month) over time at each site at park level ####

ScatterPerMonthPark(data= discrete, month = "08", park="ACAD",parm= "pH", trend = "N")

####### Plots two variables against each other at a site wiht options to color by year and add stats ######
# select reg ="LM" if you want to show regression stats of linear model
# select reg ="M" if you want to show smoothed model with SE (no stats)
# select reg ="C" if you want to show correlation w/stats

BivarSite(data=  discrete, site = "ACMRSL", x= "SpCond", y = "Temp_C", reg = "C")

####### Plots two variables against each other at a site wiht options to color by year and add stats ######
# select reg ="LM" if you want to show regression stats of linear model
# select reg ="M" if you want to show smoothed model with SE (no stats)
# select reg ="C" if you want to show correlation w/stats

#### STILL IN PROGRESS, ALL OF THE STATS NEED TO BE UPDATED TO CALC PER SITE
BivarPark(data=  discrete, type= "lk_pnd", park = "ACAD", x= "Discharge", y = "SpCond" , reg = "N")




