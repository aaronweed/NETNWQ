###NPS Water - visualize data
#NKL
#7/16/2019

rm(list=ls())

#load NETN_water functions:
source("~/Google Drive File Stream/My Drive/NETN Water R coding project/NETN_water_functions.R")
#there are the functions:
ls()

#read in data
waterDat <- read.csv("~/Google Drive File Stream/My Drive/NETN Water R coding project/NETN_Water_data/NETN_Water_Data.csv", stringsAsFactors=F)
waterDat$Visit.Start.Date <- as.Date(waterDat$Visit.Start.Date)
MD <- read.csv("~/Google Drive File Stream/My Drive/NETN Water R coding project/NETN_Water_data/MetaData.csv", stringsAsFactors=F)


#Plot spatio-temporal sampling effort for all variables in a loop and write to tex file directory.
variables <-unique(waterDat$Local.Characteristic.Name)
#remove special symbols from name 
variables_cleaned <- replace(variables, variables=="NO2.NO3.mgL", "NO2NO3.mgL")
variables_cleaned <- sapply(strsplit(variables_cleaned, "[.]"), "[",1)

for (i in seq_along(variables)){
		pdf(file = paste0("~/Google Drive File Stream/My Drive/NETN Water R coding project/Water_summaries/figs/SamplingEffort",variables_cleaned[i],".pdf"), height = 8, width = 8)
	plot.sampling.effort(data = waterDat, metadata = MD, variable = variables[i])
	dev.off()
	}


#plot depth profiles for the lakes only:
lakes <- unique(MD$SiteCode[which(MD$Type == "Lake")])
lake.names <- unique(MD$SiteName[which(MD$Type == "Lake")])
#remove lakes that are not deeper than 2m:
shallow.lakes <- c("NETN_ACAD_BOWL", "NETN_ACAD_SAMP", "NETN_ACAD_SEAW")
lakes <- lakes[-which(lakes %in% shallow.lakes)]
shallow.lake.names <- c("The Bowl", "Sargent Mtn Pond", "Seawall Pond")
lake.names <- lake.names[-which(lake.names %in% shallow.lake.names)]
lake.names <- gsub(" ", "", lake.names)
for(l in seq_along(lakes)){
pdf(file = paste0("~/Google Drive File Stream/My Drive/NETN Water R coding project/Water_summaries/figs/DepthProfiles",lake.names[l],".pdf"), height = 8, width = 8)
plot.depth.profiles(data = waterDat, location = lakes[l])
dev.off()
}

#use the newer plotting function to make one fig at a time for any location, time interval, months, or variable: e.g., for Eagle Lake
#pdf(file = "~/Google Drive File Stream/My Drive/NETN Water R coding project/Water_summaries/figs/EAGL_DO_DepthProfile.pdf", height = 8, width = 8)
plot.depth.profile(data = waterDat, location = "NETN_ACAD_EAGL", variable = "Temp.C", months = 6, years = 2006:2019, add.legend = T) 
#dev.off()

pdf(file = "~/Google Drive File Stream/My Drive/NETN Water R coding project/Water_summaries/figs/EAGL_DO_2018_DepthProfile.pdf", height = 8, width = 8)
plot.depth.profile(data = waterDat, location = "NETN_ACAD_EAGL", variable = "DO.mgL", years = 2018, add.legend = T) 
dev.off()


plot.depth.profile(data = waterDat, location = "NETN_ACAD_JORD", variable = "PenetrationRatio", years = 2018, add.legend = T)


#try plotting time series:
plot.ts(location = "NETN_ACAD_EAGL", variable = "SO4.ueqL")
plot.ts(location = "NETN_ACAD_JORD", variable = "SO4.ueqL")
plot.ts(location = "NETN_ACAD_JORD", variable = "Ca.ueqL")
plot.ts(location = "NETN_ACAD_JORD", variable = "NO3.ueqL")
###############################################
## function to plot time series of a variable at a location with Theil-Sen trend line:

plot.ts <- function (data=waterDat, location, variable, months = min(data$Month):max(data$Month), years=min(data$Year):max(data$Year), add.legend = T) {
	
	}
	#add option to include/calculate trend line:
	#check to be sure enough data
	#how to handle NA values?
	#Add assessment limits, or something like that?
	#Secchi Depth... make negative? It's not intuitive.
	#add to github?
	#what to write up
	
	###clone Aarons aaronweed/NETNWQ_Viz

waterDat$Month <- as.numeric(format(waterDat$Visit.Start.Date, "%m"))
waterDat$Year <- as.numeric(format(waterDat$Visit.Start.Date, "%Y"))

	
data = waterDat	
#location = "NETN_ACAD_EAGL"
#variable = "DO.mgL"
#variable = "Na.ueqL"
#location = "NETN_SARA_SA00"
#variable = "Turbidity.NTU"	
location = "NETN_ACAD_ECHO"
variable = "DOC.mgL"
#months = c(5:10)
months = min(data$Month):max(data$Month)
#years= min(data$Year):max(data$Year)
years= 2012:2017

temp <- waterDat %>%
	dplyr::filter(Local.Characteristic.Name == variable & StationID == location) %>%
	dplyr::filter(SampleDepth == "stream" | SampleDepth == "epilimnion") %>%
	dplyr::filter(Month %in% months) %>%
	dplyr::filter(Year %in% years)
	#account for detection limits
temp <- temp %>%
	mutate(value = as.numeric(ifelse(temp$Result.Value.Text == "*Present <QL", temp$Lower.Quantification.Limit, temp$Result.Value.Text))) %>%
	mutate(plotting.symbol = ifelse(temp$Result.Value.Text == "*Present <QL", 1, 19)) %>%
	mutate(plotting.color = ifelse(temp$Result.Value.Text == "*Present <QL", "red", "black"))
	#Plot time series

#calculate overall time trend, accounting for seasonality
library(openair)

temp2 <- temp %>%
	mutate(date = Visit.Start.Date)
temp2$date <- as.POSIXct(temp2$date)

out2 <- TheilSen(mydata = temp2, pollutant = "value", deseason = FALSE)

#make base plot:
plot(temp$Visit.Start.Date, temp$value, 
	bty = "l", 
	type = "o", 
	pch = temp$plotting.symbol, 
	col = temp$plotting.color, 
	xlab = "Date", 
	ylab = variable, 
	main = paste0("m = ",round(mean(out2$data$main.data$slope, na.rm=T),2),"[",round(mean(out2$data$main.data$lower, na.rm=T),2),",",round(mean(out2$data$main.data$upper, na.rm=T),2),"], p = ",round(mean(out2$data$main.data$p, na.rm=T),2))
	)

 #add trend line to plot:
slope <- mean(out2$data$main.data$slope, na.rm=T)
int <- mean(out2$data$main.data$intercept, na.rm=T)
line.type <- ifelse(mean(out2$data$main.data$p, na.rm=T) <= 0.05, 1, 2)
#'openair' TheilSen assumes intercept is at "1970/1/1"
ex <- seq(as.Date(min(temp2$date)), as.Date(max(temp2$date)), "years")
why <- int + slope*(as.numeric(format(ex, '%Y'))-1970)
points(ex, why, lty = line.type, type = "l")

#calculate monthly trends for any month with >5 observations and make a separate plot for each month:
effort.by.month <- as.data.frame(tapply(temp$value, temp$Month, length))
colnames(effort.by.month) = 'n.obs'
effort.by.month$month <- rownames(effort.by.month)
effort.by.month <- subset(effort.by.month, n.obs > 5)
temp3 <- temp2 %>%
	filter(Month %in% effort.by.month$month) 

TheilSen(mydata = temp3, pollutant = "value", deseason = TRUE, type = "month")


#to Do:
#time trend function - calculate & add in covariate if possible
#plot bivariate relationships (option to do log-log transformation)

#The NADA package accepts annual values only (no seasonality), but can incorporate a single covariate and account for non-detects. Calculates Akritas-Theil-Sen slope.
library('NADA') 


#The EnvStats package also calculates a seasonal Mann-Kendall but the output does not make sense (I'm probably using it wrong). Also, cannot accept covariates. 
library(EnvStats)
out <- kendallSeasonalTrendTest(y=value, season=temp$Month, year=temp$Year)
out$estimate[[2]] #slope
out$estimate[[3]] #intercept
line.type <- ifelse(out$p.value[[2]] > 0.05, 2, 1)

ex <- seq(2006, 2018, 1)
why <- out$estimate[[3]] + out$estimate[[2]]*(ex-2020)
abline(a = out$estimate[[3]], b = out$estimate[[2]], lty = line.type)



####################################
#integrate with NCRNWater package:
#####################################
# Install and load NCRN/NCRNWater package from github. Install it each session in case JP has made changes.
library('devtools')
install_github('NCRN/NCRNWater')
library('NCRNWater')

#what functions are available in the package?
ls('package:NCRNWater')

#import the NETN data:
dat <- importNCRNWater(Dir = "~/Google Drive File Stream/My Drive/NETN Water R coding project/NETN_Water_data")

#how to make figures?
dat2 <- getWData(dat, parkcode = "WEFA")

