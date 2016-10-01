### These functions create various WQ plots for monthly discrete data collected by NETN.
### A Weed 8/25/2016

### Still need to add in Discharge and add in site metadata for plotting and to aggregate at park scale

## imports a df

#  $ SiteGroup   : Factor w/ 2 levels "ACAD","LNETN": 1 1 1 1 1 1 1 1 1 1 ...
#  $ NETNCode    : Factor w/ 44 levels "ACABIN","ACBRKB",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ LocationType: Factor w/ 1 level "Stream": 1 1 1 1 1 1 1 1 1 1 ...
#  $ Year        : int  2006 2006 2006 2006 2006 2006 2008 2008 2008 2008 ...
#  $ StartDate   : Factor w/ 591 levels "10/1/2009","10/1/2012",..: 147 245 339 456 507 66 101 189 371 459 ...
#  $ Discharge   : num  1.14 2.4 6.26 0.229 0.353 3.54 0.314 NA NA 4.59 ...
#  $ DO_mg.L     : num  9.47 8.1 6.22 6.09 6.26 ...
#  $ pH          : num  6.44 6.45 5.98 6.17 5.61 5.62 6.21 6.3 6.17 5.76 ...
#  $ SpCond      : int  25 27 23 29 28 33 27 30 38 26 ...
#  $ Temp_C      : num  17.4 22.9 22.6 20 16.8 ...

###### plots a single WQ parmeter per month showing historical variation (boxplot) vs current value at site level ######
boxyrsite<-function(data, curyr, site, parm){
  library(plyr)
  library(ggplot2)
  library(reshape)
	month_tlu<-read.csv("month.csv", colClasses = "character") 
	sites<- read.csv("~/Desktop/NETNWQ/tblLocations.txt") # site details
	head(sites)

	######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  head(data)
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=c("Temp_C","DO_mg.L","SpCond","Discharge", "pH", "Turbidity" ))
  head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= sum , add.missing=T , fill=NA )# using sum function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  # join in site metadata
  discrete.month<-join(sites[,c("ParkCode","Description","NETNCode","StartLat","StartLon")],discrete.month, by="NETNCode")
  head(discrete.month)
  
  # index data by site
  data<-discrete.month[discrete.month$NETNCode %in% site,]
  data<-droplevels(data)
  #head(data)
 
  data2<-data[,c("NETNCode","Year","month",parm)]
  data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Temp_C))+ labs(y = "Temperature (C)", x= "Month") + geom_boxplot(outlier.size=.5) +ylim(0,32)### plots variation in previous Year's data
    
    p<- (p+facet_wrap(~Description)+  geom_point(data= data2[data2$Year %in% curyr,],  color="red", size=2)+ geom_hline(yintercept = 31, color ="blue")+
    	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
      theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
      theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
      theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
      theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])+ theme_bw())
    
    print(p) 
  }
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p+ facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+  
      geom_hline(yintercept = 4, color ="blue") +
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
      theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
      theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
      theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
      theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])+ theme_bw())
    
    print(p) 
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Month") + geom_boxplot(outlier.size=.5) 
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+  
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
      theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
      theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
      theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
      theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]) + theme_bw())
    print(p) 
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Month") + geom_boxplot(outlier.size=.5) + ylim(5, 10)
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+geom_hline(yintercept = 9, color ="blue") +geom_hline(yintercept = 6, color ="blue") + 
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
      theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
      theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
      theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
      theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])+ theme_bw()) 
    print(p)    
  }
 
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), pH))+ labs(y = "pH", x= "Month") + geom_boxplot(outlier.size=.5) + ylim(5, 10)
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+geom_hline(yintercept = 9, color ="blue") +geom_hline(yintercept = 6, color ="blue") + 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])+ theme_bw()) 
    print(p)    
  }
  
  if(parm== "Turbidity"){
  	## pH per month
  	p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Turbidity))+ labs(y = "Turbidity", x= "Month") + geom_boxplot(outlier.size=.5) 
  	
  	p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2) + 
  			 	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
  			 	theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
  			 	theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
  			 	theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
  			 	theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
  			 	theme(legend.key = element_rect(fill = "white", color = "black")) +
  			 	theme(panel.background =  element_rect(fill="white", colour="black")) +
  			 	theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETParkCodeNCode[1])+ theme_bw()) 
  	print(p)    
  }
   
}

###### FOR QC: plots a single WQ parmeter per month showing historical variation (boxplot) vs current value at site level ######

boxyrsiteQC<-function(data, curyr, site, parm){
	library(plyr)
	library(ggplot2)
	library(reshape)
	month_tlu<-read.csv("month.csv", colClasses = "character") 
	sites<- read.csv("~/Desktop/NETNWQ/tblLocations.txt") # site details
	head(sites)
	
	######### Add in missing monthly obervations as NA
	# Create Year variable (can skip)
	data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
	data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
	head(data)
	
	## create molten data frame (all values are represented for each site*time combination)
	discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=c("Temp_C","DO_mg.L","SpCond","Discharge", "pH", "Turbidity" ))
	head(discrete.melt)
	
	##### Add in missing monthly observations for final analysis file
	## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
	discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= sum , add.missing=T , fill=NA )# using sum function becauise only one value per cell
	
	# sort
	discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
	#head(discrete.month)
	
	# join in site metadata
	discrete.month<-join(sites[,c("ParkCode","Description","NETNCode","StartLat","StartLon")],discrete.month, by="NETNCode")
	head(discrete.month)
	
	# index data by site
	data<-discrete.month[discrete.month$NETNCode %in% site,]
	data<-droplevels(data)
	#head(data)
	
	data2<-data[,c("NETNCode","Year","month",parm)]
	data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Temp_C))+ labs(y = "Temperature (C)", x= "Month") + geom_boxplot(outlier.size=.5)### plots variation in previous Year's data
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2) + 
    		 	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
    		 	theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
    		 	theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    		 	theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(legend.key = element_rect(fill = "white", color = "black")) +
    		 	theme(panel.background =  element_rect(fill="white", colour="black")) +
    		 	theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETParkCodeNCode[1])+ theme_bw()) 
    
    print(p) 
  }
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2) + 
    		 	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
    		 	theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
    		 	theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    		 	theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(legend.key = element_rect(fill = "white", color = "black")) +
    		 	theme(panel.background =  element_rect(fill="white", colour="black")) +
    		 	theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETParkCodeNCode[1])+ theme_bw()) 
    
    print(p) 
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Month") + geom_boxplot(outlier.size=.5) 
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2) + 
    		 	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
    		 	theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
    		 	theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    		 	theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(legend.key = element_rect(fill = "white", color = "black")) +
    		 	theme(panel.background =  element_rect(fill="white", colour="black")) +
    		 	theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETParkCodeNCode[1])+ theme_bw()) 
    print(p) 
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2) + 
    		 	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
    		 	theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
    		 	theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    		 	theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(legend.key = element_rect(fill = "white", color = "black")) +
    		 	theme(panel.background =  element_rect(fill="white", colour="black")) +
    		 	theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETParkCodeNCode[1])+ theme_bw()) 
    print(p)    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), pH))+ labs(y = "pH", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2) + 
    		 	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
    		 	theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
    		 	theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
    		 	theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
    		 	theme(legend.key = element_rect(fill = "white", color = "black")) +
    		 	theme(panel.background =  element_rect(fill="white", colour="black")) +
    		 	theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETParkCodeNCode[1])+ theme_bw()) 
    print(p)    
  }
  
  if(parm== "Turbidity"){
  	## pH per month
  	p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Turbidity))+ labs(y = "Turbidity", x= "Month") + geom_boxplot(outlier.size=.5)
  	
  	p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2) + 
  			 	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
  			 	theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
  			 	theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
  			 	theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
  			 	theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
  			 	theme(legend.key = element_rect(fill = "white", color = "black")) +
  			 	theme(panel.background =  element_rect(fill="white", colour="black")) +
  			 	theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETParkCodeNCode[1])+ theme_bw()) 
  	print(p)    
  }
}
####### plots a single WQ parmeter per month showing historical variation (boxplot) vs current value at park level ######

boxyrpark<-function(data, curyr, park, parm){
	library(plyr)
	library(ggplot2)
	library(reshape)
	month_tlu<-read.csv("month.csv", colClasses = "character") 
	sites<- read.csv("~/Desktop/NETNWQ/tblLocations.txt") # site details
	head(sites)
	
	######### Add in missing monthly obervations as NA
	# Create Year variable (can skip)
	data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
	data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
	head(data)
	
	## create molten data frame (all values are represented for each site*time combination)
	discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=c("Temp_C","DO_mg.L","SpCond","Discharge", "pH", "Turbidity" ))
	head(discrete.melt)
	
	##### Add in missing monthly observations for final analysis file
	## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
	discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= sum , add.missing=T , fill=NA )# using sum function becauise only one value per cell
	
	# sort
	discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
	#head(discrete.month)
	
	# join in site metadata
	discrete.month<-join(sites[,c("ParkCode","Description","NETNCode","StartLat","StartLon")],discrete.month, by="NETNCode")
	head(discrete.month)
	
	# index data by site
	data<-discrete.month[discrete.month$ParkCode %in% park,]
	data<-droplevels(data)
	#head(data)
	
	data2<-data[,c("NETNCode","Year","month",parm)]
	data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, Temp_C))+ labs(y = "Temperature (C)", x= "Month") + geom_boxplot(outlier.size=.5)+ ylim(0,32) ### plots variation in previous Year's data
    
  
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free") + geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           geom_hline(yintercept = 31, color ="blue") +ggtitle(data$ParkCode[1]) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")) +theme_bw())
    
    print(p) 
  }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_boxplot(outlier.size=.5) 
    
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free") + geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+ggtitle(data$ParkCode[1]) +
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+theme_bw())
    print(p) 
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Month") + geom_boxplot(outlier.size=.5) 
    
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free") + geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+   
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+ggtitle(data$ParkCode[1]) +
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+theme_bw())
    print(p) 
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, pH))+ labs(y = "pH", x= "Month") + geom_boxplot(outlier.size=.5)+ ylim(5, 10) 
    
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free") + geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           geom_hline(yintercept = 9, color ="blue") +geom_hline(yintercept = 6, color ="blue") +ggtitle(data$ParkCode[1]) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+theme_bw()) 
    print(p)    
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+ggtitle(data$ParkCode[1]) +
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETNCode[1])+theme_bw()) 
    print(p)    
  }
	if(parm== "Turbidity"){
		## Discharge per month
		p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Turbidity))+ labs(y = "Turbidity", x= "Month") + geom_boxplot(outlier.size=.5)
		
		p<- (p + facet_wrap(~Description)+geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
				 	theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+ggtitle(data$ParkCode[1]) +
				 	theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
				 	theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
				 	theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1))+
				 	theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1))+
				 	theme(legend.key = element_rect(fill = "white", color = "black")) +
				 	theme(panel.background =  element_rect(fill="white", colour="black")) +
				 	theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$NETNCode[1])+theme_bw()) 
		print(p)    
	}
}

###### Interactive (HTML)plot: plots single WQ parmeter over time period at site level ######

SeriesBySiteInt<-function(data, site, parm) {
  require(dygraphs)
  library(reshape)
  library(zoo)
  library(xts)
  
  data<-data[data$NETNCode %in% site,]
  data<-droplevels(data)
  data.raw<-data[c("Temp_C","DO_mg.L","SpCond","pH")]
  time.raw <- as.POSIXct(data$StartDate, format = "%m/%d/%Y")
  series.raw<-xts(data.raw, order.by= time.raw)
  #plot.zoo(series.raw)
  #str(series.raw)

  ##### Create dynamic plot
# raw data points

if(parm == "Temp_C"){
 y<- dygraph(series.raw$Temp_C, main = site, xlab= "Date", ylab= "Temperature (C)")%>%
  dyRangeSelector()%>%
  dyOptions(drawPoints = TRUE, connectSeparatedPoints = FALSE, pointSize = 2)%>%
    dySeries("Temp_C", label = "Temperature (C)")%>%
    dyLegend(show = "always", labelsSeparateLines = T)
  print(y)
}
  if(parm == "DO_mg.L"){
    y<- dygraph(series.raw$DO_mg.L, main = site, xlab= "Date", ylab= "Dissolved Oxygen (mg/L)")%>%
      dyRangeSelector()%>%
      dyOptions(drawPoints = TRUE, connectSeparatedPoints = FALSE, pointSize = 2)%>%
      dySeries("DO_mg.L", label = "Dissolved Oxygen (mg/L)")%>%
      dyLegend(show = "always", labelsSeparateLines = T)
    print(y)
  }
  if(parm == "SpCond"){
    y<-dygraph(series.raw$SpCond, main = site, xlab= "Date", ylab= "Specific Conductance (mS/cm)")%>%
      dyRangeSelector()%>%
      dyOptions(drawPoints = TRUE, connectSeparatedPoints = FALSE, pointSize = 2)%>%
      dySeries("SpCond", label = "Specific Conductance (mS/cm)")%>%
      dyLegend(show = "always", labelsSeparateLines = T)
    print(y)
  }
    
    if(parm == "pH"){
     y<- dygraph(series.raw$pH, main = site, xlab= "Date", ylab= "pH")%>%
        dyRangeSelector()%>%
        dyOptions(drawPoints = TRUE, connectSeparatedPoints = FALSE, pointSize = 2)%>%
        dySeries("pH", label = "pH")%>%
        dyLegend(show = "always", labelsSeparateLines = T)
      print(y)
      
    }
  }
  
###### Plots single WQ parmeter per month  per site at park level; current Year's value in red ######

scattermonthsite<-function(data, curyr, site,park, parm) {
  library(plyr)
  library(ggplot2)
  library(reshape2)
  
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=c("Temp_C","DO_mg.L","SpCond","pH" ))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= sum , add.missing=T , fill=NA )# using sum function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  # join site metadata to site data
  discrete.month<-join(discrete.month, sites, by="NETNCode")

  data<-discrete.month[discrete.month$Park_Code %in% park,]
  data<-droplevels(data)
  
  data2<-data[,c("NETNCode","Year","month",parm)]
  data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, Temp_C))+ labs(y = "Temperature (C)", x= "Month") + geom_point(colour = "black", size = 1)
    
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "free_x") + geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    
    print(p) 
  }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_point(colour = "black", size = 1)
    
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "free_x") + geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p) 
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Month") + geom_point(colour = "black", size = 1) 
    
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "free_x") + geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+   
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p) 
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, pH))+ labs(y = "pH", x= "Month") + geom_point(colour = "black", size = 1) 
    
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "free_x") + geom_point(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))) 
    print(p)    
  }
}
  

###### Plots single WQ parmeter over time at park level ######

scattertimesite<-function(data, site, parm, trend) {
  library(ggplot2)
  library(mgcv)
  
  data<-data[data$NETNCode %in% site,]
  data<-droplevels(data)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  

  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data, aes(StartDate, Temp_C))+ labs(y = "Temperature (C)", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
  
    
      }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data, aes(StartDate, DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data, aes(StartDate, SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data, aes(StartDate, pH))+ labs(y = "pH", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(trend == "Y"){
    
    p<- (p+ facet_wrap(~NETNCode, ncol=2, scales = "free_x") +
           geom_smooth(method= "lm", se= FALSE) +
           #geom_ribbon(aes( x = DT, ymin = Y.lci, ymax = Y.uci ), fill = 'gray80', alpha= 0.80) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
  }
  
  if(trend == "N"){
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "free_x") +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))  
  }
    print(p)
}
  

###### Plots single WQ parmeter over time in each site at park level ######

scattertimepark<-function(data, park, parm, trend) {
  library(plyr)
  library(ggplot2)
  library(reshape2)
  library(zoo)
  library(xts)
  
  # join site metadata to  data
  data<-join(data, sites, by="NETNCode")
  
  data<-data[data$Park %in% park,]
  data<-droplevels(data)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data, aes(StartDate, Temp_C))+ labs(y = "Temperature (C)", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")+ylim(0,31)
    
  }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data, aes(StartDate, DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data, aes(StartDate, SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data, aes(StartDate, pH))+ labs(y = "pH", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
  }
    
    if(trend == "Y"){
      
      
      p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "fixed") + 
             geom_smooth(method= "lm", se= FALSE) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
    }
    
    if(trend == "N"){
      p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "fixed") + 
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))  
      
      
    }
    
    
    print(p) 
}


###### Plots trend in monthly value per site  ######

TrendPerMonthsite<-function(data, month, site, parm, trend){
  library(plyr)
  library(ggplot2)
  library(reshape2)
  
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=c("Temp_C","DO_mg.L","SpCond","pH" ))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= sum , add.missing=T , fill=NA )# using sum function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  data<-discrete.month[discrete.month$NETNCode %in% site & discrete.month$month %in% month ,]
  data<-droplevels(data)
  
  ## append month text for plotting
  data<-join(data,month_tlu, by="month")
  
  if(parm== "Temp_C"){
    ## Temperature in month per Year
    p <- ggplot(data, aes(x= Year, y = Temp_C))+ labs(y = "Temperature (C)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")+ylim(0,31)
    
  }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data, aes(x= Year, y= DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data, aes(x=Year, y=SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data, aes(x=Year,y=pH))+ labs(y = "pH", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey") +ylim(6,9)
  }
  
  if(trend == "Y"){
    
    
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "fixed") + ggtitle(data$month2)+ 
           geom_smooth(method= "lm", se= FALSE) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
  }
  
  if(trend == "N"){
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "fixed") + ggtitle(data$month2)+  
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))  
    
    
  }
  
  
  print(p) 
}


###### Plots trend in monthly value per site at park level ######

TrendPerMonthPark<-function(data, month, park, parm, trend){
  library(plyr)
  library(ggplot2)
  library(reshape2)
  
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=c("Temp_C","DO_mg.L","SpCond","pH" ))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= sum , add.missing=T , fill=NA )# using sum function becauise only one value per cell
  
  # join site metadata to site data
  discrete.month<-join(discrete.month, sites, by="NETNCode")
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  ### Extract values by park and month
  data<-discrete.month[discrete.month$Park_Code %in% park & discrete.month$month %in% month ,]
  data<-droplevels(data)
  
  ## append month text for plotting
  data<-join(data,month_tlu, by="month")
  
  if(parm== "Temp_C"){
    ## Temperature in month per Year
    p <- ggplot(data, aes(x= Year, y = Temp_C))+ labs(y = "Temperature (C)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")+ylim(0,31)
    
  }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data, aes(x= Year, y= DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data, aes(x= Year, y= SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data, aes(x=Year , y= pH))+ labs(y = "pH", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
  }
  
  if(trend == "Y"){
    
    
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "fixed") + ggtitle(data$month2)+ 
           geom_smooth(method= "lm", se= FALSE) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
  }
  
  if(trend == "N"){
    p<- (p+facet_wrap(~NETNCode, ncol=2, scales = "fixed") + ggtitle(data$month2)+  
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))  
    
    
  }
  
  
  print(p) 
}