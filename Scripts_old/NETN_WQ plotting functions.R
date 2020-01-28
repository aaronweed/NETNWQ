
### These functions create various WQ plots for monthly discrete data collected by NETN.
### A Weed 8/25/2016; updated 11/5/2016

######### Box plots ######
## plots a single WQ parmeter per month showing historical variation (boxplot) vs current value at site level #
boxyrsite<-function(data, curyr, site, parm){
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  data<-discrete.month[discrete.month$NETNCode %in% site,]
  data<-droplevels(data)
  
  # bind in site names for plotting
  data<-join(data, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  
  data2<-data[,c("NETNCode","Year","month",parm)]
  data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Temp_C))+ labs(y = "Temperature (C)", x= "Month") + geom_boxplot(outlier.size=.5) +ylim(0,32)### plots variation in previous Year's data
    
    p<- (p+facet_wrap(~Description)+  geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ geom_hline(yintercept = 31, color ="blue") +
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
      theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
      theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
      theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
      theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    
    print(p) 
  }
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p+ facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+  
      geom_hline(yintercept = 4, color ="blue") +
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
      theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
      theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
      theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
      theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    print(p) 
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), SpCond))+ labs(y = expression("Specific Conductance (" * mu ~ "S/cm)"), x= "Month") + geom_boxplot(outlier.size=.5) 
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+  
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
      theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
      theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
      theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
      theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    print(p) 
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Month") + geom_boxplot(outlier.size=.5) + ylim(5, 10)
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+geom_hline(yintercept = 9, color ="blue") +geom_hline(yintercept = 6, color ="blue") + 
      theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
      theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
      theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
      theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
      theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
      theme(legend.key = element_rect(fill = "white", color = "black")) +
      theme(panel.background =  element_rect(fill="white", colour="black")) +
      theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])) 
    print(p)    
  }
 
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), pH))+ labs(y = "pH", x= "Month") + geom_boxplot(outlier.size=.5) + ylim(5, 10)
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+geom_hline(yintercept = 9, color ="blue") +geom_hline(yintercept = 6, color ="blue") + 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])) 
    print(p)    
  }
  
}
### FOR QC: plots a single WQ parmeter per month showing historical variation (boxplot) vs current value at site level ###

boxyrsiteQC<-function(data, curyr, site, parm){
  library(plyr)
  library(ggplot2)
  library(reshape)
  
  ######### Add in missing monthly obervations as NA
  sites <- read.csv("tblLocations.csv") ### import site metadata
    # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  data<-discrete.month[discrete.month$NETNCode %in% site,]
  data<-droplevels(data)
  
  # bind in site names for plotting
  data<-join(data, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  
  data2<-data[,c("NETNCode","Year","month",parm)]
  data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Temp_C))+ labs(y = "Temperature (C)", x= "Month") + geom_boxplot(outlier.size=.5)### plots variation in previous Year's data
    
    p<- (p+facet_wrap(~Description)+  geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    
    print(p) 
  }
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p+ facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+  
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    print(p) 
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), SpCond))+ labs(y = expression("Specific Conductance (" * mu ~ "S/cm)"), x= "Month") + geom_boxplot(outlier.size=.5) 
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+  
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    print(p) 
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])) 
    print(p)    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), pH))+ labs(y = "pH", x= "Month") + geom_boxplot(outlier.size=.5)
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2) + 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])) 
    print(p)    
  }
  
}

##### plots a single WQ parmeter per month showing historical variation (boxplot) vs current value at park level ###

boxyrpark<-function(data, curyr, park, parm){
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  # join site metadata to site data
  discrete.month<-join(discrete.month, sites, by="NETNCode")
  
  data<-discrete.month[discrete.month$ParkCode %in% park,]
  data<-droplevels(data)
  
  # bind in site names for plotting
  data<-join(data, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  
  data2<-data[,c("Description","NETNCode","Year","month",parm)]
  data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, Temp_C))+ labs(y = "Temperature (C)", x= "Month") + geom_boxplot(outlier.size=.5)+ ylim(0,32) ### plots variation in previous Year's data
          
    
        if(park== "ACAD"){
             
       p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ ### plots current Year's data
         geom_hline(yintercept = 31, color ="blue") +
         theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
         theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
         theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(legend.key = element_rect(fill = "white", color = "black")) +
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90")))
         print(p)
         
           }else{
              
          p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
           geom_hline(yintercept = 31, color ="blue") +
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
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_boxplot(outlier.size=.5) 
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
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
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, SpCond))+ labs(y = expression("Specific Conductance (" * mu ~ "S/cm)"), x= "Month") + geom_boxplot(outlier.size=.5) 
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
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
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, pH))+ labs(y = "pH", x= "Month") + geom_boxplot(outlier.size=.5)+ ylim(5, 10) 
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             geom_hline(yintercept = 9, color ="blue") + geom_hline(yintercept = 6, color ="blue") +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             geom_hline(yintercept = 9, color ="blue") + geom_hline(yintercept = 6, color ="blue") +
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
  
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Month") + geom_boxplot(outlier.size=.5)
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
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
  
}

###### Interactive (HTML) plot: plots single WQ parmeter over time period at site level ######

SeriesBySiteInt<-function(data, site, parm) {
  require(dygraphs)
  library(zoo)
  library(xts)
  units<-read.csv("tlu_Units.csv") ## table with untis for plotting
  data<-data[data$NETNCode %in% site,]
  data<-droplevels(data)
  
  
  data.raw<-data[,c("StartDate",parm)]
  time.raw <- as.POSIXct(data$StartDate, format = "%m/%d/%Y")
  series.raw<-xts(data.raw, order.by= time.raw)
  #plot.zoo(series.raw$parm)
  #str(series.raw)

  ##### Create dynamic plot
# raw data points

 y<- dygraph(series.raw[,c(parm)], main = site, xlab= "Date", ylab= units$unit[units$parm %in% parm])%>%
  dyRangeSelector()%>%
  dyOptions(drawPoints = TRUE,  pointSize = 2, strokeWidth=0)%>%
    #dySeries(parm, label = units$unit[units$parm %in% parm])%>%
    dyLegend(show = "always", hideOnMouseOut = FALSE)
  print(y)
  
  }
  
#### Scatter plots per month ##############

scattermonthsite<-function(data, curyr, site, parm){
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$Year<-as.factor(data$Year)
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  ## programmed to select only numeric variables as measure vars. 
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=  names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  data<-discrete.month[discrete.month$NETNCode %in% site,]
  data<-droplevels(data)
  
  # bind in site names for plotting
  data<-join(data, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  
  data2<-data[,c("NETNCode","Year","month",parm)]
  data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), data[,c(parm)]))+ labs(y = "Temperature (C)", x= "Month") + geom_point(colour = "black", size = 1) +ylim(0,32)### plots variation in previous Year's data
    
    p<- (p+facet_wrap(~Description)+  geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ geom_hline(yintercept = 31, color ="blue") +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    
    print(p) 
  }
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_point(colour = "black", size = 1)
    
    p<- (p+ facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+  
           geom_hline(yintercept = 4, color ="blue") +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    print(p) 
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), SpCond))+ labs(y = expression("Specific Conductance (" * mu ~ "S/cm)"), x= "Month") + geom_point(colour = "black", size = 1)
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+  
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
    print(p) 
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Month") + geom_point(colour = "black", size = 1) + ylim(5, 10)
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+geom_hline(yintercept = 9, color ="blue") +geom_hline(yintercept = 6, color ="blue") + 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])) 
    print(p)    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(as.factor(month), pH))+ labs(y = "pH", x= "Month") + geom_point(colour = "black", size = 1) + ylim(5, 10)
    
    p<- (p + facet_wrap(~Description)+geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+geom_hline(yintercept = 9, color ="blue") +geom_hline(yintercept = 6, color ="blue") + 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1])) 
    print(p)    
  }
  
}

#### Scatter plots over time

###### Plots single WQ parmeter per month  per site at park level; current Year's value in red ###

scattermonthpark<-function(data, curyr, site,park, parm) {
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  # join site metadata to site data
  discrete.month<-join(discrete.month, sites, by="NETNCode")
  
  data<-discrete.month[discrete.month$ParkCode %in% park,]
  data<-droplevels(data)
  
  # bind in site names for plotting
  data<-join(data, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  
  
  data2<-data[,c("Description","NETNCode","Year","month",parm)]
  data2<-droplevels(data2)
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, Temp_C))+ labs(y = "Temperature (C)", x= "Month") + geom_point(colour = "black", size = 1)
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             geom_hline(yintercept = 31, color ="blue") +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             geom_hline(yintercept = 31, color ="blue") +
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
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_point(colour = "black", size = 1)
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
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
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, SpCond))+ labs(y = "Specific Conductance (mS/cm)", x= "Month") + geom_point(colour = "black", size = 1) 
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
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
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, pH))+ labs(y = "pH", x= "Month") + geom_point(colour = "black", size = 1) 
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             geom_hline(yintercept = 9, color ="blue") + geom_hline(yintercept = 6, color ="blue") +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             geom_hline(yintercept = 9, color ="blue") + geom_hline(yintercept = 6, color ="blue") +
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
  
  if(parm== "Discharge"){
    ## pH per month
    p <- ggplot(data[!data$Year %in% curyr,], aes(month, Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Month") + geom_point(colour = "black", size = 1) 
    
    if(park== "ACAD"){
      
      p<- (p+ facet_wrap(~Description, ncol=3, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "black")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else{
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + geom_jitter(data= data2[data2$Year %in% curyr,], width= 0.1, color="red", size=2)+ 
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
}

###### Plots single WQ parmeter over time at park level ###

scattertimesite<-function(data, site, parm, trend, scale) {
  library(ggplot2)
  library(mgcv)
  units<-read.csv("tlu_Units.csv") ## table with untis for plotting
  data<-data[data$NETNCode %in% site,]
  data<-droplevels(data)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  if(scale == "log"){
  p <- ggplot(data, aes(StartDate, y = log(data[,c(parm)])))+ labs(y = paste("log", units$unit[units$parm %in% parm]), x= "Date") + geom_point(colour = "black", size = 1,na.rm=TRUE)
  
  }
  if(scale == "norm"){
    
    p <- ggplot(data, aes(StartDate, y = data[,c(parm)]))+ labs(y = units$unit[units$parm %in% parm], x= "Date") + geom_point(colour = "black", size = 1,na.rm=TRUE)
  }
  
  
  
  if(trend == "Y"){
    
    p<- (p+ facet_wrap(~Description, ncol=2, scales = "free_x") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
           geom_smooth(method= "lm", se= TRUE) +
           #geom_ribbon(aes( x = DT, ymin = Y.lci, ymax = Y.uci ), fill = 'gray80', alpha= 0.80) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
  }
  
  if(trend == "N"){
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free_x") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
  }
  print(p)
}

###### Plots single WQ parmeter over time in each site at park level ###

scattertimepark<-function(data, type, park, parm, trend, scale, overlay) {
  library(plyr)
  library(ggplot2)
  library(reshape)
  library(zoo)
  library(xts)
  
  data<-data[data$ParkCode %in% park,]
  data<-droplevels(data)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  
  if(type =="stream"){
    data<-data[data$LocationType == "Stream", ]
    data<-droplevels(data)
    data<-data[,c("Description", "StartDate", parm)]
  }
  
  if(type =="lk_pnd"){
    data<-data[data$LocationType == "Lake" | data$LocationType == "Pond", ]
    data<-droplevels(data)
    data<-data[,c("Description", "StartDate", parm)]
  }
  
  if(overlay =="N"){
    if(scale == "log"){
    p <- ggplot(data, aes(StartDate, y = log(data[,c(parm)])))+ labs(y = paste("log", units$unit[units$parm %in% parm]), x= "Date") + geom_point(colour = "black", size = 1,na.rm=TRUE)
      }
  if(scale == "norm"){
    p <- ggplot(data, aes(StartDate, y = data[,c(parm)]))+ labs(y = units$unit[units$parm %in% parm], x= "Date") + geom_point(colour = "black", size = 1,na.rm=TRUE)
  }
    
    if(trend == "Y"){
      
      if(park== "ACAD"){
        
        p<- (p+facet_wrap(~Description, ncol=3, scales = "fixed") + geom_point(colour = "black", size = 1.5,na.rm=TRUE) +
               scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
               geom_smooth(method= "lm", se= TRUE) +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90")))
        print(p)
        
      }else
      {
      
      p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + 
             scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
              geom_smooth(method= "lm", se= TRUE) +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90")))
        
        print(p)
      }
    }
    
    if(trend == "N"){
      
      if(park== "ACAD"){
        
        p<- (p+facet_wrap(~Description, ncol=3, scales = "fixed") + geom_point(colour = "black", size = 1,na.rm=TRUE) +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
               theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(legend.position = "top") +
               theme(legend.key = element_rect(fill = "white", color = "white")) +
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90")))
        print(p)
        
      }else
      {
     
        p<- (p+ facet_wrap(~Description, ncol=2, scales = "free") + 
               scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
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
  }
  
  if(overlay =="Y"){
    if(scale == "log"){
    p <- ggplot(data, aes(StartDate, y = log(data[,c(parm)]), colour=Description))+ labs(y = paste("log", units$unit[units$parm %in% parm]), x= "Date") + geom_point( size = 1.5,na.rm=TRUE)
    
    }
    if(scale == "norm"){
      
      p <- ggplot(data, aes(StartDate, y = data[,c(parm)], colour=Description))+ labs(y = units$unit[units$parm %in% parm], x= "Date") + geom_point( size = 1.5,na.rm=TRUE)
    }  
    
    if(trend == "Y"){
      
      p<- (p+ 
               scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
               geom_smooth(method= "lm", se= TRUE) +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(legend.position = "top", legend.title = element_blank()) +  
               theme(legend.key = element_rect(fill = "white", color = "white")) +
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90")))
        
        print(p)
      }
    
    if(trend == "N"){
        
        p<- (p+  
               scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(legend.position = "top", legend.title = element_blank()) + 
               theme(legend.key = element_rect(fill = "white", color = "white")) +
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90")))
        
        print(p)
      }
    }
  }
   

###### Plots trend in monthly value per site  ######

TrendPerMonthsite<-function(data, month, site, parm, trend){
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  data<-discrete.month[discrete.month$NETNCode %in% site & discrete.month$month %in% month ,]
  data<-droplevels(data)
  
  ## append month text for plotting
  data<-join(data,month_tlu, by="month")
  
  # bind in site names for plotting
  data<-join(data, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  
  
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
    p <- ggplot(data, aes(x=Year, y=SpCond))+ labs(y = expression("Specific Conductance (" * mu ~ "S/cm)"), x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data, aes(x=Year,y=pH))+ labs(y = "pH", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey") +ylim(6,9)
  }
  
  if(parm== "Discharge"){
    ## pH per month
    p <- ggplot(data, aes(x=Year,y=Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)+ geom_line(size=1 , color = "grey") +ylim(6,9)
  }
  
  if(trend == "Y"){
    
    
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free") + ggtitle(data$month2)+ 
           geom_smooth(method= "lm", se= TRUE) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p)
  }
  
  if(trend == "N"){
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free") + ggtitle(data$month2)+  
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "black")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))  
    
    print(p)
  }
  
}

###### Plots trend in monthly value per site at park level ###

TrendPerMonthPark<-function(data, month, park, parm, trend){
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  # bind in site names for plotting
  data<-join(discrete.month, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  head(data)
  ### Extract values by park and month
  
  data<-data[data$ParkCode %in% park & data$month %in% month ,]
    
  ## append month text for plotting
  data<-join(data,month_tlu, by="month")
 
   ## sort prior to plotting
  data<-data[order(data$Description,data$Year),]
  
  if(parm== "Temp_C"){
    ## Temperature in month per Year
    p <- ggplot(data, aes(x= Year, y = Temp_C))+ labs(y = "Temperature (C)", x= "Year") + geom_point(size = 2,na.rm=TRUE)
    # geom_line(size=1 , color = "grey")+ylim(0,31)
    
  }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data, aes(x= Year, y= DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    # geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data, aes(x= Year, y= SpCond))+ labs(y = expression("Specific Conductance (" * mu ~ "S/cm)"), x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    #geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data, aes(x=Year , y= pH))+ labs(y = "pH", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    #geom_line(size=1 , color = "grey")
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data, aes(x=Year , y= Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Year") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    #geom_line(size=1 , color = "grey")
  }
  
  if(trend == "Y"){
    
    if(park== "ACAD"){
      
    p<- (p+facet_wrap(~Description, ncol=3, scales = "free_x") + 
           geom_smooth(method= "lm", se= FALSE) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.position = "top") +
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    
    }else
      {
      p<- (p+facet_wrap(~Description, ncol=2, scales = "free_x") + ggtitle(data$month2)+ 
             geom_smooth(method= "lm", se= FALSE) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.position = "top") +
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
    }
    print(p)
  }
  
  if(trend == "N"){
    
    if(park== "ACAD"){
    
    p<- (p+facet_wrap(~Description, ncol=3, scales = "free_x") + ggtitle(data$month2)+  
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.position = "top") +
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))  
    }else
      {
        p<- (p+facet_wrap(~Description, ncol=2, scales = "free_x") + ggtitle(data$month2)+  
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
               theme(legend.position = "top") +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(legend.key = element_rect(fill = "white", color = "white")) +
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90"))) 
      }
    
    print(p)
    
  }
  
  
   
}

##### Scatter plot of monthly values (colored by month) over time in each site at park level ##

ScatterPerMonthPark<-function(data, month, park, parm, trend){
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function because only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  # bind in site names for plotting
  data<-join(discrete.month, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  head(data)
  ### Extract values by park and month
  
  if(month == ""){
    ### just index by park
    
    data<-data[data$ParkCode %in% park,]
    data<-droplevels(data)
    
  }else{
    
    # index by month too
    data<-data[data$ParkCode %in% park & data$month %in% month ,]
    
  }
  
  ## append month text for plotting
  data<-join(data,month_tlu, by="month")
  
  ## sort prior to plotting
  data<-data[order(data$Description,data$Year),]
  
  if(parm== "Temp_C"){
    ## Temperature in month per Year
    p <- ggplot(data, aes(x= Year, y = Temp_C, colour =factor(month2)))+ labs(colour = "Month",y = "Temperature (C)", x= "Year") + geom_point(size = 2,na.rm=TRUE)
    # geom_line(size=1 , color = "grey")+ylim(0,31)
    
  }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data, aes(x= Year, y= DO_mg.L, colour =factor(month2)))+ labs(colour = "Month",y = "Dissolved Oxygen (mg/L)", x= "Year") + geom_point( size = 2,na.rm=TRUE)
    # geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data, aes(x= Year, y= SpCond, colour =factor(month2)))+ labs(colour = "Month",y = expression("Specific Conductance (" * mu ~ "S/cm)"), x= "Year") + geom_point( size = 2,na.rm=TRUE)
    #geom_line(size=1 , color = "grey")
    
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data, aes(x=Year , y= pH, colour =factor(month2)))+ labs(colour = "Month",y = "pH", x= "Year") + geom_point( size = 2,na.rm=TRUE)
    #geom_line(size=1 , color = "grey")
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data, aes(x=Year , y= Discharge, colour =factor(month2)))+ labs(colour = "Month",y = "Discharge (cu ft/s)", x= "Year") + geom_point( size = 2,na.rm=TRUE)
    #geom_line(size=1 , color = "grey")
  }
  
  if(trend == "Y"){
    
    if(park== "ACAD"){
      
      p<- (p+facet_wrap(~Description, ncol=3, scales = "free_x") + 
             geom_smooth(method= "lm", se= FALSE) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.position = "top") +
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      
    }else
    {
      p<- (p+facet_wrap(~Description, ncol=2, scales = "free_x") + 
             geom_smooth(method= "lm", se= FALSE) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.position = "top") +
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
    }
    print(p)
  }
  
  if(trend == "N"){
    
    if(park== "ACAD"){
      
      p<- (p+facet_wrap(~Description, ncol=3, scales = "free_x") +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.position = "top") +
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))  
    }else
    {
      p<- (p+facet_wrap(~Description, ncol=2, scales = "free_x") +  
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 90,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
             theme(legend.position = "top") +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90"))) 
    }
    
    print(p)
    
  }
  
  
  
}

##### Bivariate plots####

BivarSite<-function(data, site, x, y, reg){
  
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  units<-read.csv("tlu_Units.csv") ## table with untis for plotting
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$Year<-as.factor(format(data$StartDate,"%Y")) #extract Year
  data$month<-as.factor(format(data$StartDate,"%m")) #extract month
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  # join site metadata to site data
  discrete.month<-join(discrete.month, sites, by="NETNCode")
  
  data<-discrete.month[discrete.month$NETNCode %in% site,]
  data<-droplevels(data)
  
  ## append month text for plotting
  data<-join(data,month_tlu, by="month")
  
  # bind in site names for plotting
  data<-join(data, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  
  data2<-data[,c("ParkCode","Description","NETNCode","Year","month","month2",x,y)]
  data2<-droplevels(data2)
  #data2<-na.omit(data2)
  
  ## fit linear model to data
  fit <- lm(data2[,c(y)] ~ data2[,c(x)], data = data2, na.action=na.omit)
  corr<-cor.test(data2[,c(y)],data2[,c(x)], method = "pearson", na.action=na.omit)
  
  #setup plot
  
  if(reg == "M"){
    
    p <- ggplot(data2, aes(x= data2[,c(x)], y = data2[,c(y)])) +  geom_point(size = 2,na.rm=TRUE) + labs(y = units$unit[units$parm %in% y], x= units$unit[units$parm %in% x]) 
        
    p<- (p+  facet_wrap(~Description, ncol=2, scales = "free") + ggtitle(data$ParkCode[1])+ 
           #annotate("text", x= min(data2[,c(x)], na.rm= TRUE)+ 0.3*max(data2[,c(x)], na.rm= TRUE), y= max(data2[,c(y)], na.rm= TRUE)- 0.2*min(data2[,c(x)], na.rm= TRUE) , label = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
           #"Intercept =",signif(fit$coef[[1]],3 )," Slope =",signif(fit$coef[[2]], 3)," P =",signif(summary(fit)$coef[2,4], 2))) +
           geom_smooth(span = 0.3)+    
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p)
    
  }
    
  if(reg == "LM"){
    
    p <- ggplot(data2, aes(x= data2[,c(x)], y = data2[,c(y)])) +  geom_point(size = 2,na.rm=TRUE) + labs(y = units$unit[units$parm %in% y], x= units$unit[units$parm %in% x]) 
    
    
    p<- (p+  facet_wrap(~Description, ncol=2, scales = "free") + ggtitle(data$ParkCode[1])+ 
           geom_smooth(method = "lm", se= TRUE)+  
           annotate("text", x= min(data2[,c(x)], na.rm= TRUE)+ 0.5*max(data2[,c(x)], na.rm= TRUE), y= max(data2[,c(y)], na.rm= TRUE)- 0.1*min(data2[,c(x)], na.rm= TRUE) , label = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
           " Intercept =",signif(fit$coef[[1]],3 )," Slope =",signif(fit$coef[[2]], 3)," P =",signif(summary(fit)$coef[2,4], 2))) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p)
    
  }
  
  if(reg == "C"){
    
    p <- ggplot(data2, aes(x= data2[,c(x)], y = data2[,c(y)], colour =factor(Year))) +  geom_point(size = 2,na.rm=TRUE)+ labs(y = units$unit[units$parm %in% y], x= units$unit[units$parm %in% x]) 
    
    
    p<- (p+  facet_wrap(~Description, ncol=2, scales = "free") + ggtitle(data$ParkCode[1])+ 
           annotate("text", x= min(data2[,c(x)], na.rm= TRUE)+ 0.5*max(data2[,c(x)], na.rm= TRUE), y= max(data2[,c(y)], na.rm= TRUE)- 0.1*min(data2[,c(x)], na.rm= TRUE) , label = paste("r = ",signif(corr$estimate, 3),
           " P =",signif(corr$p.value, 2))) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p)
    
  }
  
  
  if(reg == "N"){
    
    p <- ggplot(data2, aes(x= data2[,c(x)], y = data2[,c(y)], colour =factor(Year))) +  geom_point(size = 2,na.rm=TRUE) + labs(y = units$unit[units$parm %in% y], x= units$unit[units$parm %in% x]) 
    
    
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free") + ggtitle(data$ParkCode[1])+  
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))  
    
    print(p)
    
  }
}

### Bivariate plots per site 

BivarPark<-function(data, type, park, x, y, reg){
  
  library(plyr)
  library(ggplot2)
  library(reshape)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  units<-read.csv("tlu_Units.csv") ## table with untis for plotting
  ######### Add in missing monthly obervations as NA
  # Create Year variable (can skip)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$Year<-as.factor(format(data$StartDate,"%Y")) #extract Year
  data$month<-as.factor(format(data$StartDate,"%m")) #extract month
  
  
  ## select water body type
  if(type =="stream"){
    data<-data[data$LocationType == "Stream", ]
    data<-droplevels(data)
  }
  
  if(type =="lk_pnd"){
    data<-data[data$LocationType == "Lake" |data$LocationType == "Pond" , ]
    data<-droplevels(data)
  }
  
  ## create molten data frame (all values are represented for each site*time combination)
  discrete.melt<-melt(data, id.vars=c("NETNCode" ,"StartDate", "Year" ,"month" ), measure.vars=names(data[, sapply(data, is.numeric)]))
  #head(discrete.melt)
  
  ##### Add in missing monthly observations for final analysis file
  ## this just reshapes the dataframe, if there are more than two obs per site/month/Year combination than the 'fun' argument may have to change. 
  discrete.month<-cast(discrete.melt,NETNCode + month+ Year  ~ variable, fun= mean , add.missing=T , fill=NA )# using mean function becauise only one value per cell
  
  # sort
  discrete.month<-discrete.month[order(discrete.month$NETNCode,discrete.month$Year,discrete.month$month),]
  #head(discrete.month)
  
  # join site metadata to site data
  discrete.month<-join(discrete.month, sites, by="NETNCode")
  
  data<-discrete.month[discrete.month$ParkCode %in% park,]
  data<-droplevels(data)
  
  ## append month text for plotting
  data<-join(data,month_tlu, by="month")
  
  # bind in site names for plotting
  data<-join(data, sites[,c("ParkCode", "LocationType", "Description", "NETNCode")], by="NETNCode")
  
  data2<-data[,c("ParkCode","Description","NETNCode","Year","month","month2",x,y)]
  data2<-droplevels(data2)
  
  ## sort prior to plotting
  data2<-data2[order(data2$Description,data2$Year),]
  
  # ## fit linear model to data at each site
  # temp<-dlply(data2, .(NETNCode), function(d) lm(data2[,c(y)] ~ data2[,c(x)], data = data2, na.action=na.omit))
  # 
  # covenience function to extract model coefs
  # linmod<-function(m) { 
  #   cf <- coef(m)
  #   resids<-resid(m)
  #   r2 <- summary(m)$adj.r.squared # adjusted R-squared
  #   p <- pf(summary(m)$fstatistic[1], summary(m)$fstatistic[2],summary(m)$fstatistic[3], lower.tail = FALSE)#model Pval
  #   out <- summary(m)$coefficients[,4]#coeff P-vals
  #   data.frame(intercept = cf[1], P = out[1],Slope = cf[2], P_Slope= out[2], Rsq = r2, ModPval=p)
  # }
  # 
  # fits <- ldply(temp, linmod)
  # 
  # ## Calculate correlatin coeff per site
  # corrs<-dlply(data2, .(NETNCode), function(d) cor.test(data2[,c(y)],data2[,c(x)], method = "pearson", na.action=na.omit))
 
  #setup plot
  
  if(reg == "M"){
    
    p <- ggplot(data2, aes(x= data2[,c(x)], y = data2[,c(y)])) +  geom_point(size = 2,na.rm=TRUE) + labs(y = units$unit[units$parm %in% y], x= units$unit[units$parm %in% x]) 
    
    
    
    if(park== "ACAD"){
      
      p<- (p+facet_wrap(~Description, ncol=3, scales = "fixed") +
             geom_smooth(span = 0.3)+ 
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.position = "top", legend.title = element_blank()) +
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else
    {
    
    
    p<- (p+  facet_wrap(~Description, ncol=2, scales = "fixed") + 
           #annotate("text", x= min(data2[,c(x)], na.rm= TRUE)+ 0.3*max(data2[,c(x)], na.rm= TRUE), y= max(data2[,c(y)], na.rm= TRUE)- 0.2*min(data2[,c(x)], na.rm= TRUE) , label = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
           #"Intercept =",signif(fit$coef[[1]],3 )," Slope =",signif(fit$coef[[2]], 3)," P =",signif(summary(fit)$coef[2,4], 2))) +
           geom_smooth(span = 0.3)+    
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.position = "top", legend.title = element_blank()) +
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p)
    
    }
  }
  
  if(reg == "LM"){
    
    p <- ggplot(data2, aes(x= data2[,c(x)], y = data2[,c(y)])) +  geom_point(size = 2,na.rm=TRUE) + labs(y = units$unit[units$parm %in% y], x= units$unit[units$parm %in% x]) 
    
    
    if(park== "ACAD"){
      
      p<- (p+facet_wrap(~Description, ncol=3, scales = "fixed") +
             geom_smooth(method = "lm", se= TRUE)+
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.position = "top", legend.title = element_blank()) +
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else
    {
    
    p<- (p+  facet_wrap(~Description, ncol=2, scales = "free") + 
           #annotate("text", x= min(data2[,c(x)], na.rm= TRUE)+ 0.7*max(data2[,c(x)], na.rm= TRUE), y= max(data2[,c(y)], na.rm= TRUE)- 0.2*min(data2[,c(x)], na.rm= TRUE) , label = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
          #  " Intercept =",signif(fit$coef[[1]],3 )," Slope =",signif(fit$coef[[2]], 3)," P =",signif(summary(fit)$coef[2,4], 2))) +
           geom_smooth(method = "lm", se= TRUE)+    
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.position = "top", legend.title = element_blank()) +
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p)
    
    }
  }
  
  if(reg == "C"){
    
    p <- ggplot(data2, aes(x= data2[,c(x)], y = data2[,c(y)], colour =factor(Year))) +  geom_point(size = 2,na.rm=TRUE) + labs(y = units$unit[units$parm %in% y], x= units$unit[units$parm %in% x]) 
    
    
    if(park== "ACAD"){
      
      p<- (p+facet_wrap(~Description, ncol=3, scales = "fixed") +
             + annotate("text", x= min(data2[,c(x)], na.rm= TRUE)+ 
               0.4*max(data2[,c(x)], na.rm= TRUE), y= max(data2[,c(y)], na.rm= TRUE)- 0.2*min(data2[,c(x)], na.rm= TRUE) , label = paste("r = ",signif(corr$estimate, 3),                                                                                                                                                                               " P =",signif(corr$p.value, 2))) +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.position = "top", legend.title = element_blank()) +
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else
    {
    
    p<- (p+  facet_wrap(~Description, ncol=2, scales = "free") + 
           annotate("text", x= min(data2[,c(x)], na.rm= TRUE)+ 
          0.4*max(data2[,c(x)], na.rm= TRUE), y= max(data2[,c(y)], na.rm= TRUE)- 0.2*min(data2[,c(x)], na.rm= TRUE) , label = paste("r = ",signif(corr$estimate, 3),                                                                                                                                                                               " P =",signif(corr$p.value, 2))) +
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.position = "top", legend.title = element_blank()) +
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))
    print(p)
    
    }
  }
  
  
  if(reg == "N"){
    
    p <- ggplot(data2, aes(x= data2[,c(x)], y = data2[,c(y)], colour =factor(Year))) +  geom_point(size = 2,na.rm=TRUE) + labs(y = units$unit[units$parm %in% y], x= units$unit[units$parm %in% x]) 
    
    
    if(park== "ACAD"){
      
      p<- (p+facet_wrap(~Description, ncol=3, scales = "fixed") +
             theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
             theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
             theme(strip.text.x= element_text(size=9, face=c("bold.italic"))) +
             theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
             theme(legend.position = "top", legend.title = element_blank()) +
             theme(legend.key = element_rect(fill = "white", color = "white")) +
             theme(panel.background =  element_rect(fill="white", colour="black")) +
             theme(panel.grid.major = element_line(colour = "grey90")))
      print(p)
      
    }else
    {
    
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free") + 
           theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
           theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
           theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
           theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
           theme(legend.position = "top", legend.title = element_blank()) +
           theme(legend.key = element_rect(fill = "white", color = "white")) +
           theme(panel.background =  element_rect(fill="white", colour="black")) +
           theme(panel.grid.major = element_line(colour = "grey90")))  
    
    print(p)
    
    }
  }
}

