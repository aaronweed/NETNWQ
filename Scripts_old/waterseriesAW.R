#### Scatterplot of WQ data over time at a site

# Updated to use import flat file generated for JP's visualizer ("output NETN data to NCRN ...R") that has physical and nutrient data 

scattertimesite<-function(data, site, parm, trend) {
  library(ggplot2)
  library(mgcv)
  sites <- read.csv("tblLocations.csv") ### import site metadata
  data<-data[data$NETNCode %in% site,]
  data<-droplevels(data)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  if(parm== "Temp_C"){
    ## Temperature per month
    p <- ggplot(data, aes(StartDate, Temp_C))+ labs(y = "Temperature (C)", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    #+ geom_line(size=1 , color = "grey")
  }
  
  if(parm== "DO_mg.L"){
    ## DO_mg.L per month
    p <- ggplot(data, aes(StartDate, DO_mg.L))+ labs(y = "Dissolved Oxygen (mg/L)", x= "Month") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    #+ geom_line(size=1 , color = "grey")
  }
  
  if(parm== "SpCond"){
    ## SpCond per month
    p <- ggplot(data, aes(StartDate, SpCond))+ labs(y = expression("Specific Conductance (" * mu ~ "S/cm)"), x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    #+ geom_line(size=1 , color = "grey")
  }
  
  if(parm== "pH"){
    ## pH per month
    p <- ggplot(data, aes(StartDate, pH))+ labs(y = "pH", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    #+ geom_line(size=1 , color = "grey")
  }
  
  if(parm== "Discharge"){
    ## Discharge per month
    p <- ggplot(data, aes(StartDate, Discharge))+ labs(y = "Discharge (cu ft/s)", x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)
    #+ geom_line(size=1 , color = "grey")
  }
  
  if(trend == "Y"){
    
    p<- (p+ facet_wrap(~Description, ncol=2, scales = "free_x") +
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
    p<- (p+facet_wrap(~Description, ncol=2, scales = "free_x") +
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