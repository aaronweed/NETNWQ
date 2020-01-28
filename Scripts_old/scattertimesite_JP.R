#### Scatterplot of WQ data over time at a site

# Updated to use import flat file generated for JP's visualizer ("output NETN data to NCRN ...R") that has physical and nutrient data 

#### Scatterplot of WQ data over time at a site

## Updating to  

scattertimesite<-function(data, site, parm, trend) {
  library(ggplot2)
  library(mgcv)
  units<-read.csv("tlu_Units.csv") ## table with untis for plotting
  
  sites <- read.csv("tblLocations.csv") ### import site metadata
  data<-data[data$NETNCode %in% site,]
  data<-droplevels(data)
  data$StartDate<-as.Date(data$StartDate, format= "%m/%d/%Y") #convert to StartDate
  data$month<-as.factor(format(data$StartDate,"%m")) #convert to month
  
  
  p <- ggplot(data, aes(StartDate, y = data[,c(parm)]))+ labs(y = units$unit[units$parm %in% parm], x= "Date") + geom_point(colour = "black", size = 2,na.rm=TRUE)
  
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